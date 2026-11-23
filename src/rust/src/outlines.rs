//! Glyph outlines, for drawing a scene's text as paths.
//!
//! Most devices draw text as native glyphs, which need no outlines, so they
//! are only computed when R draws a scene as paths: from the scene's glyph
//! table (font, glyph id, origin, and transform) and the fonts it names.

use extendr_api::prelude::{List, Robj, list};
use typst::text::Font;

use crate::error::RenderError;
use crate::scene::{Fonts, Glyphs, Points, find_font, flatten_cubic, push, r_int};

/// Flattened outlines of a scene's glyphs, in the scene's coordinates.
#[derive(Debug, Default)]
pub struct Outlines {
    pub points: Points,
    /// Row in `glyphs` of each point: one path per glyph.
    pub owner: Vec<i32>,
    /// First point of each glyph's outline and how many points it has, so a
    /// run of glyphs takes a contiguous range of points.
    pub point: Vec<i32>,
    pub n_points: Vec<i32>,
}

impl Outlines {
    pub fn into_r_list(self) -> List {
        let Outlines {
            points: p,
            owner,
            point,
            n_points,
        } = self;
        list!(
            x = p.x,
            y = p.y,
            sub = p.sub,
            owner = owner,
            point = point,
            n_points = n_points
        )
    }
}

/// Outlines of `glyphs`, whose `font` column indexes `fonts`.
pub fn glyph_outlines(fonts: &Fonts, glyphs: &Glyphs) -> Result<Outlines, RenderError> {
    let resolved = (0..fonts.path.len())
        .map(|i| resolve_font(fonts, i))
        .collect::<Result<Vec<_>, _>>()?;
    let faces = resolved
        .iter()
        .zip(&fonts.variations)
        .map(|(font, variations)| face_of(font, variations))
        .collect::<Result<Vec<_>, _>>()?;

    let mut out = Outlines::default();
    let mut sub = 0;
    for i in 0..glyphs.id.len() {
        let face = usize::try_from(glyphs.font[i] - 1)
            .ok()
            .and_then(|row| faces.get(row))
            .ok_or_else(|| unavailable(format!("glyph {} has no font", i + 1)))?;
        let id = u16::try_from(glyphs.id[i])
            .map_err(|_| unavailable(format!("glyph {} has no valid id", i + 1)))?;

        let first = out.points.x.len();
        let mut sink = OutlineSink {
            out: &mut out,
            sub: &mut sub,
            owner: r_int(i + 1),
            origin: (glyphs.x[i], glyphs.y[i]),
            transform: [glyphs.xx[i], glyphs.xy[i], glyphs.yx[i], glyphs.yy[i]],
            per_unit: 1.0 / f64::from(face.units_per_em()),
            last: (0.0, 0.0),
        };
        // Colour glyphs without an outline draw nothing.
        face.outline_glyph(ttf_parser::GlyphId(id), &mut sink);
        out.point.push(r_int(first + 1));
        out.n_points.push(r_int(out.points.x.len() - first));
    }
    Ok(out)
}

/// Font `i` of a scene's font table.
fn resolve_font(fonts: &Fonts, i: usize) -> Result<Font, RenderError> {
    find_font(
        fonts.store_index[i],
        &fonts.path[i],
        fonts.collection_index[i],
        &fonts.cache_key[i],
    )
    .map_err(unavailable)
}

/// The face of `font` at the variable font coordinates `variations`, as the
/// scene's text used it.
fn face_of<'a>(font: &'a Font, variations: &str) -> Result<ttf_parser::Face<'a>, RenderError> {
    let mut face = ttf_parser::Face::parse(font.data().as_slice(), font.index())
        .map_err(|err| unavailable(format!("cannot read font {}: {err}", font.info().family)))?;
    for item in variations.split(',').filter(|item| !item.is_empty()) {
        let parsed = item.split_once('=').and_then(|(tag, value)| {
            let tag: [u8; 4] = tag.as_bytes().try_into().ok()?;
            Some((
                ttf_parser::Tag::from_bytes(&tag),
                value.parse::<f32>().ok()?,
            ))
        });
        let Some((tag, value)) = parsed else {
            return Err(unavailable(format!("invalid font variation `{item}`")));
        };
        // Axes the font lacks are ignored, as Typst does.
        face.set_variation(tag, value);
    }
    Ok(face)
}

fn unavailable(message: String) -> RenderError {
    RenderError::OutlinesUnavailable { message }
}

/// The font and glyph columns of R lists, as the scene gave them.
pub fn from_r(fonts: &List, glyphs: &List) -> Result<(Fonts, Glyphs), RenderError> {
    let fonts = Fonts {
        store_index: integers(fonts, "store_index")?,
        path: strings(fonts, "path")?,
        collection_index: integers(fonts, "collection_index")?,
        cache_key: strings(fonts, "cache_key")?,
        variations: strings(fonts, "variations")?,
        ..Fonts::default()
    };
    let glyphs = Glyphs {
        font: integers(glyphs, "font")?,
        id: integers(glyphs, "id")?,
        x: doubles(glyphs, "x")?,
        y: doubles(glyphs, "y")?,
        xx: doubles(glyphs, "xx")?,
        xy: doubles(glyphs, "xy")?,
        yx: doubles(glyphs, "yx")?,
        yy: doubles(glyphs, "yy")?,
        ..Glyphs::default()
    };

    let n = fonts.path.len();
    let m = glyphs.id.len();
    let same_length = [
        fonts.store_index.len(),
        fonts.collection_index.len(),
        fonts.cache_key.len(),
        fonts.variations.len(),
    ]
    .iter()
    .all(|&len| len == n)
        && [
            glyphs.font.len(),
            glyphs.x.len(),
            glyphs.y.len(),
            glyphs.xx.len(),
            glyphs.xy.len(),
            glyphs.yx.len(),
            glyphs.yy.len(),
        ]
        .iter()
        .all(|&len| len == m);
    if !same_length {
        return Err(unavailable(
            "the scene's tables have uneven columns".to_string(),
        ));
    }
    Ok((fonts, glyphs))
}

fn column(list: &List, name: &str) -> Result<Robj, RenderError> {
    list.iter()
        .find(|(key, _)| *key == name)
        .map(|(_, value)| value)
        .ok_or_else(|| unavailable(format!("the scene has no column `{name}`")))
}

fn integers(list: &List, name: &str) -> Result<Vec<i32>, RenderError> {
    column(list, name)?
        .as_integer_vector()
        .ok_or_else(|| unavailable(format!("column `{name}` is not an integer vector")))
}

fn doubles(list: &List, name: &str) -> Result<Vec<f64>, RenderError> {
    column(list, name)?
        .as_real_vector()
        .ok_or_else(|| unavailable(format!("column `{name}` is not a double vector")))
}

fn strings(list: &List, name: &str) -> Result<Vec<String>, RenderError> {
    column(list, name)?
        .as_string_vector()
        .ok_or_else(|| unavailable(format!("column `{name}` is not a character vector")))
}

/// Receives a glyph outline in font units and records it flattened in the
/// scene's coordinates.
struct OutlineSink<'a> {
    out: &'a mut Outlines,
    sub: &'a mut i32,
    owner: i32,
    origin: (f64, f64),
    /// Points per em: `xx`, `xy`, `yx`, `yy`.
    transform: [f64; 4],
    /// Ems per font unit.
    per_unit: f64,
    last: (f64, f64),
}

impl OutlineSink<'_> {
    fn map(&self, x: f32, y: f32) -> (f64, f64) {
        let (u, v) = (f64::from(x) * self.per_unit, f64::from(y) * self.per_unit);
        let [xx, xy, yx, yy] = self.transform;
        (
            self.origin.0 + xx * u + xy * v,
            self.origin.1 + yx * u + yy * v,
        )
    }

    /// Flatten a cubic from the last point, in the scene's coordinates.
    fn cubic(&mut self, c1: (f64, f64), c2: (f64, f64), p: (f64, f64)) {
        let mut points = Vec::new();
        flatten_cubic(self.last, c1, c2, p, 0, &mut points);
        for point in points {
            self.emit(point);
        }
    }

    fn emit(&mut self, p: (f64, f64)) {
        push(&mut self.out.points, p, *self.sub);
        self.out.owner.push(self.owner);
        self.last = p;
    }
}

impl ttf_parser::OutlineBuilder for OutlineSink<'_> {
    fn move_to(&mut self, x: f32, y: f32) {
        *self.sub += 1;
        let p = self.map(x, y);
        self.emit(p);
    }

    fn line_to(&mut self, x: f32, y: f32) {
        let p = self.map(x, y);
        self.emit(p);
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        let (p0, q, p) = (self.last, self.map(x1, y1), self.map(x, y));
        let c1 = (
            p0.0 + 2.0 / 3.0 * (q.0 - p0.0),
            p0.1 + 2.0 / 3.0 * (q.1 - p0.1),
        );
        let c2 = (p.0 + 2.0 / 3.0 * (q.0 - p.0), p.1 + 2.0 / 3.0 * (q.1 - p.1));
        self.cubic(c1, c2, p);
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        let (c1, c2, p) = (self.map(x1, y1), self.map(x2, y2), self.map(x, y));
        self.cubic(c1, c2, p);
    }

    fn close(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fonts;
    use crate::scene::Scene;
    use crate::testing::scene_of;
    use typst::layout::Frame;

    fn outlines_of(scene: &Scene) -> Outlines {
        match glyph_outlines(&scene.fonts, &scene.glyphs) {
            Ok(outlines) => outlines,
            Err(err) => panic!("outlines should be computed: {err}"),
        }
    }

    fn range(values: &[f64]) -> (f64, f64) {
        values
            .iter()
            .fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), &v| {
                (lo.min(v), hi.max(v))
            })
    }

    /// Ink bounds of `frame`, y up from its bottom, as (min x, max x, min y,
    /// max y).
    fn ink_up(frame: &Frame) -> (f64, f64, f64, f64) {
        let Some(ink) = crate::bounds::ink_bounds(frame) else {
            panic!("the frame should have ink");
        };
        let height = frame.height().to_pt();
        (ink.min_x, ink.max_x, height - ink.max_y, height - ink.min_y)
    }

    #[test]
    fn outlines_belong_to_their_glyphs_and_match_the_ink() {
        let (frame, scene) = scene_of("Hxgy");
        let o = outlines_of(&scene);
        assert!(!o.points.x.is_empty());
        let owners: std::collections::BTreeSet<i32> = o.owner.iter().copied().collect();
        assert_eq!(owners.into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 4]);

        // Each glyph's points are contiguous, one glyph after another.
        assert_eq!(o.point[0], 1);
        for i in 1..o.point.len() {
            assert_eq!(o.point[i], o.point[i - 1] + o.n_points[i - 1]);
        }

        // Font bounding boxes include Bézier control points, so the outline
        // lies inside them, a little within the box.
        let (ink_min_x, _, ink_min_y, _) = ink_up(&frame);
        let (min_x, _) = range(&o.points.x);
        let (min_y, _) = range(&o.points.y);
        assert!(
            min_x >= ink_min_x - 1e-6 && min_x - ink_min_x < 0.1,
            "{min_x} vs {ink_min_x}"
        );
        assert!(
            min_y >= ink_min_y - 1e-6 && min_y - ink_min_y < 0.1,
            "{min_y} vs {ink_min_y}"
        );
    }

    #[test]
    fn skewed_and_rotated_text_follows_its_transform() {
        // The plain glyph's outline, relative to its origin.
        let (_, plain) = scene_of("H");
        let o = outlines_of(&plain);
        let (gx, gy) = (plain.glyphs.x[0], plain.glyphs.y[0]);
        let relative: Vec<(f64, f64)> = o
            .points
            .x
            .iter()
            .zip(&o.points.y)
            .map(|(x, y)| (x - gx, y - gy))
            .collect();

        let (sin, cos) = 30f64.to_radians().sin_cos();
        for (body, [xx, xy, yx, yy]) in [
            (
                "#scale(x: 200%, y: 100%, reflow: true)[H]",
                [2.0, 0.0, 0.0, 1.0],
            ),
            // Typst turns clockwise for positive angles; y points up here.
            ("#rotate(30deg, reflow: true)[H]", [cos, sin, -sin, cos]),
        ] {
            let (_, scene) = scene_of(body);
            let o = outlines_of(&scene);
            let (gx, gy) = (scene.glyphs.x[0], scene.glyphs.y[0]);
            let expected: Vec<(f64, f64)> = relative
                .iter()
                .map(|(u, v)| (gx + xx * u + xy * v, gy + yx * u + yy * v))
                .collect();
            let xs: Vec<f64> = expected.iter().map(|p| p.0).collect();
            let ys: Vec<f64> = expected.iter().map(|p| p.1).collect();
            // Flattening adapts to the drawn size, so compare the extent.
            for (actual, expected) in [
                (range(&o.points.x), range(&xs)),
                (range(&o.points.y), range(&ys)),
            ] {
                assert!(
                    (actual.0 - expected.0).abs() < 0.01 && (actual.1 - expected.1).abs() < 0.01,
                    "{body}: {actual:?} vs {expected:?}"
                );
            }
        }
    }

    #[test]
    fn fonts_are_found_again_when_the_store_index_is_stale() {
        // As for a scene saved in another session, whose store indices no
        // longer match: the built-in font is found by its cache key.
        let (_, scene) = scene_of("H");
        let expected = outlines_of(&scene);
        let mut fonts = Fonts {
            store_index: vec![i32::MAX],
            ..Fonts::default()
        };
        fonts.path.clone_from(&scene.fonts.path);
        fonts
            .collection_index
            .clone_from(&scene.fonts.collection_index);
        fonts.cache_key.clone_from(&scene.fonts.cache_key);
        fonts.variations.clone_from(&scene.fonts.variations);
        let Ok(found) = glyph_outlines(&fonts, &scene.glyphs) else {
            panic!("the built-in font should be found by its cache key");
        };
        assert_eq!(found.points.x, expected.points.x);
        assert_eq!(found.points.y, expected.points.y);
    }

    #[test]
    fn variations_are_applied_to_the_face() {
        let (_, scene) = scene_of("H");
        let Ok(store_index) = usize::try_from(scene.fonts.store_index[0]) else {
            panic!("store index should be non-negative");
        };
        let Some(font) = fonts::get_fonts().font(store_index) else {
            panic!("the font should be in the store");
        };
        // Libertinus has no weight axis: the setting is accepted and ignored.
        assert!(face_of(&font, "wght=700").is_ok());
        assert!(face_of(&font, "").is_ok());
        assert!(face_of(&font, "weight=700").is_err());
        assert!(face_of(&font, "wght=heavy").is_err());
    }

    #[test]
    fn a_missing_font_is_an_error() {
        let (_, scene) = scene_of("H");
        let fonts = Fonts {
            store_index: vec![i32::MAX],
            path: vec![String::new()],
            collection_index: vec![0],
            cache_key: vec!["Missing-0-0000000000000000".to_string()],
            variations: vec![String::new()],
            ..Fonts::default()
        };
        assert!(matches!(
            glyph_outlines(&fonts, &scene.glyphs),
            Err(RenderError::OutlinesUnavailable { .. })
        ));
    }
}
