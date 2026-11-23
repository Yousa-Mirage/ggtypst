//! A drawable description of a Typst page, for R to draw with grid.
//!
//! The frame tree is walked once, accumulating group transforms the way
//! typst-svg does, and flattened into plain tables in page coordinates: points
//! from the page's bottom-left corner, y pointing up, as grid draws them.
//!
//! - glyphs: which font and glyph to draw where, and how it is transformed;
//!   enough to draw them natively, or to compute their outlines (outlines.rs)
//!   when they are drawn as paths;
//! - shapes and their points: lines, rectangles, and curves, flattened;
//! - clips and their points: the outlines of clip regions;
//! - runs: what to draw, in Typst's drawing order, one grob each;
//! - fonts: the font files the glyphs come from.
//!
//! Rows and points are numbered from 1, so R can index with them directly.
//! Text that native glyphs cannot draw (stroked or skewed) clears `glyph_ok`,
//! so R draws it as paths. Everything else can be drawn either way: content
//! grid cannot draw at all (gradients, tilings, images) is drawn as images.

use std::any::Any;
use std::collections::HashMap;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{LazyLock, Mutex};

use extendr_api::prelude::{List, list};
use typst::foundations::Bytes;
use typst::foundations::Smart;
use typst::layout::{Abs, Frame, FrameItem, Point, Transform};
use typst::text::{Font, FontInstance, FontStyle, TextItem};
use typst::visualize::{
    Curve, CurveItem, FillRule, Geometry, ImageScaling, LineCap, LineJoin, Paint,
};
use typst_kit::fonts::FontPath;
use typst_layout::Page;

use crate::fonts;
use crate::raster::{Rasters, rasterize};

/// Maximum distance, in points, between a flattened curve and the true curve.
const FLATTEN_TOLERANCE_PT: f64 = 0.005;

/// Grid line widths (`lwd`, 1/96 in) per Typst point (1/72 in).
const LWD_PER_PT: f64 = 96.0 / 72.0;

/// Glyphs to draw natively, one row per glyph.
#[derive(Debug, Default)]
pub struct Glyphs {
    /// Row in `Fonts`.
    pub font: Vec<i32>,
    pub id: Vec<i32>,
    /// Glyph origin on the baseline.
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    /// Font size after transforms, in points.
    pub size: Vec<f64>,
    /// Rotation in degrees, counter-clockwise like grid's `rot`.
    pub rot: Vec<f64>,
    /// The glyph's transform, in points per em from its origin: a point
    /// `(u, v)` of the glyph, in ems, is drawn at
    /// `(x + xx * u + xy * v, y + yx * u + yy * v)`.
    pub xx: Vec<f64>,
    pub xy: Vec<f64>,
    pub yx: Vec<f64>,
    pub yy: Vec<f64>,
    /// `#RRGGBBAA`.
    pub fill: Vec<String>,
    /// `#RRGGBBAA`, or `NA` for no stroke.
    pub stroke: Vec<Option<String>>,
    /// Stroke width as grid's `lwd`.
    pub lwd: Vec<f64>,
}

/// Points of flattened subpaths.
#[derive(Debug, Default)]
pub struct Points {
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    /// Subpath id, unique within the table.
    pub sub: Vec<i32>,
}

/// Lines, rectangles, and curves, one row per shape.
#[derive(Debug, Default)]
pub struct Shapes {
    /// `#RRGGBBAA`, or `NA` for no fill.
    pub fill: Vec<Option<String>>,
    /// `#RRGGBBAA`, or `NA` for no stroke.
    pub stroke: Vec<Option<String>>,
    /// Stroke width as grid's `lwd`.
    pub lwd: Vec<f64>,
    /// `"winding"` or `"evenodd"`, as for `grid::pathGrob()`.
    pub rule: Vec<String>,
    /// `"butt"`, `"round"`, or `"square"`.
    pub lineend: Vec<String>,
    /// `"mitre"`, `"round"`, or `"bevel"`.
    pub linejoin: Vec<String>,
}

/// Where each clip region's points are in `Scene::clip_points`, one row per
/// clip region.
#[derive(Debug, Default)]
pub struct Clips {
    pub point: Vec<i32>,
    pub n_points: Vec<i32>,
    /// The clip region this one is inside (row of `clips`), or 0. Content is
    /// drawn clipped by its region and all regions around it.
    pub parent: Vec<i32>,
}

/// What to draw, in Typst's drawing order: one grob per run.
#[derive(Debug, Default)]
pub struct Runs {
    /// `"glyph"` (consecutive glyphs), `"path"` (a closed or filled shape),
    /// `"polyline"` (an open line), or `"raster"` (an image, row of
    /// `rasters`).
    pub kind: Vec<String>,
    /// First row in `glyphs` or `shapes`, and how many rows the run draws.
    pub row: Vec<i32>,
    pub n_rows: Vec<i32>,
    /// First point in `shape_points` and how many points the run draws; 0 for
    /// glyph runs, whose outlines are computed on demand.
    pub point: Vec<i32>,
    pub n_points: Vec<i32>,
    /// Clip region (row of `clips`) the run is drawn in, or 0.
    pub clip: Vec<i32>,
    /// Consecutive runs in the same clip region share a segment.
    pub segment: Vec<i32>,
}

/// Font files used by the glyphs, one row per font.
#[derive(Debug, Default)]
pub struct Fonts {
    /// Index in the global font store, to fetch embedded font data.
    pub store_index: Vec<i32>,
    /// Font file path, or empty for a font embedded in the binary.
    pub path: Vec<String>,
    /// Index of the font in its file (font collections).
    pub collection_index: Vec<i32>,
    /// Stable name for caching an embedded font's data in a file.
    pub cache_key: Vec<String>,
    pub family: Vec<String>,
    pub weight: Vec<i32>,
    /// `"normal"`, `"italic"`, or `"oblique"`.
    pub style: Vec<String>,
    /// Variable font coordinates, such as `"wght=700,wdth=100"`, or empty.
    pub variations: Vec<String>,
}

#[derive(Debug, Default)]
pub struct Scene {
    pub glyphs: Glyphs,
    pub shapes: Shapes,
    pub shape_points: Points,
    pub clips: Clips,
    pub clip_points: Points,
    pub runs: Runs,
    pub rasters: Rasters,
    pub fonts: Fonts,
    /// Everything can be drawn from `glyphs` and `shapes`.
    pub glyph_ok: bool,
    /// Why native glyphs cannot draw the scene, for diagnostics.
    pub unsupported: Vec<String>,
}

impl Scene {
    pub fn into_r_list(self) -> List {
        let Scene {
            glyphs: g,
            shapes: s,
            shape_points: sp,
            clips: c,
            clip_points: cp,
            runs: r,
            rasters,
            fonts: f,
            glyph_ok,
            unsupported,
        } = self;
        list!(
            glyphs = list!(
                font = g.font,
                id = g.id,
                x = g.x,
                y = g.y,
                size = g.size,
                rot = g.rot,
                xx = g.xx,
                xy = g.xy,
                yx = g.yx,
                yy = g.yy,
                fill = g.fill,
                stroke = g.stroke,
                lwd = g.lwd
            ),
            shapes = list!(
                fill = s.fill,
                stroke = s.stroke,
                lwd = s.lwd,
                rule = s.rule,
                lineend = s.lineend,
                linejoin = s.linejoin
            ),
            shape_points = list!(x = sp.x, y = sp.y, sub = sp.sub),
            clips = list!(point = c.point, n_points = c.n_points, parent = c.parent),
            clip_points = list!(x = cp.x, y = cp.y, sub = cp.sub),
            runs = list!(
                kind = r.kind,
                row = r.row,
                n_rows = r.n_rows,
                point = r.point,
                n_points = r.n_points,
                clip = r.clip,
                segment = r.segment
            ),
            rasters = rasters.into_r_list(),
            fonts = list!(
                store_index = f.store_index,
                path = f.path,
                collection_index = f.collection_index,
                cache_key = f.cache_key,
                family = f.family,
                weight = f.weight,
                style = f.style,
                variations = f.variations
            ),
            glyph_ok = glyph_ok,
            unsupported = unsupported
        )
    }
}

/// Describe everything `page` draws. Items that grid can draw neither as
/// glyphs nor as paths are drawn as images at `raster_dpi`.
pub fn extract_scene(page: &Page, raster_dpi: f64) -> Scene {
    let frame = &page.frame;
    let mut builder = SceneBuilder {
        scene: Scene {
            glyph_ok: true,
            ..Scene::default()
        },
        font_rows: HashMap::new(),
        page: page.clone(),
        raster_dpi,
        next_shape_sub: 0,
        next_clip_sub: 0,
        current_clip: 0,
    };
    builder.frame(frame, Transform::identity());

    // Typst's y axis points down from the page's top; grid's points up from
    // its bottom.
    let mut scene = builder.scene;
    let height = frame.height().to_pt();
    for y in [
        &mut scene.glyphs.y,
        &mut scene.shape_points.y,
        &mut scene.clip_points.y,
        &mut scene.rasters.y,
    ] {
        y.iter_mut().for_each(|y| *y = height - *y);
    }
    scene
}

struct SceneBuilder {
    scene: Scene,
    /// (Store index, variations) -> row in `scene.fonts`.
    font_rows: HashMap<(usize, String), i32>,
    next_shape_sub: i32,
    next_clip_sub: i32,
    /// Clip region of the group being walked, or 0.
    current_clip: i32,
    /// The page, for rendering items as images with its settings.
    page: Page,
    raster_dpi: f64,
}

impl SceneBuilder {
    /// Note that native glyphs cannot draw the scene, and why.
    fn no_glyphs(&mut self, reason: &str) {
        self.scene.glyph_ok = false;
        if !self.scene.unsupported.iter().any(|r| r == reason) {
            self.scene.unsupported.push(reason.to_string());
        }
    }

    fn frame(&mut self, frame: &Frame, ts: Transform) {
        for (pos, item) in frame.items() {
            // Same composition order as typst-svg: item position, then the
            // group's own transform.
            let ts = ts.pre_concat(Transform::translate(pos.x, pos.y));
            match item {
                FrameItem::Group(group) => {
                    let inner = ts.pre_concat(group.transform);
                    match &group.clip {
                        None => self.frame(&group.frame, inner),
                        Some(clip) => {
                            // The clip outline is in the group's own coordinates.
                            let first = self.scene.clip_points.x.len();
                            for subpath in curve_subpaths(clip, inner).0 {
                                self.next_clip_sub += 1;
                                for point in subpath {
                                    push(&mut self.scene.clip_points, point, self.next_clip_sub);
                                }
                            }
                            let clips = &mut self.scene.clips;
                            clips.point.push(r_int(first + 1));
                            clips
                                .n_points
                                .push(r_int(self.scene.clip_points.x.len() - first));
                            clips.parent.push(self.current_clip);

                            let outer = self.current_clip;
                            self.current_clip = r_int(clips.point.len());
                            self.frame(&group.frame, inner);
                            self.current_clip = outer;
                        }
                    }
                }
                FrameItem::Text(text) => self.text(text, ts, item),
                FrameItem::Shape(shape, _) => self.shape(shape, ts, item),
                FrameItem::Image(image, ..) => {
                    let pixelated = image.scaling() == Smart::Custom(ImageScaling::Pixelated);
                    self.raster(item, ts, !pixelated);
                }
                FrameItem::Link(..) | FrameItem::Tag(_) => {}
            }
        }
    }

    fn text(&mut self, text: &TextItem, ts: Transform, item: &FrameItem) {
        let stroke = match &text.stroke {
            None => Some(None),
            Some(stroke) => solid_hex(&stroke.paint).map(|hex| Some((hex, stroke))),
        };
        // Text painted with a gradient or tiling, or in a font outside the font
        // store, is drawn as an image.
        let (Some(fill), Some(stroke), Some(font_row)) =
            (solid_hex(&text.fill), stroke, self.font_row(&text.font))
        else {
            self.raster(item, ts, true);
            return;
        };
        let (stroke, lwd) = match stroke {
            None => (None, 0.0),
            Some((hex, stroke)) => {
                // Native glyphs cannot be stroked.
                self.no_glyphs("stroked text");
                (
                    Some(hex),
                    stroke.thickness.to_pt() * max_scale(ts) * LWD_PER_PT,
                )
            }
        };

        let (scale, rotation) = match similarity(ts) {
            Some(st) => st,
            None => {
                // Skewed or unevenly scaled text cannot be drawn as glyphs.
                self.no_glyphs("skewed text");
                (max_scale(ts), 0.0)
            }
        };

        let ttf = text.font.ttf();
        // The linear part of the transform, from points down the page to
        // points per em up it.
        let size = text.size.to_pt();
        let (a, b, c, d) = (ts.sx.get(), ts.ky.get(), ts.kx.get(), ts.sy.get());

        let mut x = 0.0;
        let mut y = 0.0;
        for glyph in &text.glyphs {
            let id = ttf_parser::GlyphId(glyph.id);
            let advanced = Transform::translate(Abs::pt(x), Abs::pt(-y));
            let origin = Point::new(
                Abs::pt(x + glyph.x_offset.at(text.size).to_pt()),
                Abs::pt(-(y + glyph.y_offset.at(text.size).to_pt())),
            );
            x += glyph.x_advance.at(text.size).to_pt();
            y += glyph.y_advance.at(text.size).to_pt();

            let colour = ttf.glyph_raster_image(id, u16::MAX).is_some()
                || ttf.glyph_svg_image(id).is_some()
                || ttf.is_color_glyph(id);
            if colour {
                // Bitmap, SVG, and layered colour glyphs (emoji) have no
                // monochrome outline to draw as a path, and devices draw them
                // as native glyphs unreliably (squashed on svglite, without
                // colour for some fonts on ragg): draw an image of the glyph.
                let mut alone = text.clone();
                alone.glyphs = vec![glyph.clone()];
                self.raster(&FrameItem::Text(alone), ts.pre_concat(advanced), true);
                continue;
            } else if ttf.glyph_bounding_box(id).is_none() {
                // Whitespace draws nothing.
                continue;
            }

            let o = origin.transform(ts);
            let g = &mut self.scene.glyphs;
            g.font.push(font_row);
            g.id.push(i32::from(glyph.id));
            g.x.push(o.x.to_pt());
            g.y.push(o.y.to_pt());
            g.size.push(size * scale);
            g.rot.push(rotation);
            g.xx.push(a * size);
            g.xy.push(-c * size);
            g.yx.push(-b * size);
            g.yy.push(d * size);
            g.fill.push(fill.clone());
            g.stroke.push(stroke.clone());
            g.lwd.push(lwd);
            self.add_glyph_to_run();
        }
    }

    /// Draw the glyph just added with the glyphs before it, if they form a
    /// run.
    fn add_glyph_to_run(&mut self) {
        let runs = &mut self.scene.runs;
        if let Some(last) = runs.kind.len().checked_sub(1)
            && runs.kind[last] == "glyph"
            && runs.clip[last] == self.current_clip
        {
            runs.n_rows[last] += 1;
            return;
        }
        let row = self.scene.glyphs.id.len();
        self.begin_run("glyph", row, 0, 0);
    }

    /// Draw `item`, transformed by `ts`, as an image.
    fn raster(&mut self, item: &FrameItem, ts: Transform, interpolate: bool) {
        let Some(raster) = rasterize(item, ts, &self.page, self.raster_dpi) else {
            return;
        };
        self.scene.rasters.push(raster, interpolate);
        let row = self.scene.rasters.x.len();
        self.begin_run("raster", row, 0, 0);
    }

    /// Start a run drawing one row from `row` and `n_points` points from
    /// `point` (both 1-based; 0 for no points).
    fn begin_run(&mut self, kind: &str, row: usize, point: usize, n_points: usize) {
        let clip = self.current_clip;
        let runs = &mut self.scene.runs;
        let segment = match (runs.segment.last(), runs.clip.last()) {
            (Some(&segment), Some(&last)) if last == clip => segment,
            (Some(&segment), _) => segment + 1,
            (None, _) => 1,
        };
        runs.kind.push(kind.to_string());
        runs.row.push(r_int(row));
        runs.n_rows.push(1);
        runs.point.push(r_int(point));
        runs.n_points.push(r_int(n_points));
        runs.clip.push(clip);
        runs.segment.push(segment);
    }

    fn shape(&mut self, shape: &typst::visualize::Shape, ts: Transform, item: &FrameItem) {
        // Shapes painted with a gradient or tiling are drawn as an image.
        let solid_fill = shape
            .fill
            .as_ref()
            .is_none_or(|paint| solid_hex(paint).is_some());
        let solid_stroke = shape
            .stroke
            .as_ref()
            .is_none_or(|stroke| solid_hex(&stroke.paint).is_some());
        if !(solid_fill && solid_stroke) {
            self.raster(item, ts, true);
            return;
        }
        let fill = shape.fill.as_ref().and_then(solid_hex);
        let rule = match shape.fill_rule {
            FillRule::NonZero => "winding",
            FillRule::EvenOdd => "evenodd",
        };

        let (subpaths, closed) = match &shape.geometry {
            Geometry::Line(end) => {
                let mut c = PathCollector::new(ts);
                c.move_to(Point::zero());
                c.line_to(*end);
                (c.subpaths, false)
            }
            Geometry::Rect(size) => {
                let mut c = PathCollector::new(ts);
                c.move_to(Point::zero());
                c.line_to(Point::new(size.x, Abs::zero()));
                c.line_to(Point::new(size.x, size.y));
                c.line_to(Point::new(Abs::zero(), size.y));
                (c.subpaths, true)
            }
            Geometry::Curve(curve) => curve_subpaths(curve, ts),
        };

        let Some(stroke) = &shape.stroke else {
            self.push_shape(fill, &ShapeStroke::none(), rule, closed, &subpaths);
            return;
        };
        let Some(hex) = solid_hex(&stroke.paint) else {
            return;
        };
        let scale = max_scale(ts);
        let style = ShapeStroke {
            colour: Some(hex),
            lwd: stroke.thickness.to_pt() * scale * LWD_PER_PT,
            lineend: match stroke.cap {
                LineCap::Butt => "butt",
                LineCap::Round => "round",
                LineCap::Square => "square",
            },
            linejoin: match stroke.join {
                LineJoin::Miter => "mitre",
                LineJoin::Round => "round",
                LineJoin::Bevel => "bevel",
            },
        };

        match &stroke.dash {
            None => self.push_shape(fill, &style, rule, closed, &subpaths),
            Some(dash) => {
                // Fill the shape, then stroke its outline cut into dashes.
                if fill.is_some() {
                    self.push_shape(fill, &ShapeStroke::none(), rule, true, &subpaths);
                }
                let array: Vec<f64> = dash.array.iter().map(|a| a.to_pt() * scale).collect();
                let dashes = dash_subpaths(&subpaths, closed, &array, dash.phase.to_pt() * scale);
                self.push_shape(None, &style, rule, false, &dashes);
            }
        }
    }

    /// Add a shape and the run drawing it: a path if it is closed or filled,
    /// otherwise a polyline. Shapes without points draw nothing.
    fn push_shape(
        &mut self,
        fill: Option<String>,
        stroke: &ShapeStroke,
        rule: &str,
        closed: bool,
        subpaths: &[Vec<(f64, f64)>],
    ) {
        let first = self.scene.shape_points.x.len();
        for subpath in subpaths {
            self.next_shape_sub += 1;
            for &point in subpath {
                push(&mut self.scene.shape_points, point, self.next_shape_sub);
            }
        }
        let n_points = self.scene.shape_points.x.len() - first;
        if n_points == 0 {
            return;
        }

        let kind = if closed || fill.is_some() {
            "path"
        } else {
            "polyline"
        };
        let s = &mut self.scene.shapes;
        s.fill.push(fill);
        s.stroke.push(stroke.colour.clone());
        s.lwd.push(stroke.lwd);
        s.rule.push(rule.to_string());
        s.lineend.push(stroke.lineend.to_string());
        s.linejoin.push(stroke.linejoin.to_string());
        let row = s.fill.len();
        self.begin_run(kind, row, first + 1, n_points);
    }

    /// Row of `instance` in the scene's font table, adding it if needed, or
    /// `None` for a font outside the global font store, which neither glyphs
    /// nor outlines can find again.
    fn font_row(&mut self, instance: &FontInstance) -> Option<i32> {
        let font = instance.font();
        let found = lookup_font(font)?;
        let variations = instance
            .variations()
            .0
            .iter()
            .map(|(tag, value)| format!("{}={}", String::from_utf8_lossy(&tag.to_bytes()), value.0))
            .collect::<Vec<_>>()
            .join(",");
        let key = (found.store_index, variations);
        if let Some(row) = self.font_rows.get(&key) {
            return Some(*row);
        }
        let info = font.info();
        let f = &mut self.scene.fonts;
        f.store_index.push(r_int(found.store_index));
        f.path.push(found.path.clone());
        f.collection_index.push(r_int(found.collection_index));
        f.cache_key.push(found.cache_key.clone());
        f.family.push(info.family.clone());
        f.weight.push(i32::from(info.variant.weight.to_number()));
        f.style.push(
            match info.variant.style {
                FontStyle::Normal => "normal",
                FontStyle::Italic => "italic",
                FontStyle::Oblique => "oblique",
            }
            .to_string(),
        );
        f.variations.push(key.1.clone());
        let row = r_int(f.path.len());
        self.font_rows.insert(key, row);
        Some(row)
    }
}

/// Stroke style of a shape row.
struct ShapeStroke {
    /// `None` for no stroke.
    colour: Option<String>,
    lwd: f64,
    lineend: &'static str,
    linejoin: &'static str,
}

impl ShapeStroke {
    fn none() -> Self {
        Self {
            colour: None,
            lwd: 0.0,
            lineend: "butt",
            linejoin: "mitre",
        }
    }
}

/// `n` as an R integer, saturating at R's largest integer.
pub(crate) fn r_int<T: TryInto<i32>>(n: T) -> i32 {
    n.try_into().unwrap_or(i32::MAX)
}

pub(crate) fn push(points: &mut Points, p: (f64, f64), sub: i32) {
    points.x.push(p.0);
    points.y.push(p.1);
    points.sub.push(sub);
}

/// Collects geometry as flattened subpaths in page coordinates.
struct PathCollector {
    ts: Transform,
    subpaths: Vec<Vec<(f64, f64)>>,
}

impl PathCollector {
    fn new(ts: Transform) -> Self {
        Self {
            ts,
            subpaths: Vec::new(),
        }
    }

    fn map(&self, p: Point) -> (f64, f64) {
        let p = p.transform(self.ts);
        (p.x.to_pt(), p.y.to_pt())
    }

    fn move_to(&mut self, p: Point) {
        let p = self.map(p);
        self.subpaths.push(vec![p]);
    }

    fn line_to(&mut self, p: Point) {
        let p = self.map(p);
        if let Some(subpath) = self.subpaths.last_mut() {
            subpath.push(p);
        }
    }

    fn cubic_to(&mut self, c1: Point, c2: Point, p: Point) {
        let (c1, c2, p) = (self.map(c1), self.map(c2), self.map(p));
        if let Some(subpath) = self.subpaths.last_mut() {
            let last = subpath.last().copied().unwrap_or(c1);
            flatten_cubic(last, c1, c2, p, 0, subpath);
        }
    }
}

/// A curve's flattened subpaths in page coordinates, and whether it is closed.
fn curve_subpaths(curve: &Curve, ts: Transform) -> (Vec<Vec<(f64, f64)>>, bool) {
    let mut c = PathCollector::new(ts);
    let mut closed = false;
    for item in &curve.0 {
        match item {
            CurveItem::Move(p) => c.move_to(*p),
            CurveItem::Line(p) => c.line_to(*p),
            CurveItem::Cubic(c1, c2, p) => c.cubic_to(*c1, *c2, *p),
            CurveItem::Close => closed = true,
        }
    }
    (c.subpaths, closed)
}

/// Cut subpaths into dashes: `array` alternates dash and gap lengths, starting
/// `phase` into the pattern. Like SVG and PDF, the pattern restarts at each
/// subpath, and an odd-length array is repeated to make dash/gap pairs.
fn dash_subpaths(
    subpaths: &[Vec<(f64, f64)>],
    closed: bool,
    array: &[f64],
    phase: f64,
) -> Vec<Vec<(f64, f64)>> {
    let period: f64 = array.iter().sum();
    if array.is_empty() || period <= 0.0 || array.iter().any(|a| *a < 0.0) {
        return subpaths.to_vec();
    }

    let mut dashes = Vec::new();
    for subpath in subpaths {
        let mut points = subpath.clone();
        if closed && let Some(&first) = points.first() {
            points.push(first);
        }

        // Find where in the pattern the subpath starts.
        let mut index = 0;
        let mut on = true;
        let mut left = array[0];
        let mut skip = phase.rem_euclid(period);
        while skip > 0.0 && skip >= left {
            skip -= left;
            index = (index + 1) % array.len();
            on = !on;
            left = array[index];
        }
        left -= skip;

        let mut current: Vec<(f64, f64)> = Vec::new();
        if on && let Some(&first) = points.first() {
            current.push(first);
        }
        for pair in points.windows(2) {
            let (a, b) = (pair[0], pair[1]);
            let length = ((b.0 - a.0).powi(2) + (b.1 - a.1).powi(2)).sqrt();
            let mut done = 0.0;
            while length - done > left {
                done += left;
                let t = done / length;
                let p = (a.0 + (b.0 - a.0) * t, a.1 + (b.1 - a.1) * t);
                if on {
                    current.push(p);
                    dashes.push(std::mem::take(&mut current));
                } else {
                    current = vec![p];
                }
                index = (index + 1) % array.len();
                on = !on;
                left = array[index];
            }
            left -= length - done;
            if on {
                current.push(b);
            }
        }
        if on && current.len() > 1 {
            dashes.push(current);
        }
    }
    dashes
}

/// Flatten a cubic Bézier into line segments, appending the end points of the
/// segments (not `p0`), by subdividing until both control points are within
/// the tolerance of the chord.
pub(crate) fn flatten_cubic(
    p0: (f64, f64),
    p1: (f64, f64),
    p2: (f64, f64),
    p3: (f64, f64),
    depth: u32,
    out: &mut Vec<(f64, f64)>,
) {
    let (dx, dy) = (p3.0 - p0.0, p3.1 - p0.1);
    let len = (dx * dx + dy * dy).sqrt();
    let distance = |p: (f64, f64)| {
        if len < 1e-12 {
            ((p.0 - p0.0).powi(2) + (p.1 - p0.1).powi(2)).sqrt()
        } else {
            ((p.0 - p0.0) * dy - (p.1 - p0.1) * dx).abs() / len
        }
    };
    if depth >= 16 || (distance(p1) <= FLATTEN_TOLERANCE_PT && distance(p2) <= FLATTEN_TOLERANCE_PT)
    {
        out.push(p3);
        return;
    }
    let mid = |a: (f64, f64), b: (f64, f64)| ((a.0 + b.0) / 2.0, (a.1 + b.1) / 2.0);
    let (a, b, c) = (mid(p0, p1), mid(p1, p2), mid(p2, p3));
    let (ab, bc) = (mid(a, b), mid(b, c));
    let m = mid(ab, bc);
    flatten_cubic(p0, a, ab, m, depth + 1, out);
    flatten_cubic(m, bc, c, p3, depth + 1, out);
}

/// `#RRGGBBAA` of a solid paint, or `None` for gradients and tilings.
fn solid_hex(paint: &Paint) -> Option<String> {
    match paint {
        Paint::Solid(color) => {
            let rgb = color.to_rgb();
            let byte = |v: f32| (v.clamp(0.0, 1.0) * 255.0).round() as u8;
            Some(format!(
                "#{:02X}{:02X}{:02X}{:02X}",
                byte(rgb.red),
                byte(rgb.green),
                byte(rgb.blue),
                byte(rgb.alpha)
            ))
        }
        _ => None,
    }
}

/// Scale and counter-clockwise rotation (degrees) of a transform that only
/// rotates, scales uniformly, and translates; `None` otherwise.
fn similarity(ts: Transform) -> Option<(f64, f64)> {
    let (a, b, c, d) = (ts.sx.get(), ts.ky.get(), ts.kx.get(), ts.sy.get());
    let scale = (a * a + b * b).sqrt();
    // A similarity has the form [[s cos, -s sin], [s sin, s cos]].
    let tolerance = 1e-9 * scale.max(1.0);
    if scale == 0.0 || (a - d).abs() > tolerance || (b + c).abs() > tolerance {
        return None;
    }
    // Typst's y axis points down, so a positive angle here turns clockwise on
    // the page; grid's rotation counts counter-clockwise.
    Some((scale, -b.atan2(a).to_degrees()))
}

/// Upper bound on how much `ts` stretches a length.
fn max_scale(ts: Transform) -> f64 {
    let column_x = ts.sx.get().abs() + ts.ky.get().abs();
    let column_y = ts.kx.get().abs() + ts.sy.get().abs();
    column_x.max(column_y)
}

/// Where a font of the global store comes from.
#[derive(Debug, Clone)]
pub(crate) struct FontRef {
    pub store_index: usize,
    /// File path, or empty for a font embedded in the binary.
    pub path: String,
    pub collection_index: u32,
    pub cache_key: String,
}

static FONT_REFS: LazyLock<Mutex<HashMap<Font, FontRef>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Find `font` in the global font store.
pub(crate) fn lookup_font(font: &Font) -> Option<FontRef> {
    if let Some(found) = FONT_REFS
        .lock()
        .ok()
        .and_then(|refs| refs.get(font).cloned())
    {
        return Some(found);
    }

    let store = fonts::get_fonts();
    let book = store.book();
    let mut index = 0;
    let found = loop {
        let info = book.info(index)?;
        // Compare metadata first so only candidate fonts get loaded.
        if info == font.info() && store.font(index).as_ref() == Some(font) {
            break index;
        }
        index += 1;
    };

    let source: &dyn Any = store.source(found)?;
    let font_ref = match source.downcast_ref::<FontPath>() {
        Some(file) => FontRef {
            store_index: found,
            path: file.path.to_string_lossy().into_owned(),
            collection_index: file.index,
            cache_key: String::new(),
        },
        None => FontRef {
            store_index: found,
            path: String::new(),
            collection_index: font.index(),
            cache_key: cache_key(font),
        },
    };

    if let Ok(mut refs) = FONT_REFS.lock() {
        refs.insert(font.clone(), font_ref.clone());
    }
    Some(font_ref)
}

/// A file name for an embedded font that changes whenever its data does.
fn cache_key(font: &Font) -> String {
    // 64-bit FNV-1a: stable across runs and Rust versions, unlike `Hash`.
    let hash = font
        .data()
        .as_slice()
        .iter()
        .fold(0xcbf2_9ce4_8422_2325_u64, |h, &b| {
            (h ^ u64::from(b)).wrapping_mul(0x0000_0100_0000_01b3)
        });
    let family: String = font
        .info()
        .family
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
        .collect();
    format!("{family}-{}-{hash:016x}", font.index())
}

/// Font `store_index` of the global font store, checked against where the
/// scene said it came from (`path`, `collection_index`, `cache_key`).
///
/// A scene from another R session (a saved plot) may name a font at another
/// store index: then a font file is read from its path, and a font built into
/// ggtypst is found by its cache key.
pub(crate) fn find_font(
    store_index: i32,
    path: &str,
    collection_index: i32,
    cache_key: &str,
) -> Result<Font, String> {
    let is_it = |font: &Font| {
        lookup_font(font).is_some_and(|found| {
            found.path == path
                && found.cache_key == cache_key
                && r_int(found.collection_index) == collection_index
        })
    };

    let store = fonts::get_fonts();
    if let Ok(store_index) = usize::try_from(store_index)
        && let Some(font) = store.font(store_index)
        && is_it(&font)
    {
        return Ok(font);
    }

    if !path.is_empty() {
        let data =
            std::fs::read(path).map_err(|err| format!("cannot read font file {path}: {err}"))?;
        let index = u32::try_from(collection_index).unwrap_or(0);
        return Font::new(Bytes::new(data), index)
            .ok_or_else(|| format!("{path} has no font {index}"));
    }

    // Only fonts built into the binary have no file; check just those.
    let embedded = |j: &usize| {
        store
            .source(*j)
            .is_some_and(|source| (source as &dyn Any).downcast_ref::<FontPath>().is_none())
    };
    (0..)
        .map_while(|j| store.book().info(j).map(|_| j))
        .filter(embedded)
        .filter_map(|j| store.font(j))
        .find(is_it)
        .ok_or_else(|| format!("the built-in font {cache_key} is not available"))
}

/// Write a font built into ggtypst to `path`, so graphics devices can load
/// it. The data goes to a temporary file next to `path` first, so a concurrent
/// reader never sees a partial font.
pub fn write_embedded_font(
    store_index: i32,
    collection_index: i32,
    cache_key: &str,
    path: &Path,
) -> Result<(), String> {
    static NEXT: AtomicU64 = AtomicU64::new(0);

    let font = find_font(store_index, "", collection_index, cache_key)?;
    let name = path
        .file_name()
        .ok_or_else(|| format!("{} is not a file path", path.display()))?
        .to_string_lossy();
    let partial = path.with_file_name(format!(
        ".{name}.{}-{}.partial",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::write(&partial, font.data().as_slice())
        .and_then(|()| std::fs::rename(&partial, path))
        .map_err(|err| {
            let _ = std::fs::remove_file(&partial);
            format!("cannot write {}: {err}", path.display())
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world::InMemoryWorld;
    use typst_layout::PagedDocument;

    const PREAMBLE: &str = concat!(
        "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
        "#set text(size: 20pt, top-edge: \"cap-height\", bottom-edge: \"baseline\")\n"
    );

    fn scene_of(body: &str) -> (Frame, Scene) {
        let world = InMemoryWorld::new(format!("{PREAMBLE}{body}"), fonts::get_fonts());
        let Ok(doc) = typst::compile::<PagedDocument>(&world).output else {
            panic!("source should compile: {body}");
        };
        let Some(page) = doc.pages().first() else {
            panic!("document should have a page");
        };
        let scene = extract_scene(page, 300.0);
        (page.frame.clone(), scene)
    }

    fn assert_close(actual: f64, expected: f64) {
        assert!(
            (actual - expected).abs() < 1e-6,
            "expected {expected}, got {actual}"
        );
    }

    fn kinds(scene: &Scene) -> Vec<&str> {
        scene.runs.kind.iter().map(String::as_str).collect()
    }

    /// The 0-based range of a run's points.
    fn run_points(scene: &Scene, run: usize) -> std::ops::Range<usize> {
        let r = &scene.runs;
        let (Ok(start), Ok(n)) = (
            usize::try_from(r.point[run] - 1),
            usize::try_from(r.n_points[run]),
        ) else {
            panic!("run {run} should have a valid point range");
        };
        start..start + n
    }

    #[test]
    fn glyphs_sit_on_the_baseline_in_page_coordinates() {
        let (_, scene) = scene_of("HH");
        let g = &scene.glyphs;
        assert_eq!(g.id.len(), 2);
        // With baseline as the bottom edge, the baseline is the page bottom,
        // and y points up from there.
        assert_close(g.y[0], 0.0);
        assert_close(g.x[0], 0.0);
        assert!(g.x[1] > g.x[0]);
        assert_close(g.size[0], 20.0);
        assert_close(g.rot[0], 0.0);
        assert_eq!(g.fill[0], "#000000FF");
        assert_eq!(g.stroke[0], None);
        assert!(scene.glyph_ok, "{:?}", scene.unsupported);
    }

    #[test]
    fn runs_follow_the_drawing_order() {
        // The box background is drawn before the text inside it, and the
        // glyphs inside and after the box form one run.
        let (_, scene) = scene_of("#box(fill: yellow)[Hi] H");
        assert_eq!(kinds(&scene), vec!["path", "glyph"]);
        let r = &scene.runs;
        assert_eq!((r.row[0], r.n_rows[0]), (1, 1));
        assert_eq!((r.row[1], r.n_rows[1]), (1, 3));
        assert_eq!(r.segment, vec![1, 1]);
        assert_eq!((r.point[1], r.n_points[1]), (0, 0));
    }

    #[test]
    fn whitespace_produces_no_glyph() {
        let (_, scene) = scene_of("H H");
        assert_eq!(scene.glyphs.id.len(), 2);
        assert_eq!(scene.runs.n_rows, vec![2]);
    }

    #[test]
    fn rotation_is_reported_counter_clockwise() {
        // Typst turns clockwise for positive angles.
        let (_, scene) = scene_of("#rotate(-90deg, reflow: true)[H]");
        assert_close(scene.glyphs.rot[0], 90.0);
        assert!(scene.glyph_ok);
    }

    #[test]
    fn scaled_text_reports_its_effective_size() {
        let (_, scene) = scene_of("#scale(200%, reflow: true)[H]");
        assert_close(scene.glyphs.size[0], 40.0);
        // 20pt text at 200%: 40pt per em, y up.
        let g = &scene.glyphs;
        assert_close(g.xx[0], 40.0);
        assert_close(g.yy[0], 40.0);
        assert_close(g.xy[0], 0.0);
        assert_close(g.yx[0], 0.0);

        let (_, skewed) = scene_of("#scale(x: 200%, y: 100%, reflow: true)[H]");
        assert!(!skewed.glyph_ok);
    }

    #[test]
    fn colours_are_srgb_hex_with_alpha() {
        let (_, scene) = scene_of("#text(fill: rgb(\"#33669980\"))[H]");
        assert_eq!(scene.glyphs.fill[0], "#33669980");
        let (_, luma) = scene_of("#text(fill: luma(50%))[H]");
        assert_eq!(luma.glyphs.fill[0], "#808080FF");
    }

    #[test]
    fn fraction_bars_and_boxes_become_shapes() {
        let (_, scene) = scene_of("$1/2$ #box(width: 10pt, height: 4pt, fill: red)");
        let s = &scene.shapes;
        assert_eq!(s.fill.len(), 2);
        // The fraction bar is a stroked line; the box a filled rectangle.
        let kinds = kinds(&scene);
        let line = kinds.iter().position(|&k| k == "polyline");
        let rect = kinds.iter().position(|&k| k == "path");
        let (Some(line), Some(rect)) = (line, rect) else {
            panic!("expected a line and a rectangle: {kinds:?}");
        };
        let row = |run: usize| usize::try_from(scene.runs.row[run] - 1).unwrap_or(usize::MAX);
        assert!(s.lwd[row(line)] > 0.0);
        assert_eq!(s.fill[row(rect)].as_deref(), Some("#FF4136FF"));
        assert_eq!(run_points(&scene, rect).len(), 4);
    }

    #[test]
    fn curves_are_flattened_within_tolerance() {
        let (_, scene) = scene_of("#circle(radius: 10pt, fill: black)");
        let p = &scene.shape_points;
        assert!(p.x.len() > 16);
        let cx = (p.x.iter().copied().fold(f64::INFINITY, f64::min)
            + p.x.iter().copied().fold(f64::NEG_INFINITY, f64::max))
            / 2.0;
        let cy = (p.y.iter().copied().fold(f64::INFINITY, f64::min)
            + p.y.iter().copied().fold(f64::NEG_INFINITY, f64::max))
            / 2.0;
        for (x, y) in p.x.iter().zip(&p.y) {
            let r = ((x - cx).powi(2) + (y - cy).powi(2)).sqrt();
            // Typst draws circles as four cubics, which are within ~0.03% of
            // the radius; flattening adds at most the tolerance.
            assert!((r - 10.0).abs() < 0.01, "radius {r}");
        }
    }

    #[test]
    fn fonts_point_at_files_or_cache_keys() {
        // The default text font is embedded in the binary.
        let (_, scene) = scene_of("H");
        let f = &scene.fonts;
        assert_eq!(f.path.len(), 1);
        assert_eq!(f.path[0], "");
        assert!(
            f.cache_key[0].starts_with("Libertinus-Serif-"),
            "{}",
            f.cache_key[0]
        );
        assert_eq!(f.weight[0], 400);
        assert_eq!(f.style[0], "normal");
        assert_eq!(f.variations[0], "");

        // The same font maps to the same row.
        let (_, two) = scene_of("H #text(weight: \"bold\")[H] H");
        assert_eq!(two.fonts.path.len(), 2);
        assert_eq!(two.glyphs.font, vec![1, 2, 1]);
    }

    #[test]
    fn embedded_fonts_are_written_whole() {
        let (_, scene) = scene_of("H");
        let f = &scene.fonts;
        let dir = std::env::temp_dir().join(format!("ggtypst-font-test-{}", std::process::id()));
        if let Err(err) = std::fs::create_dir_all(&dir) {
            panic!("cannot create {}: {err}", dir.display());
        }
        let Ok(store_index) = usize::try_from(f.store_index[0]) else {
            panic!("store index should be non-negative");
        };
        let Some(font) = fonts::get_fonts().font(store_index) else {
            panic!("the font should be in the store");
        };

        // A stale store index, as in a scene from another session, still
        // writes the font named by the cache key.
        for (i, store_index) in [f.store_index[0], i32::MAX].into_iter().enumerate() {
            let path = dir.join(format!("{i}.otf"));
            let written =
                write_embedded_font(store_index, f.collection_index[0], &f.cache_key[0], &path);
            assert!(written.is_ok(), "{written:?}");
            let Ok(data) = std::fs::read(&path) else {
                panic!("{} should be written", path.display());
            };
            assert!(data == font.data().as_slice());
        }

        let missing = write_embedded_font(i32::MAX, 0, "Missing-0-0", &dir.join("missing.otf"));
        assert!(missing.is_err());
        assert!(!dir.join("missing.otf").exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn dashed_strokes_are_cut_into_dashes() {
        // A 30pt line with 4pt dashes and 2pt gaps: dashes start at 0, 6, ...
        let (_, scene) = scene_of(
            "#line(length: 30pt, stroke: (paint: black, thickness: 1pt, dash: (4pt, 2pt)))",
        );
        assert!(scene.unsupported.is_empty(), "{:?}", scene.unsupported);
        let p = &scene.shape_points;
        let mut starts = Vec::new();
        let mut lengths = Vec::new();
        let mut i = 0;
        while i < p.sub.len() {
            let j = p.sub[i..]
                .iter()
                .position(|&s| s != p.sub[i])
                .map_or(p.sub.len(), |k| i + k);
            starts.push(p.x[i]);
            lengths.push(p.x[j - 1] - p.x[i]);
            i = j;
        }
        assert_eq!(starts.len(), 5);
        for (k, (start, length)) in starts.iter().zip(&lengths).enumerate() {
            assert_close(*start, 6.0 * k as f64);
            assert_close(*length, 4.0);
        }
        assert_eq!(kinds(&scene), vec!["polyline"]);
    }

    #[test]
    fn dash_phase_and_odd_arrays_follow_svg() {
        let line = vec![vec![(0.0, 0.0), (10.0, 0.0)]];
        // Odd arrays repeat: (3) means 3 on, 3 off.
        let dashes = dash_subpaths(&line, false, &[3.0], 0.0);
        assert_eq!(dashes.len(), 2);
        assert_close(dashes[1][0].0, 6.0);
        // A phase of 1 starts one point into the first dash.
        let shifted = dash_subpaths(&line, false, &[3.0, 1.0], 1.0);
        assert_close(shifted[0][1].0, 2.0);
        assert_close(shifted[1][0].0, 3.0);
    }

    #[test]
    fn filled_dashed_shapes_keep_their_fill() {
        let (_, scene) = scene_of(
            "#rect(width: 10pt, height: 10pt, fill: red, stroke: (paint: black, dash: \"dotted\"))",
        );
        let s = &scene.shapes;
        assert_eq!(s.fill.len(), 2);
        assert_eq!(s.fill[0].as_deref(), Some("#FF4136FF"));
        assert_eq!(s.stroke[0], None);
        assert_eq!(s.fill[1], None);
        assert_eq!(s.stroke[1].as_deref(), Some("#000000FF"));
        assert_eq!(kinds(&scene), vec!["path", "polyline"]);
    }

    #[test]
    fn clipped_groups_record_their_clip_region() {
        let (_, scene) = scene_of("#box(clip: true, width: 5pt)[HHH] H");
        assert!(scene.glyph_ok, "{:?}", scene.unsupported);
        assert_eq!(scene.clips.point, vec![1]);
        assert_eq!(
            usize::try_from(scene.clips.n_points[0]).ok(),
            Some(scene.clip_points.x.len())
        );
        let max_x = scene
            .clip_points
            .x
            .iter()
            .copied()
            .fold(f64::NEG_INFINITY, f64::max);
        assert_close(max_x, 5.0);
        // The three clipped glyphs, then the one after the box: two runs in
        // two segments.
        let r = &scene.runs;
        assert_eq!(r.clip, vec![1, 0]);
        assert_eq!(r.n_rows, vec![3, 1]);
        assert_eq!(r.segment, vec![1, 2]);
    }

    /// The pixel of raster `i` at the fraction `(u, v)` of its width and
    /// height from the top-left, as `[r, g, b, a]`.
    fn pixel(scene: &Scene, i: usize, u: f64, v: f64) -> [u8; 4] {
        let r = &scene.rasters;
        let (Ok(w), Ok(h)) = (
            usize::try_from(r.width_px[i]),
            usize::try_from(r.height_px[i]),
        ) else {
            panic!("raster {i} should have a size");
        };
        let (col, row) = ((u * w as f64) as usize, (v * h as f64) as usize);
        r.pixels[i][row.min(h - 1) * w + col.min(w - 1)].to_le_bytes()
    }

    fn assert_colour(actual: [u8; 4], expected: [u8; 4]) {
        assert!(
            actual
                .iter()
                .zip(&expected)
                .all(|(a, e)| a.abs_diff(*e) <= 3),
            "expected {expected:?}, got {actual:?}"
        );
    }

    #[test]
    fn gradients_are_drawn_as_images_like_typst() {
        let (_, scene) = scene_of(
            "#rect(width: 40pt, height: 10pt, stroke: none, fill: gradient.linear(rgb(\"#ff0000\"), rgb(\"#0000ff\")))",
        );
        assert!(scene.glyph_ok, "{:?}", scene.unsupported);
        assert_eq!(kinds(&scene), vec!["raster"]);
        let r = &scene.rasters;
        // The shape plus a pixel of margin on each side, at 300 dpi.
        let margin = 72.0 / 300.0;
        assert_close(r.width[0], 40.0 + 2.0 * margin);
        assert_close(r.height[0], 10.0 + 2.0 * margin);
        assert_eq!(
            r.width_px[0],
            r_int((r.width[0] * 300.0 / 72.0).round() as i64)
        );
        assert!(r.interpolate[0]);
        // Typst mixes gradients in Oklab: red and blue meet at #8C53A2, not at
        // the sRGB mix #800080.
        assert_colour(pixel(&scene, 0, 0.5, 0.5), [0x8C, 0x53, 0xA2, 0xFF]);
    }

    #[test]
    fn painted_text_is_drawn_as_an_image() {
        let (_, scene) = scene_of("H #text(fill: gradient.linear(red, blue))[ello] H");
        assert_eq!(kinds(&scene), vec!["glyph", "raster", "glyph"]);
        assert_eq!(scene.glyphs.id.len(), 2);
    }

    #[test]
    fn large_images_are_capped() {
        let (_, scene) =
            scene_of("#rect(width: 1000pt, height: 1000pt, fill: gradient.radial(red, blue))");
        let r = &scene.rasters;
        let pixels = f64::from(r.width_px[0]) * f64::from(r.height_px[0]);
        assert!(pixels <= crate::raster::MAX_PIXELS * 1.01, "{pixels}");
        assert!(pixels > crate::raster::MAX_PIXELS * 0.9, "{pixels}");
    }

    #[test]
    fn images_are_drawn_as_images() {
        let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><rect width='10' height='10' fill='red'/></svg>";
        for (scaling, smooth) in [("smooth", true), ("pixelated", false)] {
            let (_, scene) = scene_of(&format!(
                "#image(bytes(\"{svg}\"), format: \"svg\", width: 10pt, scaling: \"{scaling}\")"
            ));
            assert_eq!(kinds(&scene), vec!["raster"]);
            assert_eq!(scene.rasters.interpolate, vec![smooth]);
            assert_colour(pixel(&scene, 0, 0.5, 0.5), [0xFF, 0x00, 0x00, 0xFF]);
        }
    }

    #[test]
    fn colour_glyphs_are_drawn_as_images() {
        let (_, scene) = scene_of("H \u{1F600} H");
        // Only on systems with a colour emoji font.
        if scene.rasters.x.is_empty() {
            return;
        }
        assert!(scene.glyph_ok, "{:?}", scene.unsupported);
        // The emoji is an image between the two glyphs, not a glyph.
        assert_eq!(kinds(&scene), vec!["glyph", "raster", "glyph"]);
        assert_eq!(scene.glyphs.id.len(), 2);
    }

    #[test]
    fn nested_clip_regions_know_their_parent() {
        let (_, scene) =
            scene_of("#box(clip: true, width: 8pt)[H #box(clip: true, width: 5pt)[HHH] H] H");
        assert!(scene.glyph_ok, "{:?}", scene.unsupported);
        assert_eq!(scene.clips.parent, vec![0, 1]);
        // Glyphs in the outer region, the inner one, the outer one again, and
        // outside both.
        assert_eq!(scene.runs.clip, vec![1, 2, 1, 0]);
        assert_eq!(scene.runs.segment, vec![1, 2, 3, 4]);
    }

    #[test]
    fn unsupported_content_is_flagged() {
        let (_, stroked) = scene_of("#text(stroke: 0.5pt + red)[H]");
        assert!(!stroked.glyph_ok);
        assert_eq!(stroked.glyphs.stroke[0].as_deref(), Some("#FF4136FF"));
        assert_close(stroked.glyphs.lwd[0], 0.5 * LWD_PER_PT);
    }
}
