//! A drawable description of a Typst page, for R to draw with grid.
//!
//! The frame tree is walked once, accumulating group transforms the way
//! typst-svg does, and flattened into plain tables in page coordinates
//! (points, y pointing down):
//!
//! - glyphs: which font and glyph to draw where, for drawing them natively;
//! - outlines: the same glyphs as flattened outlines, for drawing them as paths;
//! - shapes and their points: lines, rectangles, and curves, flattened;
//! - fonts: the font files the glyphs come from.
//!
//! Content that a mode cannot express clears `path_ok` or `glyph_ok`, so R can
//! fall back to another way of drawing the page.

use std::any::Any;
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

use extendr_api::prelude::{List, list};
use typst::layout::{Abs, Frame, FrameItem, Point, Transform};
use typst::text::{Font, FontStyle, TextItem};
use typst::visualize::{Curve, CurveItem, FillRule, Geometry, LineCap, LineJoin, Paint};
use typst_kit::fonts::FontPath;

use crate::fonts;

/// Maximum distance, in points, between a flattened curve and the true curve.
const FLATTEN_TOLERANCE_PT: f64 = 0.005;

/// Glyphs to draw natively, one row per glyph.
#[derive(Debug, Default)]
pub struct Glyphs {
    /// 1-based row in `Fonts`.
    pub font: Vec<i32>,
    pub id: Vec<i32>,
    /// Glyph origin on the baseline.
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    /// Font size after transforms, in points.
    pub size: Vec<f64>,
    /// Rotation in degrees, counter-clockwise like grid's `rot`.
    pub rot: Vec<f64>,
    /// `#RRGGBBAA`.
    pub fill: Vec<String>,
    /// `#RRGGBBAA`, or empty for no stroke.
    pub stroke: Vec<String>,
    pub stroke_width: Vec<f64>,
    /// Drawing order among glyphs and shapes.
    pub z: Vec<i32>,
    /// Clip region the glyph is drawn in (row of `Scene::clips` owners), or 0.
    pub clip: Vec<i32>,
}

/// Points of flattened subpaths, each owned by a glyph or shape row.
#[derive(Debug, Default)]
pub struct Points {
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    /// Subpath id, unique within the table.
    pub sub: Vec<i32>,
    /// 1-based row of the owning glyph or shape.
    pub owner: Vec<i32>,
}

/// Lines, rectangles, and curves, one row per shape.
#[derive(Debug, Default)]
pub struct Shapes {
    /// `#RRGGBBAA`, or empty for no fill.
    pub fill: Vec<String>,
    /// `#RRGGBBAA`, or empty for no stroke.
    pub stroke: Vec<String>,
    pub stroke_width: Vec<f64>,
    /// `"winding"` or `"evenodd"`, as for `grid::pathGrob()`.
    pub rule: Vec<String>,
    /// `"butt"`, `"round"`, or `"square"`.
    pub lineend: Vec<String>,
    /// `"mitre"`, `"round"`, or `"bevel"`.
    pub linejoin: Vec<String>,
    /// 1 if the outline is closed, 0 if it is an open line.
    pub closed: Vec<i32>,
    /// Drawing order among glyphs and shapes.
    pub z: Vec<i32>,
    /// Clip region the shape is drawn in (row of `Scene::clips` owners), or 0.
    pub clip: Vec<i32>,
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
}

#[derive(Debug)]
pub struct Scene {
    pub glyphs: Glyphs,
    pub outlines: Points,
    pub shapes: Shapes,
    pub shape_points: Points,
    /// Clip regions: closed outlines, owned by clip id (1-based).
    pub clips: Points,
    pub fonts: Fonts,
    /// Everything can be drawn from `outlines` and `shapes`.
    pub path_ok: bool,
    /// Everything can be drawn from `glyphs` and `shapes`.
    pub glyph_ok: bool,
    /// Why a mode is unavailable, for diagnostics.
    pub unsupported: Vec<String>,
}

impl Scene {
    pub fn into_r_list(self) -> List {
        let Scene {
            glyphs: g,
            outlines: o,
            shapes: s,
            shape_points: sp,
            clips: c,
            fonts: f,
            path_ok,
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
                fill = g.fill,
                stroke = g.stroke,
                stroke_width = g.stroke_width,
                z = g.z,
                clip = g.clip
            ),
            outlines = list!(x = o.x, y = o.y, sub = o.sub, owner = o.owner),
            shapes = list!(
                fill = s.fill,
                stroke = s.stroke,
                stroke_width = s.stroke_width,
                rule = s.rule,
                lineend = s.lineend,
                linejoin = s.linejoin,
                closed = s.closed,
                z = s.z,
                clip = s.clip
            ),
            shape_points = list!(x = sp.x, y = sp.y, sub = sp.sub, owner = sp.owner),
            clips = list!(x = c.x, y = c.y, sub = c.sub, owner = c.owner),
            fonts = list!(
                store_index = f.store_index,
                path = f.path,
                collection_index = f.collection_index,
                cache_key = f.cache_key,
                family = f.family,
                weight = f.weight,
                style = f.style
            ),
            path_ok = path_ok,
            glyph_ok = glyph_ok,
            unsupported = unsupported
        )
    }
}

/// Describe everything `frame` draws.
pub fn extract_scene(frame: &Frame) -> Scene {
    let mut builder = SceneBuilder {
        scene: Scene {
            glyphs: Glyphs::default(),
            outlines: Points::default(),
            shapes: Shapes::default(),
            shape_points: Points::default(),
            clips: Points::default(),
            fonts: Fonts::default(),
            path_ok: true,
            glyph_ok: true,
            unsupported: Vec::new(),
        },
        font_rows: HashMap::new(),
        next_outline_sub: 0,
        next_shape_sub: 0,
        next_clip_sub: 0,
        next_z: 0,
        current_clip: 0,
        next_clip: 0,
    };
    builder.frame(frame, Transform::identity());
    builder.scene
}

struct SceneBuilder {
    scene: Scene,
    /// Store index -> 1-based row in `scene.fonts`.
    font_rows: HashMap<usize, i32>,
    next_outline_sub: i32,
    next_shape_sub: i32,
    next_clip_sub: i32,
    next_z: i32,
    /// Clip id of the group being walked, or 0.
    current_clip: i32,
    next_clip: i32,
}

impl SceneBuilder {
    fn next_z(&mut self) -> i32 {
        self.next_z += 1;
        self.next_z
    }

    fn unsupported(&mut self, path: bool, glyph: bool, reason: &str) {
        self.scene.path_ok &= path;
        self.scene.glyph_ok &= glyph;
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
                        Some(_) if self.current_clip != 0 => {
                            self.unsupported(false, false, "nested clipped group");
                            self.frame(&group.frame, inner);
                        }
                        Some(clip) => {
                            // The clip outline is in the group's own coordinates.
                            self.next_clip += 1;
                            let id = self.next_clip;
                            for subpath in curve_subpaths(clip, inner).0 {
                                self.next_clip_sub += 1;
                                let sub = self.next_clip_sub;
                                for point in subpath {
                                    push(&mut self.scene.clips, point, sub, id);
                                }
                            }
                            self.current_clip = id;
                            self.frame(&group.frame, inner);
                            self.current_clip = 0;
                        }
                    }
                }
                FrameItem::Text(text) => self.text(text, ts),
                FrameItem::Shape(shape, _) => self.shape(shape, ts),
                FrameItem::Image(..) => self.unsupported(false, false, "image"),
                FrameItem::Link(..) | FrameItem::Tag(_) => {}
            }
        }
    }

    fn text(&mut self, text: &TextItem, ts: Transform) {
        let Some(fill) = solid_hex(&text.fill) else {
            self.unsupported(false, false, "non-solid text fill");
            return;
        };
        let (stroke, stroke_width) = match &text.stroke {
            None => (String::new(), 0.0),
            Some(stroke) => {
                // Native glyphs cannot be stroked.
                self.unsupported(true, false, "stroked text");
                match solid_hex(&stroke.paint) {
                    Some(hex) => (hex, stroke.thickness.to_pt() * max_scale(ts)),
                    None => {
                        self.unsupported(false, false, "non-solid text stroke");
                        return;
                    }
                }
            }
        };

        let (scale, rotation) = match similarity(ts) {
            Some(st) => st,
            None => {
                // Skewed or unevenly scaled text cannot be drawn as glyphs.
                self.unsupported(true, false, "skewed text");
                (max_scale(ts), 0.0)
            }
        };

        let font_row = self.font_row(text.font.font());
        let ttf = text.font.ttf();
        let units = text.size.to_pt() / text.font.units_per_em();

        let mut x = 0.0;
        let mut y = 0.0;
        for glyph in &text.glyphs {
            let id = ttf_parser::GlyphId(glyph.id);
            // Fonts use a y-up coordinate system.
            let origin = (
                x + glyph.x_offset.at(text.size).to_pt(),
                -(y + glyph.y_offset.at(text.size).to_pt()),
            );
            x += glyph.x_advance.at(text.size).to_pt();
            y += glyph.y_advance.at(text.size).to_pt();

            let colour = ttf.glyph_raster_image(id, u16::MAX).is_some()
                || ttf.glyph_svg_image(id).is_some()
                || ttf.is_color_glyph(id);
            if colour {
                // Bitmap, SVG, and layered colour glyphs (emoji) have no
                // monochrome outline to draw as a path.
                self.unsupported(false, true, "colour glyph");
            }

            let row = r_int(self.scene.glyphs.id.len() + 1);
            let mut outline = OutlineSink {
                builder: self,
                ts,
                units,
                origin,
                owner: row,
                last: (0.0, 0.0),
            };
            let has_outline = ttf.outline_glyph(id, &mut outline).is_some();
            if !has_outline && !colour {
                // Whitespace draws nothing.
                continue;
            }

            let o = Point::new(Abs::pt(origin.0), Abs::pt(origin.1)).transform(ts);
            let z = self.next_z();
            let g = &mut self.scene.glyphs;
            g.font.push(font_row);
            g.id.push(i32::from(glyph.id));
            g.x.push(o.x.to_pt());
            g.y.push(o.y.to_pt());
            g.size.push(text.size.to_pt() * scale);
            g.rot.push(rotation);
            g.fill.push(fill.clone());
            g.stroke.push(stroke.clone());
            g.stroke_width.push(stroke_width);
            g.z.push(z);
            g.clip.push(self.current_clip);
        }
    }

    fn shape(&mut self, shape: &typst::visualize::Shape, ts: Transform) {
        let fill = match &shape.fill {
            None => String::new(),
            Some(paint) => match solid_hex(paint) {
                Some(hex) => hex,
                None => {
                    self.unsupported(false, false, "non-solid shape fill");
                    return;
                }
            },
        };
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
            self.push_shape(&fill, &ShapeStroke::none(), rule, closed, &subpaths);
            return;
        };
        let Some(hex) = solid_hex(&stroke.paint) else {
            self.unsupported(false, false, "non-solid shape stroke");
            return;
        };
        let scale = max_scale(ts);
        let style = ShapeStroke {
            colour: hex,
            width: stroke.thickness.to_pt() * scale,
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
            None => self.push_shape(&fill, &style, rule, closed, &subpaths),
            Some(dash) => {
                // Fill the shape, then stroke its outline cut into dashes.
                if !fill.is_empty() {
                    self.push_shape(&fill, &ShapeStroke::none(), rule, true, &subpaths);
                }
                let array: Vec<f64> = dash.array.iter().map(|a| a.to_pt() * scale).collect();
                let dashes = dash_subpaths(&subpaths, closed, &array, dash.phase.to_pt() * scale);
                self.push_shape("", &style, rule, false, &dashes);
            }
        }
    }

    fn push_shape(
        &mut self,
        fill: &str,
        stroke: &ShapeStroke,
        rule: &str,
        closed: bool,
        subpaths: &[Vec<(f64, f64)>],
    ) {
        let z = self.next_z();
        let row = r_int(self.scene.shapes.fill.len() + 1);
        for subpath in subpaths {
            self.next_shape_sub += 1;
            let sub = self.next_shape_sub;
            for &point in subpath {
                push(&mut self.scene.shape_points, point, sub, row);
            }
        }
        let s = &mut self.scene.shapes;
        s.fill.push(fill.to_string());
        s.stroke.push(stroke.colour.clone());
        s.stroke_width.push(stroke.width);
        s.rule.push(rule.to_string());
        s.lineend.push(stroke.lineend.to_string());
        s.linejoin.push(stroke.linejoin.to_string());
        s.closed.push(i32::from(closed));
        s.z.push(z);
        s.clip.push(self.current_clip);
    }

    /// 1-based row of `font` in the scene's font table, adding it if needed.
    fn font_row(&mut self, font: &Font) -> i32 {
        let Some(found) = lookup_font(font) else {
            // Not from the global store: glyph mode needs a font file.
            self.unsupported(true, false, "font without a file");
            return 0;
        };
        if let Some(row) = self.font_rows.get(&found.store_index) {
            return *row;
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
        let row = r_int(f.path.len());
        self.font_rows.insert(found.store_index, row);
        row
    }
}

/// Receives a glyph outline in font units and records it flattened in page
/// coordinates.
struct OutlineSink<'a> {
    builder: &'a mut SceneBuilder,
    ts: Transform,
    /// Points per font unit.
    units: f64,
    origin: (f64, f64),
    owner: i32,
    last: (f64, f64),
}

impl OutlineSink<'_> {
    fn map(&self, x: f32, y: f32) -> (f64, f64) {
        let p = Point::new(
            Abs::pt(self.origin.0 + f64::from(x) * self.units),
            Abs::pt(self.origin.1 - f64::from(y) * self.units),
        )
        .transform(self.ts);
        (p.x.to_pt(), p.y.to_pt())
    }

    fn emit(&mut self, p: (f64, f64)) {
        let sub = self.builder.next_outline_sub;
        push(&mut self.builder.scene.outlines, p, sub, self.owner);
        self.last = p;
    }
}

impl ttf_parser::OutlineBuilder for OutlineSink<'_> {
    fn move_to(&mut self, x: f32, y: f32) {
        self.builder.next_outline_sub += 1;
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
        let mut points = Vec::new();
        flatten_cubic(p0, c1, c2, p, 0, &mut points);
        for point in points {
            self.emit(point);
        }
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        let (c1, c2, p) = (self.map(x1, y1), self.map(x2, y2), self.map(x, y));
        let mut points = Vec::new();
        flatten_cubic(self.last, c1, c2, p, 0, &mut points);
        for point in points {
            self.emit(point);
        }
    }

    fn close(&mut self) {}
}

/// Stroke style of a shape row; an empty colour means no stroke.
struct ShapeStroke {
    colour: String,
    width: f64,
    lineend: &'static str,
    linejoin: &'static str,
}

impl ShapeStroke {
    fn none() -> Self {
        Self {
            colour: String::new(),
            width: 0.0,
            lineend: "butt",
            linejoin: "mitre",
        }
    }
}

/// `n` as an R integer, saturating at R's largest integer.
fn r_int<T: TryInto<i32>>(n: T) -> i32 {
    n.try_into().unwrap_or(i32::MAX)
}

fn push(points: &mut Points, p: (f64, f64), sub: i32, owner: i32) {
    points.x.push(p.0);
    points.y.push(p.1);
    points.sub.push(sub);
    points.owner.push(owner);
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
fn flatten_cubic(
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
struct FontRef {
    store_index: usize,
    /// File path, or empty for a font embedded in the binary.
    path: String,
    collection_index: u32,
    cache_key: String,
}

static FONT_REFS: LazyLock<Mutex<HashMap<Font, FontRef>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Find `font` in the global font store.
fn lookup_font(font: &Font) -> Option<FontRef> {
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

/// Data of the font at `store_index` in the global font store.
pub fn font_data(store_index: usize) -> Option<Vec<u8>> {
    fonts::get_fonts()
        .font(store_index)
        .map(|font| font.data().as_slice().to_vec())
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
        let scene = extract_scene(&page.frame);
        (page.frame.clone(), scene)
    }

    fn assert_close(actual: f64, expected: f64) {
        assert!(
            (actual - expected).abs() < 1e-6,
            "expected {expected}, got {actual}"
        );
    }

    #[test]
    fn glyphs_sit_on_the_baseline_in_page_coordinates() {
        let (frame, scene) = scene_of("HH");
        let g = &scene.glyphs;
        assert_eq!(g.id.len(), 2);
        // With baseline as the bottom edge, the baseline is the page bottom.
        assert_close(g.y[0], frame.height().to_pt());
        assert_close(g.x[0], 0.0);
        assert!(g.x[1] > g.x[0]);
        assert_close(g.size[0], 20.0);
        assert_close(g.rot[0], 0.0);
        assert_eq!(g.fill[0], "#000000FF");
        assert!(scene.path_ok && scene.glyph_ok, "{:?}", scene.unsupported);
    }

    #[test]
    fn drawing_order_follows_the_frame() {
        // The box background is drawn before the text inside it.
        let (_, scene) = scene_of("#box(fill: yellow)[Hi] H");
        assert_eq!(scene.shapes.z, vec![1]);
        assert_eq!(scene.glyphs.z, vec![2, 3, 4]);
    }

    #[test]
    fn whitespace_produces_no_glyph() {
        let (_, scene) = scene_of("H H");
        assert_eq!(scene.glyphs.id.len(), 2);
    }

    #[test]
    fn outlines_belong_to_their_glyphs_and_match_the_ink() {
        let (frame, scene) = scene_of("Hxgy");
        let o = &scene.outlines;
        assert!(!o.x.is_empty());
        let owners: std::collections::BTreeSet<i32> = o.owner.iter().copied().collect();
        assert_eq!(owners.into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 4]);

        let Some(ink) = crate::bounds::ink_bounds(&frame) else {
            panic!("text should have ink");
        };
        // Font bounding boxes include Bézier control points, so the outline
        // lies inside them, a little within the box.
        let min_x = o.x.iter().copied().fold(f64::INFINITY, f64::min);
        let max_y = o.y.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        assert!(
            min_x >= ink.min_x - 1e-6 && min_x - ink.min_x < 0.1,
            "{min_x} vs {}",
            ink.min_x
        );
        assert!(
            max_y <= ink.max_y + 1e-6 && ink.max_y - max_y < 0.1,
            "{max_y} vs {}",
            ink.max_y
        );
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

        let (_, skewed) = scene_of("#scale(x: 200%, y: 100%, reflow: true)[H]");
        assert!(skewed.path_ok);
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
        let line = s.closed.iter().position(|&c| c == 0);
        let rect = s.closed.iter().position(|&c| c == 1);
        let (Some(line), Some(rect)) = (line, rect) else {
            panic!("expected a line and a rectangle: {s:?}");
        };
        assert!(s.stroke_width[line] > 0.0);
        assert_eq!(s.fill[rect], "#FF4136FF");
        let rect_points = scene
            .shape_points
            .owner
            .iter()
            .filter(|&&o| o == r_int(rect + 1))
            .count();
        assert_eq!(rect_points, 4);
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
        let Ok(store_index) = usize::try_from(f.store_index[0]) else {
            panic!("store index should be non-negative");
        };
        assert!(font_data(store_index).is_some_and(|data| !data.is_empty()));

        // The same font maps to the same row.
        let (_, two) = scene_of("H #text(weight: \"bold\")[H] H");
        assert_eq!(two.fonts.path.len(), 2);
        assert_eq!(two.glyphs.font, vec![1, 2, 1]);
    }

    #[test]
    fn dashed_strokes_are_cut_into_dashes() {
        // A 30pt line with 4pt dashes and 2pt gaps: dashes start at 0, 6, ...
        let (_, scene) = scene_of(
            "#line(length: 30pt, stroke: (paint: black, thickness: 1pt, dash: (4pt, 2pt)))",
        );
        assert!(scene.path_ok, "{:?}", scene.unsupported);
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
        assert_eq!(scene.shapes.closed, vec![0]);
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
        assert_eq!(s.fill[0], "#FF4136FF");
        assert_eq!(s.stroke[0], "");
        assert_eq!(s.fill[1], "");
        assert_eq!(s.stroke[1], "#000000FF");
    }

    #[test]
    fn clipped_groups_record_their_clip_region() {
        let (_, scene) = scene_of("#box(clip: true, width: 5pt)[HHH] H");
        assert!(scene.path_ok && scene.glyph_ok, "{:?}", scene.unsupported);
        let c = &scene.clips;
        assert!(c.owner.iter().all(|&o| o == 1));
        let max_x = c.x.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        assert_close(max_x, 5.0);
        // The three clipped glyphs, then the one after the box.
        assert_eq!(scene.glyphs.clip, vec![1, 1, 1, 0]);
    }

    #[test]
    fn unsupported_content_is_flagged() {
        let (_, nested) =
            scene_of("#box(clip: true, width: 8pt)[#box(clip: true, width: 5pt)[HHH]]");
        assert!(!nested.path_ok && !nested.glyph_ok);

        let (_, gradient) = scene_of("#text(fill: gradient.linear(red, blue))[H]");
        assert!(!gradient.path_ok);

        let (_, stroked) = scene_of("#text(stroke: 0.5pt + red)[H]");
        assert!(stroked.path_ok && !stroked.glyph_ok);
        assert_eq!(stroked.glyphs.stroke[0], "#FF4136FF");
    }
}
