//! Bounding box of the ink that a Typst frame draws.
//!
//! Typst lays text out by advance widths, so glyph ink can extend past the
//! page (italic overhangs, descender tails, content moved with `#move` or
//! rotated without reflow). The renderer sizes the SVG canvas from these
//! bounds so that nothing is cut off.

use typst::layout::{Abs, Frame, FrameItem, Point, Ratio, Rect, Transform};
use typst::text::TextItem;
use typst::text::color::{glyph_frame, should_outline};

/// Axis-aligned bounds in page coordinates: points, y pointing down.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Bounds {
    pub min_x: f64,
    pub min_y: f64,
    pub max_x: f64,
    pub max_y: f64,
}

impl Bounds {
    pub fn union(self, other: Self) -> Self {
        Self {
            min_x: self.min_x.min(other.min_x),
            min_y: self.min_y.min(other.min_y),
            max_x: self.max_x.max(other.max_x),
            max_y: self.max_y.max(other.max_y),
        }
    }

    pub fn width(&self) -> f64 {
        self.max_x - self.min_x
    }

    pub fn height(&self) -> f64 {
        self.max_y - self.min_y
    }

    pub fn expand(self, by: f64) -> Self {
        Self {
            min_x: self.min_x - by,
            min_y: self.min_y - by,
            max_x: self.max_x + by,
            max_y: self.max_y + by,
        }
    }

    /// Bounds of `rect` after transforming it by `ts`, or `None` if the
    /// rectangle is not finite (e.g. the bbox of a run without outlines).
    fn of_rect(rect: Rect, ts: Transform) -> Option<Self> {
        let corners = [
            rect.min,
            Point::new(rect.max.x, rect.min.y),
            Point::new(rect.min.x, rect.max.y),
            rect.max,
        ]
        .map(|p| p.transform(ts));

        let xs = corners.map(|p| p.x.to_pt());
        let ys = corners.map(|p| p.y.to_pt());
        if !xs.iter().chain(ys.iter()).all(|v| v.is_finite()) {
            return None;
        }

        Some(Self {
            min_x: xs.into_iter().fold(f64::INFINITY, f64::min),
            min_y: ys.into_iter().fold(f64::INFINITY, f64::min),
            max_x: xs.into_iter().fold(f64::NEG_INFINITY, f64::max),
            max_y: ys.into_iter().fold(f64::NEG_INFINITY, f64::max),
        })
    }
}

/// Ink bounds of everything `frame` draws, or `None` if it draws nothing.
#[cfg(test)]
pub fn ink_bounds(frame: &Frame) -> Option<Bounds> {
    frame_bounds(frame, Transform::identity())
}

fn frame_bounds(frame: &Frame, ts: Transform) -> Option<Bounds> {
    frame
        .items()
        .filter_map(|(pos, item)| {
            // Same composition order as typst-svg: item position, then the
            // group's own transform.
            item_bounds(item, ts.pre_concat(Transform::translate(pos.x, pos.y)))
        })
        .reduce(Bounds::union)
}

/// Ink bounds of `item` drawn with transform `ts`, or `None` if it draws
/// nothing.
pub fn item_bounds(item: &FrameItem, ts: Transform) -> Option<Bounds> {
    match item {
        FrameItem::Group(group) => frame_bounds(&group.frame, ts.pre_concat(group.transform)),
        FrameItem::Text(text) => text_bounds(text, ts),
        FrameItem::Shape(shape, _) => Bounds::of_rect(shape.bbox(true), ts),
        FrameItem::Image(_, size, _) => {
            Bounds::of_rect(Rect::from_pos_size(Point::zero(), *size), ts)
        }
        FrameItem::Link(..) | FrameItem::Tag(_) => None,
    }
}

fn text_bounds(text: &TextItem, ts: Transform) -> Option<Bounds> {
    let ttf = text.font.ttf();
    let metrics = text.font.metrics();
    let size = text.size;

    // Glyph origins advance along the run. Fonts use a y-up coordinate system,
    // so font y values are negated to get page coordinates.
    let mut x = Abs::zero();
    let mut y = Abs::zero();
    let mut bounds: Option<Bounds> = None;

    for glyph in &text.glyphs {
        let id = ttf_parser::GlyphId(glyph.id);
        let origin_x = x + glyph.x_offset.at(size);
        let origin_y = -(y + glyph.y_offset.at(size));

        // Colour glyphs (emoji) are drawn from a frame in font units, whose
        // image need not fit the glyph's outline box: bitmap emoji reach below
        // the baseline. Bound what is drawn, as typst-render places it.
        if !should_outline(&text.font, id)
            && let Some(frame) = glyph_frame(&text.font, glyph.id)
        {
            let scale = Ratio::new(size.to_pt() / text.font.units_per_em());
            let glyph_ts = ts
                .pre_concat(Transform::translate(origin_x, origin_y))
                .pre_concat(Transform::scale(scale, scale));
            if let Some(glyph_bounds) = frame_bounds(&frame.into(), glyph_ts) {
                bounds = Some(match bounds {
                    Some(acc) => acc.union(glyph_bounds),
                    None => glyph_bounds,
                });
            }
            x += glyph.x_advance.at(size);
            y += glyph.y_advance.at(size);
            continue;
        }

        let rect = if let Some(outline) = ttf.glyph_bounding_box(id) {
            Some(Rect::new(
                Point::new(
                    origin_x + text.font.to_em(outline.x_min).at(size),
                    origin_y - text.font.to_em(outline.y_max).at(size),
                ),
                Point::new(
                    origin_x + text.font.to_em(outline.x_max).at(size),
                    origin_y - text.font.to_em(outline.y_min).at(size),
                ),
            ))
        } else if ttf.glyph_raster_image(id, u16::MAX).is_some()
            || ttf.glyph_svg_image(id).is_some()
            || ttf.is_color_glyph(id)
        {
            // Bitmap, SVG, or layered colour glyphs (emoji) have no outline:
            // use the glyph's advance box from ascender to descender.
            Some(Rect::new(
                Point::new(origin_x, origin_y - metrics.ascender.at(size)),
                Point::new(
                    origin_x + glyph.x_advance.at(size),
                    origin_y - metrics.descender.at(size),
                ),
            ))
        } else {
            // Whitespace draws nothing.
            None
        };

        if let Some(glyph_bounds) = rect.and_then(|rect| Bounds::of_rect(rect, ts)) {
            bounds = Some(match bounds {
                Some(acc) => acc.union(glyph_bounds),
                None => glyph_bounds,
            });
        }

        x += glyph.x_advance.at(size);
        y += glyph.y_advance.at(size);
    }

    // A text stroke (also used for synthesized bold) extends past the outline.
    let bounds = bounds?;
    Some(match &text.stroke {
        Some(stroke) => bounds.expand(stroke.thickness.to_pt() / 2.0 * max_scale(ts)),
        None => bounds,
    })
}

/// Largest font descender among the text runs in `frame`, in points: how far
/// below the baseline descenders may reach. Zero if the frame has no text.
pub fn max_descent_pt(frame: &Frame) -> f64 {
    frame
        .items()
        .map(|(_, item)| match item {
            FrameItem::Text(text) => -text.font.metrics().descender.at(text.size).to_pt(),
            FrameItem::Group(group) => max_descent_pt(&group.frame),
            _ => 0.0,
        })
        .fold(0.0, f64::max)
}

/// Upper bound on how much `ts` stretches a length.
fn max_scale(ts: Transform) -> f64 {
    let column_x = ts.sx.get().abs() + ts.ky.get().abs();
    let column_y = ts.kx.get().abs() + ts.sy.get().abs();
    column_x.max(column_y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fonts;
    use crate::world::InMemoryWorld;
    use typst_layout::PagedDocument;

    const PREAMBLE: &str = concat!(
        "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
        "#set text(size: 20pt, top-edge: \"bounds\", bottom-edge: \"bounds\")\n"
    );

    fn page_and_ink(body: &str) -> (f64, f64, Bounds) {
        let world = InMemoryWorld::new(format!("{PREAMBLE}{body}"), fonts::get_fonts());
        let Ok(doc) = typst::compile::<PagedDocument>(&world).output else {
            panic!("source should compile: {body}");
        };
        let Some(page) = doc.pages().first() else {
            panic!("document should have a page");
        };
        let Some(ink) = ink_bounds(&page.frame) else {
            panic!("page should draw something: {body}");
        };
        (page.frame.width().to_pt(), page.frame.height().to_pt(), ink)
    }

    fn assert_close(actual: f64, expected: f64) {
        assert!(
            (actual - expected).abs() < 1e-6,
            "expected {expected}, got {actual}"
        );
    }

    #[test]
    fn shapes_are_bounded_exactly() {
        let (width, height, ink) = page_and_ink("#box(width: 30pt, height: 10pt, fill: black)");
        assert_close(width, 30.0);
        assert_close(ink.min_x, 0.0);
        assert_close(ink.max_x, 30.0);
        assert_close(ink.max_y - ink.min_y, 10.0);
        assert!(ink.max_y <= height + 1e-6);
    }

    #[test]
    fn moved_content_outside_the_page_is_included() {
        // A 2pt box laid out at x = 0 and drawn 10pt further left.
        let (width, _, ink) =
            page_and_ink("#move(dx: -10pt)[#box(width: 2pt, height: 2pt, fill: black)]");
        assert_close(width, 2.0);
        assert_close(ink.min_x, -10.0);
        assert_close(ink.max_x, -8.0);
    }

    #[test]
    fn rotation_without_reflow_is_included() {
        // A 10pt square rotated 45deg about its centre reaches 5 * sqrt(2)
        // from the centre, beyond its unrotated box.
        let (_, _, ink) =
            page_and_ink("#rotate(45deg)[#box(width: 10pt, height: 10pt, fill: black)]");
        assert_close(ink.width(), 10.0 * 2f64.sqrt());
        assert_close(ink.min_x, 5.0 - 5.0 * 2f64.sqrt());
    }

    #[test]
    fn italic_overhang_extends_past_the_page() {
        let (width, _, ink) = page_and_ink("#text(style: \"italic\")[f]");
        assert!(
            ink.min_x < 0.0,
            "italic f should overhang on the left: {ink:?}"
        );
        assert!(
            ink.max_x > width,
            "italic f should overhang on the right: {ink:?}"
        );
    }

    #[test]
    fn upright_text_stays_inside_the_page() {
        // With `bounds` text edges the page hugs the ink vertically too.
        let (width, height, ink) = page_and_ink("Hello");
        assert!(ink.min_x >= 0.0 && ink.max_x <= width, "{ink:?}");
        assert!(ink.min_y >= -1e-6 && ink.max_y <= height + 1e-6, "{ink:?}");
    }

    #[test]
    fn descent_is_the_font_descender() {
        let world = InMemoryWorld::new(format!("{PREAMBLE}ace"), fonts::get_fonts());
        let Ok(doc) = typst::compile::<PagedDocument>(&world).output else {
            panic!("source should compile");
        };
        let Some(page) = doc.pages().first() else {
            panic!("document should have a page");
        };
        let descent = max_descent_pt(&page.frame);
        // Text without descenders still reports the font's descender.
        assert!(descent > 0.1 * 20.0 && descent < 0.4 * 20.0, "{descent}");
    }

    #[test]
    fn whitespace_draws_nothing() {
        let (_, _, ink) = page_and_ink("#box(width: 2pt, height: 2pt, fill: black)#h(10pt) ");
        assert_close(ink.max_x, 2.0);
    }
}
