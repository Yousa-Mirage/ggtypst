//! Images of scene items that grid cannot draw as native glyphs or paths:
//! text and shapes painted with gradients or tilings, images, and colour
//! glyphs (emoji).
//!
//! Typst's own renderer (typst-render) draws each such item on its own, so the
//! image looks exactly as Typst draws it. R places it with `grid::rasterGrob()`
//! over the item's ink bounds.

use extendr_api::prelude::{IntoRobj, List, list};
use typst::foundations::Smart;
use typst::layout::{Abs, Frame, FrameItem, GroupItem, Point, Size, Transform};
use typst::utils::Scalar;
use typst_layout::Page;
use typst_render::RenderOptions;

use crate::bounds::{Bounds, item_bounds};

/// Most pixels one image may have. Larger items get a lower resolution, so a
/// big gradient cannot use gigabytes of memory.
pub const MAX_PIXELS: f64 = 4_000_000.0;

/// An item drawn as an image.
#[derive(Debug)]
pub struct Raster {
    /// Where the image goes: points, y pointing down from the page's top.
    pub bounds: Bounds,
    pub width_px: u32,
    pub height_px: u32,
    /// Pixels row by row from the top, as R colours (`nativeRaster`).
    pub pixels: Vec<i32>,
}

/// Images of a scene, one row per image.
#[derive(Debug, Default)]
pub struct Rasters {
    /// Bottom-left corner and size of the image, in points.
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    pub width: Vec<f64>,
    pub height: Vec<f64>,
    pub width_px: Vec<i32>,
    pub height_px: Vec<i32>,
    /// Whether to smooth the image when it is scaled (not for pixelated
    /// images).
    pub interpolate: Vec<bool>,
    pub pixels: Vec<Vec<i32>>,
}

impl Rasters {
    /// Add `raster`. Its `y` is the bottom edge, still measured down the page.
    pub fn push(&mut self, raster: Raster, interpolate: bool) {
        let b = raster.bounds;
        self.x.push(b.min_x);
        self.y.push(b.max_y);
        self.width.push(b.width());
        self.height.push(b.height());
        self.width_px
            .push(i32::try_from(raster.width_px).unwrap_or(i32::MAX));
        self.height_px
            .push(i32::try_from(raster.height_px).unwrap_or(i32::MAX));
        self.interpolate.push(interpolate);
        self.pixels.push(raster.pixels);
    }

    pub fn into_r_list(self) -> List {
        let pixels = List::from_values(self.pixels.into_iter().map(IntoRobj::into_robj));
        list!(
            x = self.x,
            y = self.y,
            width = self.width,
            height = self.height,
            width_px = self.width_px,
            height_px = self.height_px,
            interpolate = self.interpolate,
            pixels = pixels
        )
    }
}

/// Draw `item`, transformed by `ts`, as an image at `dpi` (fewer if it would
/// exceed `MAX_PIXELS`). `container` is the size and transform of the hard
/// frame the item is in, which paint relative to its parent (the default for
/// text) spans. `page` supplies the page settings Typst renders with.
pub fn rasterize(
    item: &FrameItem,
    ts: Transform,
    container: (Size, Transform),
    page: &Page,
    dpi: f64,
) -> Option<Raster> {
    if !(dpi.is_finite() && dpi > 0.0) {
        return None;
    }
    // A pixel of margin keeps the antialiased edges.
    let bounds = item_bounds(item, ts)?.expand(72.0 / dpi);
    let (width, height) = (bounds.width(), bounds.height());
    if !(width > 0.0 && height > 0.0) {
        return None;
    }
    // Typst renders whole pixels, at least one each way: limit those.
    let pixels_at =
        |per_pt: f64| (width * per_pt).round().max(1.0) * (height * per_pt).round().max(1.0);
    let mut per_pt = dpi / 72.0;
    for _ in 0..64 {
        let pixels = pixels_at(per_pt);
        if pixels <= MAX_PIXELS {
            break;
        }
        per_pt *= (MAX_PIXELS / pixels).sqrt() * 0.999;
    }
    if pixels_at(per_pt) > MAX_PIXELS {
        // Thinner than a pixel and longer than the limit: nothing to see.
        return None;
    }

    // The item alone, moved so its ink bounds start at the image's corner,
    // inside a hard frame like its container so that paint relative to the
    // parent spans the same area as in Typst.
    let (container_size, container_ts) = container;
    let shift = Transform::translate(Abs::pt(-bounds.min_x), Abs::pt(-bounds.min_y));
    let content = match container_ts.invert() {
        Some(into_container) => {
            let mut hard = Frame::hard(container_size);
            hard.push(
                Point::zero(),
                FrameItem::Group(group_of(item, into_container.pre_concat(ts))),
            );
            let mut outer = GroupItem::new(hard);
            outer.transform = shift.pre_concat(container_ts);
            outer
        }
        None => group_of(item, shift.pre_concat(ts)),
    };
    let mut frame = Frame::soft(Size::new(Abs::pt(width), Abs::pt(height)));
    frame.push(Point::zero(), FrameItem::Group(content));
    let mut page = page.clone();
    page.frame = frame;
    page.fill = Smart::Custom(None);

    let pixmap = typst_render::render(
        &page,
        &RenderOptions {
            pixel_per_pt: Scalar::new(per_pt),
            render_bleed: false,
        },
    );
    let pixels = pixmap
        .pixels()
        .iter()
        .map(|pixel| {
            let c = pixel.demultiply();
            let colour = i32::from_le_bytes([c.red(), c.green(), c.blue(), c.alpha()]);
            // R reads this value as NA; the next alpha looks the same.
            if colour == i32::MIN {
                i32::from_le_bytes([0, 0, 0, 0x81])
            } else {
                colour
            }
        })
        .collect();
    Some(Raster {
        bounds,
        width_px: pixmap.width(),
        height_px: pixmap.height(),
        pixels,
    })
}

/// A group drawing `item` with transform `ts`.
fn group_of(item: &FrameItem, ts: Transform) -> GroupItem {
    let mut frame = Frame::soft(Size::zero());
    frame.push(Point::zero(), item.clone());
    let mut group = GroupItem::new(frame);
    group.transform = ts;
    group
}
