//! Shared helpers for the Rust tests.

use typst::layout::Frame;
use typst_layout::PagedDocument;

use crate::fonts;
use crate::scene::{Scene, extract_scene};
use crate::world::InMemoryWorld;

/// The page and text settings of the package's preamble, at 20pt.
pub const PREAMBLE: &str = concat!(
    "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
    "#set text(size: 20pt, top-edge: \"cap-height\", bottom-edge: \"baseline\")\n"
);

/// The first page of `body` after `PREAMBLE`, and its scene with images at
/// the default 600 dpi.
pub fn scene_of(body: &str) -> (Frame, Scene) {
    let world = InMemoryWorld::new(format!("{PREAMBLE}{body}"), fonts::get_fonts());
    let Ok(doc) = typst::compile::<PagedDocument>(&world).output else {
        panic!("source should compile: {body}");
    };
    let Some(page) = doc.pages().first() else {
        panic!("document should have a page");
    };
    (page.frame.clone(), extract_scene(page, 600.0))
}

/// The kinds of `scene`'s runs, in drawing order.
pub fn kinds(scene: &Scene) -> Vec<&str> {
    scene.runs.kind.iter().map(String::as_str).collect()
}

pub fn assert_close(actual: f64, expected: f64) {
    assert!(
        (actual - expected).abs() < 1e-6,
        "expected {expected}, got {actual}"
    );
}
