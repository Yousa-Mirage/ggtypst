use extendr_api::prelude::*;

mod error;
mod fonts;
mod render;
mod world;

/// Return raw SVG bytes and dimensions for the given Typst input text.
/// @param text The Typst input text to render.
/// @export
#[extendr]
fn typst_svg(text: &str) -> List {
    let text = format!("#set page(width: auto, height: auto, margin: 0pt, fill: none)\n{text}");

    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(text, fonts);
    let rendered_svg = match world.compile_to_svg() {
        Ok(rendered_svg) => rendered_svg,
        Err(err) => throw_r_error(err.to_string()),
    };

    list!(
        svg = rendered_svg.svg,
        width_pt = rendered_svg.width_pt,
        height_pt = rendered_svg.height_pt
    )
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ggtypst;
    fn typst_svg;
}
