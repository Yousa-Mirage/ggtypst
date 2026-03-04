use extendr_api::prelude::*;

mod fonts;
mod render;
mod world;

/// Return string `"Hello world!"` to R.
/// @export
#[extendr]
fn typst_svg(text: &str) -> List {
    let text = format!("#set page(width: auto, height: auto, margin: 0pt, fill: none)\n{text}");

    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(text, fonts);
    let rendered_svg = world.compile_to_svg();

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
