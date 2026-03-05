use extendr_api::prelude::*;

mod error;
mod fonts;
mod mitex_integration;
mod render;
mod world;

#[extendr]
fn typst_svg_impl(source_code: &str, is_latex: bool) -> List {
    let source_code = if is_latex {
        match mitex_integration::convert_latex_to_typst_source(source_code) {
            Ok(source) => source,
            Err(err) => return err.to_typst_error(),
        }
    } else {
        source_code.to_string()
    };

    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(source_code, fonts);

    match world.compile_to_svg() {
        Ok(rendered_svg) => rendered_svg.to_r_list(),
        Err(err) => err.to_typst_error(),
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ggtypst;
    fn typst_svg_impl;
}
