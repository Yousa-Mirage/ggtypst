use extendr_api::prelude::*;

mod error;
mod fonts;
mod mitex;
mod render;
mod world;

#[extendr]
fn rs_typst_svg(typst_code: &str) -> List {
    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(typst_code.to_string(), fonts);

    match world.compile_to_svg() {
        Ok(rendered_svg) => rendered_svg.into_r_list(),
        Err(err) => err.to_typst_error(),
    }
}

#[extendr]
fn rs_convert_latex_to_typst(latex_code: &str) -> Robj {
    match mitex::convert_latex_to_typst(latex_code) {
        Ok(typst_code) => r!(typst_code),
        Err(err) => err.to_typst_error().into_robj(),
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ggtypst;
    fn rs_typst_svg;
    fn rs_convert_latex_to_typst;
}
