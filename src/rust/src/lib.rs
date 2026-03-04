use extendr_api::prelude::*;

mod error;
mod fonts;
mod render;
mod world;

#[extendr]
fn typst_svg_impl(source_code: &str) -> List {
    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(source_code.to_string(), fonts);

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_system_font_loading() {
        let fonts = fonts::get_fonts();

        let text = r#"#set text(font: "Arial")
        Hello World"#;

        let world = world::InMemoryWorld::new(text.to_string(), fonts.clone());
        let result = world.compile_to_svg();

        assert!(result.is_ok(), "Typst compilation failed with system font");
    }
}
