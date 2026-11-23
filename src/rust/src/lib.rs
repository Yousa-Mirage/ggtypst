use extendr_api::prelude::*;

mod bounds;
mod error;
mod fonts;
mod mitex;
mod outlines;
mod render;
mod scene;
mod world;

/// Compile a Typst document and describe its first page for drawing.
#[extendr]
fn rs_typst_render(typst_code: &str) -> List {
    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(typst_code.to_string(), fonts);

    let result = world.render();

    // Like typst-cli's watch mode, drop entries unused in the last 10 renders,
    // so nearby repeated labels still reuse their results.
    typst::comemo::evict(10);

    match result {
        Ok(rendered) => rendered.into_r_list(),
        Err(err) => err.to_typst_error(),
    }
}

/// Outlines of a scene's glyphs, for drawing its text as paths.
#[extendr]
fn rs_glyph_outlines(fonts: List, glyphs: List) -> List {
    let outlines = outlines::from_r(&fonts, &glyphs)
        .and_then(|(fonts, glyphs)| outlines::glyph_outlines(&fonts, &glyphs));
    match outlines {
        Ok(outlines) => outlines.into_r_list(),
        Err(err) => err.to_typst_error(),
    }
}

/// Write a font built into ggtypst to `path`, so devices can load it.
#[extendr]
fn rs_typst_write_font(
    store_index: i32,
    collection_index: i32,
    cache_key: &str,
    path: &str,
) -> Robj {
    let written = scene::write_embedded_font(
        store_index,
        collection_index,
        cache_key,
        std::path::Path::new(path),
    );
    match written {
        Ok(()) => true.into_robj(),
        Err(message) => error::RenderError::FontFileFailed { message }
            .to_typst_error()
            .into_robj(),
    }
}

/// Convert LaTeX code to Typst code.
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
    fn rs_typst_render;
    fn rs_glyph_outlines;
    fn rs_typst_write_font;
    fn rs_convert_latex_to_typst;
}
