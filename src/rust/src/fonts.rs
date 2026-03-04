use std::sync::{Arc, LazyLock};
use typst_kit::fonts::Fonts;

static FONTS: LazyLock<Arc<Fonts>> = LazyLock::new(|| {
    let fonts = typst_kit::fonts::FontSearcher::new()
        .include_embedded_fonts(true)
        .include_system_fonts(true)
        .search();
    Arc::new(fonts)
});

pub fn get_fonts() -> Arc<Fonts> {
    FONTS.clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world::InMemoryWorld;

    #[test]
    fn test_system_font_loading() {
        let fonts = get_fonts();

        let text = r#"#set text(font: "Arial")
        Hello World"#;

        let world = InMemoryWorld::new(text.to_string(), fonts);
        let result = world.compile_to_svg();

        assert!(result.is_ok(), "Typst compilation failed with system font");
    }

    #[test]
    fn test_unexist_font() {
        let fonts = get_fonts();

        let text = r#"#set text(font: "NonExistentFont")
        Hello World"#;

        let world = InMemoryWorld::new(text.to_string(), fonts);
        let result = world.compile_to_svg();

        assert!(
            result.is_ok(),
            "Typst compilation failed with non-existent font"
        );

        let Ok(rendered) = result else {
            panic!("unexpected compile failure: {result:?}");
        };

        assert!(
            !rendered.warnings.is_empty(),
            "expected warnings for non-existent font family"
        );

        assert!(
            rendered
                .warnings
                .iter()
                .any(|warning| warning.message.contains("unknown font family")),
            "expected unknown font family warning, got: {:?}",
            rendered.warnings
        );
    }
}
