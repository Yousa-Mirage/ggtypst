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
