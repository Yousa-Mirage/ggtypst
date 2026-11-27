use std::sync::{Arc, LazyLock, Once};
use typst::text::{FontFlags, FontStyle};
use typst_kit::fonts::FontStore;

static FONTS: LazyLock<Arc<FontStore>> = LazyLock::new(|| {
    let mut fonts = FontStore::new();
    fonts.extend(typst_kit::fonts::embedded());
    fonts.extend(typst_kit::fonts::system());
    Arc::new(fonts)
});

pub fn get_fonts() -> Arc<FontStore> {
    FONTS.clone()
}

/// The fonts Typst can use, one row each, as columns.
#[derive(Debug, Default)]
pub struct FontTable {
    /// The typographic family name, which `#set text(font: ...)` takes.
    pub family: Vec<String>,
    pub weight: Vec<i32>,
    /// `"normal"`, `"italic"`, or `"oblique"`.
    pub style: Vec<String>,
    /// Width in percent of the normal width.
    pub stretch: Vec<f64>,
    pub variable: Vec<bool>,
    /// Whether the font has a MATH table.
    pub math: Vec<bool>,
}

/// List the fonts of the global store.
pub fn font_table() -> FontTable {
    let store = get_fonts();
    let book = store.book();
    let mut table = FontTable::default();
    let mut index = 0;
    while let Some(info) = book.info(index) {
        let variant = info.variant;
        table.family.push(info.family.clone());
        table.weight.push(i32::from(variant.weight.to_number()));
        table.style.push(
            match variant.style {
                FontStyle::Normal => "normal",
                FontStyle::Italic => "italic",
                FontStyle::Oblique => "oblique",
            }
            .to_string(),
        );
        table.stretch.push(variant.stretch.to_ratio().get() * 100.0);
        table
            .variable
            .push(info.flags.contains(FontFlags::VARIABLE));
        table.math.push(info.flags.contains(FontFlags::MATH));
        index += 1;
    }
    table
}

/// Load the fonts on a background thread
pub fn warm_up() {
    static STARTED: Once = Once::new();
    STARTED.call_once(|| {
        #[cfg(unix)]
        fork::load_fonts_before_fork();
        let _ = std::thread::Builder::new()
            .name("ggtypst-fonts".to_string())
            .spawn(|| {
                // Hash the font book too: Typst hashes it on every compile,
                // and the shared book keeps the hash.
                let _ = typst::utils::hash128(FONTS.book());
            });
    });
}

/// A forked process has only the thread that forked. If R forked (as
/// `parallel::mclapply()` and future's multicore plan do) while the fonts were
/// loading, the child would wait for them forever, so every fork first waits
/// until they are loaded.
#[cfg(unix)]
mod fork {
    use std::ffi::c_int;
    use std::sync::LazyLock;

    unsafe extern "C" {
        fn pthread_atfork(
            prepare: Option<unsafe extern "C" fn()>,
            parent: Option<unsafe extern "C" fn()>,
            child: Option<unsafe extern "C" fn()>,
        ) -> c_int;
    }

    extern "C" fn finish_loading() {
        let _ = std::panic::catch_unwind(|| LazyLock::force(&super::FONTS));
    }

    pub(super) fn load_fonts_before_fork() {
        // SAFETY: the handler takes no arguments and cannot unwind. If it
        // cannot be registered, forks simply do not wait.
        unsafe {
            pthread_atfork(Some(finish_loading), None, None);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world::InMemoryWorld;

    #[test]
    fn warm_up_loads_the_shared_fonts_once() {
        warm_up();
        warm_up();
        let fonts = get_fonts();
        assert!(Arc::ptr_eq(&fonts, &get_fonts()));
        assert!(fonts.book().families().next().is_some());
    }

    #[test]
    fn font_table_lists_the_built_in_fonts() {
        let table = font_table();
        let n = table.family.len();
        assert!(n > 0);
        for column in [table.weight.len(), table.style.len(), table.math.len()] {
            assert_eq!(column, n);
        }

        let row = |family: &str, weight: i32| {
            (0..n).find(|&i| {
                table.family[i] == family && table.weight[i] == weight && table.style[i] == "normal"
            })
        };
        let Some(libertinus) = row("Libertinus Serif", 400) else {
            panic!("Libertinus Serif is missing");
        };
        assert!(!table.math[libertinus]);
        assert_eq!(table.stretch[libertinus], 100.0);
        assert!(row("Libertinus Serif", 600).is_some());

        let Some(math) = row("New Computer Modern Math", 400) else {
            panic!("New Computer Modern Math is missing");
        };
        assert!(table.math[math]);
    }

    #[test]
    fn test_system_font_loading() {
        let fonts = get_fonts();

        let text = r#"#set text(font: "Arial")
        Hello World"#;

        let world = InMemoryWorld::new(text.to_string(), fonts);
        let result = world.render(600.0);

        assert!(result.is_ok(), "Typst compilation failed with system font");
    }

    #[test]
    fn test_unexist_font() {
        let fonts = get_fonts();

        let text = r#"#set text(font: "NonExistentFont")
        Hello World"#;

        let world = InMemoryWorld::new(text.to_string(), fonts);
        let result = world.render(600.0);

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
