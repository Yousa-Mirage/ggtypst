use std::sync::{Arc, LazyLock, Once};
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
