use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, LazyLock, Mutex, PoisonError};

use typst::diag::{FileError, FileResult};
use typst::foundations::{Bytes, Datetime, Duration};
use typst::syntax::{FileId, RootedPath, Source, VirtualPath, VirtualRoot};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Library, LibraryExt, World};
use typst_kit::fonts::FontStore;

const MITEX_MOD_TYP: &str = include_str!("../specs/mod.typ");
const MITEX_PRELUDE_TYP: &str = include_str!("../specs/prelude.typ");
const MITEX_STANDARD_TYP: &str = include_str!("../specs/latex/standard.typ");

static LIBRARY: LazyLock<LazyHash<Library>> =
    LazyLock::new(|| LazyHash::new(Library::builder().build()));

/// Parsed MiTeX files by file id.
static SPEC_SOURCES: LazyLock<Mutex<HashMap<FileId, Source>>> = LazyLock::new(Default::default);

fn spec_source(id: FileId, contents: &'static str) -> Source {
    let mut sources = SPEC_SOURCES.lock().unwrap_or_else(PoisonError::into_inner);
    sources
        .entry(id)
        .or_insert_with(|| Source::new(id, contents.to_string()))
        .clone()
}

fn virtual_typst_file(path: &str) -> Option<&'static str> {
    match path {
        "/specs/mod.typ" => Some(MITEX_MOD_TYP),
        "/specs/prelude.typ" => Some(MITEX_PRELUDE_TYP),
        "/specs/latex/standard.typ" => Some(MITEX_STANDARD_TYP),
        _ => None,
    }
}

fn normalize_virtual_path(path: &Path) -> String {
    let normalized = path.to_string_lossy().replace('\\', "/");

    if normalized.starts_with('/') {
        normalized
    } else {
        format!("/{normalized}")
    }
}

pub struct InMemoryWorld {
    main: FileId,
    source: Source,
    bytes: Bytes,
    library: &'static LazyHash<Library>,
    fonts: Arc<FontStore>,
}

impl World for InMemoryWorld {
    fn library(&self) -> &LazyHash<Library> {
        self.library
    }

    fn book(&self) -> &LazyHash<FontBook> {
        self.fonts.book()
    }

    fn main(&self) -> FileId {
        self.main
    }

    fn source(&self, id: FileId) -> FileResult<Source> {
        if id == self.main {
            Ok(self.source.clone())
        } else {
            let p = Path::new(id.vpath().get_with_slash()).to_path_buf();
            let normalized = normalize_virtual_path(&p);

            if let Some(contents) = virtual_typst_file(&normalized) {
                Ok(spec_source(id, contents))
            } else {
                Err(FileError::NotFound(p))
            }
        }
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        if id == self.main {
            Ok(self.bytes.clone())
        } else {
            let p = Path::new(id.vpath().get_with_slash()).to_path_buf();
            let normalized = normalize_virtual_path(&p);

            if let Some(contents) = virtual_typst_file(&normalized) {
                Ok(Bytes::from_string(contents))
            } else {
                Err(FileError::NotFound(p))
            }
        }
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.fonts.font(index)
    }

    fn today(&self, _offset: Option<Duration>) -> Option<Datetime> {
        None
    }
}

impl InMemoryWorld {
    pub fn new(typst_code: String, fonts: Arc<FontStore>) -> Self {
        let Ok(main_path) = VirtualPath::new("ggtypst.typ") else {
            unreachable!("static virtual path should be valid");
        };
        let main = RootedPath::new(VirtualRoot::Project, main_path).intern();
        let source = Source::new(main, typst_code.clone());
        let bytes = Bytes::from_string(typst_code);

        Self {
            main,
            source,
            bytes,
            library: &LIBRARY,
            fonts,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn worlds_share_the_font_book() {
        let fonts = crate::fonts::get_fonts();
        let a = InMemoryWorld::new("a".to_string(), fonts.clone());
        let b = InMemoryWorld::new("b".to_string(), fonts.clone());
        assert!(std::ptr::eq(a.book(), b.book()));
        assert!(std::ptr::eq(a.book(), fonts.book()));
    }

    #[test]
    fn worlds_share_parsed_mitex_files() {
        let Ok(path) = VirtualPath::new("/specs/mod.typ") else {
            panic!("the path should be valid");
        };
        let id = RootedPath::new(VirtualRoot::Project, path).intern();
        let fonts = crate::fonts::get_fonts();
        let a = InMemoryWorld::new("a".to_string(), fonts.clone());
        let b = InMemoryWorld::new("b".to_string(), fonts);
        let (Ok(a), Ok(b)) = (a.source(id), b.source(id)) else {
            panic!("MiTeX's module should be found");
        };
        assert!(std::ptr::eq(a.text(), b.text()));
    }

    #[test]
    fn normalize_virtual_path_accepts_unix_style_paths() {
        let normalized = normalize_virtual_path(Path::new("/specs/mod.typ"));
        assert_eq!(normalized, "/specs/mod.typ");
    }

    #[test]
    fn normalize_virtual_path_accepts_windows_style_paths() {
        let normalized = normalize_virtual_path(Path::new(r"\specs\mod.typ"));
        assert_eq!(normalized, "/specs/mod.typ");
    }

    #[test]
    fn virtual_typst_file_matches_normalized_windows_path() {
        let normalized = normalize_virtual_path(Path::new(r"\specs\latex\standard.typ"));
        assert!(virtual_typst_file(&normalized).is_some());
    }
}
