use std::path::Path;
use std::sync::{Arc, LazyLock};

use typst::diag::{FileError, FileResult};
use typst::foundations::{Bytes, Datetime};
use typst::syntax::{FileId, Source, VirtualPath};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Library, LibraryExt, World};
use typst_kit::fonts::Fonts;

const MITEX_MOD_TYP: &str = include_str!("../specs/mod.typ");
const MITEX_PRELUDE_TYP: &str = include_str!("../specs/prelude.typ");
const MITEX_STANDARD_TYP: &str = include_str!("../specs/latex/standard.typ");

static LIBRARY: LazyLock<LazyHash<Library>> =
    LazyLock::new(|| LazyHash::new(Library::builder().build()));

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
    book: LazyHash<FontBook>,
    fonts: Arc<Fonts>,
}

impl World for InMemoryWorld {
    fn library(&self) -> &LazyHash<Library> {
        self.library
    }

    fn book(&self) -> &LazyHash<FontBook> {
        &self.book
    }

    fn main(&self) -> FileId {
        self.main
    }

    fn source(&self, id: FileId) -> FileResult<Source> {
        if id == self.main {
            Ok(self.source.clone())
        } else {
            let p = id.vpath().as_rooted_path().to_path_buf();
            let normalized = normalize_virtual_path(&p);

            if let Some(contents) = virtual_typst_file(&normalized) {
                Ok(Source::new(id, contents.to_string()))
            } else {
                Err(FileError::NotFound(p))
            }
        }
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        if id == self.main {
            Ok(self.bytes.clone())
        } else {
            let p = id.vpath().as_rooted_path().to_path_buf();
            let normalized = normalize_virtual_path(&p);

            if let Some(contents) = virtual_typst_file(&normalized) {
                Ok(Bytes::from_string(contents))
            } else {
                Err(FileError::NotFound(p))
            }
        }
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.fonts.fonts.get(index)?.get()
    }

    fn today(&self, _offset: Option<i64>) -> Option<Datetime> {
        None
    }
}

impl InMemoryWorld {
    pub fn new(typst_code: String, fonts: Arc<Fonts>) -> Self {
        let main = FileId::new_fake(VirtualPath::new("<ggtypst>"));
        let source = Source::new(main, typst_code.clone());
        let bytes = Bytes::from_string(typst_code);
        let book = LazyHash::new(fonts.book.clone());

        Self {
            main,
            source,
            bytes,
            library: &LIBRARY,
            book,
            fonts,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
