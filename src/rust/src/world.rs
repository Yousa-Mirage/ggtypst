use chrono::Datelike;
use std::sync::Arc;

use typst::diag::{FileError, FileResult};
use typst::foundations::{Bytes, Datetime};
use typst::syntax::{FileId, Source, VirtualPath};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Library, LibraryExt, World};
use typst_kit::fonts::Fonts;

pub struct InMemoryWorld {
    main: FileId,
    source: Source,
    bytes: Bytes,
    library: LazyHash<Library>,
    book: LazyHash<FontBook>,
    fonts: Arc<Fonts>,
}

impl World for InMemoryWorld {
    fn library(&self) -> &LazyHash<Library> {
        &self.library
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
            Err(FileError::NotFound(p))
        }
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        if id == self.main {
            Ok(self.bytes.clone())
        } else {
            let p = id.vpath().as_rooted_path().to_path_buf();
            Err(FileError::NotFound(p))
        }
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.fonts.fonts.get(index)?.get()
    }

    fn today(&self, offset: Option<i64>) -> Option<Datetime> {
        let now = chrono::Local::now();

        let naive = match offset {
            None => now.naive_local(),
            Some(o) => now.naive_utc() + chrono::Duration::hours(o),
        };

        Datetime::from_ymd(
            naive.year(),
            naive.month().try_into().ok()?,
            naive.day().try_into().ok()?,
        )
    }
}

impl InMemoryWorld {
    pub fn new(typst_src: String, fonts: Arc<Fonts>) -> Self {
        let main = FileId::new_fake(VirtualPath::new("<ggtypst>"));
        let source = Source::new(main, typst_src.clone());
        let bytes = Bytes::from_string(typst_src);
        let library = LazyHash::new(Library::builder().build());
        let book = LazyHash::new(fonts.book.clone());

        Self {
            main,
            source,
            bytes,
            library,
            book,
            fonts,
        }
    }
}
