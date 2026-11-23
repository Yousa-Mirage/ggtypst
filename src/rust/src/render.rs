use extendr_api::prelude::{IntoRobj, List, list};
use typst::compile;
use typst::diag::Warned;
use typst::layout::{Abs, FrameItem, Point, Size};
use typst_layout::{Page, PagedDocument};

use crate::bounds::{Bounds, ink_bounds, max_descent_pt};
use crate::error::{RenderDiagnostic, RenderError, diagnostics_to_r_list};
use crate::scene::{Scene, extract_scene};
use crate::world::InMemoryWorld;

/// The first page of a compiled Typst document, described for R.
#[derive(Debug)]
pub struct RenderedPage {
    /// Size of the layout box (the page).
    pub width_pt: f64,
    pub height_pt: f64,
    pub descent_pt: f64,
    pub scene: Scene,
    /// The page as SVG, only when the scene cannot be drawn with paths.
    pub fallback: Option<FallbackSvg>,
    pub warnings: Vec<RenderDiagnostic>,
}

impl RenderedPage {
    pub fn into_r_list(self) -> List {
        let fallback = self
            .fallback
            .map_or_else(|| ().into_robj(), |f| f.into_r_list().into_robj());
        list!(
            width_pt = self.width_pt,
            height_pt = self.height_pt,
            descent_pt = self.descent_pt,
            scene = self.scene.into_r_list(),
            fallback = fallback,
            warnings = diagnostics_to_r_list(&self.warnings)
        )
    }
}

/// The SVG fallback for a page, when the scene cannot be drawn with paths.
#[derive(Debug)]
pub struct FallbackSvg {
    /// The page as SVG, encoded as UTF-8 bytes.
    pub svg: Vec<u8>,
    /// The canvas size in points.
    pub width_pt: f64,
    pub height_pt: f64,
    /// The page's top-left corner is at (`offset_x_pt`, `offset_y_pt`) on the canvas.
    pub offset_x_pt: f64,
    pub offset_y_pt: f64,
}

impl FallbackSvg {
    fn into_r_list(self) -> List {
        list!(
            svg = self.svg,
            width_pt = self.width_pt,
            height_pt = self.height_pt,
            offset_x_pt = self.offset_x_pt,
            offset_y_pt = self.offset_y_pt
        )
    }
}

impl InMemoryWorld {
    /// Compile the document and describe its first page. Items that grid
    /// cannot draw as glyphs or paths are drawn as images at `raster_dpi`.
    pub fn render(&self, raster_dpi: f64) -> Result<RenderedPage, RenderError> {
        let Warned { output, warnings } = compile::<PagedDocument>(&self);

        let warnings: Vec<RenderDiagnostic> = warnings.iter().map(RenderDiagnostic::from).collect();

        let output = output.map_err(|diagnostics| RenderError::CompilationFailed {
            diagnostics: diagnostics.iter().map(RenderDiagnostic::from).collect(),
        })?;

        let page = output
            .pages()
            .first()
            .ok_or(RenderError::NoPagesGenerated)?;

        if !has_visual_content(&page.frame) {
            return Err(RenderError::EmptyPage);
        }

        let scene = extract_scene(page, raster_dpi);

        // Only generate an SVG fallback if the scene cannot be drawn with paths.
        let fallback = if scene.path_ok {
            None
        } else {
            Some(fallback_svg(page))
        };

        Ok(RenderedPage {
            width_pt: page.frame.width().to_pt(),
            height_pt: page.frame.height().to_pt(),
            descent_pt: max_descent_pt(&page.frame),
            scene,
            fallback,
            warnings,
        })
    }
}

/// Render `page` as SVG on a canvas covering all of its ink.
fn fallback_svg(page: &Page) -> FallbackSvg {
    let page_bounds = Bounds {
        min_x: 0.0,
        min_y: 0.0,
        max_x: page.frame.width().to_pt(),
        max_y: page.frame.height().to_pt(),
    };
    let canvas = ink_bounds(&page.frame).map_or(page_bounds, |ink| ink.union(page_bounds));

    let offset_x_pt = -canvas.min_x;
    let offset_y_pt = -canvas.min_y;
    let width_pt = canvas.width().ceil();
    let height_pt = canvas.height().ceil();

    let mut page = page.clone();
    page.frame
        .translate(Point::new(Abs::pt(offset_x_pt), Abs::pt(offset_y_pt)));
    page.frame
        .set_size(Size::new(Abs::pt(width_pt), Abs::pt(height_pt)));

    FallbackSvg {
        svg: typst_svg::svg(&page, &typst_svg::SvgOptions::default()).into_bytes(),
        width_pt,
        height_pt,
        offset_x_pt,
        offset_y_pt,
    }
}

/// Check if there are any visual content items.
fn has_visual_content(frame: &typst::layout::Frame) -> bool {
    for (_, item) in frame.items() {
        match item {
            // If we encounter a nested Group, recursively check its internal frame
            FrameItem::Group(group) => {
                if has_visual_content(&group.frame) {
                    return true;
                }
            }
            // A Text, Shape, or Image item draws something
            FrameItem::Text(..) | FrameItem::Shape(..) | FrameItem::Image(..) => {
                return true;
            }
            // Ignore all Meta tags, links, bookmarks, and other non-visible elements
            _ => {}
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fonts;
    use crate::mitex;
    use crate::world::InMemoryWorld;
    use typst::layout::{Abs, Frame, GroupItem, Point, Size};
    use typst::syntax::Span;
    use typst::visualize::{Color, Geometry};

    fn compile_source(source: &str) -> Result<RenderedPage, RenderError> {
        let world = InMemoryWorld::new(source.to_string(), fonts::get_fonts());
        world.render(300.0)
    }

    fn compile_latex(latex_source: &str) -> Result<RenderedPage, RenderError> {
        let fragment = mitex::convert_latex_to_typst(latex_source)?;
        let typst_source = format!(
            "{import}\n#let _ggtypst_mitex_expr = \"{fragment}\"\n{eval_expr}",
            import = r#"#import "/specs/mod.typ": mitex-scope"#,
            eval_expr = r#"#eval("$ " + _ggtypst_mitex_expr + " $", scope: mitex-scope)"#
        );
        compile_source(&typst_source)
    }

    /// The SVG fallback of `source`'s page. No content needs it any more, so
    /// it is made directly.
    fn fallback_of(source: &str) -> (RenderedPage, FallbackSvg) {
        let world = InMemoryWorld::new(source.to_string(), fonts::get_fonts());
        let Ok(doc) = typst::compile::<PagedDocument>(&world).output else {
            panic!("source should compile: {source}");
        };
        let Some(page) = doc.pages().first() else {
            panic!("document should have a page");
        };
        let Ok(rendered) = compile_source(source) else {
            panic!("source should render: {source}");
        };
        (rendered, fallback_svg(page))
    }

    #[test]
    fn test_render_success_for_visible_text() {
        let result = compile_source("Hello, Typst!");
        assert!(result.is_ok(), "visible text should render successfully");

        let Ok(rendered) = result else {
            panic!("unexpected render failure: {result:?}");
        };

        assert!(rendered.width_pt > 0.0, "rendered width should be positive");
        assert!(
            rendered.height_pt > 0.0,
            "rendered height should be positive"
        );
        assert!(
            !rendered.scene.glyphs.id.is_empty(),
            "rendered scene should have glyphs"
        );
    }

    #[test]
    fn test_render_needs_no_svg_for_content_grid_can_draw() {
        // Gradients are images and nested clip regions are grid groups.
        for source in [
            "Hello, Typst!",
            "#text(fill: gradient.linear(red, blue))[Hello]",
            "#box(clip: true, width: 8pt)[#box(clip: true, width: 5pt)[HHH]]",
        ] {
            let Ok(rendered) = compile_source(source) else {
                panic!("source should render: {source}");
            };
            assert!(rendered.fallback.is_none(), "{source}");
        }
    }

    #[test]
    fn test_fallback_canvas_hugs_the_page_when_no_ink_overhangs() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
            "#box(width: 20.5pt, height: 10.25pt, fill: black)"
        );
        let (_, fallback) = fallback_of(source);

        assert_eq!(fallback.offset_x_pt, 0.0);
        assert_eq!(fallback.offset_y_pt, 0.0);
        assert_eq!(fallback.width_pt, 21.0);
        assert_eq!(fallback.height_pt, 11.0);

        let svg = String::from_utf8_lossy(&fallback.svg);
        assert!(
            svg.contains("viewBox=\"0 0 21 "),
            "svg header should use the canvas"
        );
    }

    #[test]
    fn test_fallback_canvas_covers_overhanging_ink() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
            "#set text(size: 40pt)\n",
            "#text(style: \"italic\")[f]"
        );
        let (rendered, fallback) = fallback_of(source);

        // The italic f overhangs its advance box on both sides.
        assert!(fallback.offset_x_pt > 0.0, "{fallback:?}");
        assert!(fallback.width_pt > fallback.offset_x_pt + rendered.width_pt);
        assert_eq!(fallback.width_pt, fallback.width_pt.trunc());
        assert_eq!(fallback.height_pt, fallback.height_pt.trunc());
    }

    #[test]
    fn test_render_returns_empty_page_error_for_default_preamble_set() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
            "#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")"
        );
        let result = compile_source(source);

        assert!(
            matches!(result, Err(RenderError::EmptyPage)),
            "expected EmptyPage for non-visual source, got: {result:?}"
        );
    }

    #[test]
    fn test_render_returns_compilation_failed_for_invalid_source() {
        let result = compile_source("#let x =");

        match result {
            Err(RenderError::CompilationFailed { diagnostics }) => {
                assert!(
                    !diagnostics.is_empty(),
                    "compilation failure should include diagnostics"
                );
            }
            other => panic!("expected compilation failure, got: {other:?}"),
        }
    }

    #[test]
    fn test_render_supports_latex_formula_via_mitex() {
        let result = compile_latex(r#"\frac{1}{2} + \sqrt{3}"#);

        let Ok(rendered) = result else {
            panic!("latex formula should render successfully: {result:?}");
        };

        assert!(rendered.width_pt > 0.0, "width should be positive");
        assert!(rendered.height_pt > 0.0, "height should be positive");
    }

    #[test]
    fn test_render_returns_mitex_error_for_invalid_latex() {
        let result = compile_latex(r#"\end{}"#);

        assert!(
            matches!(result, Err(RenderError::MitexConversionFailed { .. })),
            "expected MiTeX conversion failure, got: {result:?}"
        );
    }

    #[test]
    fn test_has_visual_content_returns_false_for_empty_frame() {
        let frame = Frame::soft(Size::new(Abs::pt(10.0), Abs::pt(10.0)));
        assert!(!has_visual_content(&frame));
    }

    #[test]
    fn test_has_visual_content_returns_true_for_shape_item() {
        let mut frame = Frame::soft(Size::new(Abs::pt(10.0), Abs::pt(10.0)));
        let shape = Geometry::Rect(Size::new(Abs::pt(1.0), Abs::pt(1.0))).filled(Color::BLACK);
        frame.push(Point::zero(), FrameItem::Shape(shape, Span::detached()));

        assert!(has_visual_content(&frame));
    }

    #[test]
    fn test_has_visual_content_checks_nested_groups_recursively() {
        let mut inner = Frame::soft(Size::new(Abs::pt(10.0), Abs::pt(10.0)));
        let shape = Geometry::Rect(Size::new(Abs::pt(1.0), Abs::pt(1.0))).filled(Color::BLACK);
        inner.push(Point::zero(), FrameItem::Shape(shape, Span::detached()));

        let mut outer = Frame::soft(Size::new(Abs::pt(10.0), Abs::pt(10.0)));
        outer.push(Point::zero(), FrameItem::Group(GroupItem::new(inner)));

        assert!(has_visual_content(&outer));
    }
}
