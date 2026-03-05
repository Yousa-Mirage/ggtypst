use extendr_api::prelude::{list, List};
use typst::compile;
use typst::diag::Warned;
use typst::layout::{FrameItem, PagedDocument};

use crate::error::{diagnostics_to_r_list, RenderDiagnostic, RenderError};
use crate::world::InMemoryWorld;

#[derive(Debug)]
pub struct RenderedSvg {
    pub svg: Vec<u8>,
    pub width_pt: f64,
    pub height_pt: f64,
    pub warnings: Vec<RenderDiagnostic>,
}

impl RenderedSvg {
    pub fn to_r_list(&self) -> List {
        list!(
            svg = self.svg.clone(),
            width_pt = self.width_pt,
            height_pt = self.height_pt,
            warnings = diagnostics_to_r_list(&self.warnings)
        )
    }
}

impl InMemoryWorld {
    pub fn compile_to_svg(&self) -> Result<RenderedSvg, RenderError> {
        let Warned { output, warnings } = compile::<PagedDocument>(&self);

        let warnings: Vec<RenderDiagnostic> = warnings.iter().map(RenderDiagnostic::from).collect();

        let output = output.map_err(|diagnostics| RenderError::CompilationFailed {
            diagnostics: diagnostics.iter().map(RenderDiagnostic::from).collect(),
        })?;

        let first_page = output.pages.first().ok_or(RenderError::NoPagesGenerated)?;

        if !has_visual_content(&first_page.frame) {
            return Err(RenderError::EmptySvg);
        }

        let svg = typst_svg::svg(first_page).into_bytes();
        let width_pt = first_page.frame.width().to_pt();
        let height_pt = first_page.frame.height().to_pt();

        Ok(RenderedSvg {
            svg,
            width_pt,
            height_pt,
            warnings,
        })
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
            // Once we encounter a Text, Shape, or Image item, we can conclude that the SVG will have visual content
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
    use crate::mitex_integration;
    use crate::world::InMemoryWorld;
    use typst::layout::{Abs, Frame, GroupItem, Point, Size};
    use typst::syntax::Span;
    use typst::visualize::{Color, Geometry};

    fn compile_source(source: &str) -> Result<RenderedSvg, RenderError> {
        let world = InMemoryWorld::new(source.to_string(), fonts::get_fonts());
        world.compile_to_svg()
    }

    fn compile_latex(latex_source: &str) -> Result<RenderedSvg, RenderError> {
        let fragment = mitex_integration::convert_latex_to_typst(latex_source)?;
        let typst_source = format!(
            "{import}\n{let_expr}\n{eval_expr}",
            import = r#"#import "/specs/mod.typ": mitex-scope"#,
            let_expr = format!(r#"#let _ggtypst_mitex_expr = "{}""#, fragment),
            eval_expr = r#"#eval("$ " + _ggtypst_mitex_expr + " $", scope: mitex-scope)"#
        );
        compile_source(&typst_source)
    }

    #[test]
    fn test_compile_to_svg_success_for_visible_text() {
        let result = compile_source("Hello, Typst!");
        assert!(result.is_ok(), "visible text should render successfully");

        let Ok(rendered) = result else {
            panic!("unexpected render failure: {result:?}");
        };

        assert!(
            !rendered.svg.is_empty(),
            "rendered SVG bytes should not be empty"
        );
        assert!(rendered.width_pt > 0.0, "rendered width should be positive");
        assert!(
            rendered.height_pt > 0.0,
            "rendered height should be positive"
        );
    }

    #[test]
    fn test_compile_to_svg_returns_empty_svg_error_for_default_preamble_set() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0.1em, fill: none)\n",
            "#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")"
        );
        let result = compile_source(source);

        assert!(
            matches!(result, Err(RenderError::EmptySvg)),
            "expected EmptySvg for non-visual source, got: {result:?}"
        );
    }

    #[test]
    fn test_compile_to_svg_returns_compilation_failed_for_invalid_source() {
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
    fn test_compile_to_svg_supports_latex_formula_via_mitex() {
        let result = compile_latex(r#"\frac{1}{2} + \sqrt{3}"#);

        let Ok(rendered) = result else {
            panic!("latex formula should render successfully: {result:?}");
        };

        assert!(!rendered.svg.is_empty(), "rendered svg should not be empty");
        assert!(rendered.width_pt > 0.0, "width should be positive");
        assert!(rendered.height_pt > 0.0, "height should be positive");
    }

    #[test]
    fn test_compile_to_svg_returns_mitex_error_for_invalid_latex() {
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
