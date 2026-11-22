use extendr_api::prelude::{List, list};
use typst::compile;
use typst::diag::Warned;
use typst::layout::{Abs, FrameItem, Point, Size};
use typst_layout::PagedDocument;

use crate::bounds::{Bounds, ink_bounds, max_descent_pt};
use crate::error::{RenderDiagnostic, RenderError, diagnostics_to_r_list};
use crate::world::InMemoryWorld;

#[derive(Debug)]
pub struct RenderedSvg {
    pub svg: Vec<u8>,
    /// Size of the Typst page, which is the layout box of the content, in points.
    pub width_pt: f64,
    pub height_pt: f64,
    /// Size of the SVG canvas: the page extended to every bit of ink drawn
    /// outside it, then rounded up to whole points.
    pub canvas_width_pt: f64,
    pub canvas_height_pt: f64,
    /// Position of the page's top-left corner on the canvas, in points.
    pub offset_x_pt: f64,
    pub offset_y_pt: f64,
    /// Largest font descender on the page, in points: how far descenders may
    /// reach below the last baseline, which is the bottom of the layout box.
    pub descent_pt: f64,
    pub warnings: Vec<RenderDiagnostic>,
}

impl RenderedSvg {
    pub fn into_r_list(self) -> List {
        list!(
            svg = self.svg,
            width_pt = self.width_pt,
            height_pt = self.height_pt,
            canvas_width_pt = self.canvas_width_pt,
            canvas_height_pt = self.canvas_height_pt,
            offset_x_pt = self.offset_x_pt,
            offset_y_pt = self.offset_y_pt,
            descent_pt = self.descent_pt,
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

        let first_page = output
            .pages()
            .first()
            .ok_or(RenderError::NoPagesGenerated)?;

        if !has_visual_content(&first_page.frame) {
            return Err(RenderError::EmptySvg);
        }

        let width_pt = first_page.frame.width().to_pt();
        let height_pt = first_page.frame.height().to_pt();

        // The page has no margin, so it is exactly the content's layout box,
        // which labels are justified against. Ink can still extend past it
        // (italic overhangs, descender tails, moved or rotated content), so
        // the canvas covers the page and all of the ink.
        let page_bounds = Bounds {
            min_x: 0.0,
            min_y: 0.0,
            max_x: width_pt,
            max_y: height_pt,
        };
        let canvas =
            ink_bounds(&first_page.frame).map_or(page_bounds, |ink| ink.union(page_bounds));
        let offset_x_pt = -canvas.min_x;
        let offset_y_pt = -canvas.min_y;

        // SVG importers such as librsvg size their canvas in whole units and
        // rescale fractional-size documents to fit. Rounding the canvas up to
        // whole points (extra space on the right and bottom) keeps the drawing
        // at exactly 1:1 through that pipeline.
        let canvas_width_pt = canvas.width().ceil();
        let canvas_height_pt = canvas.height().ceil();

        let mut page = first_page.clone();
        page.frame
            .translate(Point::new(Abs::pt(offset_x_pt), Abs::pt(offset_y_pt)));
        page.frame.set_size(Size::new(
            Abs::pt(canvas_width_pt),
            Abs::pt(canvas_height_pt),
        ));

        let svg = typst_svg::svg(&page, &typst_svg::SvgOptions::default()).into_bytes();

        Ok(RenderedSvg {
            svg,
            width_pt,
            height_pt,
            canvas_width_pt,
            canvas_height_pt,
            offset_x_pt,
            offset_y_pt,
            descent_pt: max_descent_pt(&first_page.frame),
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
    use crate::mitex;
    use crate::world::InMemoryWorld;
    use typst::layout::{Abs, Frame, GroupItem, Point, Size};
    use typst::syntax::Span;
    use typst::visualize::{Color, Geometry};

    fn compile_source(source: &str) -> Result<RenderedSvg, RenderError> {
        let world = InMemoryWorld::new(source.to_string(), fonts::get_fonts());
        world.compile_to_svg()
    }

    fn compile_latex(latex_source: &str) -> Result<RenderedSvg, RenderError> {
        let fragment = mitex::convert_latex_to_typst(latex_source)?;
        let typst_source = format!(
            "{import}\n#let _ggtypst_mitex_expr = \"{fragment}\"\n{eval_expr}",
            import = r#"#import "/specs/mod.typ": mitex-scope"#,
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
    fn test_compile_to_svg_canvas_hugs_the_page_when_no_ink_overhangs() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
            "#box(width: 20.5pt, height: 10.25pt, fill: black)"
        );
        let Ok(rendered) = compile_source(source) else {
            panic!("a box should render successfully");
        };

        assert_eq!(rendered.offset_x_pt, 0.0);
        assert_eq!(rendered.offset_y_pt, 0.0);
        assert_eq!(rendered.canvas_width_pt, 21.0);
        assert_eq!(rendered.canvas_height_pt, rendered.height_pt.ceil());

        let svg = String::from_utf8_lossy(&rendered.svg);
        assert!(
            svg.contains("viewBox=\"0 0 21 "),
            "svg header should use the canvas"
        );
    }

    #[test]
    fn test_compile_to_svg_canvas_covers_overhanging_ink() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
            "#set text(size: 40pt)\n",
            "#text(style: \"italic\")[f]"
        );
        let Ok(rendered) = compile_source(source) else {
            panic!("italic text should render successfully");
        };

        // The italic f overhangs its advance box on both sides.
        assert!(rendered.offset_x_pt > 0.0, "{rendered:?}");
        assert!(rendered.canvas_width_pt > rendered.offset_x_pt + rendered.width_pt);
        assert_eq!(rendered.canvas_width_pt, rendered.canvas_width_pt.trunc());
        assert_eq!(rendered.canvas_height_pt, rendered.canvas_height_pt.trunc());
    }

    #[test]
    fn test_compile_to_svg_returns_empty_svg_error_for_default_preamble_set() {
        let source = concat!(
            "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
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
