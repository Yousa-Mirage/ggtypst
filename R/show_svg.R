library(grid)
library(rsvg)

render_typst_svg <- function(code) {
  # 2. 调用底层的 Rust 渲染函数
  # 假设 render_formula 是我们用 Rust 编译暴露给 R 的函数
  result <- typst_svg(code)

  # 3. 将 Rust 传回的 raw 字节流直接转为 R 的图形对象 (grob)
  svg_grob <- rsvg::rsvg_nativeraster(
    result$svg,
    width = result$width * 4,
    height = result$height * 4
  )

  # 4. 注入精确的物理尺寸
  svg_grob <- grid::rasterGrob(svg_grob)
  svg_grob$width <- grid::unit(result$width * 2, "pt")
  svg_grob$height <- grid::unit(result$height * 2, "pt")

  # 5. 清空当前画板并居中绘制公式
  grid::grid.newpage()
  grid::grid.draw(svg_grob)

  # 6. 隐式返回这个 grob，方便你把它塞进 ggplot 的 element_custom 里
  invisible(svg_grob)
}
