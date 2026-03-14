#!/usr/bin/env Rscript
# tools/benchmark.R
# ─────────────────────────────────────────────────────────────────────────────
# Benchmark script for ggtypst rendering performance.
#
# Usage:
#   Rscript tools/benchmark.R
#
# Output:
#   tools/benchmark-results.txt
#
# Uses base R timing with manual loops for accurate measurement of Rust FFI
# and ggplot2 rendering workloads.
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(ggtypst)

if (!requireNamespace("ggtext", quietly = TRUE)) {
  stop(
    "The benchmark script requires the 'ggtext' package for styled-text comparisons.",
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(ggtext))

build_typst_source_internal <- utils::getFromNamespace("build_typst_source", "ggtypst")
typst_svg_internal <- utils::getFromNamespace("typst_svg", "ggtypst")
convert_latex_to_typst_internal <- utils::getFromNamespace("convert_latex_to_typst", "ggtypst")

# ── Helpers ──────────────────────────────────────────────────────────────────

#' Run an expression `n` times and return elapsed times (seconds) as a numeric
#' vector. A garbage collection is run before each iteration to reduce noise.
time_iterations <- function(expr, n) {
  expr <- substitute(expr)
  env <- parent.frame()
  times <- numeric(n)
  for (i in seq_len(n)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    eval(expr, envir = env)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  times
}

#' Summarise a numeric vector of timings into a one-row data.frame.
summarise_times <- function(times) {
  data.frame(
    median = median(times),
    mean = mean(times),
    min = min(times),
    max = max(times),
    sd = sd(times),
    n = length(times)
  )
}

#' Pretty-print a time value in human-readable units.
fmt_time <- function(x) {
  if (x < 1e-3) {
    sprintf("%.1f\u00b5s", x * 1e6)
  } else if (x < 1) {
    sprintf("%.1fms", x * 1e3)
  } else {
    sprintf("%.2fs", x)
  }
}

#' Format a summary row for text output.
fmt_row <- function(label, s) {
  sprintf(
    "  %-25s %8s   %8s   %8s   %8s   %6d",
    label,
    fmt_time(s$median),
    fmt_time(s$min),
    fmt_time(s$max),
    fmt_time(s$sd),
    s$n
  )
}

header_line <- function() {
  sprintf(
    "  %-25s %8s   %8s   %8s   %8s   %6s",
    "expression",
    "median",
    "min",
    "max",
    "sd",
    "n_itr"
  )
}

divider <- function() {
  paste(rep("\u2500", 78), collapse = "")
}

#' Render a ggplot to a null device (triggers full grob construction without
#' file I/O overhead).
render_plot <- function(p) {
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(NULL)
}

zero_pt <- grid::unit(rep(0, 4), "pt")

ggtext_layer_args <- function() {
  list(
    fill = NA,
    label.color = NA,
    label.padding = zero_pt,
    label.margin = zero_pt,
    label.r = grid::unit(0, "pt")
  )
}

# ── Typst code specimens ────────────────────────────────────────────────────

typst_plain_short <- "Hello"

typst_plain_styled <- r"(*bold* _italic_ #linebreak() some text here)"

typst_math_simple <- r"($sum_(i=1)^n i = (n(n+1))/2$)"

typst_math_moderate <- r"($integral_0^1 frac(1, 1+x^2) dif x = pi / 4$)"

typst_math_complex <- r"(
  $cal(F)(s) = integral_(-oo)^(+oo) ( sum_(n=1)^oo ((-1)^(n-1) x^(2n)) / ((2n)!) ) / ( root(3, lim_(k->oo) product_(j=1)^k (1 - 1/p_j^2) + (alpha^2)/(beta^3) ) ) e^(-2 pi i s x) dif x + mat(
    sum_(i=1)^n i^3 = ( (n(n+1)) / 2 )^2,
    underbrace(a_1 + a_2 + dots.c + a_k, text("Total: ") k text(" terms"));
    cases(
      (integral_0^x e^(-t^2) dif t) / (sqrt(pi) + sum_(m=1)^M 1/m) & text("if ") x > 0,
      vec(alpha, (beta + gamma)/(delta), zeta) & text("otherwise")
    ),
    overbrace(product_(p " prime") (1 - p^(-s))^(-1), zeta(s))
  )$
)"

latex_simple <- r"(\frac{1}{2} + \sqrt{3})"

latex_complex <- r"(
  \mathcal{F}(s) = \int_{-\infty}^{+\infty} \frac{\sum_{n=1}^{\infty} \frac{(-1)^{n-1} x^{2n}}{(2n)!}}{\sqrt[3]{\lim_{k \to \infty} \prod_{j=1}^{k} \left(1 - \frac{1}{p_j^2}\right) + \frac{\alpha^2}{\beta^3}}} e^{-2 \pi i s x} \,\mathrm{d}x + \begin{pmatrix} \sum_{i=1}^{n} i^3 = \left( \frac{n(n+1)}{2} \right)^2 & \underbrace{a_1 + a_2 + \cdots + a_k}_{\text{Total: } k \text{ terms}} \\ \begin{cases} \frac{\int_{0}^{x} e^{-t^2} \,\mathrm{d}t}{\sqrt{\pi} + \sum_{m=1}^{M} \frac{1}{m}} & \text{if } x > 0 \\ \begin{pmatrix} \alpha \\ \frac{\beta + \gamma}{\delta} \\ \zeta \end{pmatrix} & \text{otherwise} \end{cases} & \overbrace{\prod_{p \text{ prime}} (1 - p^{-s})^{-1}}^{\zeta(s)} \end{pmatrix}
)"

typst_styled_compare <- r"(*Bold* _italic_ plain text)"

ggtext_styled_compare <- "<b>Bold</b> <i>italic</i> plain text"

# ── Warmup ──────────────────────────────────────────────────────────────────

cat("Warming up Rust backend (font loading, JIT, etc.)...\n")
for (i in seq_len(3)) {
  typst_svg_internal(build_typst_source_internal("warmup"))
}
cat("Warmup complete.\n\n")

# ── Configuration ───────────────────────────────────────────────────────────

N_RENDER <- 30L # iterations for Layer 1 (fast)
N_PLOT <- 15L # iterations for Layer 2 (slower)

output <- character()
out <- function(...) {
  output <<- c(output, paste0(...))
}

# ── Header ──────────────────────────────────────────────────────────────────

out(paste(rep("\u2550", 78), collapse = ""))
out("  ggtypst Benchmark Results")
out(sprintf("  Date: %s", Sys.Date()))
out(sprintf(
  "  R %s | ggtypst %s | %s",
  paste0(R.version$major, ".", R.version$minor),
  as.character(utils::packageVersion("ggtypst")),
  R.version$platform
))
out(paste(rep("\u2550", 78), collapse = ""))
out("")

# ═════════════════════════════════════════════════════════════════════════════
# Layer 1: Typst Rendering (Rust FFI only)
# ═════════════════════════════════════════════════════════════════════════════

cat("Running Layer 1: Typst Rendering (Rust FFI)...\n")
out("\u2500\u2500 Layer 1: Typst Rendering (Rust FFI) \u2500\u2500")
out("")
out("  Measures the time of build_typst_source() + typst_svg() calls only,")
out("  bypassing ggplot2 layer construction and SVG-to-grob import.")
out("")
out(header_line())
out(divider())

layer1_cases <- list(
  list(
    label = "plain_short",
    expr = quote(typst_svg_internal(build_typst_source_internal(typst_plain_short)))
  ),
  list(
    label = "plain_styled",
    expr = quote(typst_svg_internal(build_typst_source_internal(typst_plain_styled)))
  ),
  list(
    label = "math_simple",
    expr = quote(typst_svg_internal(build_typst_source_internal(typst_math_simple)))
  ),
  list(
    label = "math_moderate",
    expr = quote(typst_svg_internal(build_typst_source_internal(typst_math_moderate)))
  ),
  list(
    label = "math_complex",
    expr = quote(typst_svg_internal(build_typst_source_internal(typst_math_complex)))
  ),
  list(
    label = "mitex_simple",
    expr = quote(typst_svg_internal(build_typst_source_internal(
      convert_latex_to_typst_internal(latex_simple)
    )))
  ),
  list(
    label = "mitex_complex",
    expr = quote(typst_svg_internal(build_typst_source_internal(
      convert_latex_to_typst_internal(latex_complex)
    )))
  )
)

layer1_results <- list()
for (case in layer1_cases) {
  cat(sprintf("  %-25s ... ", case$label))
  times <- numeric(N_RENDER)
  for (i in seq_len(N_RENDER)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    eval(case$expr)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  s <- summarise_times(times)
  layer1_results[[case$label]] <- s
  out(fmt_row(case$label, s))
  cat(sprintf("%s\n", fmt_time(s$median)))
}

out("")

# ═════════════════════════════════════════════════════════════════════════════
# Layer 2: Full Plot Performance (End-to-End)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nRunning Layer 2: Full Plot Performance...\n")
out("\u2500\u2500 Layer 2: Full Plot Performance (End-to-End) \u2500\u2500")
out("")
out("  Measures build-and-draw time from already assembled ggplot objects,")
out("  including Typst compilation, SVG import, and grid drawing to a null PDF device.")
out("")

# ── 2A: API Type Comparison ─────────────────────────────────────────────────

out("\u2500\u2500 2A: API Type Comparison \u2500\u2500")
out("")
out("  Fixed math content: sum_(i=1)^n x_i")
out("")
out(header_line())
out(divider())

base_plot <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_minimal()

# annotate
p_annotate <- base_plot +
  annotate_math_typst(r"(sum_(i=1)^n x_i)", x = 3.5, y = 30, size = 16)

# geom with varying N
make_geom_df <- function(n) {
  set.seed(42)
  data.frame(
    x = runif(n, 2, 5),
    y = runif(n, 12, 33),
    label = rep(r"(sum_(i=1)^n x_i)", n)
  )
}

p_geom_1 <- base_plot + geom_math_typst(aes(x, y, label = label), data = make_geom_df(1), size = 12)
p_geom_10 <- base_plot + geom_math_typst(aes(x, y, label = label), data = make_geom_df(10), size = 12)
p_geom_50 <- base_plot + geom_math_typst(aes(x, y, label = label), data = make_geom_df(50), size = 12)

# element
p_element <- base_plot +
  labs(title = r"(sum_(i=1)^n x_i)") +
  theme(plot.title = element_math_typst(size = 16))

# 5 annotates on one plot
p_annotate_5 <- base_plot +
  annotate_math_typst(r"(sum_(i=1)^n x_i)", x = 2.0, y = 33, size = 14) +
  annotate_typst(r"(*Fuel efficiency model:* $hat(y) = beta_0 + beta_1 x$)", x = 4.0, y = 28, size = 12) +
  annotate_math_mitex(r"(\int_0^1 x^2 \, dx = \frac{1}{3})", x = 2.5, y = 18, size = 14) +
  annotate_typst(r"(*bold* _italic_ #linebreak() styled text)", x = 4.2, y = 14, size = 12) +
  annotate_math_typst(r"(integral_0^1 frac(1, 1+x^2) dif x = pi / 4)", x = 3.0, y = 22, size = 13)

cases_2a <- list(
  list(label = "base plot", p = base_plot),
  list(label = "annotate (1 item)", p = p_annotate),
  list(label = "annotate (5 items)", p = p_annotate_5),
  list(label = "geom (1 row)", p = p_geom_1),
  list(label = "geom (10 rows)", p = p_geom_10),
  list(label = "geom (50 rows)", p = p_geom_50),
  list(label = "element (title)", p = p_element)
)

for (case in cases_2a) {
  cat(sprintf("  %-25s ... ", case$label))
  times <- numeric(N_PLOT)
  for (i in seq_len(N_PLOT)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    render_plot(case$p)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  s <- summarise_times(times)
  out(fmt_row(case$label, s))
  cat(sprintf("%s\n", fmt_time(s$median)))
}

out("")

# ── 2B: Content Complexity ──────────────────────────────────────────────────

out("\u2500\u2500 2B: Content Complexity (annotate_typst) \u2500\u2500")
out("")
out("  All rendered via annotate_typst() at the same position.")
out("")
out(header_line())
out(divider())

complexity_cases <- list(
  list(
    label = "text_only",
    code = "Hello from ggtypst"
  ),
  list(
    label = "text_styled",
    code = r"(*bold* _italic_ #text(fill: red)[colored])"
  ),
  list(
    label = "math_inline",
    code = r"(Result: $x^2 + y^2 = r^2$)"
  ),
  list(
    label = "math_display",
    code = r"($integral_0^1 frac(1, 1+x^2) dif x = pi / 4$)"
  ),
  list(
    label = "math_massive",
    code = typst_math_complex
  )
)

for (case in complexity_cases) {
  p <- base_plot +
    annotate_typst(case$code, x = 3.5, y = 25, size = 14)
  cat(sprintf("  %-25s ... ", case$label))
  times <- numeric(N_PLOT)
  for (i in seq_len(N_PLOT)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    render_plot(p)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  s <- summarise_times(times)
  out(fmt_row(case$label, s))
  cat(sprintf("%s\n", fmt_time(s$median)))
}

out("")

# ── 2C: Geom Row Scaling ───────────────────────────────────────────────────

out("\u2500\u2500 2C: Geom Row Scaling (geom_typst) \u2500\u2500")
out("")
out(r"(  Fixed label: "*Point* #linebreak() text")")
out("")
out(header_line())
out(divider())

row_counts <- c(1, 5, 10, 25, 50)

for (n in row_counts) {
  set.seed(42)
  df <- data.frame(
    x = runif(n, 2, 5),
    y = runif(n, 12, 33),
    label = rep(r"(*Point* #linebreak() text)", n)
  )
  p <- base_plot + geom_typst(aes(x, y, label = label), data = df, size = 10)

  label <- sprintf("n = %d", n)
  cat(sprintf("  %-25s ... ", label))
  times <- numeric(N_PLOT)
  for (i in seq_len(N_PLOT)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    render_plot(p)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  s <- summarise_times(times)
  out(fmt_row(label, s))
  cat(sprintf("%s\n", fmt_time(s$median)))
}

out("")

# ── 2D: ggtypst vs ggtext (styled text only) ───────────────────────────────

out("\u2500\u2500 2D: ggtypst vs ggtext (styled text only) \u2500\u2500")
out("")
out("  These comparisons use equivalent bold/italic rich text content.")
out("  ggtext layers disable background, outline, and padding to focus on text rendering cost.")
out("")

styled_df <- function(n) {
  set.seed(42)
  data.frame(
    x = runif(n, 2, 5),
    y = runif(n, 12, 33),
    typst_label = rep(typst_styled_compare, n),
    ggtext_label = rep(ggtext_styled_compare, n)
  )
}

annotate_positions <- data.frame(
  x = c(2.0, 2.7, 3.4, 4.1, 4.8),
  y = c(33, 28, 23, 18, 13)
)

p_annotate_typst_text <- base_plot +
  annotate_typst(
    typst_code = typst_styled_compare,
    x = 3.5,
    y = 30,
    size = 4,
    size.unit = "mm"
  )

p_annotate_ggtext_text <- do.call(
  ggplot2::annotate,
  c(
    list(
      geom = "richtext",
      x = 3.5,
      y = 30,
      label = ggtext_styled_compare,
      size = 4
    ),
    ggtext_layer_args()
  )
) |>
  (function(layer) base_plot + layer)()

p_annotate_typst_text_5 <- Reduce(
  f = `+`,
  x = lapply(seq_len(nrow(annotate_positions)), function(i) {
    annotate_typst(
      typst_code = typst_styled_compare,
      x = annotate_positions$x[i],
      y = annotate_positions$y[i],
      size = 4,
      size.unit = "mm"
    )
  }),
  init = base_plot
)

p_annotate_ggtext_text_5 <- Reduce(
  f = `+`,
  x = lapply(seq_len(nrow(annotate_positions)), function(i) {
    do.call(
      ggplot2::annotate,
      c(
        list(
          geom = "richtext",
          x = annotate_positions$x[i],
          y = annotate_positions$y[i],
          label = ggtext_styled_compare,
          size = 4
        ),
        ggtext_layer_args()
      )
    )
  }),
  init = base_plot
)

styled_df_1 <- styled_df(1)
styled_df_10 <- styled_df(10)
styled_df_50 <- styled_df(50)

p_geom_typst_text_1 <- base_plot +
  geom_typst(
    aes(x, y, label = typst_label),
    data = styled_df_1,
    size = 4,
    size.unit = "mm"
  )

p_geom_ggtext_text_1 <- do.call(
  ggtext::geom_richtext,
  c(
    list(
      mapping = aes(x, y, label = ggtext_label),
      data = styled_df_1,
      size = 4
    ),
    ggtext_layer_args()
  )
) |>
  (function(layer) base_plot + layer)()

p_geom_typst_text_10 <- base_plot +
  geom_typst(
    aes(x, y, label = typst_label),
    data = styled_df_10,
    size = 4,
    size.unit = "mm"
  )

p_geom_ggtext_text_10 <- do.call(
  ggtext::geom_richtext,
  c(
    list(
      mapping = aes(x, y, label = ggtext_label),
      data = styled_df_10,
      size = 4
    ),
    ggtext_layer_args()
  )
) |>
  (function(layer) base_plot + layer)()

p_geom_typst_text_50 <- base_plot +
  geom_typst(
    aes(x, y, label = typst_label),
    data = styled_df_50,
    size = 4,
    size.unit = "mm"
  )

p_geom_ggtext_text_50 <- do.call(
  ggtext::geom_richtext,
  c(
    list(
      mapping = aes(x, y, label = ggtext_label),
      data = styled_df_50,
      size = 4
    ),
    ggtext_layer_args()
  )
) |>
  (function(layer) base_plot + layer)()

out("\u2500\u2500 2D-1: Annotate comparison \u2500\u2500")
out("")
out("  Equivalent styled text: Typst '*Bold* _italic_ plain text' vs HTML '<b>Bold</b> <i>italic</i> plain text'")
out("")
out(header_line())
out(divider())

cases_2d_annotate <- list(
  list(label = "base plot", p = base_plot),
  list(label = "ggtypst annotate (1)", p = p_annotate_typst_text),
  list(label = "ggtext annotate (1)", p = p_annotate_ggtext_text),
  list(label = "ggtypst annotate (5)", p = p_annotate_typst_text_5),
  list(label = "ggtext annotate (5)", p = p_annotate_ggtext_text_5)
)

for (case in cases_2d_annotate) {
  cat(sprintf("  %-25s ... ", case$label))
  times <- numeric(N_PLOT)
  for (i in seq_len(N_PLOT)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    render_plot(case$p)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  s <- summarise_times(times)
  out(fmt_row(case$label, s))
  cat(sprintf("%s\n", fmt_time(s$median)))
}

out("")
out("\u2500\u2500 2D-2: Geom comparison \u2500\u2500")
out("")
out("  Equivalent styled text repeated across rows with no label box styling on ggtext layers.")
out("")
out(header_line())
out(divider())

cases_2d_geom <- list(
  list(label = "ggtypst geom (1)", p = p_geom_typst_text_1),
  list(label = "ggtext geom (1)", p = p_geom_ggtext_text_1),
  list(label = "ggtypst geom (10)", p = p_geom_typst_text_10),
  list(label = "ggtext geom (10)", p = p_geom_ggtext_text_10),
  list(label = "ggtypst geom (50)", p = p_geom_typst_text_50),
  list(label = "ggtext geom (50)", p = p_geom_ggtext_text_50)
)

for (case in cases_2d_geom) {
  cat(sprintf("  %-25s ... ", case$label))
  times <- numeric(N_PLOT)
  for (i in seq_len(N_PLOT)) {
    gc(verbose = FALSE, full = TRUE)
    t0 <- proc.time()[["elapsed"]]
    render_plot(case$p)
    t1 <- proc.time()[["elapsed"]]
    times[i] <- t1 - t0
  }
  s <- summarise_times(times)
  out(fmt_row(case$label, s))
  cat(sprintf("%s\n", fmt_time(s$median)))
}

out("")

# ── Write output ────────────────────────────────────────────────────────────

out_file <- file.path("tools", "benchmark-results.txt")
writeLines(output, out_file)
cat(sprintf("\nResults written to %s\n", out_file))
