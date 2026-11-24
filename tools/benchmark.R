#!/usr/bin/env Rscript
# tools/benchmark.R
# -----------------------------------------------------------------------------
# Benchmark of plots with ggtypst labels saved with ggsave(), compared with the
# same plots labelled by ggplot2 (geom_text(), element_text()) and by ggtext.
#
# Usage, from the package root:
#   Rscript tools/benchmark.R
#
# The package is installed from this source tree into a separate library as a
# release build (the Rust library optimized, with LTO), so the benchmark
# measures what users install rather than a `devtools::load_all()` debug build.
# Set GGTYPST_BENCH_LIB to a directory to keep that library: later runs reuse
# it while the source is unchanged. GGTYPST_BENCH_QUICK=true runs every case
# with a few small iterations, to check the script.
#
# Every label is different, in every plot and every iteration, as in real
# data. Each plot is timed from building the ggplot object to the file written
# by ggsave(), as PNG (ggsave() uses ragg when installed) and as PDF (pdf()).
#
# Outputs, read by the pkgdown article vignettes/articles/benchmark.Rmd and
# left out of the package (.Rbuildignore):
#   vignettes/articles/benchmark-results.csv     one row per measured case
#   vignettes/articles/benchmark-environment.log machine, versions, settings
# -----------------------------------------------------------------------------

if (!file.exists("DESCRIPTION") || !file.exists("tools/benchmark.R")) {
  stop("Run this script from the ggtypst package root.", call. = FALSE)
}
for (pkg in c("bench", "ggplot2", "ggtext", "ragg")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("The benchmark needs the '%s' package.", pkg), call. = FALSE)
  }
}
if (nzchar(Sys.getenv("DEBUG"))) {
  stop("Unset DEBUG: the benchmark needs a release build.", call. = FALSE)
}

started <- Sys.time()
quick <- identical(Sys.getenv("GGTYPST_BENCH_QUICK"), "true")
results_file <- file.path("vignettes", "articles", "benchmark-results.csv")
log_file <- file.path("vignettes", "articles", "benchmark-environment.log")

# ── Settings ────────────────────────────────────────────────────────────────

devices <- c("png", "pdf")
plot_width <- 7
plot_height <- 5
png_dpi <- 300

geom_rows <- if (quick) c(1, 10) else c(1, 10, 100, 500, 1000)
geom_iterations <- function(n) {
  if (quick) {
    2L
  } else if (n >= 500) {
    7L
  } else {
    15L
  }
}
annotate_counts <- c(1, 10)
theme_cases <- c("titles", "axis_text", "legend", "strips", "all")
plot_iterations <- if (quick) 2L else 15L
redraw_rows <- 100
redraw_iterations <- if (quick) 2L else 10L
cold_runs <- if (quick) 1L else 7L

# ── Release build of this source tree ───────────────────────────────────────

# Files the installed package is built from; the library is reused only while
# they are unchanged.
source_fingerprint <- function() {
  files <- c(
    "DESCRIPTION",
    list.files("R", full.names = TRUE),
    list.files("src/rust/src", full.names = TRUE),
    list.files("src/rust/specs", full.names = TRUE, recursive = TRUE),
    "src/rust/Cargo.toml",
    "src/rust/Cargo.lock",
    "src/entrypoint.c",
    "src/Makevars.in",
    "tools/config.R"
  )
  sums <- tools::md5sum(files)
  listing <- tempfile()
  writeLines(paste(files, sums), listing)
  unname(tools::md5sum(listing))
}

r_cmd <- function(args, log) {
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", args),
    stdout = log,
    stderr = log
  )
  if (status != 0) {
    cat(utils::tail(readLines(log), 30), sep = "\n")
    stop(sprintf("R CMD %s failed; see %s.", args[[1]], log), call. = FALSE)
  }
}

# Build a source tarball (which leaves out the Rust build cache, see
# .Rbuildignore) and install it as a release build. NOT_CRAN lets cargo use the
# usual crate cache instead of downloading every crate again.
install_release <- function(lib, fingerprint) {
  build_dir <- tempfile("ggtypst-build-")
  dir.create(build_dir)
  log <- file.path(build_dir, "install.log")
  root <- normalizePath(".")
  old <- setwd(build_dir)
  on.exit(setwd(old), add = TRUE)

  message("Building the source tarball...")
  r_cmd(c("build", "--no-manual", shQuote(root)), log)
  tarball <- list.files(build_dir, "^ggtypst_.*\\.tar\\.gz$", full.names = TRUE)

  message("Installing a release build (this compiles the Rust library)...")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  old_not_cran <- Sys.getenv("NOT_CRAN", NA)
  Sys.setenv(NOT_CRAN = "true")
  on.exit(
    if (is.na(old_not_cran)) {
      Sys.unsetenv("NOT_CRAN")
    } else {
      Sys.setenv(NOT_CRAN = old_not_cran)
    },
    add = TRUE
  )
  r_cmd(
    c("INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(tarball)),
    log
  )

  install_log <- readLines(log)
  release <- any(grepl("cargo build .*--release", install_log)) &&
    any(grepl("Finished `release` profile", install_log, fixed = TRUE))
  if (!release) {
    stop("The Rust library was not built in release mode.", call. = FALSE)
  }
  writeLines(fingerprint, file.path(lib, "ggtypst-benchmark-source"))
  invisible(lib)
}

fingerprint <- source_fingerprint()
bench_lib <- Sys.getenv("GGTYPST_BENCH_LIB", tempfile("ggtypst-lib-"))
marker <- file.path(bench_lib, "ggtypst-benchmark-source")
reused <- file.exists(marker) && identical(readLines(marker), fingerprint)
if (reused) {
  message("Reusing the release build in ", bench_lib)
} else {
  install_release(bench_lib, fingerprint)
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtext)
  library(ggtypst, lib.loc = bench_lib)
})
if (
  !startsWith(normalizePath(find.package("ggtypst")), normalizePath(bench_lib))
) {
  stop("ggtypst was not loaded from the release build.", call. = FALSE)
}

# ── Environment log ─────────────────────────────────────────────────────────

command_output <- function(command, args = character()) {
  out <- tryCatch(
    suppressWarnings(system2(command, args, stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (length(out) == 0) NA_character_ else trimws(out[[1]])
}

cpu_name <- function() {
  sys <- Sys.info()[["sysname"]]
  if (sys == "Darwin") {
    return(command_output("sysctl", c("-n", "machdep.cpu.brand_string")))
  }
  if (sys == "Linux" && file.exists("/proc/cpuinfo")) {
    model <- grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)
    if (length(model) > 0) return(trimws(sub("^[^:]*:", "", model[[1]])))
  }
  Sys.getenv("PROCESSOR_IDENTIFIER", NA_character_)
}

memory_gb <- function() {
  sys <- Sys.info()[["sysname"]]
  bytes <- if (sys == "Darwin") {
    as.numeric(command_output("sysctl", c("-n", "hw.memsize")))
  } else if (sys == "Linux" && file.exists("/proc/meminfo")) {
    total <- grep("^MemTotal", readLines("/proc/meminfo"), value = TRUE)
    as.numeric(gsub("[^0-9]", "", total)) * 1024
  } else {
    NA_real_
  }
  round(bytes / 1024^3)
}

source_revision <- function() {
  jj <- command_output(
    "jj",
    c("log", "-r", "@", "--no-graph", "-T", shQuote("commit_id.short(12)"))
  )
  if (!is.na(jj)) {
    return(paste("jj", jj))
  }
  git <- command_output("git", c("rev-parse", "--short=12", "HEAD"))
  if (is.na(git)) NA_character_ else paste("git", git)
}

release_profile <- function() {
  toml <- readLines("src/rust/Cargo.toml")
  start <- grep("^\\[profile\\.release\\]", toml)
  if (length(start) == 0) {
    return("default")
  }
  rest <- toml[-seq_len(start)]
  end <- grep("^\\[", rest)
  body <- if (length(end) > 0) rest[seq_len(end[[1]] - 1)] else rest
  body <- trimws(body[nzchar(trimws(body)) & !startsWith(trimws(body), "#")])
  paste(body, collapse = ", ")
}

package_version <- function(pkg) as.character(utils::packageVersion(pkg))

png_device <- if (requireNamespace("ragg", quietly = TRUE)) {
  "ragg::agg_png()"
} else {
  "grDevices::png()"
}

write_environment_log <- function(finished = NULL) {
  lines <- c(
    "ggtypst benchmark environment",
    "",
    sprintf("Date:            %s", format(started, "%Y-%m-%d %H:%M %Z")),
    sprintf("CPU:             %s", cpu_name()),
    sprintf("Cores:           %s", parallel::detectCores()),
    sprintf("Memory:          %s GB", memory_gb()),
    sprintf("OS:              %s", utils::sessionInfo()$running),
    sprintf("Platform:        %s", R.version$platform),
    sprintf("R:               %s", R.version.string),
    sprintf("Rust:            %s", command_output("rustc", "--version")),
    "",
    "Packages:",
    sprintf("  ggtypst        %s", package_version("ggtypst")),
    sprintf("  ggplot2        %s", package_version("ggplot2")),
    sprintf("  ggtext         %s", package_version("ggtext")),
    sprintf("  gridtext       %s", package_version("gridtext")),
    sprintf("  ragg           %s", package_version("ragg")),
    sprintf("  bench          %s", package_version("bench")),
    "",
    "ggtypst build:",
    sprintf("  Source:        %s", source_revision()),
    sprintf("  Fingerprint:   %s", fingerprint),
    "  Profile:       release",
    sprintf("  Cargo release: %s", release_profile()),
    sprintf(
      "  Library:       %s",
      if (reused) "reused" else "built for this run"
    ),
    "",
    "Settings:",
    sprintf(
      "  Plots:         %g x %g in, saved with ggsave()",
      plot_width,
      plot_height
    ),
    sprintf("  PNG:           %s via ggsave(), %d dpi", png_device, png_dpi),
    "  PDF:           grDevices::pdf() via ggsave()",
    sprintf(
      "  ggtypst:       renderer \"%s\", raster dpi %s",
      getOption("ggtypst.renderer", "auto"),
      getOption("ggtypst.raster_dpi", 600)
    ),
    sprintf("  Geom rows:     %s", paste(geom_rows, collapse = ", ")),
    sprintf("  Quick run:     %s", quick),
    "  Labels:        all different, in every plot and every iteration",
    "  Timing:        bench::hires_time(), after a garbage collection; cases",
    "                 interleaved in a random order each iteration, after one",
    "                 untimed warm-up run of each case"
  )
  if (!is.null(finished)) {
    lines <- c(
      lines,
      "",
      sprintf(
        "Total run time:  %.1f min",
        as.numeric(difftime(finished, started, units = "mins"))
      )
    )
  }
  writeLines(lines, log_file)
}

write_environment_log()

# ── Timing ──────────────────────────────────────────────────────────────────

plot_files <- c(
  png = tempfile(fileext = ".png"),
  pdf = tempfile(fileext = ".pdf")
)

save_plot <- function(plot, device) {
  ggsave(
    plot_files[[device]],
    plot,
    width = plot_width,
    height = plot_height,
    units = "in",
    dpi = png_dpi
  )
}

time_ms <- function(fn) {
  gc(verbose = FALSE)
  start <- bench::hires_time()
  fn()
  as.numeric(bench::hires_time() - start) * 1000
}

# Time one run of `case`: building its plot and saving it ("first"), or saving
# a plot that was already saved once ("second").
run_case <- function(case) {
  if (identical(case$measure, "second")) {
    plot <- case$make()
    save_plot(plot, case$device)
    return(time_ms(function() save_plot(plot, case$device)))
  }
  time_ms(function() save_plot(case$make(), case$device))
}

summarise_ms <- function(times) {
  q <- stats::quantile(times, c(0.25, 0.5, 0.75), names = FALSE)
  data.frame(
    iterations = length(times),
    median_ms = q[[2]],
    q25_ms = q[[1]],
    q75_ms = q[[3]],
    min_ms = min(times),
    max_ms = max(times)
  )
}

# Run every case once untimed, then `iterations` timed rounds with the cases
# in a new random order each round.
measure_cases <- function(cases, iterations, label) {
  for (case in cases) {
    run_case(case)
  }
  times <- matrix(NA_real_, iterations, length(cases))
  for (i in seq_len(iterations)) {
    message(sprintf("  %s: round %d of %d", label, i, iterations))
    for (j in sample(seq_along(cases))) {
      times[i, j] <- run_case(cases[[j]])
    }
  }
  rows <- lapply(seq_along(cases), function(j) {
    case <- cases[[j]]
    cbind(
      data.frame(
        section = case$section,
        content = case$content,
        package = case$package,
        method = case$method,
        device = case$device,
        n = case$n,
        measure = case$measure
      ),
      summarise_ms(times[, j])
    )
  })
  do.call(rbind, rows)
}

make_case <- function(
  section,
  content,
  package,
  method,
  n,
  make,
  measure = "first"
) {
  lapply(devices, function(device) {
    list(
      section = section,
      content = content,
      package = package,
      method = method,
      device = device,
      n = n,
      measure = measure,
      make = make
    )
  })
}

# ── Labels ──────────────────────────────────────────────────────────────────

# A counter that numbers every label, so no two labels are ever the same.
label_counter <- new.env()
label_counter$last <- 0L
fresh_ids <- function(n) {
  ids <- label_counter$last + seq_len(n)
  label_counter$last <- label_counter$last + as.integer(n)
  ids
}

# Equivalent styled text: bold and italic, or plain for ggplot2.
styled_label <- function(package, ids) {
  switch(
    package,
    ggplot2 = sprintf("Bold %d italic text", ids),
    ggtext = sprintf("<b>Bold %d</b> <i>italic</i> text", ids),
    ggtypst = sprintf("*Bold %d* _italic_ text", ids)
  )
}

# Equivalent theme text: Markdown for ggtext.
theme_label <- function(package, ids) {
  switch(
    package,
    ggplot2 = sprintf("Bold %d italic", ids),
    ggtext = sprintf("**Bold %d** *italic*", ids),
    ggtypst = sprintf("*Bold %d* _italic_", ids)
  )
}

# Equivalent formulas: plotmath for geom_text(parse = TRUE).
math_label <- function(method, ids) {
  switch(
    method,
    "geom_text(parse = TRUE)" = sprintf(
      "x[%d]^2 + frac(alpha, beta + %d)",
      ids,
      ids
    ),
    geom_math_typst = sprintf("x_%d^2 + alpha / (beta + %d)", ids, ids),
    geom_math_mitex = sprintf(
      "x_{%d}^2 + \\frac{\\alpha}{\\beta + %d}",
      ids,
      ids
    )
  )
}

# ── Plots ───────────────────────────────────────────────────────────────────

set.seed(2026)
point_counts <- sort(unique(c(geom_rows, redraw_rows)))
point_sets <- lapply(
  stats::setNames(point_counts, point_counts),
  function(n) data.frame(x = stats::runif(n, 0, 10), y = stats::runif(n, 0, 10))
)

base_plot <- function(data) {
  ggplot(data, aes(x, y)) +
    geom_point(size = 0.5, colour = "grey60") +
    theme_minimal()
}

no_box <- function() {
  list(
    fill = NA,
    label.colour = NA,
    label.padding = grid::unit(0, "pt"),
    label.margin = grid::unit(0, "pt"),
    label.r = grid::unit(0, "pt")
  )
}

# The label layer of `method`, with text 3 mm high like geom_text()'s default.
label_layer <- function(method) {
  switch(
    method,
    geom_text = geom_text(aes(label = label), size = 3),
    "geom_text(parse = TRUE)" = geom_text(
      aes(label = label),
      size = 3,
      parse = TRUE
    ),
    geom_richtext = do.call(
      ggtext::geom_richtext,
      c(list(aes(label = label), size = 3), no_box())
    ),
    geom_typst = geom_typst(aes(label = label), size = 3, size.unit = "mm"),
    geom_math_typst = geom_math_typst(
      aes(label = label),
      size = 3,
      size.unit = "mm"
    ),
    geom_math_mitex = geom_math_mitex(
      aes(label = label),
      size = 3,
      size.unit = "mm"
    )
  )
}

geom_plot <- function(n, content, method) {
  data <- point_sets[[as.character(n)]]
  if (method == "geom_point only") {
    return(base_plot(data))
  }
  ids <- fresh_ids(n)
  data$label <- if (content == "styled text") {
    package <- switch(
      method,
      geom_text = "ggplot2",
      geom_richtext = "ggtext",
      geom_typst = "ggtypst"
    )
    styled_label(package, ids)
  } else {
    math_label(method, ids)
  }
  base_plot(data) + label_layer(method)
}

geom_methods <- list(
  list(content = "none", package = "ggplot2", method = "geom_point only"),
  list(content = "styled text", package = "ggplot2", method = "geom_text"),
  list(content = "styled text", package = "ggtext", method = "geom_richtext"),
  list(content = "styled text", package = "ggtypst", method = "geom_typst"),
  list(
    content = "math",
    package = "ggplot2",
    method = "geom_text(parse = TRUE)"
  ),
  list(content = "math", package = "ggtypst", method = "geom_math_typst"),
  list(content = "math", package = "ggtypst", method = "geom_math_mitex")
)

geom_cases <- function(n, section = "geom", measure = "first") {
  unlist(
    lapply(geom_methods, function(m) {
      make_case(
        section,
        m$content,
        m$package,
        m$method,
        n,
        function() geom_plot(n, m$content, m$method),
        measure = measure
      )
    }),
    recursive = FALSE
  )
}

annotate_plot <- function(k, package) {
  plot <- base_plot(point_sets[[as.character(redraw_rows)]])
  if (package == "none") {
    return(plot)
  }
  ids <- fresh_ids(k)
  labels <- styled_label(package, ids)
  x <- seq(1, 9, length.out = k)
  y <- seq(9, 1, length.out = k)
  layers <- switch(
    package,
    ggplot2 = list(annotate("text", x = x, y = y, label = labels, size = 3)),
    ggtext = list(do.call(
      annotate,
      c(
        list("richtext", x = x, y = y, label = labels, size = 3),
        no_box()
      )
    )),
    ggtypst = lapply(seq_len(k), function(i) {
      annotate_typst(
        labels[[i]],
        x = x[[i]],
        y = y[[i]],
        size = 3,
        size.unit = "mm"
      )
    })
  )
  plot + layers
}

annotate_cases <- function() {
  methods <- list(
    list(package = "ggplot2", method = "annotate(\"text\")"),
    list(package = "ggtext", method = "annotate(\"richtext\")"),
    list(package = "ggtypst", method = "annotate_typst")
  )
  base <- make_case(
    "annotate",
    "none",
    "ggplot2",
    "geom_point only",
    0,
    function() annotate_plot(0, "none")
  )
  cases <- unlist(
    lapply(annotate_counts, function(k) {
      unlist(
        lapply(methods, function(m) {
          make_case(
            "annotate",
            "styled text",
            m$package,
            m$method,
            k,
            function() annotate_plot(k, m$package)
          )
        }),
        recursive = FALSE
      )
    }),
    recursive = FALSE
  )
  c(base, cases)
}

theme_element <- function(package) {
  switch(
    package,
    ggplot2 = element_text(),
    ggtext = ggtext::element_markdown(),
    ggtypst = element_typst()
  )
}

theme_method <- function(package) {
  switch(
    package,
    ggplot2 = "element_text",
    ggtext = "element_markdown",
    ggtypst = "element_typst"
  )
}

# Set the leaf elements: ggtext's element_markdown() set on a parent such as
# `axis.text` does not reach every child.
theme_with <- function(package, elements) {
  element <- theme_element(package)
  do.call(
    theme,
    stats::setNames(rep(list(element), length(elements)), elements)
  )
}

set.seed(7)
theme_data <- data.frame(
  x = stats::runif(240, 0.5, 10.5),
  y = stats::runif(240, 0.5, 10.5),
  group = factor(rep(1:8, 30)),
  panel = factor(rep(1:12, each = 20))
)

title_elements <- c(
  "plot.title",
  "plot.subtitle",
  "plot.caption",
  "axis.title.x",
  "axis.title.y"
)
axis_elements <- c("axis.text.x", "axis.text.y")
legend_elements <- c("legend.title", "legend.text")
strip_elements <- "strip.text.x"

# A plot whose theme draws `case`'s text with `package`. "titles": title,
# subtitle, caption, and axis titles; "axis_text": 10 x and 10 y tick labels;
# "legend": a title and 8 keys; "strips": 12 facet strips; "all": all of them.
theme_plot <- function(case, package) {
  ids <- fresh_ids(46)
  label <- function(i) theme_label(package, ids[i])
  plot <- ggplot(theme_data, aes(x, y)) + theme_minimal()
  parts <- if (case == "all") {
    c("titles", "axis_text", "legend", "strips")
  } else {
    case
  }
  elements <- character()
  if ("titles" %in% parts) {
    plot <- plot +
      labs(
        title = label(1),
        subtitle = label(2),
        caption = label(3),
        x = label(4),
        y = label(5)
      )
    elements <- c(elements, title_elements)
  }
  if ("axis_text" %in% parts) {
    plot <- plot +
      scale_x_continuous(breaks = 1:10, labels = label(6:15)) +
      scale_y_continuous(breaks = 1:10, labels = label(16:25))
    elements <- c(elements, axis_elements)
  }
  if ("legend" %in% parts) {
    plot <- plot +
      geom_point(aes(colour = group), size = 0.5) +
      scale_colour_discrete(name = label(26), labels = label(27:34))
    elements <- c(elements, legend_elements)
  } else {
    plot <- plot + geom_point(size = 0.5, colour = "grey60")
  }
  if ("strips" %in% parts) {
    strips <- stats::setNames(label(35:46), levels(theme_data$panel))
    plot <- plot + facet_wrap(~panel, labeller = as_labeller(strips))
    elements <- c(elements, strip_elements)
  }
  plot + theme_with(package, elements)
}

theme_cases_for <- function(cases, measure = "first", section = "theme") {
  unlist(
    lapply(cases, function(case) {
      unlist(
        lapply(c("ggplot2", "ggtext", "ggtypst"), function(package) {
          make_case(
            section,
            case,
            package,
            theme_method(package),
            NA_integer_,
            function() theme_plot(case, package),
            measure = measure
          )
        }),
        recursive = FALSE
      )
    }),
    recursive = FALSE
  )
}

# ── Cold start ──────────────────────────────────────────────────────────────

# In a new R session: load the packages, then build and save a plot with 10
# labels. "Cold" means a new R process, not empty operating system caches.
cold_start_script <- function() {
  script <- tempfile(fileext = ".R")
  writeLines(
    c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "lib <- args[[1]]",
      "variant <- args[[2]]",
      "file <- args[[3]]",
      ".libPaths(c(lib, .libPaths()))",
      "start <- Sys.time()",
      "suppressPackageStartupMessages({",
      "  library(ggplot2)",
      "  if (variant == 'ggtext') library(ggtext)",
      "  if (variant == 'ggtypst') library(ggtypst, lib.loc = lib)",
      "})",
      "loaded <- Sys.time()",
      "set.seed(1)",
      "data <- data.frame(x = runif(10), y = runif(10))",
      "data$label <- switch(",
      "  variant,",
      "  ggplot2 = sprintf('Bold %d italic text', 1:10),",
      "  ggtext = sprintf('<b>Bold %d</b> <i>italic</i> text', 1:10),",
      "  ggtypst = sprintf('*Bold %d* _italic_ text', 1:10)",
      ")",
      "layer <- switch(",
      "  variant,",
      "  ggplot2 = geom_text(aes(label = label), size = 3),",
      "  ggtext = geom_richtext(aes(label = label), size = 3, fill = NA,",
      "    label.colour = NA),",
      "  ggtypst = geom_typst(aes(label = label), size = 3, size.unit = 'mm')",
      ")",
      "plot <- ggplot(data, aes(x, y)) + geom_point() + layer + theme_minimal()",
      "ggsave(file, plot, width = 7, height = 5, units = 'in', dpi = 300)",
      "saved <- Sys.time()",
      "cat(",
      "  as.numeric(difftime(loaded, start, units = 'secs')) * 1000,",
      "  as.numeric(difftime(saved, loaded, units = 'secs')) * 1000",
      ")"
    ),
    script
  )
  script
}

measure_cold_start <- function() {
  script <- cold_start_script()
  variants <- c("ggplot2", "ggtext", "ggtypst")
  grid <- expand.grid(
    variant = variants,
    device = devices,
    stringsAsFactors = FALSE
  )
  times <- list()
  for (run in seq_len(cold_runs)) {
    message(sprintf("  cold start: round %d of %d", run, cold_runs))
    for (i in sample(seq_len(nrow(grid)))) {
      out <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(
          shQuote(script),
          shQuote(bench_lib),
          grid$variant[[i]],
          shQuote(plot_files[[grid$device[[i]]]])
        ),
        stdout = TRUE,
        stderr = FALSE
      )
      value <- suppressWarnings(
        as.numeric(strsplit(utils::tail(c("", out), 1), " ")[[1]])
      )
      if (length(value) != 2 || anyNA(value)) {
        stop(
          sprintf("The cold start run of %s failed.", grid$variant[[i]]),
          call. = FALSE
        )
      }
      key <- paste(grid$variant[[i]], grid$device[[i]])
      times[[key]] <- rbind(times[[key]], value)
    }
  }
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    key <- paste(grid$variant[[i]], grid$device[[i]])
    value <- times[[key]]
    measures <- list(
      load = value[, 1],
      "first plot" = value[, 2],
      "load and first plot" = value[, 1] + value[, 2]
    )
    do.call(
      rbind,
      lapply(names(measures), function(measure) {
        cbind(
          data.frame(
            section = "cold start",
            content = "styled text",
            package = grid$variant[[i]],
            method = switch(
              grid$variant[[i]],
              ggplot2 = "geom_text",
              ggtext = "geom_richtext",
              ggtypst = "geom_typst"
            ),
            device = grid$device[[i]],
            n = 10,
            measure = measure
          ),
          summarise_ms(measures[[measure]])
        )
      })
    )
  })
  do.call(rbind, rows)
}

# ── Run ─────────────────────────────────────────────────────────────────────

results <- list()
set.seed(42)

message("Cold start...")
results$cold <- measure_cold_start()

message("Geom layers...")
for (n in geom_rows) {
  results[[paste("geom", n)]] <- measure_cases(
    geom_cases(n),
    geom_iterations(n),
    sprintf("geom, %d rows", n)
  )
}

message("Annotations...")
results$annotate <- measure_cases(annotate_cases(), plot_iterations, "annotate")

message("Theme elements...")
results$theme <- measure_cases(
  theme_cases_for(theme_cases),
  plot_iterations,
  "theme"
)

message("Saving the same plot a second time...")
results$redraw <- measure_cases(
  c(
    geom_cases(redraw_rows, section = "redraw", measure = "second"),
    theme_cases_for("all", measure = "second", section = "redraw")
  ),
  redraw_iterations,
  "redraw"
)

results <- do.call(rbind, unname(results))

# Time each label adds to a geom layer: the median above the same plot without
# labels, divided by the number of rows.
results$per_label_ms <- NA_real_
geom_rows_idx <- which(
  results$section %in% c("geom", "redraw") & results$method != "geom_point only"
)
for (i in geom_rows_idx) {
  base <- results$section == results$section[[i]] &
    results$method == "geom_point only" &
    results$device == results$device[[i]] &
    results$n == results$n[[i]]
  results$per_label_ms[[i]] <- (results$median_ms[[i]] -
    results$median_ms[base]) /
    results$n[[i]]
}

numeric_cols <- vapply(results, is.double, logical(1))
results[numeric_cols] <- lapply(results[numeric_cols], round, digits = 2)
utils::write.csv(results, results_file, row.names = FALSE)
write_environment_log(Sys.time())

message(sprintf(
  "Done in %.1f min. Results: %s; environment: %s",
  as.numeric(difftime(Sys.time(), started, units = "mins")),
  results_file,
  log_file
))
