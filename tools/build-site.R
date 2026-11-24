Sys.setenv(NOT_CRAN = "true", DEBUG = "true")

agents_path <- "AGENTS.md"
hidden_agents_path <- ".AGENTS.md.pkgdown-hidden"

if (file.exists("README.Rmd")) {
  devtools::load_all(quiet = TRUE)

  rmarkdown::render(
    input = "README.Rmd",
    output_format = rmarkdown::github_document(html_preview = FALSE),
    output_file = "README.md",
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}

if (file.exists(".github/CONTRIBUTING.Rmd")) {
  rmarkdown::render(
    input = ".github/CONTRIBUTING.Rmd",
    output_format = rmarkdown::github_document(html_preview = FALSE),
    output_file = "CONTRIBUTING.md",
    output_dir = ".github",
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}

restore_agents <- function() {
  if (file.exists(hidden_agents_path) && !file.exists(agents_path)) {
    file.rename(hidden_agents_path, agents_path)
  }
}

if (file.exists(agents_path)) {
  ok <- file.rename(agents_path, hidden_agents_path)

  if (!ok) {
    stop("Failed to hide AGENTS.md before building the site.", call. = FALSE)
  }
}

tryCatch(
  {
    pkgdown::init_site()
    pkgdown::build_site(
      preview = FALSE,
      devel = TRUE,
      lazy = TRUE,
      install = TRUE
    )
  },
  finally = restore_agents()
)

unlink("docs/AGENTS.html")
unlink("docs/dev/AGENTS.html")
unlink("docs/AGENTS.md")
unlink("docs/dev/AGENTS.md")
