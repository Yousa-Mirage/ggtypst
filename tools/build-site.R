agents_path <- "AGENTS.md"
hidden_agents_path <- ".AGENTS.md.pkgdown-hidden"

if (file.exists("README.Rmd")) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

  rmarkdown::render(
    input = "README.Rmd",
    output_format = "github_document",
    output_file = "README.md",
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  unlink("README.html")
}

if (file.exists(".github/CONTRIBUTING.Rmd")) {
  rmarkdown::render(
    input = ".github/CONTRIBUTING.Rmd",
    output_format = "github_document",
    output_file = "CONTRIBUTING.md",
    output_dir = ".github",
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  unlink(".github/CONTRIBUTING.html")
}

restore_agents <- function() {
  if (file.exists(hidden_agents_path) && !file.exists(agents_path)) {
    file.rename(hidden_agents_path, agents_path)
  }
}

on.exit(restore_agents(), add = TRUE)

if (file.exists(agents_path)) {
  ok <- file.rename(agents_path, hidden_agents_path)

  if (!ok) {
    stop("Failed to hide AGENTS.md before building the site.", call. = FALSE)
  }
}

pkgdown::init_site()
pkgdown::build_site(preview = FALSE, devel = TRUE, lazy = TRUE)

unlink("docs/AGENTS.html")
unlink("docs/dev/AGENTS.html")
unlink("docs/AGENTS.md")
unlink("docs/dev/AGENTS.md")
