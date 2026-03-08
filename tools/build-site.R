agents_path <- "AGENTS.md"
hidden_agents_path <- ".AGENTS.md.pkgdown-hidden"

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

pkgdown::build_site(preview = FALSE, devel = TRUE, lazy = TRUE)

unlink("docs/AGENTS.html")
unlink("docs/dev/AGENTS.html")
