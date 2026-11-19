# Shiny Server Open Source / Deployment Entrypoint for CodingManageR
if (requireNamespace("CodingManageR", quietly = TRUE)) {
  CodingManageR::run_app(
    sqlite_path = Sys.getenv("SQLITE_PATH", unset = "lokale_datenbank.sqlite")
  )
} else if (requireNamespace("pkgload", quietly = TRUE) && file.exists(file.path("..", "..", "DESCRIPTION"))) {
  pkgload::load_all(file.path("..", ".."), quiet = TRUE)
  run_app(
    sqlite_path = Sys.getenv("SQLITE_PATH", unset = "lokale_datenbank.sqlite")
  )
} else {
  stop("Package 'CodingManageR' must be installed to launch the Shiny application.")
}
