# Launch the CodingManageR Shiny Application (Backward-Compatible Wrapper)

Constructs and returns a `shiny.appobj` configured automatically for
either pooled MySQL/MariaDB storage (when MySQL credentials are
supplied) or local single-user SQLite storage (with automatic fallback).

## Usage

``` r
run_app(
  sqlite_path = NULL,
  config = NULL,
  empty = TRUE,
  max_upload_size_gb = 3,
  options = list(),
  ...
)
```

## Arguments

- sqlite_path:

  Optional file path to a local SQLite database. Passing a different
  `.sqlite` path creates or reopens a separate, isolated project; the
  parent directory must already exist.

- config:

  Optional named list of database configuration parameters passed to
  [`get_db_pool()`](https://fberding.github.io/CodingManageR/reference/get_db_pool.md).

- empty:

  Logical passed to
  [`seed_initial_data()`](https://fberding.github.io/CodingManageR/reference/seed_initial_data.md)
  when initializing a fresh database. Defaults to `TRUE` (creates only
  the default `admin` and `historic_user` accounts). When `FALSE`,
  demonstration data is only inserted into a brand-new, empty database;
  existing SQLite files or databases that already contain rows are left
  untouched and trigger a warning.

- max_upload_size_gb:

  Positive numeric maximum file upload size in gigabytes applied via
  `options(shiny.maxRequestSize = ...)` while the app runs (restored
  cleanly on app shutdown). Defaults to `3` (3 GB). Set to `NULL` to
  leave the session's existing `shiny.maxRequestSize` option unchanged.

- options:

  Optional list of options forwarded to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- ...:

  Additional arguments forwarded to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

A `shiny.appobj` object that launches the Shiny portal when printed or
returned from `app.R`.

## Details

For new scripts and deployments, prefer the explicit entrypoints
[`run_app_local()`](https://fberding.github.io/CodingManageR/reference/run_app_local.md)
(minimal local SQLite launcher) and
[`run_app_server()`](https://fberding.github.io/CodingManageR/reference/run_app_server.md)
(strict MySQL/MariaDB server launcher without SQLite fallback).
`run_app()` is retained for full backward compatibility with existing
code.

## See also

[`run_app_local()`](https://fberding.github.io/CodingManageR/reference/run_app_local.md),
[`run_app_server()`](https://fberding.github.io/CodingManageR/reference/run_app_server.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Preferred explicit launchers:
run_app_local(sqlite_path = "study_a.sqlite")
run_app_server()

# Backward-compatible wrapper:
app <- run_app(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
app
} # }
```
