# Launch the CodingManageR Shiny Application

Constructs and returns a `shiny.appobj` configured for either local
single-user SQLite storage or pooled MySQL/MariaDB server deployments.

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

To manage multiple independent local projects on one computer, supply a
distinct `sqlite_path` file for each study (each `.sqlite` file stores
its own scales, sources, case texts, user accounts, ratings, and
training logs). Re-passing the same `sqlite_path` reopens that project.
When `sqlite_path` is `NULL` (and no `SQLITE_PATH` or MySQL environment
variables are set), a single default database is stored under
`tools::R_user_dir("CodingManageR", which = "data")`. Always stop the
running Shiny app before copying, renaming, moving, or archiving a
`.sqlite` file so SQLite merges its Write-Ahead Log (`-wal`/`-shm`)
cleanly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch separate local projects in isolated SQLite files
run_app(sqlite_path = "study_a.sqlite", empty = TRUE)
run_app(sqlite_path = "study_b.sqlite", empty = TRUE)

# Launch a temporary demo session with sample data
app <- run_app(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
app
} # }
```
