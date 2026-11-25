# Launch CodingManageR Locally (Single-User SQLite Mode)

Constructs and returns a `shiny.appobj` strictly configured for local,
single-user workstation use with a local SQLite database file. Any
`MYSQL_HOST` or `MYSQL_USER` environment variables present on the
machine are ignored so local sessions never connect accidentally to a
remote server.

## Usage

``` r
run_app_local(sqlite_path = NULL, empty = TRUE)
```

## Arguments

- sqlite_path:

  Optional file path to a local SQLite database. Passing a different
  `.sqlite` path creates or reopens a separate, isolated project; the
  parent directory must already exist.

- empty:

  Logical passed to
  [`seed_initial_data()`](https://fberding.github.io/CodingManageR/reference/seed_initial_data.md)
  when initializing a fresh database. Defaults to `TRUE` (creates only
  the default `admin` and `historic_user` accounts). When `FALSE`,
  demonstration data is only inserted into a brand-new, empty database;
  existing SQLite files or databases that already contain rows are left
  untouched and trigger a warning.

## Value

A `shiny.appobj` object that launches the local Shiny portal when
printed or run interactively.

## Details

To manage multiple independent local projects on one computer, supply a
distinct `sqlite_path` file for each study (each `.sqlite` file stores
its own scales, sources, case texts, user accounts, ratings, and
training logs). Re-passing the same `sqlite_path` reopens that project.
When `sqlite_path` is `NULL` (and no `SQLITE_PATH` environment variable
is set), a single default database is stored under
`tools::R_user_dir("CodingManageR", which = "data")`. Always stop the
running Shiny app before copying, renaming, moving, or archiving a
`.sqlite` file so SQLite merges its Write-Ahead Log (`-wal`/`-shm`)
cleanly.

For multi-user server deployments backed by MySQL/MariaDB, use
[`run_app_server()`](https://fberding.github.io/CodingManageR/reference/run_app_server.md)
instead.

## See also

[`run_app_server()`](https://fberding.github.io/CodingManageR/reference/run_app_server.md),
[`run_app()`](https://fberding.github.io/CodingManageR/reference/run_app.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the default local SQLite project
run_app_local()

# Launch separate local projects in isolated SQLite files
run_app_local(sqlite_path = "study_a.sqlite")
run_app_local(sqlite_path = "study_b.sqlite")

# Launch a temporary demo session with sample data
run_app_local(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
} # }
```
