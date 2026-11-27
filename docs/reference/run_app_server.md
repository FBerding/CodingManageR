# Launch CodingManageR on a Multi-User Server (MySQL/MariaDB Mode)

Constructs and returns a `shiny.appobj` configured strictly for
multi-user server deployments (such as Shiny Server Open Source) backed
by a central MySQL or MariaDB database pool.

## Usage

``` r
run_app_server(
  config = NULL,
  empty = TRUE,
  max_upload_size_gb = 3,
  options = list(),
  ...
)
```

## Arguments

- config:

  Optional named list of MySQL/MariaDB connection parameters passed to
  [`get_db_pool()`](https://fberding.github.io/CodingManageR/reference/get_db_pool.md)
  (`host`, `port`, `user`, `password`, `dbname`, `minSize`, `maxSize`,
  `idleTimeout`). When `NULL` (default, recommended for server
  deployments), credentials are read from `.Renviron`.

- empty:

  Logical passed to
  [`seed_initial_data()`](https://fberding.github.io/CodingManageR/reference/seed_initial_data.md)
  when initializing a brand-new database. Defaults to `TRUE`. When
  omitted, the optional `CODINGMANAGER_EMPTY` environment variable is
  consulted before falling back to `TRUE`.

- max_upload_size_gb:

  Positive numeric maximum file upload size in gigabytes applied via
  `options(shiny.maxRequestSize = ...)` while the app runs (restored
  cleanly on app shutdown). Defaults to `3` (3 GB). When omitted, the
  optional `CODINGMANAGER_MAX_UPLOAD_GB` environment variable is
  consulted before falling back to `3`. Set to `NULL` to leave the
  existing `shiny.maxRequestSize` option unchanged.

- options:

  Optional list of options forwarded to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- ...:

  Additional arguments forwarded to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

A `shiny.appobj` object suitable for returning from a Shiny Server
`app.R` entrypoint.

## Details

Unlike
[`run_app_local()`](https://fberding.github.io/CodingManageR/reference/run_app_local.md)
and
[`run_app()`](https://fberding.github.io/CodingManageR/reference/run_app.md),
`run_app_server()` permanently disables the SQLite fallback
(`fallback_to_sqlite = FALSE`). If MySQL/MariaDB credentials are missing
or the database server cannot be reached, startup stops with an
actionable error message rather than silently creating a local SQLite
file on the server.

On Shiny Server, create your `app.R` entrypoint **once** with a
zero-argument call to `CodingManageR::run_app_server()` (or copy the
bundled file via
`system.file("shiny", "app.R", package = "CodingManageR")`) and store
all database and runtime settings in `.Renviron`:

- Required database variables: `MYSQL_HOST`, `MYSQL_USER`, and
  `MYSQL_DBNAME` (plus optional `MYSQL_PORT` and `MYSQL_PASSWORD`). If
  an `.Renviron` file exists in the application directory, missing
  variables are loaded automatically at startup without overwriting
  already set environment variables.

- Optional runtime variables: `CODINGMANAGER_EMPTY` (`TRUE`/`FALSE`,
  default `TRUE`) and `CODINGMANAGER_MAX_UPLOAD_GB` (positive number in
  GB or `"null"`, default `3`).

Because all configuration resides in `.Renviron` and all logic resides
inside the package, future updates of **CodingManageR** require only
updating the R package and restarting Shiny Server – no manual edits to
`app.R` are needed.

## See also

[`run_app_local()`](https://fberding.github.io/CodingManageR/reference/run_app_local.md),
[`run_app()`](https://fberding.github.io/CodingManageR/reference/run_app.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal, set-once Shiny Server app.R (reads credentials from .Renviron)
CodingManageR::run_app_server()

# Explicit programmatic MySQL/MariaDB configuration
CodingManageR::run_app_server(
  config = list(
    host = "127.0.0.1",
    port = 3306L,
    user = "shiny_user",
    password = "secret",
    dbname = "kodierer_portal"
  )
)
} # }
```
