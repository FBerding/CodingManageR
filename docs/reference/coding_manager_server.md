# Shiny Server Function for CodingManageR

Manages database connectivity, user authentication (including inactive
account lockout), bilingual UI localization (`de`/`en`), reactive scale
catalog synchronization, the on-demand role-aware Getting Started
spotlight tutorial (including automatic tab/subtab switching), and
routing to the Dashboard, Coding Training, Live Coding, and Admin
Console Shiny modules.

## Usage

``` r
coding_manager_server(
  input,
  output,
  session,
  db_con = NULL,
  sqlite_path = NULL,
  config = NULL,
  seed_empty = NULL,
  fallback_to_sqlite = TRUE
)
```

## Arguments

- input:

  Standard Shiny server `input` object.

- output:

  Standard Shiny server `output` object.

- session:

  Standard Shiny `session` object.

- db_con:

  Optional pre-existing DBI connection or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object. When supplied (e.g., during
  [`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
  tests), the caller retains ownership and the connection is not
  automatically closed on session end.

- sqlite_path:

  Optional path to a local SQLite database file used when `db_con` is
  `NULL`.

- config:

  Optional named database configuration list passed to
  [`get_db_pool()`](https://fberding.github.io/CodingManageR/reference/get_db_pool.md).

- seed_empty:

  Logical passed to
  [`seed_initial_data()`](https://fberding.github.io/CodingManageR/reference/seed_initial_data.md)
  on first startup. Defaults to `TRUE` (minimal admin account) unless
  overridden via options or argument. When `FALSE`, demonstration data
  is only seeded into a brand-new, empty database; existing SQLite files
  or populated databases are protected and emit a warning instead.

- fallback_to_sqlite:

  Logical passed to
  [`get_db_pool()`](https://fberding.github.io/CodingManageR/reference/get_db_pool.md).
  Defaults to `TRUE`. Set to `FALSE` (used by
  [`run_app_server()`](https://fberding.github.io/CodingManageR/reference/run_app_server.md))
  to enforce strict MySQL/MariaDB connectivity without silently falling
  back to a local SQLite file.

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::shinyApp(ui = coding_manager_ui(), server = coding_manager_server)
} # }
```
