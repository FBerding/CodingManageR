# Create a Database Connection Pool or Direct DBI Connection

Creates a thread-safe
[`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
(recommended for multi-user Shiny Server deployments) or a direct
`DBIConnection` targeting MySQL/MariaDB, with optional fallback to a
local SQLite database for single-user desktop sessions.

## Usage

``` r
get_db_pool(config = NULL, fallback_to_sqlite = TRUE, force_pool = TRUE)
```

## Arguments

- config:

  Optional named list with connection settings: `host`, `port`, `user`,
  `password`, `dbname`, `sqlite_path`, `minSize`, `maxSize`, and
  `idleTimeout`. Missing entries fall back to environment variables.

- fallback_to_sqlite:

  Logical; if `TRUE` (default), falls back to a local SQLite database
  when no MySQL/MariaDB connection is configured or available.

- force_pool:

  Logical; if `TRUE` (default), wraps the connection in a
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  when the `pool` package is available.

## Value

A [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md) or
`DBIConnection` object.

## Examples

``` r
if (FALSE) { # \dontrun{
pool_con <- get_db_pool(config = list(sqlite_path = ":memory:"))
close_db_pool(pool_con)
} # }
```
