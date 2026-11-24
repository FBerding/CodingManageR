# Create a Pooled Database Connection (MySQL or SQLite)

Initializes a pool::dbPool connection (or direct DBI connection when
force_pool is FALSE) with SQLite fallback for local sessions.

## Usage

``` r
get_db_pool(config = NULL, fallback_to_sqlite = TRUE, force_pool = TRUE)
```

## Arguments

- config:

  Optional named list of database connection parameters.

- fallback_to_sqlite:

  Logical; if TRUE falls back to SQLite when MySQL/MariaDB is
  unavailable.

- force_pool:

  Logical; if TRUE returns a pool::dbPool connection instead of a direct
  DBI connection.

## Value

A pool::dbPool object or DBI connection object.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- get_db_pool(config = list(sqlite_path = ":memory:"))
close_db_pool(p)
} # }
```
