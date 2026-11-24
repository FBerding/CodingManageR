# Open a Database Connection (Backward-Compatible Wrapper)

Backward-compatible wrapper around
[`get_db_pool()`](https://fberding.github.io/CodingManageR/reference/get_db_pool.md)
with `force_pool = TRUE`.

## Usage

``` r
get_db_connection(config = NULL, fallback_to_sqlite = TRUE)
```

## Arguments

- config:

  Optional named list of database configuration settings.

- fallback_to_sqlite:

  Logical; whether to fall back to SQLite if MySQL/MariaDB is not
  configured or unreachable. Defaults to `TRUE`.

## Value

A [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md) or
`DBIConnection` object.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_db_connection(config = list(sqlite_path = ":memory:"))
close_db_connection(con)
} # }
```
