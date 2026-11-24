# Determine SQL Random Order Expression

Returns the SQL function for random row ordering depending on whether
the connection uses MySQL/MariaDB (`RAND()`) or SQLite (`RANDOM()`).

## Usage

``` r
get_db_random_function(con)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object representing the active database connection.

## Value

A character scalar: `"RAND()"` for MySQL/MariaDB or `"RANDOM()"` for
SQLite.

## Examples

``` r
if (FALSE) { # \dontrun{
rand_expr <- get_db_random_function(con)
query <- paste0("SELECT * FROM texts ORDER BY ", rand_expr, " LIMIT 1")
} # }
```
