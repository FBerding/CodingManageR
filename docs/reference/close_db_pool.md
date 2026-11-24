# Safely Close a Connection Pool or DBI Connection

Closes a
[`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md) via
[`pool::poolClose()`](http://rstudio.github.io/pool/reference/Pool-class.md)
or disconnects a direct `DBIConnection` via
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html),
emitting a warning instead of throwing if the handle is already closed.

## Usage

``` r
close_db_pool(con)
```

## Arguments

- con:

  A [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md),
  `DBIConnection`, or `NULL`.

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
close_db_pool(pool_con)
} # }
```
