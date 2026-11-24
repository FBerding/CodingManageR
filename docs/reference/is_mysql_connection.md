# Check Whether a Connection Targets MySQL or MariaDB

Inspects a direct `DBIConnection` or
[`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
handle to determine whether the underlying database backend is MySQL or
MariaDB.

## Usage

``` r
is_mysql_connection(con)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

## Value

A logical scalar: `TRUE` for MySQL/MariaDB connections, `FALSE`
otherwise.

## Examples

``` r
is_mysql_connection(NULL)
#> [1] FALSE
```
