# Check Whether a Connection Uses MySQL or MariaDB

Tests whether a direct `DBIConnection` or
[`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
handle targets MySQL or MariaDB (`MariaDBConnection` or
`MySQLConnection`).

## Usage

``` r
is_mysql(con)
```

## Arguments

- con:

  A `DBIConnection`,
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md),
  or `NULL`.

## Value

A logical scalar: `TRUE` for MySQL/MariaDB connections, `FALSE`
otherwise.

## Examples

``` r
is_mysql(NULL)
#> [1] FALSE
```
