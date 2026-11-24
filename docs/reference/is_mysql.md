# Check Whether a Connection Uses MySQL or MariaDB

Tests whether a direct `DBIConnection` inherits from `MariaDBConnection`
or `MySQLConnection`.

## Usage

``` r
is_mysql(con)
```

## Arguments

- con:

  A `DBIConnection` object or `NULL`.

## Value

A logical scalar: `TRUE` for MySQL/MariaDB connections, `FALSE`
otherwise.

## Examples

``` r
is_mysql(NULL)
#> [1] FALSE
```
