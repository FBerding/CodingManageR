# Check Whether an Object Is a `pool::dbPool` Connection

Tests whether the supplied database handle inherits from the `Pool`
class provided by the `pool` package.

## Usage

``` r
is_pool_connection(con)
```

## Arguments

- con:

  Database connection or pool object to inspect.

## Value

A logical scalar: `TRUE` if `con` is a `Pool` object, `FALSE` otherwise.

## Examples

``` r
is_pool_connection(NULL)
#> [1] FALSE
```
