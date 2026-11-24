# Check Whether an Object Is a pool::dbPool Connection

Tests whether the supplied database handle inherits from the Pool class
provided by the pool package.

## Usage

``` r
is_pool_connection(con)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

## Value

A logical scalar (TRUE if the object is a Pool, FALSE otherwise).

## Examples

``` r
is_pool_connection(NULL)
#> [1] FALSE
```
