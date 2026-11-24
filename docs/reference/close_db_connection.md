# Safely Close a Database Connection (Backward-Compatible Alias)

Backward-compatible alias for
[`close_db_pool()`](https://fberding.github.io/CodingManageR/reference/close_db_pool.md).

## Usage

``` r
close_db_connection(con)
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
close_db_connection(con)
} # }
```
