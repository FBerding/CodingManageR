# Safely Close a Database Connection Pool or DBI Connection

Closes a pool::dbPool object via pool::poolClose() or falls back to
closing a direct DBI connection.

## Usage

``` r
close_db_pool(con)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

## Value

Invisibly returns NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
close_db_pool(pool_con)
} # }
```
