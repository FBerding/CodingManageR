# Safely Close a Direct DBI Database Connection

Checks whether a DBI connection is open and valid before disconnecting
it cleanly.

## Usage

``` r
close_db_connection(con)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

## Value

Invisibly returns TRUE if closed or FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
close_db_connection(con)
} # }
```
