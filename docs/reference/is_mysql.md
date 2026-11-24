# Check Whether a Connection Uses MySQL or MariaDB

Inspects a DBI connection or pool object to determine whether the
underlying backend is MySQL or MariaDB.

## Usage

``` r
is_mysql(con)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

## Value

A logical scalar (TRUE for MySQL/MariaDB, FALSE otherwise).

## Examples

``` r
if (FALSE) { # \dontrun{
is_mysql(con)
} # }
```
