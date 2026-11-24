# Check Whether a DBI or Pool Connection Targets MySQL/MariaDB

Detects RMariaDB or RMySQL backend connections, including connections
managed inside a pool::dbPool.

## Usage

``` r
is_mysql_connection(con)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

## Value

A logical scalar indicating whether the connection uses MySQL or
MariaDB.

## Examples

``` r
if (FALSE) { # \dontrun{
is_mysql_connection(con)
} # }
```
