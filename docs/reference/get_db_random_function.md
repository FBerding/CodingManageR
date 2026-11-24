# Return the SQL Random Ordering Expression for a Connection

Returns "RAND()" for MySQL/MariaDB connections and "RANDOM()" for SQLite
connections.

## Usage

``` r
get_db_random_function(con)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

## Value

A character scalar containing the SQL random function name.

## Examples

``` r
if (FALSE) { # \dontrun{
get_db_random_function(con)
} # }
```
