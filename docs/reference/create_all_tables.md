# Create All CodingManageR Database Tables

Creates the complete 9-table relational schema (users, permissions,
sources, texts, scales, gold standards, ratings, and training logs) for
SQLite or MySQL/MariaDB.

## Usage

``` r
create_all_tables(con, verbose = TRUE)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- verbose:

  Logical; if TRUE emits status messages during table creation.

## Value

Invisibly returns TRUE upon successful schema initialization.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_all_tables(con, verbose = FALSE)
DBI::dbDisconnect(con)
} # }
```
