# Create All CodingManageR Database Tables

Initializes the complete 9-table relational schema (users, permissions,
sources, texts, scales, ratings, and training logs) in the target SQLite
or MySQL/MariaDB database using `CREATE TABLE IF NOT EXISTS`.

## Usage

``` r
create_all_tables(con, verbose = TRUE)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- verbose:

  Logical; if `TRUE` (default), prints status messages during schema
  creation.

## Value

Logical `TRUE` upon successful table initialization.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_all_tables(con, verbose = FALSE)
DBI::dbDisconnect(con)
} # }
```
