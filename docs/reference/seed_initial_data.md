# Seed Initial Accounts or Demo Data into the Database

Populates a newly initialized CodingManageR database either with minimal
default accounts (empty = TRUE) or with a complete demonstration dataset
(empty = FALSE).

## Usage

``` r
seed_initial_data(con, empty = TRUE)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- empty:

  Logical; if TRUE seeds minimal default accounts only, if FALSE seeds
  the full demonstration dataset.

## Value

Invisibly returns TRUE after seeding completes.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_all_tables(con, verbose = FALSE)
seed_initial_data(con, empty = TRUE)
DBI::dbDisconnect(con)
} # }
```
