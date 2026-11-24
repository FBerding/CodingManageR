# Seed Initial Accounts or Demonstration Data into the Database

Populates a CodingManageR database either with minimal default accounts
(`empty = TRUE`) or with a complete demonstration dataset
(`empty = FALSE`) including users, coding scales, scientific sources,
case texts, permissions, gold-standard ratings, and training history.
Prints a structured tabular summary report via
[`message()`](https://rdrr.io/r/base/message.html).

## Usage

``` r
seed_initial_data(con, empty = TRUE)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- empty:

  Logical; if `TRUE` (default), seeds only the default `admin` and
  inactive `historic_user` accounts. If `FALSE`, seeds the full
  demonstration dataset only when all 9 tables are completely empty; if
  any table already contains rows, emits a warning and skips all
  demonstration inserts.

## Value

Invisibly returns a named list summarizing row counts, schema creation
status, and checksums for all 9 tables.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_all_tables(con, verbose = FALSE)
seed_initial_data(con, empty = TRUE)
DBI::dbDisconnect(con)
} # }
```
