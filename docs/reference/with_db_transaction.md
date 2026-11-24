# Execute Database Operations Atomically Inside a Transaction

Runs an R expression block or a `function(tx_con)` callback atomically
within a database transaction. Supports both direct `DBIConnection`
objects and
[`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
handles (checking out a dedicated connection for the duration of the
transaction). Any error triggers an automatic rollback and is re-thrown
to guarantee data integrity.

## Usage

``` r
with_db_transaction(con, expr)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- expr:

  An R expression block or a function accepting the active transaction
  connection `tx_con`.

## Value

The value returned by evaluating `expr`.

## Examples

``` r
if (FALSE) { # \dontrun{
with_db_transaction(con, function(tx_con) {
  DBI::dbExecute(tx_con, "SELECT 1")
})
} # }
```
