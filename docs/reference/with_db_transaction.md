# Execute Database Operations Atomically Inside a Transaction

Executes either an R expression block or a function(tx_con) closure
within a DBI transaction on a direct connection or checked-out pool
connection, automatically rolling back on error.

## Usage

``` r
with_db_transaction(con, expr)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- expr:

  R expression block or function(tx_con) closure to execute within the
  transaction.

## Value

The return value of the evaluated expression or callback function.

## Examples

``` r
if (FALSE) { # \dontrun{
with_db_transaction(con, function(tx_con) {
  DBI::dbExecute(tx_con, "SELECT 1")
})
} # }
```
