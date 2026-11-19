% Generated documentation for with_db_transaction
\name{with_db_transaction}
\alias{with_db_transaction}
\title{Execute Database Operations Atomically Inside a Transaction}
\usage{
with_db_transaction(con, expr)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{expr}{R expression block or function(tx_con) closure to execute within the transaction.}
}
\value{
The return value of the evaluated expression or callback function.
}
\description{
Executes either an R expression block or a function(tx_con) closure within a DBI transaction on a direct connection or checked-out pool connection, automatically rolling back on error.
}
\examples{
\dontrun{
with_db_transaction(con, function(tx_con) {
  DBI::dbExecute(tx_con, "SELECT 1")
})
}
}
