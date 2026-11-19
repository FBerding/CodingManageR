% Generated documentation for close_db_pool
\name{close_db_pool}
\alias{close_db_pool}
\title{Safely Close a Database Connection Pool or DBI Connection}
\usage{
close_db_pool(con)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
}
\value{
Invisibly returns NULL.
}
\description{
Closes a pool::dbPool object via pool::poolClose() or falls back to closing a direct DBI connection.
}
\examples{
\dontrun{
close_db_pool(pool_con)
}
}
