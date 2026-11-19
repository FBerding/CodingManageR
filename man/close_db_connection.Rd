% Generated documentation for close_db_connection
\name{close_db_connection}
\alias{close_db_connection}
\title{Safely Close a Direct DBI Database Connection}
\usage{
close_db_connection(con)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
}
\value{
Invisibly returns TRUE if closed or FALSE otherwise.
}
\description{
Checks whether a DBI connection is open and valid before disconnecting it cleanly.
}
\examples{
\dontrun{
close_db_connection(con)
}
}
