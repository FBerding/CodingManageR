% Generated documentation for is_mysql_connection
\name{is_mysql_connection}
\alias{is_mysql_connection}
\title{Check Whether a DBI or Pool Connection Targets MySQL/MariaDB}
\usage{
is_mysql_connection(con)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
}
\value{
A logical scalar indicating whether the connection uses MySQL or MariaDB.
}
\description{
Detects RMariaDB or RMySQL backend connections, including connections managed inside a pool::dbPool.
}
\examples{
\dontrun{
is_mysql_connection(con)
}
}
