% Generated documentation for is_mysql
\name{is_mysql}
\alias{is_mysql}
\title{Check Whether a Connection Uses MySQL or MariaDB}
\usage{
is_mysql(con)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
}
\value{
A logical scalar (TRUE for MySQL/MariaDB, FALSE otherwise).
}
\description{
Inspects a DBI connection or pool object to determine whether the underlying backend is MySQL or MariaDB.
}
\examples{
\dontrun{
is_mysql(con)
}
}
