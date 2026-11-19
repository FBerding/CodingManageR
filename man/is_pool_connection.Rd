% Generated documentation for is_pool_connection
\name{is_pool_connection}
\alias{is_pool_connection}
\title{Check Whether an Object Is a pool::dbPool Connection}
\usage{
is_pool_connection(con)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
}
\value{
A logical scalar (TRUE if the object is a Pool, FALSE otherwise).
}
\description{
Tests whether the supplied database handle inherits from the Pool class provided by the pool package.
}
\examples{
is_pool_connection(NULL)
}
