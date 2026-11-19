% Generated documentation for get_db_random_function
\name{get_db_random_function}
\alias{get_db_random_function}
\title{Return the SQL Random Ordering Expression for a Connection}
\usage{
get_db_random_function(con)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
}
\value{
A character scalar containing the SQL random function name.
}
\description{
Returns "RAND()" for MySQL/MariaDB connections and "RANDOM()" for SQLite connections.
}
\examples{
\dontrun{
get_db_random_function(con)
}
}
