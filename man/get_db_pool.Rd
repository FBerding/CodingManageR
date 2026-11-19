% Generated documentation for get_db_pool
\name{get_db_pool}
\alias{get_db_pool}
\title{Create a Pooled Database Connection (MySQL or SQLite)}
\usage{
get_db_pool(config = NULL, fallback_to_sqlite = TRUE, force_pool = TRUE)
}
\arguments{
\item{config}{Optional named list of database connection parameters.}
\item{fallback_to_sqlite}{Logical; if TRUE falls back to SQLite when MySQL/MariaDB is unavailable.}
\item{force_pool}{Logical; if TRUE returns a pool::dbPool connection instead of a direct DBI connection.}
}
\value{
A pool::dbPool object or DBI connection object.
}
\description{
Initializes a pool::dbPool connection (or direct DBI connection when force_pool is FALSE) with SQLite fallback for local sessions.
}
\examples{
\dontrun{
p <- get_db_pool(config = list(sqlite_path = ":memory:"))
close_db_pool(p)
}
}
