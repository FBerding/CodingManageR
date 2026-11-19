% Generated documentation for get_db_connection
\name{get_db_connection}
\alias{get_db_connection}
\title{Open a Direct Database Connection (MySQL or SQLite)}
\usage{
get_db_connection(config = NULL, fallback_to_sqlite = TRUE)
}
\arguments{
\item{config}{Optional named list of database connection parameters.}
\item{fallback_to_sqlite}{Logical; if TRUE falls back to SQLite when MySQL/MariaDB is unavailable.}
}
\value{
An active DBI connection object.
}
\description{
Establishes a direct DBI connection using MySQL/MariaDB configuration if available, with automatic fallback to local SQLite storage.
}
\examples{
\dontrun{
con <- get_db_connection(config = list(sqlite_path = ":memory:"))
close_db_connection(con)
}
}
