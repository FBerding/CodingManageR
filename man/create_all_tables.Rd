% Generated documentation for create_all_tables
\name{create_all_tables}
\alias{create_all_tables}
\title{Create All CodingManageR Database Tables}
\usage{
create_all_tables(con, verbose = TRUE)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{verbose}{Logical; if TRUE emits status messages during table creation.}
}
\value{
Invisibly returns TRUE upon successful schema initialization.
}
\description{
Creates the complete 9-table relational schema (users, permissions, sources, texts, scales, gold standards, ratings, and training logs) for SQLite or MySQL/MariaDB.
}
\examples{
\dontrun{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_all_tables(con, verbose = FALSE)
DBI::dbDisconnect(con)
}
}
