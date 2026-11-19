% Generated documentation for seed_initial_data
\name{seed_initial_data}
\alias{seed_initial_data}
\title{Seed Initial Accounts or Demo Data into the Database}
\usage{
seed_initial_data(con, empty = TRUE)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{empty}{Logical; if TRUE seeds minimal default accounts only, if FALSE seeds the full demonstration dataset.}
}
\value{
Invisibly returns TRUE after seeding completes.
}
\description{
Populates a newly initialized CodingManageR database either with minimal default accounts (empty = TRUE) or with a complete demonstration dataset (empty = FALSE).
}
\examples{
\dontrun{
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
create_all_tables(con, verbose = FALSE)
seed_initial_data(con, empty = TRUE)
DBI::dbDisconnect(con)
}
}
