% Generated documentation for count_unrated_cases_db
\name{count_unrated_cases_db}
\alias{count_unrated_cases_db}
\title{Count Unrated Cases for a Coder and Scale}
\usage{
count_unrated_cases_db(con, scale_identifier, user_id, mode = "alle",
  source_id = NULL, permitted_source_ids = NULL)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{scale_identifier}{Integer scale_id or character short_label identifying the coding scale.}
\item{user_id}{Integer scalar user identifier.}
\item{mode}{Character scalar rating filter mode (e.g., "alle").}
\item{source_id}{Optional integer scalar source identifier filter.}
\item{permitted_source_ids}{Optional integer vector of source IDs permitted for the coder.}
}
\value{
An integer scalar giving the number of remaining unrated cases.
}
\description{
Queries the database to count how many eligible case texts have not yet been coded by a specific user on a given scale and optional source filter.
}
\examples{
\dontrun{
count_unrated_cases_db(con, scale_identifier = 1L, user_id = 1L)
}
}
