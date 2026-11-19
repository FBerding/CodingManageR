% Generated documentation for get_coding_detail_db
\name{get_coding_detail_db}
\alias{get_coding_detail_db}
\title{Retrieve Detailed Metadata for a Single Rating Record}
\usage{
get_coding_detail_db(con, rating_id)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{rating_id}{Integer scalar identifier of the rating record in user_ratings.}
}
\value{
A single-row data frame with rating details, or a 0-row data frame if not found.
}
\description{
Queries full details for a single user_ratings row joined with its case text, scale definition, and coder username.
}
\examples{
\dontrun{
get_coding_detail_db(con, rating_id = 1L)
}
}
