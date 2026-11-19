% Generated documentation for search_case_codings_db
\name{search_case_codings_db}
\alias{search_case_codings_db}
\title{Search Case Coding Ratings in the Database}
\usage{
search_case_codings_db(con, scale_filter = "all", type_filter = "all",
  search_query = "")
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{scale_filter}{Character scalar scale filter ("all" or a scale ID).}
\item{type_filter}{Character scalar rating type filter ("all", "kodierung", or "schulung").}
\item{search_query}{Character scalar full-text search query.}
}
\value{
A data frame of matching rating records.
}
\description{
Queries user_ratings joined with case texts, scales, and coder accounts using optional scale, rating type, and full-text search filters.
}
\examples{
\dontrun{
search_case_codings_db(con, scale_filter = "all", type_filter = "all")
}
}
