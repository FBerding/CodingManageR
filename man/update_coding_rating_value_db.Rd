% Generated documentation for update_coding_rating_value_db
\name{update_coding_rating_value_db}
\alias{update_coding_rating_value_db}
\title{Update an Existing Coding Rating Value in the Database}
\usage{
update_coding_rating_value_db(con, rating_id, new_value)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{rating_id}{Integer scalar identifier of the rating record in user_ratings.}
\item{new_value}{Integer scalar new ordinal rating value.}
}
\value{
An integer count of affected rows (1 on success).
}
\description{
Updates the ordinal rating value of a single record in user_ratings within a validated database statement.
}
\examples{
\dontrun{
update_coding_rating_value_db(con, rating_id = 1L, new_value = 2L)
}
}
