% Generated documentation for draw_unrated_case_db
\name{draw_unrated_case_db}
\alias{draw_unrated_case_db}
\title{Draw a Random Unrated Case Text from the Database}
\usage{
draw_unrated_case_db(con, scale_identifier, user_id, mode = "alle",
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
A single-row data frame containing the drawn case text, or a 0-row data frame if none remain.
}
\description{
Selects a single random unrated case text for a coder on a specified scale, respecting source permissions and rating mode.
}
\examples{
\dontrun{
draw_unrated_case_db(con, scale_identifier = 1L, user_id = 1L)
}
}
