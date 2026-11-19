% Generated documentation for get_user_permitted_coding_sources_db
\name{get_user_permitted_coding_sources_db}
\alias{get_user_permitted_coding_sources_db}
\title{Get Sources Permitted for Live Field Coding by a User}
\usage{
get_user_permitted_coding_sources_db(con, user_id, is_admin = FALSE)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{user_id}{Integer scalar user identifier.}
\item{is_admin}{Logical indicating whether the user holds administrator privileges.}
}
\value{
A data frame of permitted coding sources.
}
\description{
Returns the subset of scientific sources that a user is authorized to code in the Live Coding module (excluding training-only sources unless permitted).
}
\examples{
\dontrun{
get_user_permitted_coding_sources_db(con, user_id = 1L, is_admin = TRUE)
}
}
