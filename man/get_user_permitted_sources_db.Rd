% Generated documentation for get_user_permitted_sources_db
\name{get_user_permitted_sources_db}
\alias{get_user_permitted_sources_db}
\title{Get All Sources Permitted for a User}
\usage{
get_user_permitted_sources_db(con, user_id, is_admin = FALSE)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{user_id}{Integer scalar user identifier.}
\item{is_admin}{Logical indicating whether the user holds administrator privileges.}
}
\value{
A data frame of permitted sources.
}
\description{
Queries all scientific sources accessible to a specific user according to admin status and user_source_permissions.
}
\examples{
\dontrun{
get_user_permitted_sources_db(con, user_id = 1L, is_admin = TRUE)
}
}
