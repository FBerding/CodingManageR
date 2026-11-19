% Generated documentation for get_user_info_db
\name{get_user_info_db}
\alias{get_user_info_db}
\title{Retrieve User Account Metadata from the Database}
\usage{
get_user_info_db(con, username, user_id = NULL)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{username}{Character scalar username to look up.}
\item{user_id}{Integer scalar user identifier.}
}
\value{
A single-row data frame with user metadata, or a 0-row data frame if not found.
}
\description{
Fetches user account information (ID, username, role permissions, and active status) by username or user ID.
}
\examples{
\dontrun{
get_user_info_db(con, username = "admin")
}
}
