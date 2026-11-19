% Generated documentation for adminServer
\name{adminServer}
\alias{adminServer}
\title{Shiny Server Module for the Admin Console}
\usage{
adminServer(id, db_con, current_user = NULL, current_user_id = NULL,
  scales_trigger = NULL, current_lang = reactiveVal("de"))
}
\arguments{
\item{id}{Character scalar module namespace identifier.}
\item{db_con}{Active DBI database connection or pool::dbPool object.}
\item{current_user}{Optional reactive expression returning the signed-in username.}
\item{current_user_id}{Optional reactive expression returning the signed-in numeric user ID.}
\item{scales_trigger}{Optional reactiveVal counter invalidated when scale definitions change.}
\item{current_lang}{Reactive expression or reactiveVal returning the active language code ("de" or "en").}
}
\value{
Invisibly returns the Shiny module server result.
}
\description{
Server logic for the multi-tab CodingManageR administration console, covering user and permission management, case text and source management, scale definitions, coder reliability monitoring, coding search/correction, CSV import/export, and database diagnostics.
}
\examples{
\dontrun{
adminServer("mod_admin", db_con = con)
}
}
