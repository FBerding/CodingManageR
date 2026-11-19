% Generated documentation for dashboardServer
\name{dashboardServer}
\alias{dashboardServer}
\title{Shiny Server Module for the Progress Dashboard}
\usage{
dashboardServer(id, db_con, user_id_reactive = NULL, username_reactive,
  trigger_reactive, kategorie_spalten, scales_trigger = NULL,
  current_lang = reactiveVal("de"))
}
\arguments{
\item{id}{Character scalar module namespace identifier.}
\item{db_con}{Active DBI database connection or pool::dbPool object.}
\item{user_id_reactive}{Optional reactive expression returning the current user ID.}
\item{username_reactive}{Reactive expression returning the current username.}
\item{trigger_reactive}{Reactive expression invalidated when ratings or data change.}
\item{kategorie_spalten}{Reactive expression returning a named character vector mapping scale short labels to scale IDs.}
\item{scales_trigger}{Optional reactiveVal counter invalidated when scale definitions change.}
\item{current_lang}{Reactive expression or reactiveVal returning the active language code ("de" or "en").}
}
\value{
Invisibly returns the Shiny module server result.
}
\description{
Computes and renders personal coding KPIs, scale-level progress tables, and longitudinal training reliability visualizations.
}
\examples{
\dontrun{
dashboardServer("mod_dashboard", db_con = con,
  username_reactive = shiny::reactive("user1"),
  trigger_reactive = shiny::reactive(1L),
  kategorie_spalten = shiny::reactive(character(0)))
}
}
