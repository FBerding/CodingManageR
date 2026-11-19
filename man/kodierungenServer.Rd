% Generated documentation for kodierungenServer
\name{kodierungenServer}
\alias{kodierungenServer}
\title{Shiny Server Module for Live Field Coding}
\usage{
kodierungenServer(id, db_con, user_id_reactive = NULL, username_reactive,
  kategorie_spalten, kategorien_liste, trigger_parent, scales_trigger = NULL,
  current_lang = reactiveVal("de"))
}
\arguments{
\item{id}{Character scalar module namespace identifier.}
\item{db_con}{Active DBI database connection or pool::dbPool object.}
\item{user_id_reactive}{Optional reactive expression returning the current user ID.}
\item{username_reactive}{Reactive expression returning the current username.}
\item{kategorie_spalten}{Reactive expression returning a named character vector mapping scale short labels to scale IDs.}
\item{kategorien_liste}{Reactive expression returning a named character vector of display labels to scale IDs.}
\item{trigger_parent}{ReactiveVal counter incremented to notify parent modules of new ratings.}
\item{scales_trigger}{Optional reactiveVal counter invalidated when scale definitions change.}
\item{current_lang}{Reactive expression or reactiveVal returning the active language code ("de" or "en").}
}
\value{
Invisibly returns the Shiny module server result.
}
\description{
Manages scale and source selection, random unrated case drawing, anchor example modals, and transactional saving of field ratings.
}
\examples{
\dontrun{
kodierungenServer("mod_codings", db_con = con,
  username_reactive = shiny::reactive("user1"),
  kategorie_spalten = shiny::reactive(character(0)),
  kategorien_liste = shiny::reactive(character(0)),
  trigger_parent = shiny::reactiveVal(0L))
}
}
