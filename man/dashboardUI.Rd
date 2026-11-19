% Generated documentation for dashboardUI
\name{dashboardUI}
\alias{dashboardUI}
\title{Shiny UI Module for the Progress Dashboard}
\usage{
dashboardUI(id, kategorie_spalten = NULL, lang = "en",
  selected_tab = "tab_progress")
}
\arguments{
\item{id}{Character scalar module namespace identifier.}
\item{kategorie_spalten}{Reactive expression returning a named character vector mapping scale short labels to scale IDs.}
\item{lang}{Character scalar language code ("de" or "en").}
\item{selected_tab}{Character scalar specifying the initially selected dashboard tab.}
}
\value{
A Shiny UI tag list.
}
\description{
Constructs the user interface for the CodingManageR personal progress and training analytics dashboard.
}
\examples{
\dontrun{
dashboardUI("mod_dashboard", lang = "en")
}
}
