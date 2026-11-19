% Generated documentation for adminUI
\name{adminUI}
\alias{adminUI}
\title{Shiny UI Module for the Admin Console}
\usage{
adminUI(id, lang = "en", selected_subtab = "tab_users")
}
\arguments{
\item{id}{Character scalar module namespace identifier.}
\item{lang}{Character scalar language code ("de" or "en").}
\item{selected_subtab}{Character scalar specifying the initially selected admin subtab.}
}
\value{
A Shiny UI tag list.
}
\description{
Builds the multi-tab Bootstrap user interface for the CodingManageR administration console.
}
\examples{
\dontrun{
adminUI("mod_admin", lang = "en")
}
}
