% Generated documentation for language_selector_ui
\name{language_selector_ui}
\alias{language_selector_ui}
\title{Build the Bilingual Language Selector Widget}
\usage{
language_selector_ui(id = "app_language", current_lang = "de", class = "")
}
\arguments{
\item{id}{Character scalar module namespace identifier.}
\item{current_lang}{Reactive expression or reactiveVal returning the active language code ("de" or "en").}
\item{class}{Optional character scalar of additional CSS classes.}
}
\value{
A Shiny UI tag object.
}
\description{
Creates a compact UI control allowing users to switch the CodingManageR interface between German and English.
}
\examples{
\dontrun{
language_selector_ui("app_language", current_lang = "en")
}
}
