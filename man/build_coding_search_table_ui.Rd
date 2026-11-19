% Generated documentation for build_coding_search_table_ui
\name{build_coding_search_table_ui}
\alias{build_coding_search_table_ui}
\title{Build HTML Table for Coding Search Results}
\usage{
build_coding_search_table_ui(ns, page_df, lang = "en")
}
\arguments{
\item{ns}{Shiny namespace function created via shiny::NS().}
\item{page_df}{Data frame slice containing the rows for the current page.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A Shiny UI tag object.
}
\description{
Renders a paginated HTML table of case coding records with action buttons for editing individual rating values.
}
\examples{
\dontrun{
build_coding_search_table_ui(shiny::NS("mod_admin"), data.frame())
}
}
