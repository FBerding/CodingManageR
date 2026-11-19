% Generated documentation for build_edit_coding_value_modal
\name{build_edit_coding_value_modal}
\alias{build_edit_coding_value_modal}
\title{Build Modal Dialog for Editing a Coding Rating Value}
\usage{
build_edit_coding_value_modal(ns, rating_row, lang = "en")
}
\arguments{
\item{ns}{Shiny namespace function created via shiny::NS().}
\item{rating_row}{Single-row data frame or list describing the rating record to edit.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A Shiny modal dialog UI object.
}
\description{
Creates a Shiny modal dialog allowing administrators to inspect and update an existing ordinal rating value.
}
\examples{
\dontrun{
build_edit_coding_value_modal(shiny::NS("mod_admin"), rating_row)
}
}
