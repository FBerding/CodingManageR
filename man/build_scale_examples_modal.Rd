% Generated documentation for build_scale_examples_modal
\name{build_scale_examples_modal}
\alias{build_scale_examples_modal}
\title{Build Modal Dialog Displaying Scale Anchor Examples}
\usage{
build_scale_examples_modal(scale_df, active_level = NULL, lang = "en")
}
\arguments{
\item{scale_df}{Data frame containing the scale definition and level anchor descriptions.}
\item{active_level}{Optional integer scalar highlighting the currently selected ordinal level.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A Shiny modal dialog UI object.
}
\description{
Constructs a Shiny modal showing the coding scale definition, descriptions, and anchor examples across all ordinal levels.
}
\examples{
\dontrun{
build_scale_examples_modal(scale_df, active_level = 2, lang = "en")
}
}
