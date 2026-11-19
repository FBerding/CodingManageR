% Generated documentation for get_source_license_choices
\name{get_source_license_choices}
\alias{get_source_license_choices}
\title{Get Localized Source License Choices}
\usage{
get_source_license_choices(lang = "en")
}
\arguments{
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A named character vector of license choices.
}
\description{
Returns standard license options (such as Creative Commons and proprietary research licenses) for source metadata forms.
}
\examples{
get_source_license_choices(lang = "en")
}
