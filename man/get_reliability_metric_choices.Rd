% Generated documentation for get_reliability_metric_choices
\name{get_reliability_metric_choices}
\alias{get_reliability_metric_choices}
\title{Get Localized Choices of Inter-Rater Reliability Metrics}
\usage{
get_reliability_metric_choices(lang = "en", include_all_core = TRUE)
}
\arguments{
\item{lang}{Character scalar language code ("de" or "en").}
\item{include_all_core}{Logical; if TRUE includes all core inter-rater reliability metrics.}
}
\value{
A named character vector of reliability metrics.
}
\description{
Returns a named character vector of supported reliability metric keys and their localized labels for UI selectors.
}
\examples{
get_reliability_metric_choices(lang = "en")
}
