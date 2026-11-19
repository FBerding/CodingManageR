% Generated documentation for get_reliability_metric_meta
\name{get_reliability_metric_meta}
\alias{get_reliability_metric_meta}
\title{Get Metadata and Thresholds for a Reliability Metric}
\usage{
get_reliability_metric_meta(metric_key, lang = "en")
}
\arguments{
\item{metric_key}{Character scalar identifying the reliability metric.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A named list of metadata fields for the requested metric.
}
\description{
Returns display label, short label, percentage flag, and visual badge thresholds for a given reliability metric key.
}
\examples{
get_reliability_metric_meta("alpha_ordinal", lang = "en")
}
