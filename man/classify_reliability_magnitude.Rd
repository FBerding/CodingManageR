% Generated documentation for classify_reliability_magnitude
\name{classify_reliability_magnitude}
\alias{classify_reliability_magnitude}
\title{Classify Inter-Rater Reliability Magnitude}
\usage{
classify_reliability_magnitude(kappa, lang = "en")
}
\arguments{
\item{kappa}{Numeric scalar inter-rater reliability coefficient.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A list with classification label, color/badge class, and interpretation metadata.
}
\description{
Classifies a numeric reliability coefficient (such as kappa or alpha) into a localized qualitative interpretation category and Bootstrap badge class.
}
\examples{
classify_reliability_magnitude(0.82, lang = "en")
}
