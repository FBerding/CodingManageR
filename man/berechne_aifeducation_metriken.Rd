% Generated documentation for berechne_aifeducation_metriken
\name{berechne_aifeducation_metriken}
\alias{berechne_aifeducation_metriken}
\title{Compute Inter-Rater Reliability Metrics via aifeducation}
\usage{
berechne_aifeducation_metriken(user_vals, true_vals, max_val)
}
\arguments{
\item{user_vals}{Numeric or integer vector of coder ratings.}
\item{true_vals}{Numeric or integer vector of gold-standard reference ratings.}
\item{max_val}{Integer scalar giving the maximum ordinal scale value.}
}
\value{
A named list containing reliability metrics, formatted summaries, and diagnostic metadata.
}
\description{
Computes 16 inter-rater reliability and agreement metrics (including Krippendorff alpha, Gwet AC1/AC2, Cohen/Fleiss kappa, Brennan-Prediger, Kendall W, and ordinal variants) comparing coder ratings against gold-standard ratings.
}
\examples{
\dontrun{
berechne_aifeducation_metriken(c(0, 1, 2, 3), c(0, 1, 2, 3), max_val = 3)
}
}
