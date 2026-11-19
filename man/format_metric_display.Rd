% Generated documentation for format_metric_display
\name{format_metric_display}
\alias{format_metric_display}
\title{Format a Reliability Metric Value for Display}
\usage{
format_metric_display(val, is_percentage = FALSE, digits = 3)
}
\arguments{
\item{val}{Numeric scalar reliability value to format.}
\item{is_percentage}{Logical; if TRUE formats the value as a percentage.}
\item{digits}{Integer scalar number of decimal digits.}
}
\value{
A formatted character string.
}
\description{
Formats a numeric reliability coefficient or percentage value with fixed decimal places and safe NA handling.
}
\examples{
format_metric_display(0.8421, is_percentage = FALSE, digits = 3)
}
