% Generated documentation for render_metric_badge_ui
\name{render_metric_badge_ui}
\alias{render_metric_badge_ui}
\title{Render a Color-Coded Reliability Metric Badge}
\usage{
render_metric_badge_ui(val, is_percentage = FALSE, thresh_high = NULL,
  thresh_mid = NULL)
}
\arguments{
\item{val}{Numeric scalar reliability value to format.}
\item{is_percentage}{Logical; if TRUE formats the value as a percentage.}
\item{thresh_high}{Optional numeric threshold for high reliability styling.}
\item{thresh_mid}{Optional numeric threshold for moderate reliability styling.}
}
\value{
A Shiny UI span tag.
}
\description{
Formats a reliability metric value inside a Bootstrap badge colored according to high and medium quality thresholds.
}
\examples{
\dontrun{
render_metric_badge_ui(0.84, is_percentage = FALSE, thresh_high = 0.8, thresh_mid = 0.6)
}
}
