% Generated documentation for render_kpi_card_ui
\name{render_kpi_card_ui}
\alias{render_kpi_card_ui}
\title{Render a Bootstrap KPI Summary Card}
\usage{
render_kpi_card_ui(title, value, subtitle = NULL, bg_class = "bg-primary",
  width = 4)
}
\arguments{
\item{title}{Character scalar card heading.}
\item{value}{Character or numeric scalar KPI value displayed on the card.}
\item{subtitle}{Optional character scalar subtitle displayed beneath the KPI value.}
\item{bg_class}{Character scalar Bootstrap background CSS class.}
\item{width}{Integer scalar Bootstrap grid column width (1 to 12).}
}
\value{
A Shiny UI tag object.
}
\description{
Constructs a styled Bootstrap column containing a KPI metric title, prominent value, and optional subtitle.
}
\examples{
\dontrun{
render_kpi_card_ui("Total Codings", "142", subtitle = "Active scale")
}
}
