% Generated documentation for run_app
\name{run_app}
\alias{run_app}
\title{Launch the CodingManageR Shiny Application}
\usage{
run_app(sqlite_path = NULL, config = NULL, empty = TRUE, options = list(), ...)
}
\arguments{
\item{sqlite_path}{Optional character path to a local SQLite database file.}
\item{config}{Optional named list of database connection parameters.}
\item{empty}{Logical; if TRUE seeds minimal default accounts only, if FALSE seeds the full demonstration dataset.}
\item{options}{Optional named list of options forwarded to shiny::shinyApp().}
\item{...}{Additional arguments forwarded to underlying functions or sprintf formatting.}
}
\value{
A shiny.appobj object.
}
\description{
Constructs and returns a shiny.appobj configured for local SQLite storage or pooled MySQL/MariaDB server deployments.
}
\examples{
\dontrun{
app <- run_app(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
app
}
}
