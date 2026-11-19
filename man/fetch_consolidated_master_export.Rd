% Generated documentation for fetch_consolidated_master_export
\name{fetch_consolidated_master_export}
\alias{fetch_consolidated_master_export}
\title{Fetch Consolidated Master Coding Export Dataset}
\usage{
fetch_consolidated_master_export(con, scale_ids = NULL, source_ids = NULL,
  user_ids = NULL)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{scale_ids}{Optional integer vector of scale IDs to filter the export.}
\item{source_ids}{Optional integer vector of source IDs to filter the export.}
\item{user_ids}{Optional integer vector of user IDs to filter the export.}
}
\value{
A consolidated data frame ready for CSV export or statistical analysis.
}
\description{
Joins user ratings, texts, sources, scale definitions, and coder metadata into a consolidated flat data frame with optional scale, source, and user filters.
}
\examples{
\dontrun{
fetch_consolidated_master_export(con)
}
}
