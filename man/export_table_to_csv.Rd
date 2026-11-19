% Generated documentation for export_table_to_csv
\name{export_table_to_csv}
\alias{export_table_to_csv}
\title{Fetch a Database Table for CSV Export}
\usage{
export_table_to_csv(con, table_name)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{table_name}{Character scalar database table name.}
}
\value{
A data frame containing the table contents.
}
\description{
Queries all rows of an allowlisted CodingManageR database table in a standardized order suitable for CSV download.
}
\examples{
\dontrun{
export_table_to_csv(con, "sources")
}
}
