% Generated documentation for export_table_template_csv
\name{export_table_template_csv}
\alias{export_table_template_csv}
\title{Build an Empty Schema Template Data Frame for CSV Export}
\usage{
export_table_template_csv(table_name)
}
\arguments{
\item{table_name}{Character scalar database table name.}
}
\value{
A zero-row data frame with the target table columns.
}
\description{
Returns a zero-row data frame with the exact expected column schema for a given CodingManageR database table to guide CSV imports.
}
\examples{
export_table_template_csv("sources")
}
