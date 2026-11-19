% Generated documentation for clean_table_columns
\name{clean_table_columns}
\alias{clean_table_columns}
\title{Clean and Normalize Imported Table Column Names}
\usage{
clean_table_columns(df)
}
\arguments{
\item{df}{Data frame to clean or validate.}
}
\value{
A data frame with cleaned column names.
}
\description{
Removes UTF-8 byte-order marks (BOM), trims whitespace, and normalizes column names of an imported data frame.
}
\examples{
clean_table_columns(data.frame(source_id = 1L, Kurzreferenz = "RefA"))
}
