% Generated documentation for validate_import_schema
\name{validate_import_schema}
\alias{validate_import_schema}
\title{Validate an Uploaded CSV Data Frame Against Table Schema Rules}
\usage{
validate_import_schema(target_table, df, lang = "en")
}
\arguments{
\item{target_table}{Character scalar target database table name.}
\item{df}{Data frame to clean or validate.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A list containing validation status (valid), cleaned data frame, and localized error messages.
}
\description{
Checks required columns, data types, non-empty keys, and domain constraints before allowing a CSV import into a target CodingManageR table.
}
\examples{
df <- data.frame(
  Kurzreferenz = "Ref1",
  Vollstaendiges_Zitat = "Full citation"
)
validate_import_schema("sources", df, lang = "en")
}
