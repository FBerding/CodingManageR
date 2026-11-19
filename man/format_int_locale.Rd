% Generated documentation for format_int_locale
\name{format_int_locale}
\alias{format_int_locale}
\title{Format an Integer with Locale-Specific Grouping}
\usage{
format_int_locale(x, lang = "en")
}
\arguments{
\item{x}{Numeric or integer value to format.}
\item{lang}{Character scalar language code ("de" or "en").}
}
\value{
A formatted character string.
}
\description{
Formats integer counts using German (".") or English (",") thousands separators.
}
\examples{
format_int_locale(12500, lang = "en")
format_int_locale(12500, lang = "de")
}
