% Generated documentation for find_translations_file
\name{find_translations_file}
\alias{find_translations_file}
\title{Locate the Bundled Translations JSON File}
\usage{
find_translations_file()
}
\value{
A character scalar path to translations.json, or NULL if not found.
}
\description{
Searches installed package paths and local development paths to locate inst/translations.json.
}
\examples{
find_translations_file()
}
