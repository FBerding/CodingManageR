% Generated documentation for load_translations
\name{load_translations}
\alias{load_translations}
\title{Load and Cache Application Translation Dictionaries}
\usage{
load_translations(force_reload = FALSE)
}
\arguments{
\item{force_reload}{Logical; if TRUE bypasses the cache and reloads translations from disk.}
}
\value{
A nested list containing translations keyed by language code.
}
\description{
Reads the bilingual translation catalog from inst/translations.json and caches it in an internal package environment.
}
\examples{
dicts <- load_translations()
names(dicts)
}
