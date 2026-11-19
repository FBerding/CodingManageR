% Generated documentation for tr
\name{tr}
\alias{tr}
\title{Translate a Localization Key into the Selected Language}
\usage{
tr(key, lang = "en", ..., default = NULL)
}
\arguments{
\item{key}{Character scalar dot-separated localization key.}
\item{lang}{Character scalar language code ("de" or "en").}
\item{...}{Additional arguments forwarded to underlying functions or sprintf formatting.}
\item{default}{Optional character scalar returned when the key is missing.}
}
\value{
A localized character string.
}
\description{
Looks up a dot-separated translation key in the active language dictionary with fallback support and optional sprintf interpolation.
}
\examples{
tr("auth.login_btn", lang = "en")
}
