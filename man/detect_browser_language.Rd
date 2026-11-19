% Generated documentation for detect_browser_language
\name{detect_browser_language}
\alias{detect_browser_language}
\title{Detect Supported Application Language from Browser Locale}
\usage{
detect_browser_language(raw_lang = NULL, supported = get_supported_languages(),
  fallback = "en")
}
\arguments{
\item{raw_lang}{Optional raw locale string reported by the browser (e.g., "de-DE").}
\item{supported}{Character vector of supported language codes.}
\item{fallback}{Character scalar fallback language code when detection fails.}
}
\value{
A single character string representing a supported language code.
}
\description{
Normalizes a raw browser locale string (e.g., "de-DE" or "en-US") to a supported CodingManageR language code with fallback.
}
\examples{
detect_browser_language("de-DE")
detect_browser_language("fr-FR", fallback = "en")
}
