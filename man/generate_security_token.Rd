% Generated documentation for generate_security_token
\name{generate_security_token}
\alias{generate_security_token}
\title{Generate a Random Alphanumeric Access Token}
\usage{
generate_security_token(length = 12)
}
\arguments{
\item{length}{Integer scalar length of the generated token.}
}
\value{
A single character string of length equal to the requested token length.
}
\description{
Generates a random character access token of the specified length for authenticating CodingManageR users.
}
\examples{
generate_security_token(12)
}
