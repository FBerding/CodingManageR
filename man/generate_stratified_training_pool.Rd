% Generated documentation for generate_stratified_training_pool
\name{generate_stratified_training_pool}
\alias{generate_stratified_training_pool}
\title{Generate a Stratified Gold-Standard Training Pool}
\usage{
generate_stratified_training_pool(con, scale_identifier, target_count,
  max_scale_value)
}
\arguments{
\item{con}{Active DBI database connection or pool::dbPool object.}
\item{scale_identifier}{Integer scale_id or character short_label identifying the coding scale.}
\item{target_count}{Integer scalar desired number of stratified training cases.}
\item{max_scale_value}{Integer scalar maximum ordinal scale value.}
}
\value{
A data frame of sampled training cases with gold-standard ratings.
}
\description{
Draws a balanced, stratified subset of gold-standard annotated case texts across all ordinal scale levels (0 to max_scale_value) for coder reliability training.
}
\examples{
\dontrun{
generate_stratified_training_pool(con, scale_identifier = 1L,
  target_count = 8L, max_scale_value = 3L)
}
}
