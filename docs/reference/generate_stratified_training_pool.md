# Generate a Stratified Gold-Standard Training Pool

Draws a balanced, stratified subset of gold-standard annotated case
texts across all ordinal scale levels (0 to max_scale_value) for coder
reliability training.

## Usage

``` r
generate_stratified_training_pool(con, scale_identifier, target_count,
  max_scale_value)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- scale_identifier:

  Integer scale_id or character short_label identifying the coding
  scale.

- target_count:

  Integer scalar desired number of stratified training cases.

- max_scale_value:

  Integer scalar maximum ordinal scale value.

## Value

A data frame of sampled training cases with gold-standard ratings.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_stratified_training_pool(con, scale_identifier = 1L,
  target_count = 8L, max_scale_value = 3L)
} # }
```
