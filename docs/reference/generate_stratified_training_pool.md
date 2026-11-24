# Generate Stratified Training Pool Guaranteeing Full Scale Range

Draws a balanced, stratified sample of training cases from `texts` and
`user_ratings` (prioritizing `gold_standard` benchmarks over field
codings), ensuring that every discrete ordinal level from `0` to
`max_scale_value` is represented whenever available.

## Usage

``` r
generate_stratified_training_pool(
  con,
  scale_identifier,
  target_count,
  max_scale_value
)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- scale_identifier:

  Character or integer identifying the coding scale.

- target_count:

  Total number of training items to draw (at least
  `max_scale_value + 1`).

- max_scale_value:

  Integer indicating the maximum allowed score on this scale.

## Value

A `data.frame` of sampled training cases including `text_id`, `text`,
`source_id`, `case_text`, `bewertung`, and `val_int`.

## Examples

``` r
if (FALSE) { # \dontrun{
pool_df <- generate_stratified_training_pool(con, scale_identifier = 1L,
  target_count = 8L, max_scale_value = 3L)
} # }
```
