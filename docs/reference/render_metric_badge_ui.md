# Render Metric Status Badge

Generates a color-coded Bootstrap HTML badge based on inter-rater
reliability thresholds.

## Usage

``` r
render_metric_badge_ui(
  val,
  is_percentage = FALSE,
  thresh_high = NULL,
  thresh_mid = NULL
)
```

## Arguments

- val:

  Numeric metric value or character representation.

- is_percentage:

  Logical; if `TRUE`, interprets values on a 0-100 scale, otherwise on a
  0.0-1.0 scale. Defaults to `FALSE`.

- thresh_high:

  Optional numeric upper threshold for high/green styling. Defaults to
  `0.80` (or `80.0` when `is_percentage = TRUE`).

- thresh_mid:

  Optional numeric middle threshold for moderate/warning styling.
  Defaults to `0.60` (or `60.0` when `is_percentage = TRUE`).

## Value

A Shiny `tags$span` UI element with formatted badge markup.

## Examples

``` r
if (FALSE) { # \dontrun{
render_metric_badge_ui(0.84, is_percentage = FALSE)
} # }
```
