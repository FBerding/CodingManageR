# Render a Color-Coded Reliability Metric Badge

Formats a reliability metric value inside a Bootstrap badge colored
according to high and medium quality thresholds.

## Usage

``` r
render_metric_badge_ui(val, is_percentage = FALSE, thresh_high = NULL,
  thresh_mid = NULL)
```

## Arguments

- val:

  Numeric scalar reliability value to format.

- is_percentage:

  Logical; if TRUE formats the value as a percentage.

- thresh_high:

  Optional numeric threshold for high reliability styling.

- thresh_mid:

  Optional numeric threshold for moderate reliability styling.

## Value

A Shiny UI span tag.

## Examples

``` r
if (FALSE) { # \dontrun{
render_metric_badge_ui(0.84, is_percentage = FALSE, thresh_high = 0.8, thresh_mid = 0.6)
} # }
```
