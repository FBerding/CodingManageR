# Format Numeric Metric or Percentage for Display

Formats numerical reliability metrics with fixed decimal places or as
percentages with safe `NA` handling.

## Usage

``` r
format_metric_display(val, is_percentage = FALSE, digits = 3)
```

## Arguments

- val:

  A numeric or character value to format.

- is_percentage:

  Logical; if `TRUE`, formats with 1 decimal place and a percent sign.
  Defaults to `FALSE`.

- digits:

  Integer indicating the number of decimal places for non-percentage
  values. Defaults to `3`.

## Value

A formatted character string, or an em dash when `val` is missing or
non-numeric.

## Examples

``` r
format_metric_display(0.8421, is_percentage = FALSE, digits = 3)
#> [1] "0.842"
```
