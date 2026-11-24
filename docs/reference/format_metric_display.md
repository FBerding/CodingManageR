# Format a Reliability Metric Value for Display

Formats a numeric reliability coefficient or percentage value with fixed
decimal places and safe NA handling.

## Usage

``` r
format_metric_display(val, is_percentage = FALSE, digits = 3)
```

## Arguments

- val:

  Numeric scalar reliability value to format.

- is_percentage:

  Logical; if TRUE formats the value as a percentage.

- digits:

  Integer scalar number of decimal digits.

## Value

A formatted character string.

## Examples

``` r
format_metric_display(0.8421, is_percentage = FALSE, digits = 3)
#> [1] "0.842"
```
