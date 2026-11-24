# Retrieve Localized Metadata for a Single Reliability Metric Key

Returns the localized title, short label, description, percentage
formatting flag, and accent color associated with a given reliability
metric key.

## Usage

``` r
get_reliability_metric_meta(metric_key, lang = "en")
```

## Arguments

- metric_key:

  Character scalar identifying the reliability metric.

- lang:

  Character language code (`"de"` or `"en"`). Defaults to `"en"`.

## Value

A named list with elements `label`, `short_label`, `desc`, `is_pct`, and
`color`.

## Examples

``` r
get_reliability_metric_meta("alpha_ordinal", lang = "en")
#> $label
#> [1] "Krippendorff's Alpha (ordinal)"
#> 
#> $short_label
#> [1] "Kripp. α (ord.)"
#> 
#> $desc
#> [1] "Content-analysis reliability with ordinal distance weighting"
#> 
#> $is_pct
#> [1] FALSE
#> 
#> $color
#> [1] "#d97706"
#> 
```
