# Get Metadata and Thresholds for a Reliability Metric

Returns display label, short label, percentage flag, and visual badge
thresholds for a given reliability metric key.

## Usage

``` r
get_reliability_metric_meta(metric_key, lang = "en")
```

## Arguments

- metric_key:

  Character scalar identifying the reliability metric.

- lang:

  Character scalar language code ("de" or "en").

## Value

A named list of metadata fields for the requested metric.

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
