# Get Localized Choices of Inter-Rater Reliability Metrics

Returns a named character vector of supported reliability metric keys
and their localized labels for UI selectors.

## Usage

``` r
get_reliability_metric_choices(lang = "en", include_all_core = TRUE)
```

## Arguments

- lang:

  Character scalar language code ("de" or "en").

- include_all_core:

  Logical; if TRUE includes all core inter-rater reliability metrics.

## Value

A named character vector of reliability metrics.

## Examples

``` r
get_reliability_metric_choices(lang = "en")
#> $`Combined Overview`
#> $`Combined Overview`$`All Core Metrics (Accuracy, Kappa, Alpha, Iota², Gwet)`
#> [1] "all_core"
#> 
#> 
#> $`Nominal Reliability`
#> $`Nominal Reliability`$`Cohen's Kappa (κ)`
#> [1] "cohens_kappa"
#> 
#> $`Nominal Reliability`$`Krippendorff's Alpha (nominal)`
#> [1] "alpha_nominal"
#> 
#> $`Nominal Reliability`$`Fleiss' Kappa`
#> [1] "fleiss_kappa"
#> 
#> $`Nominal Reliability`$`Gwet's AC1 (nominal)`
#> [1] "gwet_ac1"
#> 
#> 
#> $`Ordinal Reliability & Rank Measures`
#> $`Ordinal Reliability & Rank Measures`$`Krippendorff's Alpha (ordinal)`
#> [1] "alpha_ordinal"
#> 
#> $`Ordinal Reliability & Rank Measures`$`Linearly Weighted Kappa`
#> [1] "kappa_linear"
#> 
#> $`Ordinal Reliability & Rank Measures`$`Quadratically Weighted Kappa`
#> [1] "kappa_squared"
#> 
#> $`Ordinal Reliability & Rank Measures`$`Gwet's AC2 (quadratic)`
#> [1] "gwet_ac2_quadratic"
#> 
#> $`Ordinal Reliability & Rank Measures`$`Kendall's W`
#> [1] "kendall_w"
#> 
#> 
#> $`Iota Metrics`
#> $`Iota Metrics`$`Average Iota (Iota²)`
#> [1] "avg_iota2"
#> 
#> $`Iota Metrics`$`Dynamic Iota Index (ι)`
#> [1] "iota_index"
#> 
#> 
#> $`Classification & Agreement Metrics`
#> $`Classification & Agreement Metrics`$`Accuracy (%)`
#> [1] "accuracy"
#> 
#> $`Classification & Agreement Metrics`$`Balanced Accuracy (%)`
#> [1] "balanced_accuracy"
#> 
#> $`Classification & Agreement Metrics`$`Macro-F1 (%)`
#> [1] "macro_f1"
#> 
#> $`Classification & Agreement Metrics`$`Macro-Precision (%)`
#> [1] "macro_precision"
#> 
#> $`Classification & Agreement Metrics`$`Macro-Recall (%)`
#> [1] "macro_recall"
#> 
#> 
```
