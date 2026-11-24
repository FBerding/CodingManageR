# Retrieve Localized Optgroup Choices for Reliability Metric Selectors

Builds a grouped list of supported inter-rater reliability metric keys
and localized display labels for use in
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

## Usage

``` r
get_reliability_metric_choices(lang = "en", include_all_core = TRUE)
```

## Arguments

- lang:

  Character language code (`"de"` or `"en"`). Defaults to `"en"`.

- include_all_core:

  Logical; whether to include the combined `"all_core"` overview option.
  Defaults to `TRUE`.

## Value

A named list suitable for `shiny::selectInput(..., choices = ...)`.

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
