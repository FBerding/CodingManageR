# Compute Inter-Rater Reliability Metrics via `aifeducation`

Computes 16 inter-rater reliability, agreement, and classification
metrics for a coder training session by comparing coder ratings against
gold-standard reference ratings via
[`aifeducation::get_coder_metrics()`](https://fberding.github.io/aifeducation/reference/get_coder_metrics.html).

## Usage

``` r
berechne_aifeducation_metriken(user_vals, true_vals, max_val)
```

## Arguments

- user_vals:

  Numeric or integer vector of coder ratings.

- true_vals:

  Numeric or integer vector of gold-standard reference ratings.

- max_val:

  Integer scalar giving the maximum ordinal scale value (used to define
  factor levels `0:max_val`).

## Value

A named list of rounded character metric values (`accuracy`,
`balanced_accuracy`, `iota_index`, `avg_iota2`, `cohens_kappa`,
`kappa_linear`, `kappa_squared`, `fleiss_kappa`, `kendall_w`,
`alpha_nominal`, `alpha_ordinal`, `gwet_ac1`, `gwet_ac2_quadratic`,
`macro_precision`, `macro_recall`, and `macro_f1`).

## Examples

``` r
if (FALSE) { # \dontrun{
berechne_aifeducation_metriken(c(0, 1, 2, 3), c(0, 1, 2, 3), max_val = 3)
} # }
```
