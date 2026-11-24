# Classify Inter-Rater Reliability Magnitude

Classifies a numeric reliability coefficient (such as kappa or alpha)
into a localized qualitative interpretation category and Bootstrap badge
class.

## Usage

``` r
classify_reliability_magnitude(kappa, lang = "en")
```

## Arguments

- kappa:

  Numeric scalar inter-rater reliability coefficient.

- lang:

  Character scalar language code ("de" or "en").

## Value

A list with classification label, color/badge class, and interpretation
metadata.

## Examples

``` r
classify_reliability_magnitude(0.82, lang = "en")
#> $label
#> [1] "Almost Perfect (> 0.80)"
#> 
#> $css_class
#> [1] "text-success bg-light border-success"
#> 
```
