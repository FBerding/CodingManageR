# Classify Inter-Rater Reliability Magnitude

Classifies a numeric reliability coefficient (such as Cohen's kappa or
Krippendorff's alpha) according to the benchmark criteria of Landis and
Koch (1977).

## Usage

``` r
classify_reliability_magnitude(kappa, lang = "en")
```

## Arguments

- kappa:

  Numeric scalar representing Cohen's kappa or another agreement
  coefficient.

- lang:

  Character language code (`"de"` or `"en"`). Defaults to `"en"`.

## Value

A named list containing `label` (localized benchmark label) and
`css_class` (Bootstrap styling class).

## Examples

``` r
classify_reliability_magnitude(0.74, lang = "en")
#> $label
#> [1] "Substantial (0.61–0.80)"
#> 
#> $css_class
#> [1] "text-info bg-light border-info"
#> 
```
