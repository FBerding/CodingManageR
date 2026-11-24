# Format an Integer with Locale-Specific Grouping

Formats integer counts using German (".") or English (",") thousands
separators.

## Usage

``` r
format_int_locale(x, lang = "en")
```

## Arguments

- x:

  Numeric or integer value to format.

- lang:

  Character scalar language code ("de" or "en").

## Value

A formatted character string.

## Examples

``` r
format_int_locale(12500, lang = "en")
#> [1] "12,500"
format_int_locale(12500, lang = "de")
#> [1] "12.500"
```
