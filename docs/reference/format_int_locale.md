# Format Integer with Locale-Aware Separators

Formats an integer count using distinct thousands and decimal separators
matching the active UI language (`"de"` uses `"."`/`","`; `"en"` uses
`","`/`"."`) so [`prettyNum()`](https://rdrr.io/r/base/formatc.html)
never emits warnings about identical `big.mark` and `decimal.mark`
characters.

## Usage

``` r
format_int_locale(x, lang = "en")
```

## Arguments

- x:

  Numeric or integer value to format.

- lang:

  Character language code (`"de"` or `"en"`). Defaults to `"en"`.

## Value

A character string containing the formatted integer.

## Examples

``` r
format_int_locale(12500, lang = "en")
#> [1] "12,500"
format_int_locale(12500, lang = "de")
#> [1] "12.500"
```
