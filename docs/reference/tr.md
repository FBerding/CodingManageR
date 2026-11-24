# Translate a Dot-Separated UI Key with Fallback and Formatting

Resolves a hierarchical translation key from `inst/translations.json`
for the requested language, falling back to `"en"`, then `"de"`, then
`default` or `[key]`. Supports optional
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) positional arguments.

## Usage

``` r
tr(key, lang = "en", ..., default = NULL)
```

## Arguments

- key:

  Dot-separated translation key (e.g., `"auth.login_title"`).

- lang:

  Target language code (`"de"` or `"en"`). Defaults to `"en"`.

- ...:

  Optional positional arguments passed to
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- default:

  Optional fallback string used when the key is missing across all
  languages.

## Value

A localized character string.

## Examples

``` r
tr("auth.login_title", lang = "en")
#> [1] "Sign In"
```
