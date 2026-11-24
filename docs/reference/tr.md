# Translate a Localization Key into the Selected Language

Looks up a dot-separated translation key in the active language
dictionary with fallback support and optional sprintf interpolation.

## Usage

``` r
tr(key, lang = "en", ..., default = NULL)
```

## Arguments

- key:

  Character scalar dot-separated localization key.

- lang:

  Character scalar language code ("de" or "en").

- ...:

  Additional arguments forwarded to underlying functions or sprintf
  formatting.

- default:

  Optional character scalar returned when the key is missing.

## Value

A localized character string.

## Examples

``` r
tr("auth.login_btn", lang = "en")
#> [1] "Sign In"
```
