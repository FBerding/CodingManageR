# Normalize and Validate a Browser Locale String

Extracts the two-letter ISO 639-1 language prefix from a raw locale
header or `navigator.language` value (e.g., `"de-DE"` -\> `"de"`) and
validates it against `supported`.

## Usage

``` r
detect_browser_language(
  raw_lang = NULL,
  supported = get_supported_languages(),
  fallback = "en"
)
```

## Arguments

- raw_lang:

  Character scalar from `navigator.language` or `HTTP_ACCEPT_LANGUAGE`.

- supported:

  Character vector of allowed language codes. Defaults to
  [`get_supported_languages()`](https://fberding.github.io/CodingManageR/reference/get_supported_languages.md).

- fallback:

  Character scalar fallback language code returned if `raw_lang` is
  unsupported or empty. Defaults to `"en"`.

## Value

A validated two-letter character language code (e.g., `"de"` or `"en"`).

## Examples

``` r
detect_browser_language("de-DE")
#> [1] "de"
detect_browser_language("fr-FR", fallback = "en")
#> [1] "en"
```
