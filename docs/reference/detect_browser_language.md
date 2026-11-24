# Detect Supported Application Language from Browser Locale

Normalizes a raw browser locale string (e.g., "de-DE" or "en-US") to a
supported CodingManageR language code with fallback.

## Usage

``` r
detect_browser_language(raw_lang = NULL, supported = get_supported_languages(),
  fallback = "en")
```

## Arguments

- raw_lang:

  Optional raw locale string reported by the browser (e.g., "de-DE").

- supported:

  Character vector of supported language codes.

- fallback:

  Character scalar fallback language code when detection fails.

## Value

A single character string representing a supported language code.

## Examples

``` r
detect_browser_language("de-DE")
#> [1] "de"
detect_browser_language("fr-FR", fallback = "en")
#> [1] "en"
```
