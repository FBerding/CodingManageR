# Load and Cache Application Translation Dictionaries

Reads the bilingual translation catalog from inst/translations.json and
caches it in an internal package environment.

## Usage

``` r
load_translations(force_reload = FALSE)
```

## Arguments

- force_reload:

  Logical; if TRUE bypasses the cache and reloads translations from
  disk.

## Value

A nested list containing translations keyed by language code.

## Examples

``` r
dicts <- load_translations()
names(dicts)
#> [1] "de" "en"
```
