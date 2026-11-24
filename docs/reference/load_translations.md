# Load Translations Dictionary from JSON with Modification-Time Caching

Reads and parses `inst/translations.json` into a nested R list, caching
the result in memory until the file's modification timestamp changes.
Falls back to a minimal built-in dictionary if the JSON file cannot be
accessed.

## Usage

``` r
load_translations(force_reload = FALSE)
```

## Arguments

- force_reload:

  Logical; if `TRUE`, bypasses the cache and reloads from disk. Defaults
  to `FALSE`.

## Value

A nested named `list` containing language sub-trees (e.g., `de`, `en`).

## Examples

``` r
dict <- load_translations()
names(dict)
#> [1] "de" "en"
```
