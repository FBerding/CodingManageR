# Locate the Canonical `translations.json` File

Resolves the path to `inst/translations.json` across standard R project
roots, `shiny::runApp("R")` working directories, installed R package
locations ([`system.file()`](https://rdrr.io/r/base/system.file.html)),
and web preview fallbacks.

## Usage

``` r
find_translations_file()
```

## Value

Character scalar file path to `translations.json`, or `NULL` if not
found.

## Examples

``` r
find_translations_file()
#> [1] "/home/runner/work/_temp/Library/CodingManageR/translations.json"
```
