# Build the Bilingual Language Selector Widget

Creates a compact UI control allowing users to switch the CodingManageR
interface between German and English.

## Usage

``` r
language_selector_ui(id = "app_language", current_lang = "de", class = "")
```

## Arguments

- id:

  Character scalar module namespace identifier.

- current_lang:

  Reactive expression or reactiveVal returning the active language code
  ("de" or "en").

- class:

  Optional character scalar of additional CSS classes.

## Value

A Shiny UI tag object.

## Examples

``` r
if (FALSE) { # \dontrun{
language_selector_ui("app_language", current_lang = "en")
} # }
```
