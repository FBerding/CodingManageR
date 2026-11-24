# Shiny UI Module for the Progress Dashboard

Constructs the user interface for the CodingManageR personal progress
and training analytics dashboard.

## Usage

``` r
dashboardUI(id, kategorie_spalten = NULL, lang = "en",
  selected_tab = "tab_progress")
```

## Arguments

- id:

  Character scalar module namespace identifier.

- kategorie_spalten:

  Reactive expression returning a named character vector mapping scale
  short labels to scale IDs.

- lang:

  Character scalar language code ("de" or "en").

- selected_tab:

  Character scalar specifying the initially selected dashboard tab.

## Value

A Shiny UI tag list.

## Examples

``` r
if (FALSE) { # \dontrun{
dashboardUI("mod_dashboard", lang = "en")
} # }
```
