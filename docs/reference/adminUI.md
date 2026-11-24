# Shiny UI Module for the Admin Console

Builds the multi-tab Bootstrap user interface for the CodingManageR
administration console.

## Usage

``` r
adminUI(id, lang = "en", selected_subtab = "tab_users")
```

## Arguments

- id:

  Character scalar module namespace identifier.

- lang:

  Character scalar language code ("de" or "en").

- selected_subtab:

  Character scalar specifying the initially selected admin subtab.

## Value

A Shiny UI tag list.

## Examples

``` r
if (FALSE) { # \dontrun{
adminUI("mod_admin", lang = "en")
} # }
```
