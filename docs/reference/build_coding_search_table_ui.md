# Build HTML Table for Coding Search Results

Renders a paginated HTML table of case coding records with action
buttons for editing individual rating values.

## Usage

``` r
build_coding_search_table_ui(ns, page_df, lang = "en")
```

## Arguments

- ns:

  Shiny namespace function created via shiny::NS().

- page_df:

  Data frame slice containing the rows for the current page.

- lang:

  Character scalar language code ("de" or "en").

## Value

A Shiny UI tag object.

## Examples

``` r
if (FALSE) { # \dontrun{
build_coding_search_table_ui(shiny::NS("mod_admin"), data.frame())
} # }
```
