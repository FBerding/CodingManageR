# Build Modal Dialog for Editing a Coding Rating Value

Creates a Shiny modal dialog allowing administrators to inspect and
update an existing ordinal rating value.

## Usage

``` r
build_edit_coding_value_modal(ns, rating_row, lang = "en")
```

## Arguments

- ns:

  Shiny namespace function created via shiny::NS().

- rating_row:

  Single-row data frame or list describing the rating record to edit.

- lang:

  Character scalar language code ("de" or "en").

## Value

A Shiny modal dialog UI object.

## Examples

``` r
if (FALSE) { # \dontrun{
build_edit_coding_value_modal(shiny::NS("mod_admin"), rating_row)
} # }
```
