# Coding Module UI

Creates the Shiny UI container for the interactive case coding
workspace.

## Usage

``` r
kodierungenUI(id)
```

## Arguments

- id:

  Character string specifying the Shiny module namespace identifier.

## Value

A `shiny.tag` (`uiOutput`) rendering the coding workspace.

## Examples

``` r
if (interactive()) {
  ui <- shiny::fluidPage(kodierungenUI("mod_codings"))
}
```
