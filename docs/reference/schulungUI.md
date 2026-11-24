# Training Module UI

Creates the Shiny UI container for the interactive coding training
module.

## Usage

``` r
schulungUI(id)
```

## Arguments

- id:

  Character string specifying the Shiny module namespace identifier.

## Value

A `shiny.tag` (`uiOutput`) rendering the training workspace.

## Examples

``` r
if (interactive()) {
  ui <- shiny::fluidPage(schulungUI("mod_training"))
}
```
