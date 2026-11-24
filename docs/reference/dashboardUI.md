# Dashboard Module UI

Creates the Shiny UI for the coder dashboard, displaying personal coding
progress, training reliability trends, open-case breakdowns, and an
interactive scale directory.

## Usage

``` r
dashboardUI(
  id,
  kategorie_spalten = NULL,
  lang = "en",
  selected_tab = "tab_progress"
)
```

## Arguments

- id:

  Character string specifying the Shiny module namespace identifier.

- kategorie_spalten:

  Optional character vector of scale column names retained for backward
  compatibility.

- lang:

  Character string specifying the initial UI language code (`"en"` or
  `"de"`).

- selected_tab:

  Character string selecting the initially active tab (`"tab_progress"`
  or `"tab_scales_overview"`).

## Value

A `shiny.tag` (`fluidPage`) representing the dashboard interface.

## Examples

``` r
if (interactive()) {
  ui <- dashboardUI("mod_dashboard", lang = "en")
}
```
