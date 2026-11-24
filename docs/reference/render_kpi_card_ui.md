# Render a Bootstrap KPI Summary Card

Constructs a styled Bootstrap column containing a KPI metric title,
prominent value, and optional subtitle.

## Usage

``` r
render_kpi_card_ui(title, value, subtitle = NULL, bg_class = "bg-primary",
  width = 4)
```

## Arguments

- title:

  Character scalar card heading.

- value:

  Character or numeric scalar KPI value displayed on the card.

- subtitle:

  Optional character scalar subtitle displayed beneath the KPI value.

- bg_class:

  Character scalar Bootstrap background CSS class.

- width:

  Integer scalar Bootstrap grid column width (1 to 12).

## Value

A Shiny UI tag object.

## Examples

``` r
if (FALSE) { # \dontrun{
render_kpi_card_ui("Total Codings", "142", subtitle = "Active scale")
} # }
```
