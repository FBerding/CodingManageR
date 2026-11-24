# Render Standardized KPI Stat Card

Generates a consistent Bootstrap card widget for dashboard and
administrative key performance indicators.

## Usage

``` r
render_kpi_card_ui(
  title,
  value,
  subtitle = NULL,
  bg_class = "bg-primary",
  width = 4
)
```

## Arguments

- title:

  Character string describing the metric (e.g., `"Total Codings"`).

- value:

  Numeric or character representation of the primary metric value.

- subtitle:

  Optional character string providing extra context or time window.

- bg_class:

  Character string defining the Bootstrap background color class (e.g.,
  `"bg-dark"`, `"bg-success"`, `"bg-info"`). Defaults to `"bg-primary"`.

- width:

  Integer column width according to Bootstrap's 12-column grid. Defaults
  to `4`.

## Value

A Shiny `column` UI tag containing the styled stat card.

## Examples

``` r
if (FALSE) { # \dontrun{
render_kpi_card_ui("Total Codings", "142", subtitle = "Active scale")
} # }
```
