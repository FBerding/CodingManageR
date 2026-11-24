# Build Modal Dialog for Coding Scale Definitions and Stage Examples

Constructs a Shiny `modalDialog()` displaying full scale descriptions,
stage boundaries, and concrete anchor examples for all ordinal levels
from `0` through `max_value`.

## Usage

``` r
build_scale_examples_modal(scale_df, active_level = NULL, lang = "en")
```

## Arguments

- scale_df:

  A 1-row `data.frame` from `scale_definitions`.

- active_level:

  Optional integer highlighting a specific score level (e.g., `0..4`).

- lang:

  Character language code (`"de"` or `"en"`) passed to
  [`tr()`](https://fberding.github.io/CodingManageR/reference/tr.md).
  Defaults to `"en"`.

## Value

A Shiny `modalDialog` object ready to be passed to `showModal()`.

## Examples

``` r
if (FALSE) { # \dontrun{
build_scale_examples_modal(scale_df, active_level = 2, lang = "en")
} # }
```
