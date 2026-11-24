# Build Modal Dialog Displaying Scale Anchor Examples

Constructs a Shiny modal showing the coding scale definition,
descriptions, and anchor examples across all ordinal levels.

## Usage

``` r
build_scale_examples_modal(scale_df, active_level = NULL, lang = "en")
```

## Arguments

- scale_df:

  Data frame containing the scale definition and level anchor
  descriptions.

- active_level:

  Optional integer scalar highlighting the currently selected ordinal
  level.

- lang:

  Character scalar language code ("de" or "en").

## Value

A Shiny modal dialog UI object.

## Examples

``` r
if (FALSE) { # \dontrun{
build_scale_examples_modal(scale_df, active_level = 2, lang = "en")
} # }
```
