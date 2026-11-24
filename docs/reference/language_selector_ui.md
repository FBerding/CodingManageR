# Render Header Language Selector Dropdown Widget

Builds a compact language dropdown for switching between supported UI
languages in the login card and application header bar. Uses an explicit
DOM `onchange` event (`selected_ui_lang`) instead of bidirectional
`selectInput()` binding so re-renders inside `renderUI()` never trigger
WebSocket echo loops on Shiny Server.

## Usage

``` r
language_selector_ui(id = "app_language", current_lang = "de", class = "")
```

## Arguments

- id:

  Logical identifier attribute (`data-lang-selector`) for the widget.
  Defaults to `"app_language"`.

- current_lang:

  Active language code (`"de"` or `"en"`). Defaults to `"de"`.

- class:

  Optional extra CSS classes appended to the wrapper element.

## Value

A Shiny `tags$div` UI element containing the styled language selector.

## Examples

``` r
if (FALSE) { # \dontrun{
language_selector_ui("app_language", current_lang = "en")
} # }
```
