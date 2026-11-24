# Build the Root Shiny UI for CodingManageR

Constructs the top-level Bootstrap 5 / `bslib` page shell for
CodingManageR, including browser language auto-detection, interactive
spotlight tutorial assets, and the reactive `page_content` outlet that
switches between the sign-in screen and the authenticated multi-tab
workspace.

## Usage

``` r
coding_manager_ui(lang = "de")
```

## Arguments

- lang:

  Default initial language code (`"de"` or `"en"`). Defaults to `"de"`.

## Value

A Shiny UI tag list suitable for
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- coding_manager_ui(lang = "en")
} # }
```
