# Get Labeled Language Choices for Shiny Inputs

Returns a named character vector suitable for
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
or
[`language_selector_ui()`](https://fberding.github.io/CodingManageR/reference/language_selector_ui.md).

## Usage

``` r
get_language_choices()
```

## Value

A named character vector mapping display labels to language codes.

## Examples

``` r
get_language_choices()
#> 🌐 DE 🌐 EN 
#>  "de"  "en" 
```
