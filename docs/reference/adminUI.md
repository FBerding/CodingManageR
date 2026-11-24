# Admin Module UI

Creates the Shiny UI for the administration panel covering user
management, case texts, source definitions, scale definitions,
reliability monitoring, coding search/correction, data import/export,
and database diagnostics.

## Usage

``` r
adminUI(id, lang = "en", selected_subtab = "tab_users")
```

## Arguments

- id:

  Character string specifying the Shiny module namespace identifier.

- lang:

  Character string specifying the initial UI language code (`"en"` or
  `"de"`).

- selected_subtab:

  Character string selecting the initially active admin subtab (one of
  `"tab_users"`, `"tab_texts"`, `"tab_sources"`, `"tab_scales"`,
  `"tab_reliability"`, `"tab_coding_search"`, `"tab_data"`, or
  `"tab_diagnose"`).

## Value

A `shiny.tag` (`fluidPage`) representing the admin interface.

## Examples

``` r
if (interactive()) {
  ui <- adminUI("mod_admin", lang = "en")
}
```
