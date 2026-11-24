# Build Modal Dialog for Editing a Case Coding Rating Value

Constructs a localized Shiny `modalDialog()` presenting read-only
context for an existing case coding (rating ID, case text, scale
definition, coder, rating type, and timestamp) alongside radio choices
`0..max_value` enriched with stage descriptions (`def_n_0..def_n_5`) so
an administrator can adjust only the numeric rating score.

## Usage

``` r
build_edit_coding_value_modal(ns, rating_row, lang = "en")
```

## Arguments

- ns:

  A Shiny namespace function (`session$ns`) from the calling module
  server.

- rating_row:

  A 1-row `data.frame` returned by
  [`get_coding_detail_db()`](https://fberding.github.io/CodingManageR/reference/get_coding_detail_db.md).

- lang:

  Character language code (`"de"` or `"en"`) passed to
  [`tr()`](https://fberding.github.io/CodingManageR/reference/tr.md).
  Defaults to `"en"`.

## Value

A Shiny `modalDialog` object ready to be passed to `showModal()`.

## Examples

``` r
if (FALSE) { # \dontrun{
detail <- get_coding_detail_db(con, 201)
showModal(build_edit_coding_value_modal(session$ns, detail, lang = "en"))
} # }
```
