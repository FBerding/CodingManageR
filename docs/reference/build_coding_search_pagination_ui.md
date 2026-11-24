# Build Pagination Bar for Admin Coding Search Table

Generates localized pagination controls (first, previous, direct page
number input, next, last) for the 25-row coding search table in
[`adminServer()`](https://fberding.github.io/CodingManageR/reference/adminServer.md).

## Usage

``` r
build_coding_search_pagination_ui(
  ns,
  total_rows,
  cur_page,
  tot_pages,
  page_size = 25,
  lang = "en"
)
```

## Arguments

- ns:

  A Shiny namespace function (`session$ns`) from the calling module
  server.

- total_rows:

  Integer total count of filtered coding rows.

- cur_page:

  Integer current 1-based page number.

- tot_pages:

  Integer total number of pages.

- page_size:

  Integer number of records per page. Defaults to `25`.

- lang:

  Character language code (`"de"` or `"en"`) passed to
  [`tr()`](https://fberding.github.io/CodingManageR/reference/tr.md).
  Defaults to `"en"`.

## Value

A Shiny UI tag containing pagination controls, or `NULL` when
`total_rows <= 0`.

## Examples

``` r
if (FALSE) { # \dontrun{
build_coding_search_pagination_ui(session$ns, total_rows = 48, cur_page = 1,
  tot_pages = 2, lang = "en")
} # }
```
