# Build Pagination Controls for Coding Search Results

Constructs Bootstrap pagination controls and result summary badges for
the admin coding search and correction view.

## Usage

``` r
build_coding_search_pagination_ui(ns, total_rows, cur_page, tot_pages,
  page_size = 25, lang = "en")
```

## Arguments

- ns:

  Shiny namespace function created via shiny::NS().

- total_rows:

  Integer scalar total number of matching rows.

- cur_page:

  Integer scalar current 1-based page index.

- tot_pages:

  Integer scalar total number of pages.

- page_size:

  Integer scalar number of rows displayed per page.

- lang:

  Character scalar language code ("de" or "en").

## Value

A Shiny UI tag object.

## Examples

``` r
if (FALSE) { # \dontrun{
build_coding_search_pagination_ui(shiny::NS("mod_admin"), 100, 1, 4)
} # }
```
