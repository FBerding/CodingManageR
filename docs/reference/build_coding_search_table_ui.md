# Build Paginated Table UI for Admin Coding Search and Correction

Renders a responsive Bootstrap table displaying filtered coding records
(`user_ratings`) with rating ID, case text preview, scale label, coder,
rating type, current score on scale, timestamp, and an edit action
button triggering `trigger_edit_coding_rating`.

## Usage

``` r
build_coding_search_table_ui(ns, page_df, lang = "en")
```

## Arguments

- ns:

  A Shiny namespace function (`session$ns`) from the calling module
  server.

- page_df:

  A sliced `data.frame` (up to 25 rows) returned by
  [`search_case_codings_db()`](https://fberding.github.io/CodingManageR/reference/search_case_codings_db.md).

- lang:

  Character language code (`"de"` or `"en"`) passed to
  [`tr()`](https://fberding.github.io/CodingManageR/reference/tr.md).
  Defaults to `"en"`.

## Value

A Shiny UI tag containing either an empty-state alert or the responsive
HTML table.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- search_case_codings_db(con)
build_coding_search_table_ui(session$ns, df[seq_len(min(25, nrow(df))), ], lang = "en")
} # }
```
