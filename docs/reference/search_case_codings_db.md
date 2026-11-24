# Search and Filter Submitted Case Codings Across Scales

Queries `user_ratings` joined with `texts`, `scale_definitions`,
`user_tokens`, and `sources` to allow administrators to locate specific
case ratings by scale, rating type (`kodierung`, `gold_standard`,
`schulung`), case ID, text excerpt, or coder username.

## Usage

``` r
search_case_codings_db(
  con,
  scale_filter = "all",
  type_filter = "all",
  search_query = ""
)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object representing the active database connection.

- scale_filter:

  Character string specifying either `"all"` or a target `scale_id` (or
  scale name/short label). Defaults to `"all"`.

- type_filter:

  Character string specifying either `"all"` or one of `"kodierung"`,
  `"gold_standard"`, `"schulung"`. Defaults to `"all"`.

- search_query:

  Optional character string used to filter results across rating ID,
  text ID, case text content, coder username, and scale labels. Defaults
  to `""`.

## Value

A `data.frame` containing matching rating records ordered by
`rating_id DESC`, including resolved scale metadata
(`resolved_scale_id`, `scale_name`, `scale_short_label`,
`scale_max_value`) and case context.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- search_case_codings_db(con, scale_filter = "1",
  type_filter = "kodierung", search_query = "101")
} # }
```
