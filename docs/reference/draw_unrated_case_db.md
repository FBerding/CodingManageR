# Draw an Unrated Case for Active Coding

Fetches a random case from `texts` that has not yet been coded by the
user or locked as a gold standard on the given scale.

## Usage

``` r
draw_unrated_case_db(
  con,
  scale_identifier,
  user_id,
  mode = "alle",
  source_id = NULL,
  permitted_source_ids = NULL
)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- scale_identifier:

  Character or integer specifying the `scale_id` or scale name.

- user_id:

  Integer representing the active user.

- mode:

  Character scalar: either `"alle"` or `"quelle"`. Defaults to `"alle"`.

- source_id:

  Optional integer filter used when `mode = "quelle"`.

- permitted_source_ids:

  Optional integer vector of sources the user is allowed to access.

## Value

A 1-row `data.frame` containing case details (`text_id`, `text`,
`source_id`, `befund_text`), or an empty `data.frame` if no cases
remain.

## Examples

``` r
if (FALSE) { # \dontrun{
draw_unrated_case_db(con, scale_identifier = 1L, user_id = 1L)
} # }
```
