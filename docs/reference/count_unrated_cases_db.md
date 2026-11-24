# Count Unrated Cases for a Specific Scale and User

Counts how many cases in the database have not yet been evaluated by the
current user or marked as gold standard for the given scale/category.

## Usage

``` r
count_unrated_cases_db(
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

  Character scalar: either `"alle"` (all permitted sources) or
  `"quelle"` (filtered by `source_id`). Defaults to `"alle"`.

- source_id:

  Optional integer filter used when `mode = "quelle"`.

- permitted_source_ids:

  Optional integer vector of sources the user is allowed to access.

## Value

An integer scalar representing the count of remaining unrated cases.

## Examples

``` r
if (FALSE) { # \dontrun{
count_unrated_cases_db(con, scale_identifier = 1L, user_id = 1L)
} # }
```
