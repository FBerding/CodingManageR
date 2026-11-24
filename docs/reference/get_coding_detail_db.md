# Retrieve Detailed Context for a Single Rating Record

Fetches a single rating from `user_ratings` together with its full case
text and complete `scale_definitions` stage descriptions (`def_n_0`
through `def_n_5`) to populate the administrator value-correction
dialog.

## Usage

``` r
get_coding_detail_db(con, rating_id)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- rating_id:

  Integer primary key identifying the target record in `user_ratings`.

## Value

A 1-row `data.frame` containing rating, case, coder, and scale stage
fields, or an empty `data.frame` if the record cannot be found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_coding_detail_db(con, rating_id = 201L)
} # }
```
