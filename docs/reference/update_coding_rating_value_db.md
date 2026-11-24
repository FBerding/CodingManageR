# Update Only the Numeric Scale Rating Value for a Stored Coding

Validates that the specified `rating_id` exists and that `new_value`
falls strictly within `0..max_value` of the associated scale definition,
then updates `user_ratings.bewertung` in place without altering coder,
type, case, or source metadata.

## Usage

``` r
update_coding_rating_value_db(con, rating_id, new_value)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- rating_id:

  Integer primary key of the record in `user_ratings`.

- new_value:

  Numeric or integer scalar representing the corrected score on the
  scale.

## Value

A named list with `success` (logical), `old_value` (integer),
`new_value` (integer), `text_id` (integer), and `message` (character).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- update_coding_rating_value_db(con, rating_id = 201, new_value = 2)
} # }
```
