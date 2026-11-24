# Retrieve Detailed Metadata for a Single Rating Record

Queries full details for a single user_ratings row joined with its case
text, scale definition, and coder username.

## Usage

``` r
get_coding_detail_db(con, rating_id)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- rating_id:

  Integer scalar identifier of the rating record in user_ratings.

## Value

A single-row data frame with rating details, or a 0-row data frame if
not found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_coding_detail_db(con, rating_id = 1L)
} # }
```
