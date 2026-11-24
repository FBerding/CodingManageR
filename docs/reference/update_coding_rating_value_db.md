# Update an Existing Coding Rating Value in the Database

Updates the ordinal rating value of a single record in user_ratings
within a validated database statement.

## Usage

``` r
update_coding_rating_value_db(con, rating_id, new_value)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- rating_id:

  Integer scalar identifier of the rating record in user_ratings.

- new_value:

  Integer scalar new ordinal rating value.

## Value

An integer count of affected rows (1 on success).

## Examples

``` r
if (FALSE) { # \dontrun{
update_coding_rating_value_db(con, rating_id = 1L, new_value = 2L)
} # }
```
