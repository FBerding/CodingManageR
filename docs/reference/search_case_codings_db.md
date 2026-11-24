# Search Case Coding Ratings in the Database

Queries user_ratings joined with case texts, scales, and coder accounts
using optional scale, rating type, and full-text search filters.

## Usage

``` r
search_case_codings_db(con, scale_filter = "all", type_filter = "all",
  search_query = "")
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- scale_filter:

  Character scalar scale filter ("all" or a scale ID).

- type_filter:

  Character scalar rating type filter ("all", "kodierung", or
  "schulung").

- search_query:

  Character scalar full-text search query.

## Value

A data frame of matching rating records.

## Examples

``` r
if (FALSE) { # \dontrun{
search_case_codings_db(con, scale_filter = "all", type_filter = "all")
} # }
```
