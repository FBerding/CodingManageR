# Count Unrated Cases for a Coder and Scale

Queries the database to count how many eligible case texts have not yet
been coded by a specific user on a given scale and optional source
filter.

## Usage

``` r
count_unrated_cases_db(con, scale_identifier, user_id, mode = "alle",
  source_id = NULL, permitted_source_ids = NULL)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- scale_identifier:

  Integer scale_id or character short_label identifying the coding
  scale.

- user_id:

  Integer scalar user identifier.

- mode:

  Character scalar rating filter mode (e.g., "alle").

- source_id:

  Optional integer scalar source identifier filter.

- permitted_source_ids:

  Optional integer vector of source IDs permitted for the coder.

## Value

An integer scalar giving the number of remaining unrated cases.

## Examples

``` r
if (FALSE) { # \dontrun{
count_unrated_cases_db(con, scale_identifier = 1L, user_id = 1L)
} # }
```
