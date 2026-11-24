# Draw a Random Unrated Case Text from the Database

Selects a single random unrated case text for a coder on a specified
scale, respecting source permissions and rating mode.

## Usage

``` r
draw_unrated_case_db(con, scale_identifier, user_id, mode = "alle",
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

A single-row data frame containing the drawn case text, or a 0-row data
frame if none remain.

## Examples

``` r
if (FALSE) { # \dontrun{
draw_unrated_case_db(con, scale_identifier = 1L, user_id = 1L)
} # }
```
