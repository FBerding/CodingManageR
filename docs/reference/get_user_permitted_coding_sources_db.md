# Get Sources Permitted for Live Field Coding by a User

Returns the subset of scientific sources that a user is authorized to
code in the Live Coding module (excluding training-only sources unless
permitted).

## Usage

``` r
get_user_permitted_coding_sources_db(con, user_id, is_admin = FALSE)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- user_id:

  Integer scalar user identifier.

- is_admin:

  Logical indicating whether the user holds administrator privileges.

## Value

A data frame of permitted coding sources.

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_permitted_coding_sources_db(con, user_id = 1L, is_admin = TRUE)
} # }
```
