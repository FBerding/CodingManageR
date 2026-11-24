# Get All Sources Permitted for a User

Queries all scientific sources accessible to a specific user according
to admin status and user_source_permissions.

## Usage

``` r
get_user_permitted_sources_db(con, user_id, is_admin = FALSE)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- user_id:

  Integer scalar user identifier.

- is_admin:

  Logical indicating whether the user holds administrator privileges.

## Value

A data frame of permitted sources.

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_permitted_sources_db(con, user_id = 1L, is_admin = TRUE)
} # }
```
