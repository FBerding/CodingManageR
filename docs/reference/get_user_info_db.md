# Retrieve User Account Metadata from the Database

Fetches user account information (ID, username, role permissions, and
active status) by username or user ID.

## Usage

``` r
get_user_info_db(con, username, user_id = NULL)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- username:

  Character scalar username to look up.

- user_id:

  Integer scalar user identifier.

## Value

A single-row data frame with user metadata, or a 0-row data frame if not
found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_info_db(con, username = "admin")
} # }
```
