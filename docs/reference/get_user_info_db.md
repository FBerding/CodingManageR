# Retrieve User Metadata and Identifier

Resolves user identification and role permissions given either a
username or an existing `user_id`. Handles Shiny reactive expressions
and falls back to database lookup.

## Usage

``` r
get_user_info_db(con, username, user_id = NULL)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- username:

  Character scalar or reactive expression containing the username.

- user_id:

  Optional integer scalar or reactive expression containing the user
  identifier.

## Value

A named list containing `id` (integer `user_id`), `name` (character
username), and `permissions` (character role: `"admin"` or `"user"`).

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_info_db(con, username = "admin")
} # }
```
