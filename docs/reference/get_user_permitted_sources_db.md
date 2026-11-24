# Retrieve Authorized Text Sources for a User

Fetches all source IDs that the specified user is authorized to read for
case texts. Administrators receive all available sources
unconditionally.

## Usage

``` r
get_user_permitted_sources_db(con, user_id, is_admin = FALSE)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- user_id:

  Integer representing the user's primary key in `user_tokens`.

- is_admin:

  Logical; if `TRUE`, returns all source IDs unconditionally. Defaults
  to `FALSE`.

## Value

An integer vector of permitted source IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_permitted_sources_db(con, user_id = 1L, is_admin = TRUE)
} # }
```
