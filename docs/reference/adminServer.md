# Shiny Server Module for the Admin Console

Server logic for the multi-tab CodingManageR administration console,
covering user and permission management, case text and source
management, scale definitions, coder reliability monitoring, coding
search/correction, CSV import/export, and database diagnostics.

## Usage

``` r
adminServer(id, db_con, current_user = NULL, current_user_id = NULL,
  scales_trigger = NULL, current_lang = reactiveVal("de"))
```

## Arguments

- id:

  Character scalar module namespace identifier.

- db_con:

  Active DBI database connection or pool::dbPool object.

- current_user:

  Optional reactive expression returning the signed-in username.

- current_user_id:

  Optional reactive expression returning the signed-in numeric user ID.

- scales_trigger:

  Optional reactiveVal counter invalidated when scale definitions
  change.

- current_lang:

  Reactive expression or reactiveVal returning the active language code
  ("de" or "en").

## Value

Invisibly returns the Shiny module server result.

## Examples

``` r
if (FALSE) { # \dontrun{
adminServer("mod_admin", db_con = con)
} # }
```
