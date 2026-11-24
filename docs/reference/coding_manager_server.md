# Root Shiny Server Function for CodingManageR

Manages database connectivity, user authentication and account lockout,
bilingual localization, reactive scale catalog updates, and child module
orchestration.

## Usage

``` r
coding_manager_server(input, output, session, db_con = NULL, sqlite_path = NULL,
  config = NULL, seed_empty = NULL)
```

## Arguments

- input:

  Standard Shiny server input object.

- output:

  Standard Shiny server output object.

- session:

  Standard Shiny server session object.

- db_con:

  Active DBI database connection or pool::dbPool object.

- sqlite_path:

  Optional character path to a local SQLite database file.

- config:

  Optional named list of database connection parameters.

- seed_empty:

  Optional logical controlling whether fresh databases receive only
  minimal default accounts.

## Value

Invisibly returns NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::shinyApp(ui = coding_manager_ui(), server = coding_manager_server)
} # }
```
