# Shiny Server Module for the Progress Dashboard

Computes and renders personal coding KPIs, scale-level progress tables,
and longitudinal training reliability visualizations.

## Usage

``` r
dashboardServer(id, db_con, user_id_reactive = NULL, username_reactive,
  trigger_reactive, kategorie_spalten, scales_trigger = NULL,
  current_lang = reactiveVal("de"))
```

## Arguments

- id:

  Character scalar module namespace identifier.

- db_con:

  Active DBI database connection or pool::dbPool object.

- user_id_reactive:

  Optional reactive expression returning the current user ID.

- username_reactive:

  Reactive expression returning the current username.

- trigger_reactive:

  Reactive expression invalidated when ratings or data change.

- kategorie_spalten:

  Reactive expression returning a named character vector mapping scale
  short labels to scale IDs.

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
dashboardServer("mod_dashboard", db_con = con,
  username_reactive = shiny::reactive("user1"),
  trigger_reactive = shiny::reactive(1L),
  kategorie_spalten = shiny::reactive(character(0)))
} # }
```
