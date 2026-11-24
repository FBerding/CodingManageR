# Dashboard Module Server

Implements the server-side logic for the coder dashboard, including
personal KPI counters, multi-metric training reliability visualizations,
session history tables, open-case counts, and searchable scale
definition cards.

## Usage

``` r
dashboardServer(
  id,
  db_con,
  user_id_reactive = NULL,
  username_reactive,
  trigger_reactive,
  kategorie_spalten,
  scales_trigger = NULL,
  current_lang = reactiveVal("de")
)
```

## Arguments

- id:

  Character string specifying the Shiny module namespace identifier.

- db_con:

  Active `DBIConnection` or
  [`pool::Pool`](http://rstudio.github.io/pool/reference/Pool-class.md)
  database handle.

- user_id_reactive:

  Optional reactive expression returning the active user's integer ID.

- username_reactive:

  Reactive expression returning the active username.

- trigger_reactive:

  Reactive trigger that invalidates dashboard outputs when ratings or
  training sessions are updated.

- kategorie_spalten:

  Reactive expression returning configured scale identifiers or column
  names.

- scales_trigger:

  Optional `reactiveVal` triggered when scale definitions change in the
  admin panel.

- current_lang:

  Reactive expression or character scalar providing the active UI
  language code (`"de"` or `"en"`).

## Value

The return value of
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).

## Examples

``` r
if (FALSE) { # \dontrun{
dashboardServer(
  id = "mod_dashboard",
  db_con = db_con,
  user_id_reactive = reactive(1L),
  username_reactive = reactive("admin"),
  trigger_reactive = reactiveVal(0L),
  kategorie_spalten = reactive(character(0))
)
} # }
```
