# Training Module Server

Implements the server-side workflow for gold-standard coding training
sessions, including stratified case pool generation, sequential rating
collection, reliability metric computation via
[`berechne_aifeducation_metriken()`](https://fberding.github.io/CodingManageR/reference/berechne_aifeducation_metriken.md),
session persistence, CSV export, and reference example inspection.

## Usage

``` r
schulungServer(
  id,
  db_con,
  user_id_reactive = NULL,
  username_reactive,
  kategorie_spalten,
  kategorien_liste,
  trigger_parent,
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

- kategorie_spalten:

  Reactive expression returning a character vector of configured scale
  identifiers or column names.

- kategorien_liste:

  Reactive expression returning a named character vector of available
  coding scales (`scale_id` values with display names).

- trigger_parent:

  A `reactiveVal` incremented when a training session completes so
  dependent modules refresh automatically.

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
schulungServer(
  id = "mod_training",
  db_con = db_con,
  user_id_reactive = reactive(1L),
  username_reactive = reactive("admin"),
  kategorie_spalten = reactive(character(0)),
  kategorien_liste = reactive(c("Scale 1" = "1")),
  trigger_parent = reactiveVal(0L)
)
} # }
```
