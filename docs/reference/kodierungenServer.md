# Shiny Server Module for Live Field Coding

Manages scale and source selection, random unrated case drawing, anchor
example modals, and transactional saving of field ratings.

## Usage

``` r
kodierungenServer(id, db_con, user_id_reactive = NULL, username_reactive,
  kategorie_spalten, kategorien_liste, trigger_parent, scales_trigger = NULL,
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

- kategorie_spalten:

  Reactive expression returning a named character vector mapping scale
  short labels to scale IDs.

- kategorien_liste:

  Reactive expression returning a named character vector of display
  labels to scale IDs.

- trigger_parent:

  ReactiveVal counter incremented to notify parent modules of new
  ratings.

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
kodierungenServer("mod_codings", db_con = con,
  username_reactive = shiny::reactive("user1"),
  kategorie_spalten = shiny::reactive(character(0)),
  kategorien_liste = shiny::reactive(character(0)),
  trigger_parent = shiny::reactiveVal(0L))
} # }
```
