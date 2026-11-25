# Admin Module Server

Implements the server-side administration workflows for managing users
and permissions, case texts, bibliographic sources, coding scales, team
reliability dashboards, targeted coding search and value edits, CSV data
import/export, and relational database integrity diagnostics.

## Usage

``` r
adminServer(
  id,
  db_con,
  current_user = NULL,
  current_user_id = NULL,
  scales_trigger = NULL,
  current_lang = reactiveVal("de"),
  current_role = NULL
)
```

## Arguments

- id:

  Character string specifying the Shiny module namespace identifier.

- db_con:

  Active `DBIConnection` or
  [`pool::Pool`](http://rstudio.github.io/pool/reference/Pool-class.md)
  database handle.

- current_user:

  Optional reactive expression or character scalar identifying the
  currently signed-in administrator's username.

- current_user_id:

  Optional reactive expression or integer scalar identifying the
  currently signed-in administrator's `user_id`.

- scales_trigger:

  Optional `reactiveVal` incremented whenever scale definitions are
  added or modified so other modules refresh dynamically.

- current_lang:

  Reactive expression or character scalar providing the active UI
  language code (`"de"` or `"en"`).

- current_role:

  Optional reactive expression or character scalar identifying the
  active user's role (`"admin"` or `"user"`). When provided (e.g., by
  [`coding_manager_server()`](https://fberding.github.io/CodingManageR/reference/coding_manager_server.md)),
  server-side admin handlers enforce that the role equals `"admin"`.
  Defaults to `NULL` for backward compatibility with standalone module
  invocations and tests.

## Value

The return value of
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).

## Examples

``` r
if (FALSE) { # \dontrun{
adminServer(
  id = "mod_admin",
  db_con = db_con,
  current_user = reactive("admin"),
  current_user_id = reactive(1L),
  scales_trigger = reactiveVal(0L)
)
} # }
```
