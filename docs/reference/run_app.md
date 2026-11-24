# Launch the CodingManageR Shiny Application

Constructs and returns a shiny.appobj configured for local SQLite
storage or pooled MySQL/MariaDB server deployments.

## Usage

``` r
run_app(sqlite_path = NULL, config = NULL, empty = TRUE, options = list(), ...)
```

## Arguments

- sqlite_path:

  Optional character path to a local SQLite database file.

- config:

  Optional named list of database connection parameters.

- empty:

  Logical; if TRUE seeds minimal default accounts only, if FALSE seeds
  the full demonstration dataset.

- options:

  Optional named list of options forwarded to shiny::shinyApp().

- ...:

  Additional arguments forwarded to underlying functions or sprintf
  formatting.

## Value

A shiny.appobj object.

## Examples

``` r
if (FALSE) { # \dontrun{
app <- run_app(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
app
} # }
```
