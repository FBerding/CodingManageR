# Open a Direct Database Connection (MySQL or SQLite)

Establishes a direct DBI connection using MySQL/MariaDB configuration if
available, with automatic fallback to local SQLite storage.

## Usage

``` r
get_db_connection(config = NULL, fallback_to_sqlite = TRUE)
```

## Arguments

- config:

  Optional named list of database connection parameters.

- fallback_to_sqlite:

  Logical; if TRUE falls back to SQLite when MySQL/MariaDB is
  unavailable.

## Value

An active DBI connection object.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_db_connection(config = list(sqlite_path = ":memory:"))
close_db_connection(con)
} # }
```
