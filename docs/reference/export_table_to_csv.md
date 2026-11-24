# Generate CSV Content for a Database Table

Queries all rows from the specified table in the active database and
serializes them into a UTF-8 CSV string with semicolon delimiters.

## Usage

``` r
export_table_to_csv(con, table_name)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- table_name:

  A character string specifying the table name.

## Value

A character string containing the serialized CSV content including
headers.

## Examples

``` r
if (FALSE) { # \dontrun{
csv_str <- export_table_to_csv(con, "sources")
} # }
```
