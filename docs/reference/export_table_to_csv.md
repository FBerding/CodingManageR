# Fetch a Database Table for CSV Export

Queries all rows of an allowlisted CodingManageR database table in a
standardized order suitable for CSV download.

## Usage

``` r
export_table_to_csv(con, table_name)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- table_name:

  Character scalar database table name.

## Value

A data frame containing the table contents.

## Examples

``` r
if (FALSE) { # \dontrun{
export_table_to_csv(con, "sources")
} # }
```
