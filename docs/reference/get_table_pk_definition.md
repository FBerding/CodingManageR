# Get Primary Key Column Name for a Database Table

Looks up the primary key column name associated with a known
CodingManageR database table.

## Usage

``` r
get_table_pk_definition(table_name)
```

## Arguments

- table_name:

  Character scalar database table name.

## Value

A character scalar naming the primary key column, or NULL if undefined.

## Examples

``` r
get_table_pk_definition("sources")
#> [1] "source_id"
```
