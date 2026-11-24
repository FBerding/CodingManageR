# Retrieve Table Primary Key Column Name

Returns the designated primary key column name for each core relational
table in the CodingManageR database schema.

## Usage

``` r
get_table_pk_definition(table_name)
```

## Arguments

- table_name:

  A character string naming the database table.

## Value

A character string with the primary key column name, or `NULL` if not
recognized.

## Examples

``` r
get_table_pk_definition("sources")
#> [1] "source_id"
```
