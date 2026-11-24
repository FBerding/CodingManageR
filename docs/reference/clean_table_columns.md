# Sanitize and Clean Data Frame Column Names

Standardizes imported or uploaded column names by stripping UTF-8
byte-order marks (BOM) and trimming leading and trailing whitespace.

## Usage

``` r
clean_table_columns(df)
```

## Arguments

- df:

  A `data.frame` whose column names should be cleaned.

## Value

A `data.frame` with trimmed, standardized column names.

## Examples

``` r
clean_table_columns(data.frame(source_id = 1L, Kurzreferenz = "RefA"))
#>   source_id Kurzreferenz
#> 1         1         RefA
```
