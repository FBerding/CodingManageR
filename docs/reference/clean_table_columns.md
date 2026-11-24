# Clean and Normalize Imported Table Column Names

Removes UTF-8 byte-order marks (BOM), trims whitespace, and normalizes
column names of an imported data frame.

## Usage

``` r
clean_table_columns(df)
```

## Arguments

- df:

  Data frame to clean or validate.

## Value

A data frame with cleaned column names.

## Examples

``` r
clean_table_columns(data.frame(source_id = 1L, Kurzreferenz = "RefA"))
#>   source_id Kurzreferenz
#> 1         1         RefA
```
