# Build an Empty Schema Template Data Frame for CSV Export

Returns a zero-row data frame with the exact expected column schema for
a given CodingManageR database table to guide CSV imports.

## Usage

``` r
export_table_template_csv(table_name)
```

## Arguments

- table_name:

  Character scalar database table name.

## Value

A zero-row data frame with the target table columns.

## Examples

``` r
export_table_template_csv("sources")
#> [1] "Kurzreferenz;Vollstaendiges_Zitat;Lizenz;Lizenz_Details;URL_DOI\r\n"
```
