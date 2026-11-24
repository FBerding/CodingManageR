# Generate Empty CSV Template for Table Upload

Generates a header-only semicolon-delimited CSV template to assist
administrators in preparing structured master-data uploads.

## Usage

``` r
export_table_template_csv(table_name)
```

## Arguments

- table_name:

  A character string specifying the target table.

## Value

A character string containing the semicolon-delimited header row.

## Examples

``` r
export_table_template_csv("sources")
#> [1] "Kurzreferenz;Vollstaendiges_Zitat;Lizenz;Lizenz_Details;URL_DOI\r\n"
```
