# Validate an Uploaded CSV Data Frame Against Table Schema Rules

Checks required columns, data types, non-empty keys, and domain
constraints before allowing a CSV import into a target CodingManageR
table.

## Usage

``` r
validate_import_schema(target_table, df, lang = "en")
```

## Arguments

- target_table:

  Character scalar target database table name.

- df:

  Data frame to clean or validate.

- lang:

  Character scalar language code ("de" or "en").

## Value

A list containing validation status (valid), cleaned data frame, and
localized error messages.

## Examples

``` r
df <- data.frame(
  Kurzreferenz = "Ref1",
  Vollstaendiges_Zitat = "Full citation"
)
validate_import_schema("sources", df, lang = "en")
#> $is_valid
#> [1] TRUE
#> 
#> $missing_cols
#> character(0)
#> 
#> $error_msg
#> NULL
#> 
```
