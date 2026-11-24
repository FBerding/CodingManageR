# Validate Table Schema for Data Upload

Validates whether an uploaded data frame contains all mandatory columns
required for insertion into the specified database table.

## Usage

``` r
validate_import_schema(target_table, df, lang = "en")
```

## Arguments

- target_table:

  A character string naming the target database table.

- df:

  A `data.frame` containing the uploaded records.

- lang:

  Character language code (`"de"` or `"en"`) used for localized
  validation error messages. Defaults to `"en"`.

## Value

A named list with elements:

- `is_valid`: Logical indicating whether schema validation passed.

- `missing_cols`: Character vector of required columns that are missing.

- `error_msg`: Character message describing the validation failure, or
  `NULL`.

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
