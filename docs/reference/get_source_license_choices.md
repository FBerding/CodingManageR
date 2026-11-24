# Retrieve Localized Source License Options

Returns standard license options (such as Creative Commons, Open Data
Commons, institutional, and custom licenses) for source metadata forms.

## Usage

``` r
get_source_license_choices(lang = "en")
```

## Arguments

- lang:

  Character language code (`"de"` or `"en"`). Defaults to `"en"`.

## Value

A named character vector mapping localized license labels to stored
license identifiers.

## Examples

``` r
get_source_license_choices(lang = "en")
#>                  CC-BY 4.0 (Attribution) 
#>                              "CC-BY 4.0" 
#>                CC-BY-SA 4.0 (ShareAlike) 
#>                           "CC-BY-SA 4.0" 
#>             CC-BY-NC 4.0 (NonCommercial) 
#>                           "CC-BY-NC 4.0" 
#>                  CC0 1.0 (Public Domain) 
#>                                "CC0 1.0" 
#>               Open Data Commons (ODC-By) 
#>                                 "ODC-By" 
#>          Copyright / All Rights Reserved 
#>                              "Copyright" 
#> Internal Data Collection (Institutional) 
#>                       "Instituts-Intern" 
#>            Custom license designation... 
#>                             "__custom__" 
```
