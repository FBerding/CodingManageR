# Generate a Random Alphanumeric Access Token

Generates a random character access token of the specified length for
authenticating CodingManageR users.

## Usage

``` r
generate_security_token(length = 12)
```

## Arguments

- length:

  Integer scalar length of the generated token.

## Value

A single character string of length equal to the requested token length.

## Examples

``` r
generate_security_token(12)
#> [1] "SwlKLUUEdLie"
```
