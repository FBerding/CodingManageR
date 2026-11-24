# Generate a Secure Random Alphanumeric Token

Generates a random alphanumeric string suitable for user login tokens or
temporary access keys.

## Usage

``` r
generate_security_token(length = 12)
```

## Arguments

- length:

  Integer specifying the number of characters in the generated token.
  Defaults to `12`. Must be a positive integer.

## Value

A character scalar of length `length` containing random ASCII letters
and digits.

## Examples

``` r
generate_security_token(16)
#> [1] "SwlKLUUEdLieexom"
```
