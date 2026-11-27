# Build and Execute Consolidated Master Data Export Query

Queries and consolidates ratings, case text contents, scale definitions,
and scientific source references (including license metadata) according
to optional administrator filters.

## Usage

``` r
fetch_consolidated_master_export(
  con,
  scale_ids = NULL,
  source_ids = NULL,
  user_ids = NULL
)
```

## Arguments

- con:

  A `DBIConnection` or
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- scale_ids:

  Optional integer or character vector of scale IDs or names.

- source_ids:

  Optional integer vector of source IDs.

- user_ids:

  Optional integer vector of user IDs.

## Value

A `data.frame` containing consolidated research export columns.

## Examples

``` r
if (FALSE) { # \dontrun{
master_df <- fetch_consolidated_master_export(con)
} # }
```
