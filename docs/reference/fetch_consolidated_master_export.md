# Fetch Consolidated Master Coding Export Dataset

Joins user ratings, texts, sources, scale definitions, and coder
metadata into a consolidated flat data frame with optional scale,
source, and user filters.

## Usage

``` r
fetch_consolidated_master_export(con, scale_ids = NULL, source_ids = NULL,
  user_ids = NULL)
```

## Arguments

- con:

  Active DBI database connection or pool::dbPool object.

- scale_ids:

  Optional integer vector of scale IDs to filter the export.

- source_ids:

  Optional integer vector of source IDs to filter the export.

- user_ids:

  Optional integer vector of user IDs to filter the export.

## Value

A consolidated data frame ready for CSV export or statistical analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_consolidated_master_export(con)
} # }
```
