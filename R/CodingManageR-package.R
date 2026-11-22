#' CodingManageR: Multi-User Qualitative Coding Portal and Training Manager
#'
#' Provides a multi-user R Shiny web application and coder training portal for
#' qualitative text coding across ordinal scales. Supports local SQLite
#' databases for single-user desktop sessions as well as pooled MySQL/MariaDB
#' connections for Open Source Shiny Server deployments.
#'
#' Key capabilities include:
#' - Sixteen inter-rater reliability and agreement metrics computed via `aifeducation`
#' - Stratified gold-standard training pools across all ordinal scale levels
#' - Live field coding with per-user source and scale permissions
#' - Consolidated master CSV exports and schema-validated CSV imports
#' - On-demand interactive Getting Started spotlight tutorial with role-aware tracks
#' - Bilingual (German/English) localization with automatic browser detection
#'
#' @import DBI
#' @import ggplot2
#' @import shiny
#' @importFrom stats setNames
#' @importFrom utils globalVariables head packageName read.csv write.csv2 write.table
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NSE notes for ggplot2::aes() column symbols
utils::globalVariables(c(
  "username",
  "plot_val",
  "tier",
  "val",
  "sitzung_nr",
  "value",
  "metric_name",
  "val_labels"
))
