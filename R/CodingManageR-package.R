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
