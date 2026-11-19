#' @title Reliability Metrics, Stratified Sampling, and Evaluation Utilities
#' @description Comprehensive statistical functions for calculating inter-rater reliability,
#' formatting metrics according to academic benchmarks (Landis & Koch), and drawing stratified
#' training pools across ordinal coding scales.
#' @author AI Studio Coding Engine
#' @keywords reliability metrics stratified-sampling statistics
NULL

# DBI is imported via NAMESPACE

#' Classify Cohen's Kappa Magnitude
#'
#' @description Classifies a Cohen's Kappa coefficient according to the benchmark criteria
#' by Landis & Koch (1977).
#'
#' @param kappa A numeric value representing Cohen's Kappa or agreement coefficient.
#' @param lang Character language code (\code{"de"} or \code{"en"}). Default is \code{"en"}.
#'
#' @return A named list containing \code{label} (character description) and \code{css_class}
#' (Tailwind/Bootstrap style class).
#'
#' @examples
#' \dontrun{
#' rating <- classify_reliability_magnitude(0.74)
#' }
#' @export
classify_reliability_magnitude <- function(kappa, lang = "en") {
  if (is.null(kappa) || is.na(kappa)) {
    return(list(label = tr("common.no_data", lang), css_class = "text-muted bg-light"))
  }
  k <- as.numeric(kappa)
  if (k > 0.80) {
    list(label = tr("metrics.bench_almost_perfect", lang), css_class = "text-success bg-light border-success")
  } else if (k > 0.60) {
    list(label = tr("metrics.bench_substantial", lang), css_class = "text-info bg-light border-info")
  } else if (k > 0.40) {
    list(label = tr("metrics.bench_moderate", lang), css_class = "text-warning bg-light border-warning")
  } else if (k > 0.20) {
    list(label = tr("metrics.bench_fair", lang), css_class = "text-warning bg-light border-warning")
  } else if (k >= 0.0) {
    list(label = tr("metrics.bench_slight", lang), css_class = "text-danger bg-light border-danger")
  } else {
    list(label = tr("metrics.bench_poor", lang), css_class = "text-danger bg-light border-danger")
  }
}

#' Retrieve Localized Optgroup Choices for Reliability Metric Selectors
#'
#' @param lang Character language code (\code{"de"} or \code{"en"}).
#' @param include_all_core Logical; whether to include the combined \code{all_core} group.
#'
#' @return A named list suitable for Shiny \code{selectInput(..., choices = ...)}.
#' @export
get_reliability_metric_choices <- function(lang = "en", include_all_core = TRUE) {
  groups <- list()
  if (isTRUE(include_all_core)) {
    grp_all <- list("all_core")
    names(grp_all) <- tr("metrics.all_core_label", lang)
    groups[[tr("metrics.group_all_overview", lang)]] <- grp_all
  }

  nom <- list("cohens_kappa", "alpha_nominal", "fleiss_kappa", "gwet_ac1")
  names(nom) <- c(
    tr("metrics.cohens_kappa.label", lang),
    tr("metrics.alpha_nominal.label", lang),
    tr("metrics.fleiss_kappa.label", lang),
    tr("metrics.gwet_ac1.label", lang)
  )
  groups[[tr("metrics.group_nominal", lang)]] <- nom

  ord <- list("alpha_ordinal", "kappa_linear", "kappa_squared", "gwet_ac2_quadratic", "kendall_w")
  names(ord) <- c(
    tr("metrics.alpha_ordinal.label", lang),
    tr("metrics.kappa_linear.label", lang),
    tr("metrics.kappa_squared.label", lang),
    tr("metrics.gwet_ac2_quadratic.label", lang),
    tr("metrics.kendall_w.label", lang)
  )
  groups[[tr("metrics.group_ordinal", lang)]] <- ord

  iota_g <- list("avg_iota2", "iota_index")
  names(iota_g) <- c(
    tr("metrics.avg_iota2.label", lang),
    tr("metrics.iota_index.label", lang)
  )
  groups[[tr("metrics.group_iota", lang)]] <- iota_g

  cls_g <- list("accuracy", "balanced_accuracy", "macro_f1", "macro_precision", "macro_recall")
  names(cls_g) <- c(
    tr("metrics.accuracy.label", lang),
    tr("metrics.balanced_accuracy.label", lang),
    tr("metrics.macro_f1.label", lang),
    tr("metrics.macro_precision.label", lang),
    tr("metrics.macro_recall.label", lang)
  )
  groups[[tr("metrics.group_classification", lang)]] <- cls_g

  groups
}

#' Retrieve Localized Metadata for a Single Reliability Metric Key
#'
#' @param metric_key Character key identifying the metric.
#' @param lang Character language code (\code{"de"} or \code{"en"}).
#'
#' @return A list with \code{label}, \code{short_label}, \code{desc}, \code{is_pct}, and \code{color}.
#' @export
get_reliability_metric_meta <- function(metric_key, lang = "en") {
  key <- as.character(metric_key)
  if (key == "all_core") {
    return(list(
      label = tr("metrics.all_core_label", lang),
      short_label = tr("metrics.all_core_short", lang),
      desc = tr("metrics.all_core_desc", lang),
      is_pct = FALSE,
      color = "#0284c7"
    ))
  }

  pct_keys <- c("accuracy", "balanced_accuracy", "macro_f1", "macro_precision", "macro_recall")
  color_map <- list(
    cohens_kappa = "#10b981",
    alpha_nominal = "#f59e0b",
    fleiss_kappa = "#059669",
    gwet_ac1 = "#8b5cf6",
    alpha_ordinal = "#d97706",
    kappa_linear = "#0d9488",
    kappa_squared = "#047857",
    gwet_ac2_quadratic = "#7c3aed",
    kendall_w = "#14b8a6",
    avg_iota2 = "#6366f1",
    iota_index = "#4f46e5",
    accuracy = "#0284c7",
    balanced_accuracy = "#0369a1",
    macro_f1 = "#2563eb",
    macro_precision = "#1d4ed8",
    macro_recall = "#3b82f6"
  )

  known_keys <- names(color_map)
  if (!(key %in% known_keys)) {
    key <- "cohens_kappa"
  }

  list(
    label = tr(paste0("metrics.", key, ".label"), lang),
    short_label = tr(paste0("metrics.", key, ".short"), lang),
    desc = tr(paste0("metrics.", key, ".desc"), lang),
    is_pct = key %in% pct_keys,
    color = color_map[[key]]
  )
}

#' Retrieve Localized Source License Options
#'
#' @param lang Character language code (\code{"de"} or \code{"en"}).
#'
#' @return A named character vector mapping localized license labels to stored values.
#' @export
get_source_license_choices <- function(lang = "en") {
  vals <- c(
    "CC-BY 4.0",
    "CC-BY-SA 4.0",
    "CC-BY-NC 4.0",
    "CC0 1.0",
    "ODC-By",
    "Copyright",
    "Instituts-Intern",
    "__custom__"
  )
  names(vals) <- c(
    tr("admin.sources.lic_cc_by", lang),
    tr("admin.sources.lic_cc_by_sa", lang),
    tr("admin.sources.lic_cc_by_nc", lang),
    tr("admin.sources.lic_cc0", lang),
    tr("admin.sources.lic_odc", lang),
    tr("admin.sources.lic_copyright", lang),
    tr("admin.sources.lic_internal", lang),
    tr("admin.sources.lic_custom", lang)
  )
  vals
}

#' Format Numeric Metric or Percentage for Display
#'
#' @description Formats numerical metrics with standard decimal digits or as formatted percentages.
#'
#' @param val A numeric or character value to format.
#' @param is_percentage Logical; if TRUE, formats with 1 decimal place and a percent sign.
#' @param digits Integer indicating number of decimal places for non-percentage values. Default 3.
#'
#' @return A character string representation of the formatted number, or \code{"—"} if NA.
#'
#' @export
format_metric_display <- function(val, is_percentage = FALSE, digits = 3) {
  if (is.null(val) || is.na(val) || val == "" || val == "NA") return("—")
  num <- suppressWarnings(as.numeric(val))
  if (is.na(num)) return("—")

  if (isTRUE(is_percentage)) {
    paste0(formatC(num, digits = 1, format = "f"), " %")
  } else {
    formatC(num, digits = digits, format = "f")
  }
}

#' Generate Stratified Training Pool Guaranteeing Full Scale Range
#'
#' @description Draws a balanced, stratified sample of training cases from \code{texts}
#' and \code{user_ratings} (prioritizing gold standards and high-consensus codings),
#' guaranteeing that all scale levels from 0 to \code{max_value} are adequately represented.
#'
#' @param con A DBIConnection object.
#' @param scale_identifier Character or integer identifying the scale.
#' @param target_count Total number of training items to draw (minimum \code{max_value + 1}).
#' @param max_scale_value Integer indicating the maximum allowed score on this scale.
#'
#' @return A \code{data.frame} of selected cases with columns \code{text_id}, \code{text},
#' \code{source_id}, and \code{true_val} (the benchmark reference score).
#'
#' @export
generate_stratified_training_pool <- function(con, scale_identifier, target_count, max_scale_value) {
  if (is.null(con) || is.null(scale_identifier)) return(data.frame())

  m_val <- as.integer(max_scale_value)
  t_cnt <- max(as.integer(target_count), m_val + 1L)

  # Fetch all benchmark ratings for the scale
  ref_sql <- "
    SELECT ur.text_id, ur.bewertung, ur.typ, ur.rating_id, t.text, t.source_id,
           COALESCE(t.text, t.befund_text) AS case_text
    FROM user_ratings ur
    JOIN texts t ON ur.text_id = t.text_id
    WHERE (ur.kategorie = ? OR ur.kategorie = CAST(? AS CHAR))
      AND ur.typ IN ('gold_standard', 'kodierung')
    ORDER BY CASE WHEN ur.typ = 'gold_standard' THEN 0 ELSE 1 END ASC,
             ur.rating_id DESC
  "

  raw_refs <- tryCatch({
    dbGetQuery(con, ref_sql, params = list(as.character(scale_identifier), as.character(scale_identifier)))
  }, error = function(e) data.frame())

  if (nrow(raw_refs) == 0) return(data.frame())

  # Deduplicate by text_id: highest priority is gold_standard, then newest rating_id
  dedup <- raw_refs[!duplicated(raw_refs$text_id), ]
  dedup$val_int <- as.integer(dedup$bewertung)

  selected_pool <- list()

  # Step 1: Ensure at least one representative for every discrete score level 0..m_val
  for (lvl in 0:m_val) {
    lvl_rows <- dedup[dedup$val_int == lvl, , drop = FALSE]
    if (nrow(lvl_rows) > 0) {
      idx <- sample(seq_len(nrow(lvl_rows)), 1)
      selected_pool[[length(selected_pool) + 1]] <- lvl_rows[idx, , drop = FALSE]
      dedup <- dedup[dedup$text_id != lvl_rows$text_id[idx], , drop = FALSE]
    }
  }

  pool_df <- do.call(rbind, selected_pool)
  if (is.null(pool_df)) pool_df <- data.frame()

  # Step 2: Fill remaining slots up to target_count randomly from remaining items
  remaining_needed <- t_cnt - nrow(pool_df)
  if (remaining_needed > 0 && nrow(dedup) > 0) {
    n_draw <- min(remaining_needed, nrow(dedup))
    draw_idx <- sample(seq_len(nrow(dedup)), n_draw)
    pool_df <- rbind(pool_df, dedup[draw_idx, , drop = FALSE])
  }

  if (nrow(pool_df) > 0) {
    pool_df <- pool_df[sample(seq_len(nrow(pool_df))), , drop = FALSE] # Shuffle
  }

  pool_df
}
