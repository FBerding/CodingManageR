# ==============================================================================
# Unit & Integration Tests: Translation, Reliability, Performance, Export & UI
# Covers: R/utils_translation.R, R/utils_reliability.R, R/utils_performance.R,
#         R/utils_export.R, and R/utils_ui.R
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Internationalization (R/utils_translation.R)
# ------------------------------------------------------------------------------

test_that("translation helpers load JSON with mtime caching, fallback on missing file, and detect locales", {
  # Verifies get_supported_languages(), get_language_choices(), find_translations_file(),
  # load_translations() cache hit/force_reload/missing-file fallback, detect_browser_language(),
  # resolve_translation_path(), tr() fallback chain & sprintf formatting, and language_selector_ui().
  expect_equal(get_supported_languages(), c("de", "en"))
  choices <- get_language_choices()
  expect_equal(unname(choices), c("de", "en"))

  json_path <- find_translations_file()
  expect_true(!is.null(json_path) && file.exists(json_path))

  # Force reload populates cache, second call hits cache
  dict1 <- load_translations(force_reload = TRUE)
  expect_true(all(c("de", "en") %in% names(dict1)))
  dict2 <- load_translations(force_reload = FALSE)
  expect_identical(dict1, dict2)

  # Missing translations file falls back to minimal built-in dictionary
  testthat::with_mocked_bindings(
    find_translations_file = function() NULL,
    .package = "CodingManageR",
    {
      fallback_dict <- load_translations(force_reload = TRUE)
      expect_equal(fallback_dict$en$auth$login_title, "Sign In")
      expect_equal(fallback_dict$de$auth$login_title, "Anmeldung")
    }
  )

  # Malformed JSON triggers warning and falls back to existing cache
  bad_json <- tempfile(fileext = ".json")
  writeLines("{ invalid json", bad_json)
  withr::defer(unlink(bad_json, force = TRUE))
  testthat::with_mocked_bindings(
    find_translations_file = function() bad_json,
    .package = "CodingManageR",
    {
      expect_warning(
        cached_res <- load_translations(force_reload = TRUE),
        regexp = "Failed to read translations.json"
      )
      expect_true(is.list(cached_res))
    }
  )

  # Restore clean cache from canonical translations.json
  load_translations(force_reload = TRUE)

  # Browser locale normalization
  expect_equal(detect_browser_language("de-DE"), "de")
  expect_equal(detect_browser_language("EN_US"), "en")
  expect_equal(detect_browser_language("deu"), "de")
  expect_equal(detect_browser_language("fr-FR", fallback = "en"), "en")
  expect_equal(detect_browser_language(NULL, fallback = "de"), "de")
  expect_equal(detect_browser_language("", fallback = "en"), "en")

  # tr() resolution, fallbacks, sprintf interpolation, and invalid sprintf recovery
  expect_equal(tr(NULL, "en"), "")
  expect_equal(tr("", "en"), "")
  expect_true(nzchar(tr("auth.login_title", "de")))
  expect_true(nzchar(tr("auth.login_title", "en")))
  expect_false(identical(tr("auth.login_title", "de"), tr("auth.login_title", "en")))
  expect_equal(tr("auth.login_title", NULL), tr("auth.login_title", "en"))
  expect_equal(tr("nonexistent.deep.key", "de", default = "Custom Fallback"), "Custom Fallback")
  expect_equal(tr("nonexistent.deep.key", "en"), "[nonexistent.deep.key]")
  expect_match(tr("common.page_x_of_y", "en", 2L, 5L), "2.*5")
  # Bad sprintf argument count falls back safely without throwing
  expect_type(tr("common.page_x_of_y", "en", "only_one_arg"), "character")

  # Direct resolve_translation_path() edge cases (missing language, non-leaf list node)
  expect_null(CodingManageR:::resolve_translation_path(dict1, "zz", c("auth", "login_title")))
  expect_null(CodingManageR:::resolve_translation_path(dict1, "en", c("auth")))

  # language_selector_ui() produces a select element with onchange Shiny.setInputValue
  sel_html <- ui_to_html(language_selector_ui("header_lang", current_lang = "en", class = "ms-2"))
  expect_match(sel_html, "data-lang-selector=\"header_lang\"")
  expect_match(sel_html, "selected_ui_lang")
  expect_match(sel_html, "value=\"en\" selected")
})

# ------------------------------------------------------------------------------
# 2. Reliability Metrics & Stratified Sampling (R/utils_reliability.R)
# ------------------------------------------------------------------------------

test_that("classify_reliability_magnitude maps all 7 Landis & Koch intervals accurately", {
  # Verifies every threshold boundary (>0.80, >0.60, >0.40, >0.20, >=0.0, <0.0, and NULL/NA).
  expect_match(classify_reliability_magnitude(NULL, "en")$css_class, "text-muted")
  expect_match(classify_reliability_magnitude(NA_real_, "en")$css_class, "text-muted")
  expect_match(classify_reliability_magnitude(0.85, "en")$css_class, "text-success")
  expect_match(classify_reliability_magnitude(0.72, "en")$css_class, "text-info")
  expect_match(classify_reliability_magnitude(0.50, "en")$css_class, "text-warning")
  expect_match(classify_reliability_magnitude(0.30, "en")$css_class, "text-warning")
  expect_match(classify_reliability_magnitude(0.10, "en")$css_class, "text-danger")
  expect_match(classify_reliability_magnitude(-0.15, "en")$css_class, "text-danger")
})

test_that("metric choices, metadata lookup, license choices, and display formatting cover all 16 metrics", {
  # Verifies get_reliability_metric_choices() (with and without all_core), get_reliability_metric_meta()
  # across all 16 metrics + all_core + unknown fallback, get_source_license_choices(), and format_metric_display().
  choices_with_all <- get_reliability_metric_choices("en", include_all_core = TRUE)
  expect_length(choices_with_all, 5L)

  choices_no_all <- get_reliability_metric_choices("de", include_all_core = FALSE)
  expect_length(choices_no_all, 4L)

  all_core_meta <- get_reliability_metric_meta("all_core", "en")
  expect_false(all_core_meta$is_pct)
  expect_equal(all_core_meta$color, "#0284c7")

  all_16_keys <- c(
    "cohens_kappa", "alpha_nominal", "fleiss_kappa", "gwet_ac1",
    "alpha_ordinal", "kappa_linear", "kappa_squared", "gwet_ac2_quadratic", "kendall_w",
    "avg_iota2", "iota_index",
    "accuracy", "balanced_accuracy", "macro_f1", "macro_precision", "macro_recall"
  )
  for (k in all_16_keys) {
    m <- get_reliability_metric_meta(k, "en")
    expect_true(nzchar(m$label))
    expect_true(nzchar(m$short_label))
    expect_match(m$color, "^#[0-9a-fA-F]{6}$")
    expect_equal(m$is_pct, k %in% c("accuracy", "balanced_accuracy", "macro_f1", "macro_precision", "macro_recall"))
  }

  # Unknown metric key falls back to cohens_kappa metadata
  fallback_meta <- get_reliability_metric_meta("unknown_metric_xyz", "en")
  expect_equal(fallback_meta$color, get_reliability_metric_meta("cohens_kappa", "en")$color)

  # Source license choices contain 8 options including __custom__
  lic <- get_source_license_choices("de")
  expect_length(lic, 8L)
  expect_true("__custom__" %in% unname(lic))
  expect_true("CC-BY 4.0" %in% unname(lic))

  # Metric display formatting
  expect_equal(format_metric_display(NULL), "\u2014")
  expect_equal(format_metric_display(NA), "\u2014")
  expect_equal(format_metric_display(""), "\u2014")
  expect_equal(format_metric_display("NA"), "\u2014")
  expect_equal(format_metric_display("not_num"), "\u2014")
  expect_equal(format_metric_display(0.8126, is_percentage = FALSE, digits = 3), "0.813")
  expect_equal(format_metric_display(87.45, is_percentage = TRUE), "87.5 %")
})

test_that("generate_stratified_training_pool guarantees every discrete level 0..max_value and prioritizes gold standards", {
  # Verifies that generate_stratified_training_pool():
  # 1) Returns empty data.frame on NULL inputs or scales with no reference ratings
  # 2) Enforces minimum size >= max_scale_value + 1 and includes at least one case per score level 0..max_value
  # 3) Deduplicates by text_id giving strict priority to 'gold_standard' over 'kodierung'
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  seed_full_gold_standards(con, scale_id = 1L, max_value = 4L)

  expect_equal(nrow(generate_stratified_training_pool(NULL, "1", 5, 4)), 0L)
  expect_equal(nrow(generate_stratified_training_pool(con, NULL, 5, 4)), 0L)
  expect_equal(nrow(generate_stratified_training_pool(con, "999", 5, 4)), 0L)

  # Request target_count = 2 (< max_value + 1 = 5): must automatically bump to at least 5 covering 0..4
  pool_min <- generate_stratified_training_pool(con, "1", target_count = 2L, max_scale_value = 4L)
  expect_equal(nrow(pool_min), 5L)
  expect_true(all(0:4 %in% pool_min$val_int))

  # Request target_count = 8: fills remaining 3 slots randomly without duplicate text_ids
  pool_8 <- generate_stratified_training_pool(con, "1", target_count = 8L, max_scale_value = 4L)
  expect_equal(nrow(pool_8), 8L)
  expect_equal(length(unique(pool_8$text_id)), 8L)
  expect_true(all(0:4 %in% pool_8$val_int))
})

# ------------------------------------------------------------------------------
# 3. Performance Metrics via aifeducation (R/utils_performance.R)
# ------------------------------------------------------------------------------

test_that("berechne_aifeducation_metriken computes all 16 inter-rater reliability metrics accurately", {
  # Verifies that berechne_aifeducation_metriken() returns a named list of all 16 character-formatted
  # metrics, reporting 100% agreement and Kappa = 1 on identical ordinal vectors and <100% on partial mismatches.
  testthat::skip_if_not_installed("aifeducation")

  expected_keys <- c(
    "accuracy", "balanced_accuracy", "iota_index", "avg_iota2",
    "cohens_kappa", "kappa_linear", "kappa_squared", "fleiss_kappa",
    "kendall_w", "alpha_nominal", "alpha_ordinal", "gwet_ac1",
    "gwet_ac2_quadratic", "macro_precision", "macro_recall", "macro_f1"
  )

  perfect_vals <- c(0, 1, 2, 3, 0, 1, 2, 3)
  res_perf <- berechne_aifeducation_metriken(perfect_vals, perfect_vals, max_val = 3L)
  expect_type(res_perf, "list")
  expect_equal(names(res_perf), expected_keys)
  expect_equal(as.numeric(res_perf$accuracy), 100.0)
  expect_equal(as.numeric(res_perf$cohens_kappa), 1.0)

  partial_pred <- c(0, 1, 2, 3, 0, 2, 2, 1)
  res_part <- berechne_aifeducation_metriken(partial_pred, perfect_vals, max_val = 3L)
  expect_equal(names(res_part), expected_keys)
  expect_lt(as.numeric(res_part$accuracy), 100.0)
  expect_gt(as.numeric(res_part$accuracy), 50.0)
})

# ------------------------------------------------------------------------------
# 4. Data Export & Schema Validation (R/utils_export.R)
# ------------------------------------------------------------------------------

test_that("export helpers sanitize BOM columns, map all 9 PKs, validate schemas, and generate CSVs/templates", {
  # Verifies clean_table_columns() (including NULL/non-df/UTF-8 BOM), get_table_pk_definition() for
  # all 9 tables + unknown, validate_import_schema(), export_table_to_csv() (populated, empty, NULL),
  # and export_table_template_csv().
  expect_null(clean_table_columns(NULL))
  expect_equal(clean_table_columns("not_df"), "not_df")

  dirty_df <- data.frame(a = 1, b = 2)
  colnames(dirty_df) <- c("\ufeff text_id ", "  source_id")
  cleaned <- clean_table_columns(dirty_df)
  expect_equal(colnames(cleaned), c("text_id", "source_id"))

  expected_pks <- c(
    user_tokens = "user_id",
    sources = "source_id",
    texts = "text_id",
    user_ratings = "rating_id",
    coding_training = "training_id",
    scale_definitions = "scale_id",
    user_source_permissions = "source_id",
    user_coding_source_permissions = "source_id",
    user_scale_permissions = "scale_id"
  )
  for (tbl in names(expected_pks)) {
    expect_equal(get_table_pk_definition(tbl), unname(expected_pks[[tbl]]))
  }
  expect_null(get_table_pk_definition("nonexistent_table"))

  # Schema validation: empty/NULL, valid, and missing required columns
  expect_false(validate_import_schema("texts", NULL)$is_valid)
  expect_false(validate_import_schema("texts", data.frame())$is_valid)
  expect_true(validate_import_schema("texts", data.frame(text = "hello"))$is_valid)
  expect_true(validate_import_schema("user_tokens", data.frame(username = "a", token = "b", permissions = "user"))$is_valid)
  expect_true(validate_import_schema("sources", data.frame(Kurzreferenz = "K", Vollstaendiges_Zitat = "V"))$is_valid)
  expect_true(validate_import_schema("scale_definitions", data.frame(short_label = "S", name = "N", max_value = 3))$is_valid)
  expect_true(validate_import_schema("unknown_tbl", data.frame(x = 1))$is_valid)

  bad_res <- validate_import_schema("user_tokens", data.frame(username = "only_one"))
  expect_false(bad_res$is_valid)
  expect_true(all(c("token", "permissions") %in% bad_res$missing_cols))

  # CSV export on populated, empty, and NULL/blank arguments
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  expect_equal(export_table_to_csv(NULL, "sources"), "")
  expect_equal(export_table_to_csv(db$con, ""), "")

  csv_sources <- export_table_to_csv(db$con, "sources")
  expect_match(csv_sources, "Kurzreferenz")
  expect_match(csv_sources, "Mueller2024")

  db_empty <- local_test_db(seeded = TRUE, empty = TRUE)
  csv_empty_sources <- export_table_to_csv(db_empty$con, "sources")
  expect_match(csv_empty_sources, "source_id;Kurzreferenz;Referenz;Lizenz;erfasst_am")

  # CSV templates for all recognized tables + unknown
  expect_match(export_table_template_csv("user_tokens"), "username;token;permissions;is_active")
  expect_match(export_table_template_csv("sources"), "Kurzreferenz;Vollstaendiges_Zitat")
  expect_match(export_table_template_csv("texts"), "text;source_id")
  expect_match(export_table_template_csv("scale_definitions"), "short_label;name;def_desc")
  expect_equal(export_table_template_csv("other"), "\r\n")
})

test_that("fetch_consolidated_master_export consolidates ratings with optional scale, source, and user filters", {
  # Verifies fetch_consolidated_master_export() joins all 5 relational tables on the canonical
  # SQLite schema (Referenz column), supports combined scale_ids/source_ids/user_ids filters,
  # handles NULL connections, and supports legacy schemas with Vollstaendiges_Zitat.
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  expect_equal(nrow(fetch_consolidated_master_export(NULL)), 0L)

  all_rows <- fetch_consolidated_master_export(con)
  expect_equal(nrow(all_rows), 34L)
  expect_true(all(c("rating_id", "falltext", "skala_name", "text_quelle_zitat", "kodier_quelle_zitat") %in% colnames(all_rows)))

  user1_id <- get_user_info_db(con, "user1")$id
  filtered <- fetch_consolidated_master_export(con, scale_ids = c("1"), source_ids = c(1L), user_ids = c(user1_id))
  expect_gt(nrow(filtered), 0L)
  expect_true(all(filtered$username == "user1"))
  expect_true(all(filtered$skala_kuerzel == "Empathie"))

  # Verify legacy Vollstaendiges_Zitat column detection branch
  con_leg <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_leg))
  DBI::dbExecute(con_leg, "CREATE TABLE user_tokens (user_id INT, username TEXT, permissions TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE sources (source_id INT, Kurzreferenz TEXT, Vollstaendiges_Zitat TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE texts (text_id INT, text TEXT, source_id INT);")
  DBI::dbExecute(con_leg, "CREATE TABLE scale_definitions (scale_id INT, short_label TEXT, name TEXT, max_value INT);")
  DBI::dbExecute(con_leg, "CREATE TABLE user_ratings (rating_id INT, user_id INT, text_id INT, kategorie TEXT, bewertung INT, typ TEXT, source_id INT, zeitstempel TEXT);")
  DBI::dbExecute(con_leg, "INSERT INTO user_tokens VALUES (1, 'admin', 'admin');")
  DBI::dbExecute(con_leg, "INSERT INTO sources VALUES (1, 'Leg2020', 'Full Legacy Citation 2020');")
  DBI::dbExecute(con_leg, "INSERT INTO texts VALUES (1, 'Case A', 1);")
  DBI::dbExecute(con_leg, "INSERT INTO scale_definitions VALUES (1, 'S1', 'Scale 1', 2);")
  DBI::dbExecute(con_leg, "INSERT INTO user_ratings VALUES (1, 1, 1, '1', 2, 'kodierung', 1, '2026-01-01');")

  leg_out <- fetch_consolidated_master_export(con_leg)
  expect_equal(nrow(leg_out), 1L)
  expect_equal(leg_out$text_quelle_zitat[1], "Full Legacy Citation 2020")
})

# ------------------------------------------------------------------------------
# 5. Reusable UI Builders (R/utils_ui.R)
# ------------------------------------------------------------------------------

test_that("UI builders generate expected Bootstrap markup for KPI cards, badges, modals, tables, pagination, and integers", {
  # Verifies render_kpi_card_ui(), render_metric_badge_ui(), build_scale_examples_modal(),
  # build_edit_coding_value_modal(), build_coding_search_table_ui(), build_coding_search_pagination_ui(),
  # and format_int_locale() across both populated and empty/edge states.
  ns <- shiny::NS("mod_test")

  # KPI Card with and without subtitle
  kpi_sub <- ui_to_html(render_kpi_card_ui("Total Ratings", 42, subtitle = "Last 7 days", bg_class = "bg-success", width = 6))
  expect_match(kpi_sub, "42")
  expect_match(kpi_sub, "Last 7 days")
  expect_match(kpi_sub, "bg-success")

  kpi_nosub <- ui_to_html(render_kpi_card_ui("Empty Sub", 0, subtitle = NULL))
  expect_match(kpi_nosub, "Empty Sub")

  # Metric status badges across NA, non-numeric, danger (<mid), warning (mid..high), success (>=high), and percentage mode
  expect_match(ui_to_html(render_metric_badge_ui(NULL)), "\u2014")
  expect_match(ui_to_html(render_metric_badge_ui("invalid")), "\u2014")
  expect_match(ui_to_html(render_metric_badge_ui(0.45)), "bg-danger")
  expect_match(ui_to_html(render_metric_badge_ui(0.70)), "bg-warning")
  expect_match(ui_to_html(render_metric_badge_ui(0.85)), "bg-success")
  expect_match(ui_to_html(render_metric_badge_ui(85.2, is_percentage = TRUE)), "85\\.2 %")
  expect_match(ui_to_html(render_metric_badge_ui(55.0, is_percentage = TRUE)), "bg-danger")

  # Scale examples modal (empty and populated with highlighted active_level)
  expect_match(ui_to_html(build_scale_examples_modal(NULL, lang = "en")), "modal")
  scale_row <- data.frame(
    name = "K1_Empathie", short_label = "Empathie", def_desc = "Desc",
    max_value = 2L, bsp_0 = "Example 0", bsp_1 = "", bsp_2 = "Example 2",
    stringsAsFactors = FALSE
  )
  ex_html <- ui_to_html(build_scale_examples_modal(scale_row, active_level = 2L, lang = "en"))
  expect_match(ex_html, "K1_Empathie")
  expect_match(ex_html, "Example 2")
  expect_match(ex_html, "bg-success")

  # Edit coding value modal (empty and populated)
  expect_match(ui_to_html(build_edit_coding_value_modal(ns, NULL, lang = "en")), "modal")
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  detail_row <- get_coding_detail_db(db$con, 201L)
  edit_html <- ui_to_html(build_edit_coding_value_modal(ns, detail_row, lang = "en"))
  expect_match(edit_html, "mod_test-edit_coding_new_value")
  expect_match(edit_html, "mod_test-btn_confirm_edit_coding")

  # Coding search table UI (empty and populated with long text truncation & all rating type badges)
  expect_match(ui_to_html(build_coding_search_table_ui(ns, data.frame(), lang = "en")), "alert-info")
  page_df <- search_case_codings_db(db$con)
  page_df$case_text[1] <- paste(rep("Long clinical narrative ", 10), collapse = "")
  tbl_html <- ui_to_html(build_coding_search_table_ui(ns, page_df[1:5, ], lang = "en"))
  expect_match(tbl_html, "mod_test-trigger_edit_coding_rating")
  expect_match(tbl_html, "\\.\\.\\.")

  # Coding search pagination UI (0 rows -> NULL, first page -> prev disabled, last page -> next disabled)
  expect_null(build_coding_search_pagination_ui(ns, total_rows = 0L, cur_page = 1L, tot_pages = 1L, lang = "en"))
  pag_first <- ui_to_html(build_coding_search_pagination_ui(ns, total_rows = 60L, cur_page = 1L, tot_pages = 3L, page_size = 25L, lang = "en"))
  expect_match(pag_first, "mod_test-btn_cs_next")
  pag_last <- ui_to_html(build_coding_search_pagination_ui(ns, total_rows = 60L, cur_page = 3L, tot_pages = 3L, page_size = 25L, lang = "de"))
  expect_match(pag_last, "mod_test-btn_cs_prev")

  # Locale-aware integer formatting
  expect_equal(format_int_locale(NULL, "en"), "0")
  expect_equal(format_int_locale("bad", "en"), "0")
  expect_equal(format_int_locale(1234567, "en"), "1,234,567")
  expect_equal(format_int_locale(1234567, "de"), "1.234.567")
})

test_that("UI builders, export, and stratified sampling handle custom thresholds, NA fields, all rating types, and DB errors", {
  # Covers remaining non-MySQL branches across R/utils_ui.R, R/utils_export.R, and R/utils_reliability.R:
  # 1) render_metric_badge_ui() with custom thresh_high/thresh_mid
  # 2) build_scale_examples_modal() with NA metadata fields and missing bsp_* columns
  # 3) build_edit_coding_value_modal() across kodierung, gold_standard, schulung, unknown typ,
  #    NA timestamp/case_text/scale_max_value, missing def_n_*, and identical vs distinct short_label
  # 4) build_coding_search_table_ui() with schulung, unknown typ, NA case_text/scale/username/timestamp,
  #    short case text (<110 chars), and identical vs distinct short_label
  # 5) fetch_consolidated_master_export() error fallback on broken DB
  # 6) generate_stratified_training_pool() on legacy befund_text schema and empty-level pool
  ns <- shiny::NS("mod_edge")

  # 1. Custom thresholds on render_metric_badge_ui()
  expect_match(ui_to_html(render_metric_badge_ui(0.75, thresh_high = 0.70, thresh_mid = 0.50)), "bg-success")
  expect_match(ui_to_html(render_metric_badge_ui(0.55, thresh_high = 0.70, thresh_mid = 0.50)), "bg-warning")
  expect_match(ui_to_html(render_metric_badge_ui(0.40, thresh_high = 0.70, thresh_mid = 0.50)), "bg-danger")

  # 2. build_scale_examples_modal() with NA metadata and no bsp_* columns
  sparse_scale <- data.frame(
    name = NA_character_,
    short_label = NA_character_,
    def_desc = "",
    max_value = NA_integer_,
    stringsAsFactors = FALSE
  )
  sparse_ex_html <- ui_to_html(build_scale_examples_modal(sparse_scale, active_level = 1L, lang = "de"))
  expect_match(sparse_ex_html, "Skala")
  expect_match(sparse_ex_html, "bg-success")

  # 3. build_edit_coding_value_modal() across all rating types & NA fallbacks
  for (rtype in c("kodierung", "gold_standard", "schulung", "custom_type")) {
    row_edge <- data.frame(
      rating_id = 501L,
      text_id = 101L,
      kategorie = "1",
      bewertung = 1L,
      username = "coder_x",
      typ = rtype,
      zeitstempel = NA_character_,
      case_text = NA_character_,
      scale_name = if (rtype == "kodierung") "SameLabel" else NA_character_,
      scale_short_label = if (rtype == "kodierung") "SameLabel" else "ShortDiff",
      scale_def_desc = if (rtype == "gold_standard") "" else "Scale definition text",
      scale_max_value = if (rtype == "schulung") -1L else 2L,
      def_n_0 = "Zero desc",
      def_n_1 = "",
      def_n_2 = NA_character_,
      stringsAsFactors = FALSE
    )
    ed_html <- ui_to_html(build_edit_coding_value_modal(ns, row_edge, lang = "en"))
    expect_match(ed_html, "mod_edge-edit_coding_new_value")
    expect_match(ed_html, "Rating #501")
  }

  # 4. build_coding_search_table_ui() with schulung, unknown typ, NA fields, short text, distinct & identical short_label
  edge_tbl_df <- data.frame(
    rating_id = c(1L, 2L, 3L),
    text_id = c(10L, 11L, 12L),
    kategorie = c("1", "2", "3"),
    bewertung = c(1L, 2L, 0L),
    typ = c("schulung", "custom_typ", "gold_standard"),
    username = c("user1", NA_character_, "admin"),
    zeitstempel = c("2026-11-10 10:00:00", NA_character_, "2026-11-11 08:30:00"),
    case_text = c("Short clinical text", NA_character_, "Another short note"),
    scale_name = c("K1_Empathie", NA_character_, "IdenticalName"),
    scale_short_label = c("Empathie", NA_character_, "IdenticalName"),
    scale_max_value = c(4L, NA_integer_, 2L),
    stringsAsFactors = FALSE
  )
  edge_tbl_html <- ui_to_html(build_coding_search_table_ui(ns, edge_tbl_df, lang = "en"))
  expect_match(edge_tbl_html, "bg-info")
  expect_match(edge_tbl_html, "bg-secondary")
  expect_match(edge_tbl_html, "bg-warning")
  expect_match(edge_tbl_html, "Short clinical text")
  expect_match(edge_tbl_html, "Empathie")

  # 5. fetch_consolidated_master_export() error fallback on empty DB without tables
  con_empty_no_tbl <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_empty_no_tbl))
  expect_message(
    err_export <- fetch_consolidated_master_export(con_empty_no_tbl),
    regexp = "Error executing master export"
  )
  expect_equal(nrow(err_export), 0L)

  # 6. generate_stratified_training_pool() on legacy befund_text table & out-of-range scores (empty selected_pool)
  con_leg <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_leg))
  DBI::dbExecute(con_leg, "CREATE TABLE texts (text_id INT, text TEXT, befund_text TEXT, source_id INT);")
  DBI::dbExecute(con_leg, "CREATE TABLE user_ratings (rating_id INT, user_id INT, text_id INT, kategorie TEXT, bewertung INT, typ TEXT);")
  DBI::dbExecute(con_leg, "INSERT INTO texts VALUES (1, NULL, 'Legacy Befund Case', 1);")
  DBI::dbExecute(con_leg, "INSERT INTO user_ratings VALUES (1, 1, 1, '1', 9, 'gold_standard');")

  pool_out_range <- generate_stratified_training_pool(con_leg, "1", target_count = 2L, max_scale_value = 2L)
  expect_equal(nrow(pool_out_range), 1L)
  expect_equal(pool_out_range$case_text[1], "Legacy Befund Case")
})

