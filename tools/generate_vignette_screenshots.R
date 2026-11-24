#!/usr/bin/env Rscript
# ==============================================================================
# Automated Vignette Screenshot Generator for CodingManageR
# ==============================================================================
# Captures 12 consistent, reproducible English-language PNG screenshots of the
# running CodingManageR Shiny application (4 core views + all 8 Admin Console
# subtabs) via shinytest2::AppDriver and headless Chrome (chromote).
#
# Usage (from repository root):
#   Rscript tools/generate_vignette_screenshots.R
#   Rscript tools/generate_vignette_screenshots.R --output-dir vignettes/figures --width 1280 --height 860 --lang en
# ==============================================================================

parse_cli_args <- function(args) {
  cfg <- list(
    output_dir = "vignettes/figures",
    width      = 1280L,
    height     = 860L,
    lang       = "en"
  )

  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (key %in% c("--output-dir", "--width", "--height", "--lang")) {
      if (i == length(args)) stop("Missing value for argument: ", key, call. = FALSE)
      val <- args[[i + 1L]]
      if (identical(key, "--output-dir")) cfg$output_dir <- val
      if (identical(key, "--width"))      cfg$width      <- as.integer(val)
      if (identical(key, "--height"))     cfg$height     <- as.integer(val)
      if (identical(key, "--lang"))       cfg$lang       <- tolower(trimws(val))
      i <- i + 2L
    } else {
      stop("Unknown argument: ", key, call. = FALSE)
    }
  }

  if (is.na(cfg$width) || cfg$width < 800L)  cfg$width  <- 1280L
  if (is.na(cfg$height) || cfg$height < 600L) cfg$height <- 860L
  if (!(cfg$lang %in% c("en", "de"))) cfg$lang <- "en"
  cfg
}

check_required_packages <- function() {
  required_pkgs <- c("CodingManageR", "DBI", "RSQLite", "shinytest2", "chromote")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L) {
    stop(
      "Missing required package(s) for screenshot generation: ",
      paste(missing_pkgs, collapse = ", "),
      call. = FALSE
    )
  }
}

build_demo_sqlite <- function() {
  db_path <- tempfile(pattern = "cm_vignette_screenshots_", fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  CodingManageR::create_all_tables(con, verbose = FALSE)
  suppressMessages(CodingManageR::seed_initial_data(con, empty = FALSE))

  # Ensure complete 0..4 gold-standard coverage for scale 1 (K1_Empathie) so
  # 1. Coding Training starts deterministically with authentic clinical cases.
  gold_uid <- DBI::dbGetQuery(
    con,
    "SELECT user_id FROM user_tokens WHERE username = 'gold_standard' LIMIT 1"
  )$user_id[1]
  if (is.null(gold_uid) || is.na(gold_uid)) gold_uid <- 1L

  extra_golds <- data.frame(
    text_id   = c(201L, 203L),
    bewertung = c(0L, 1L),
    source_id = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  for (idx in seq_len(nrow(extra_golds))) {
    exists_n <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM user_ratings
       WHERE text_id = ? AND kategorie = '1' AND typ = 'gold_standard'",
      params = list(extra_golds$text_id[idx])
    )$n[1]
    if (is.na(exists_n) || as.integer(exists_n) == 0L) {
      DBI::dbExecute(
        con,
        "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id, zeitstempel)
         VALUES (?, ?, '1', ?, 'gold_standard', ?, '2026-10-01 11:00:00')",
        params = list(
          as.integer(gold_uid),
          extra_golds$text_id[idx],
          extra_golds$bewertung[idx],
          extra_golds$source_id[idx]
        )
      )
    }
  }

  db_path
}

wait_for_selector <- function(app, selector, timeout_s = 25, poll_s = 0.25) {
  js_expr <- sprintf("Boolean(document.querySelector(%s))", jsonlite::toJSON(selector, auto_unbox = TRUE))
  deadline <- Sys.time() + timeout_s
  repeat {
    found <- tryCatch(isTRUE(app$get_js(js_expr)), error = function(e) FALSE)
    if (found) return(invisible(TRUE))
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for CSS selector: ", selector, call. = FALSE)
    }
    Sys.sleep(poll_s)
  }
}

wait_for_plot_img <- function(app, plot_output_id, timeout_s = 30, poll_s = 0.25) {
  js_expr <- sprintf(
    "(function() {
      var el = document.querySelector('#%s img');
      return Boolean(el && el.complete && el.naturalWidth > 0 && el.naturalHeight > 0);
    })()",
    plot_output_id
  )
  deadline <- Sys.time() + timeout_s
  repeat {
    ready <- tryCatch(isTRUE(app$get_js(js_expr)), error = function(e) FALSE)
    if (ready) return(invisible(TRUE))
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for rendered plot image in #", plot_output_id, call. = FALSE)
    }
    Sys.sleep(poll_s)
  }
}

capture_view <- function(app, out_dir, filename, delay_s = 0.4) {
  app$wait_for_idle(duration = 400, timeout = 30000)
  app$run_js("
    var panel = document.getElementById('shiny-notification-panel');
    if (panel) panel.remove();
    window.scrollTo(0, 0);
  ")
  Sys.sleep(delay_s)

  dest <- file.path(out_dir, filename)
  app$get_screenshot(
    file = dest,
    screenshot_args = list(cliprect = "viewport", scale = 1)
  )

  if (!file.exists(dest) || isTRUE(file.info(dest)$size == 0)) {
    stop("Failed to capture screenshot: ", dest, call. = FALSE)
  }

  kb <- file.info(dest)$size / 1024
  message(sprintf("  [OK] %-34s (%6.1f KB)", filename, kb))
  invisible(dest)
}

set_ui_language <- function(app, lang = "en") {
  js_cmd <- sprintf(
    "Shiny.setInputValue('selected_ui_lang', %s, {priority: 'event'});",
    jsonlite::toJSON(lang, auto_unbox = TRUE)
  )
  app$run_js(js_cmd)
  app$wait_for_idle(duration = 400, timeout = 30000)
}

sign_in_as <- function(app, username, token, wait_selector) {
  wait_for_selector(app, "#login_username")
  app$set_inputs(login_username = username, login_token = token)
  app$click("btn_login")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, wait_selector)
}

sign_out <- function(app) {
  wait_for_selector(app, "#btn_logout")
  app$click("btn_logout")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#btn_login")
}

main <- function() {
  cfg <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  check_required_packages()

  if (!dir.exists(cfg$output_dir)) {
    dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  options(chromote.headless = "new")
  is_linux <- identical(Sys.info()[["sysname"]], "Linux")
  chrome_flags <- if (is_linux) {
    c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage", "--force-device-scale-factor=1")
  } else {
    c("--disable-gpu", "--force-device-scale-factor=1")
  }
  try(chromote::set_chrome_args(chrome_flags), silent = TRUE)

  on.exit(
    try(chromote::default_chromote_object()$close(), silent = TRUE),
    add = TRUE
  )

  db_path <- build_demo_sqlite()
  on.exit(
    unlink(c(db_path, paste0(db_path, "-wal"), paste0(db_path, "-shm")), force = TRUE),
    add = TRUE
  )

  message(sprintf(
    "Launching CodingManageR (%dx%d, lang='%s') -> '%s'",
    cfg$width, cfg$height, cfg$lang, cfg$output_dir
  ))

  # Database is pre-seeded above; pass empty = TRUE to avoid existing-file warning.
  shiny_app <- CodingManageR::run_app(sqlite_path = db_path, empty = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = shiny_app,
    name = "vignette_screenshots",
    width = cfg$width,
    height = cfg$height,
    load_timeout = 60000,
    timeout = 30000,
    wait = TRUE,
    expect_values_screenshot_args = FALSE
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_window_size(width = cfg$width, height = cfg$height)
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#btn_login")
  set_ui_language(app, cfg$lang)

  # ----------------------------------------------------------------------------
  # 1. Sign-In Screen
  # ----------------------------------------------------------------------------
  capture_view(app, cfg$output_dir, "01-login.png")

  # ----------------------------------------------------------------------------
  # 2-4. Coder Views (user1): Dashboard, Active Coding Training, Active Live Coding
  # ----------------------------------------------------------------------------
  sign_in_as(app, "user1", "token2026", "#mod_dashboard-stat_card_total_kodierung")
  set_ui_language(app, cfg$lang)
  wait_for_plot_img(app, "mod_dashboard-training_reliability_plot")
  capture_view(app, cfg$output_dir, "02-dashboard.png")

  # 3. 1. Coding Training (active case view with text, slider, live level description)
  app$set_inputs(main_nav_tabs = "tab_training")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_training-start_schulung_btn")
  app$set_inputs(`mod_training-gewaehlte_kategorie` = "1")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_training-kodiereinheiten")
  app$set_inputs(`mod_training-kodiereinheiten` = 5L)
  app$wait_for_idle(duration = 300, timeout = 30000)
  app$click("mod_training-start_schulung_btn")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#mod_training-naechster_fall_btn")
  app$set_inputs(`mod_training-bewertung_slider` = 3L)
  app$wait_for_idle(duration = 400, timeout = 30000)
  capture_view(app, cfg$output_dir, "03-coding-training.png")

  # 4. 2. Live Coding (active workspace with banner, case text, slider, level description)
  app$set_inputs(main_nav_tabs = "tab_codings")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_codings-start_kodierung_btn")
  app$set_inputs(`mod_codings-gewaehlte_kategorie` = "1", `mod_codings-gewaehlte_quelle` = "1")
  app$wait_for_idle(duration = 400, timeout = 30000)
  app$click("mod_codings-start_kodierung_btn")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#mod_codings-save_next_btn")
  app$set_inputs(`mod_codings-bewertung_slider` = 2L)
  app$wait_for_idle(duration = 400, timeout = 30000)
  capture_view(app, cfg$output_dir, "04-live-coding.png")

  # ----------------------------------------------------------------------------
  # 5-12. Administrator Console (all 8 subtabs)
  # ----------------------------------------------------------------------------
  sign_out(app)
  set_ui_language(app, cfg$lang)
  sign_in_as(app, "admin", "geheim123", "#main_nav_tabs")
  set_ui_language(app, cfg$lang)

  app$set_inputs(main_nav_tabs = "tab_admin")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#mod_admin-users_table_container table")

  # 5. Admin: User Management
  capture_view(app, cfg$output_dir, "05-admin-users.png")

  # 6. Admin: Sources Directory
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_sources")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_admin-sources_table_container table")
  capture_view(app, cfg$output_dir, "06-admin-sources.png")

  # 7. Admin: Scales Management
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_scales")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_admin-scales_table_container table")
  capture_view(app, cfg$output_dir, "07-admin-scales.png")

  # 8. Admin: Case Texts & Source Assignment
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_texts")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_admin-texts_management_table_container table")
  capture_view(app, cfg$output_dir, "08-admin-texts.png")

  # 9. Admin: Reliability & Progress
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_reliability")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_plot_img(app, "mod_admin-admin_reliability_comparison_plot")
  wait_for_selector(app, "#mod_admin-admin_reliability_table_container")
  capture_view(app, cfg$output_dir, "09-admin-reliability.png")

  # 10. Admin: Edit Codings
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_coding_search")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#mod_admin-coding_search_table_container table")
  capture_view(app, cfg$output_dir, "10-admin-edit-codings.png")

  # 11. Admin: Data & Import/Export
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_data")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#mod_admin-export_filter_summary_badge")
  capture_view(app, cfg$output_dir, "11-admin-data-import-export.png")

  # 12. Admin: DB Diagnosis
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_diagnose")
  app$wait_for_idle(duration = 400, timeout = 30000)
  wait_for_selector(app, "#mod_admin-btn_refresh_diagnostics")
  app$click("mod_admin-btn_refresh_diagnostics")
  app$wait_for_idle(duration = 500, timeout = 30000)
  wait_for_selector(app, "#mod_admin-db_diag_tables_overview table")
  wait_for_selector(app, "#mod_admin-db_diag_integrity_audit")
  capture_view(app, cfg$output_dir, "12-admin-db-diagnosis.png")

  expected_files <- c(
    "01-login.png",
    "02-dashboard.png",
    "03-coding-training.png",
    "04-live-coding.png",
    "05-admin-users.png",
    "06-admin-sources.png",
    "07-admin-scales.png",
    "08-admin-texts.png",
    "09-admin-reliability.png",
    "10-admin-edit-codings.png",
    "11-admin-data-import-export.png",
    "12-admin-db-diagnosis.png"
  )
  paths <- file.path(cfg$output_dir, expected_files)
  missing_out <- expected_files[!file.exists(paths)]
  if (length(missing_out) > 0L) {
    stop("Missing expected screenshot file(s): ", paste(missing_out, collapse = ", "), call. = FALSE)
  }

  total_kb <- sum(file.info(paths)$size, na.rm = TRUE) / 1024
  message(sprintf(
    "Successfully generated %d vignette screenshots in '%s' (total raw size: %.1f KB).",
    length(paths), cfg$output_dir, total_kb
  ))
}

if (sys.nframe() == 0L) {
  main()
}
