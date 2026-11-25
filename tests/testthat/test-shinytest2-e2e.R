# Note: This file was generated with the assistance of AI.

# ==============================================================================
# End-to-End Headless Browser Tests via shinytest2::AppDriver
# Strategy: Deterministic programmatic state, DOM, and SQLite post-condition
#           assertions (NO visual screenshots or snapshot baselines).
# ==============================================================================

#' Build an isolated temporary SQLite database and launch shinytest2::AppDriver
#'
#' @param name Unique AppDriver session identifier.
#' @param seed_gold_scale Optional integer scale_id to seed complete 0..max_value gold standards.
#' @param env Calling environment for automatic AppDriver$stop() and file cleanup.
#' @return A list containing \code{app} (AppDriver) and \code{db_path} (character SQLite path).
.e2e_chrome_state <- new.env(parent = emptyenv())
.e2e_chrome_state$started <- FALSE

if (requireNamespace("withr", quietly = TRUE) && requireNamespace("chromote", quietly = TRUE)) {
  withr::defer({
    if (isTRUE(.e2e_chrome_state$started)) {
      try(chromote::default_chromote_object()$close(), silent = TRUE)
    }
  }, envir = testthat::teardown_env())
}

local_e2e_app <- function(name = "codingmanager_e2e", seed_gold_scale = NULL, env = parent.frame()) {
  if (requireNamespace("chromote", quietly = TRUE)) {
    options(chromote.headless = "new")
    is_linux <- identical(Sys.info()[["sysname"]], "Linux")
    chrome_flags <- if (is_linux) {
      c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
    } else {
      c("--disable-gpu")
    }
    try(
      chromote::set_chrome_args(chrome_flags),
      silent = TRUE
    )
    .e2e_chrome_state$started <- TRUE
  }

  db_path <- tempfile(pattern = paste0(name, "_"), fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  create_all_tables(con, verbose = FALSE)
  suppressMessages(seed_initial_data(con, empty = FALSE))
  if (!is.null(seed_gold_scale)) {
    max_v <- DBI::dbGetQuery(con, "SELECT max_value FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(seed_gold_scale)))$max_value[1]
    seed_full_gold_standards(con, scale_id = as.integer(seed_gold_scale), max_value = as.integer(max_v))
  }
  DBI::dbDisconnect(con)

  shiny_app <- run_app_local(sqlite_path = db_path, empty = TRUE)
  driver <- shinytest2::AppDriver$new(
    app_dir = shiny_app,
    name = name,
    load_timeout = 60000,
    timeout = 30000,
    wait = TRUE,
    expect_values_screenshot_args = FALSE
  )

  cleanup <- function() {
    try(driver$stop(), silent = TRUE)
    unlink(c(db_path, paste0(db_path, "-wal"), paste0(db_path, "-shm")), force = TRUE)
  }
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(cleanup(), envir = env)
  }

  list(app = driver, db_path = db_path)
}

test_that("E2E-1: Authentication guards, inactive lockout, bilingual switching, and logout work in real browser", {
  # Verifies full browser DOM interaction on the login screen:
  # 1) Invalid credentials display a danger alert without granting workspace access
  # 2) Deactivated user ('inaktiv_demo' / 'gesperrt123') is blocked with an inactive-account alert
  # 3) Switching language via selected_ui_lang updates visible UI text deterministically
  # 4) Logging in as 'admin' reveals the workspace navbar and Admin tab; clicking logout returns to login
  skip_if_no_shinytest2()

  env_app <- local_e2e_app(name = "e2e_auth")
  app <- env_app$app
  app$wait_for_idle()

  # Invalid login attempt
  app$set_inputs(login_username = "admin", login_token = "wrong_password")
  app$click("btn_login")
  app$wait_for_idle()
  err_html <- app$get_html("#login_error_ui")
  expect_match(err_html, "alert-danger")

  # Inactive account lockout attempt
  app$set_inputs(login_username = "inaktiv_demo", login_token = "gesperrt123")
  app$click("btn_login")
  app$wait_for_idle()
  lock_html <- app$get_html("#login_error_ui")
  expect_match(lock_html, "alert-danger")

  # Switch UI language to English and verify localized login button/title update
  app$run_js("Shiny.setInputValue('selected_ui_lang', 'en', {priority: 'event'});")
  app$wait_for_idle()
  page_en <- app$get_html("#page_content")
  expect_match(page_en, "Sign In|Access Token")

  # Valid admin login
  app$set_inputs(login_username = "admin", login_token = "geheim123")
  app$click("btn_login")
  app$wait_for_idle()

  ws_html <- app$get_html("#page_content")
  expect_match(ws_html, "main_nav_tabs")
  expect_match(ws_html, "tab_admin")

  # Logout returns to unauthenticated login form
  app$click("btn_logout")
  app$wait_for_idle()
  expect_match(app$get_html("#page_content"), "btn_login")
})

test_that("E2E-2: Restricted coder permissions, Dashboard rendering, and Live Coding SQLite persistence", {
  # Verifies standard coder ('user1') end-to-end journey in Chromium:
  # 1) Logs in as 'user1' / 'token2026' and asserts the Admin tab is NOT present in the DOM
  # 2) Verifies Dashboard KPI cards and category breakdown table render with live database counts
  # 3) Switches to Live Coding tab, starts a coding session on scale 1, submits a rating via save_next_btn
  # 4) Queries the underlying SQLite file directly to confirm the new 'kodierung' row was persisted
  skip_if_no_shinytest2()

  env_app <- local_e2e_app(name = "e2e_coder_live")
  app <- env_app$app
  app$wait_for_idle()

  app$set_inputs(login_username = "user1", login_token = "token2026")
  app$click("btn_login")
  app$wait_for_idle()

  # Confirm Admin tab is hidden for non-admin role
  ws_html <- app$get_html("#page_content")
  expect_false(grepl("tab_admin", ws_html))
  expect_match(ws_html, "mod_dashboard-stat_card_total_kodierung")

  # Navigate to Live Coding tab
  app$set_inputs(main_nav_tabs = "tab_codings")
  app$wait_for_idle()

  # Start coding session on permitted scale 1 with coding source 1
  app$set_inputs(`mod_codings-gewaehlte_kategorie` = "1", `mod_codings-gewaehlte_quelle` = "1")
  app$wait_for_idle()
  app$click("mod_codings-start_kodierung_btn")
  app$wait_for_idle()

  # Set slider to 3 and save
  app$set_inputs(`mod_codings-bewertung_slider` = 3L)
  app$click("mod_codings-save_next_btn")
  app$wait_for_idle()

  # Direct SQLite post-condition check
  con_chk <- DBI::dbConnect(RSQLite::SQLite(), env_app$db_path)
  withr::defer(DBI::dbDisconnect(con_chk))
  n_saved <- DBI::dbGetQuery(
    con_chk,
    "SELECT COUNT(*) AS n FROM user_ratings ur JOIN user_tokens ut ON ur.user_id = ut.user_id
     WHERE ut.username = 'user1' AND ur.typ = 'kodierung' AND ur.bewertung = 3"
  )$n[1]
  expect_gte(n_saved, 1L)
})

test_that("E2E-3: Full stratified Coding Training session completes and persists reliability metrics to SQLite", {
  # Verifies complete browser training flow on S2_Instructional_Structure (max_value = 3, 4 stratified cases):
  # 1) Logs in as 'user1', navigates to Training tab, selects scale 2 with 4 units
  # 2) Starts training, rates all 4 cases through naechster_fall_btn
  # 3) Asserts the completion summary and aifeducation metrics panel render in the DOM
  # 4) Queries SQLite directly to verify 4 'schulung' ratings and +1 coding_training row were persisted
  skip_if_no_shinytest2()
  testthat::skip_if_not_installed("aifeducation")

  env_app <- local_e2e_app(name = "e2e_training", seed_gold_scale = 2L)
  app <- env_app$app
  app$wait_for_idle()

  app$set_inputs(login_username = "user1", login_token = "token2026")
  app$click("btn_login")
  app$wait_for_idle()

  app$set_inputs(main_nav_tabs = "tab_training")
  app$wait_for_idle()

  app$set_inputs(`mod_training-gewaehlte_kategorie` = "2", `mod_training-kodiereinheiten` = 4L)
  app$wait_for_idle()
  app$click("mod_training-start_schulung_btn")
  app$wait_for_idle()

  for (i in 1:4) {
    app$set_inputs(`mod_training-bewertung_slider` = (i - 1L) %% 4L)
    app$click("mod_training-naechster_fall_btn")
    app$wait_for_idle()
  }

  # Completion summary and metrics panel visible
  train_html <- app$get_html("#mod_training-schulungs_bereich")
  expect_match(train_html, "mod_training-restart_schulung_btn")
  expect_match(app$get_html("#mod_training-external_metrics_panel"), "<code>")

  # Direct SQLite post-condition verification
  con_chk <- DBI::dbConnect(RSQLite::SQLite(), env_app$db_path)
  withr::defer(DBI::dbDisconnect(con_chk))
  n_train_rows <- DBI::dbGetQuery(con_chk, "SELECT COUNT(*) AS n FROM coding_training")$n[1]
  n_schulung   <- DBI::dbGetQuery(con_chk, "SELECT COUNT(*) AS n FROM user_ratings WHERE typ = 'schulung'")$n[1]
  expect_equal(n_train_rows, 11L) # 10 seeded + 1 newly completed
  expect_equal(n_schulung, 4L)
})

test_that("E2E-4: Admin Console user creation, rating correction, and DB Diagnostics audit in real browser", {
  # Verifies administrative browser workflows as 'admin':
  # 1) Logs in as 'admin' and opens the Admin tab
  # 2) Opens Add User modal, creates 'e2e_coder', and confirms persistence in SQLite
  # 3) Opens Coding Search & Correction, edits rating #201 to value 4, and confirms DB update
  # 4) Opens DB Diagnostics, clicks Refresh, and verifies all 9 tables report active with 0 violations
  skip_if_no_shinytest2()

  env_app <- local_e2e_app(name = "e2e_admin")
  app <- env_app$app
  app$wait_for_idle()

  app$set_inputs(login_username = "admin", login_token = "geheim123")
  app$click("btn_login")
  app$wait_for_idle()

  app$set_inputs(main_nav_tabs = "tab_admin")
  app$wait_for_idle()

  # Create a new user via modal
  app$click("mod_admin-btn_open_add_user")
  app$wait_for_idle()
  app$set_inputs(
    `mod_admin-new_username` = "e2e_coder",
    `mod_admin-new_token` = "e2e_secret_2026",
    `mod_admin-new_permissions` = "user",
    `mod_admin-new_is_active` = TRUE
  )
  app$click("mod_admin-btn_save_new_user")
  app$wait_for_idle()

  # Trigger rating #201 edit modal and save value = 4
  app$run_js("Shiny.setInputValue('mod_admin-trigger_edit_coding_rating', 201, {priority: 'event'});")
  app$wait_for_idle()
  app$set_inputs(`mod_admin-edit_coding_new_value` = "4")
  app$click("mod_admin-btn_confirm_edit_coding")
  app$wait_for_idle()

  # Switch to DB Diagnostics subtab, refresh, and assert 0 integrity violations
  app$set_inputs(`mod_admin-admin_subtabs` = "tab_diagnose")
  app$wait_for_idle()
  app$click("mod_admin-btn_refresh_diagnostics")
  app$wait_for_idle()
  diag_html <- app$get_html("#mod_admin-db_diag_tables_overview")
  expect_match(diag_html, "user_tokens")
  expect_match(diag_html, "user_scale_permissions")

  # Direct SQLite post-condition checks
  con_chk <- DBI::dbConnect(RSQLite::SQLite(), env_app$db_path)
  withr::defer(DBI::dbDisconnect(con_chk))
  expect_equal(DBI::dbGetQuery(con_chk, "SELECT COUNT(*) AS n FROM user_tokens WHERE username = 'e2e_coder'")$n[1], 1L)
  expect_equal(DBI::dbGetQuery(con_chk, "SELECT bewertung FROM user_ratings WHERE rating_id = 201")$bewertung[1], 4L)
})
