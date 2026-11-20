# ==============================================================================
# Application Shell & Authentication Server Tests
# Covers: R/app.R (coding_manager_ui, coding_manager_server, run_app)
# ==============================================================================

test_that("coding_manager_ui and run_app construct valid Shiny UI and shiny.appobj objects", {
  # Verifies coding_manager_ui() emits the root page_content container and browser locale script,
  # and run_app() returns a valid shiny.appobj configured with an isolated temporary SQLite path.
  ui_html <- ui_to_html(coding_manager_ui())
  expect_match(ui_html, "page_content")
  expect_match(ui_html, "browser_lang")

  tmp_sqlite <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(c(tmp_sqlite, paste0(tmp_sqlite, "-wal"), paste0(tmp_sqlite, "-shm")), force = TRUE))

  app_obj <- run_app(sqlite_path = tmp_sqlite, empty = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("coding_manager_server enforces authentication, inactive lockout, RBAC navigation, i18n, refresh, and logout", {
  # Verifies the complete root server lifecycle via shiny::testServer():
  # 1) Unauthenticated initial render displays the login form
  # 2) Empty credentials, invalid token, and inactive account ('inaktiv_demo') render distinct error alerts
  # 3) Standard user ('user1') login renders Dashboard, Training, and Codings tabs WITHOUT the Admin tab
  # 4) Browser locale auto-detection sets language until overridden by manual selected_ui_lang
  # 5) Header refresh button increments data_trigger and scales_trigger
  # 6) Logout resets authentication reactives and returns to the login card
  # 7) Administrator ('admin') login renders the Admin tab alongside all coder tabs
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  withr::local_options(list(CodingManageR.db_con = con))

  shiny::testServer(
    coding_manager_server,
    {
      # 1. Initial unauthenticated screen shows login controls
      session$flushReact()
      expect_false(logged_in())
      expect_match(ui_to_html(output$page_content), "login_username")
      expect_match(ui_to_html(output$page_content), "btn_login")

      # 2a. Missing credentials error
      session$setInputs(login_username = "", login_token = "", btn_login = 1L)
      session$flushReact()
      expect_false(logged_in())
      expect_equal(login_error_key(), "missing")
      expect_match(ui_to_html(output$login_error_ui), "alert-warning")

      # 2b. Invalid credentials error
      session$setInputs(login_username = "user1", login_token = "wrong_pass", btn_login = 2L)
      session$flushReact()
      expect_false(logged_in())
      expect_equal(login_error_key(), "invalid")
      expect_match(ui_to_html(output$login_error_ui), "alert-danger")

      # 2c. Inactive account lockout ('inaktiv_demo' / 'gesperrt123')
      session$setInputs(login_username = "inaktiv_demo", login_token = "gesperrt123", btn_login = 3L)
      session$flushReact()
      expect_false(logged_in())
      expect_equal(login_error_key(), "inactive")
      expect_match(ui_to_html(output$login_error_ui), "alert-danger")

      # 3. Browser locale auto-detection followed by manual override
      session$setInputs(browser_lang = "en-US")
      session$flushReact()
      expect_equal(current_lang(), "en")

      session$setInputs(selected_ui_lang = "de")
      session$flushReact()
      expect_equal(current_lang(), "de")
      expect_true(manual_lang_set())

      # Subsequent browser_lang events must not override manual selection
      session$setInputs(browser_lang = "en-GB")
      session$flushReact()
      expect_equal(current_lang(), "de")

      # 4. Standard user login ('user1' / 'token2026'): Admin tab MUST NOT be rendered
      session$setInputs(login_username = "user1", login_token = "token2026", btn_login = 4L)
      session$flushReact()
      expect_true(logged_in())
      expect_equal(current_user(), "user1")
      expect_equal(current_role(), "user")
      expect_length(kategorie_spalten(), 3L)
      expect_length(kategorien_liste(), 3L)

      user_page <- ui_to_html(output$page_content)
      expect_match(user_page, "tab_dashboard")
      expect_match(user_page, "tab_training")
      expect_match(user_page, "tab_codings")
      expect_false(grepl("tab_admin", user_page))

      # 5. Header Refresh button bumps both triggers
      prev_dt <- data_trigger()
      prev_st <- scales_trigger()
      session$setInputs(btn_refresh_view = 1L)
      session$flushReact()
      expect_equal(data_trigger(), prev_dt + 1L)
      expect_equal(scales_trigger(), prev_st + 1L)

      # 6. Logout clears session state and returns to login screen
      session$setInputs(btn_logout = 1L)
      session$flushReact()
      expect_false(logged_in())
      expect_null(current_user())
      expect_null(current_user_id())
      expect_match(ui_to_html(output$page_content), "btn_login")

      # 7. Admin login ('admin' / 'geheim123') exposes the Admin tab
      session$setInputs(login_username = "admin", login_token = "geheim123", btn_login = 5L)
      session$flushReact()
      expect_true(logged_in())
      expect_equal(current_role(), "admin")
      admin_page <- ui_to_html(output$page_content)
      expect_match(admin_page, "tab_admin")
      expect_match(admin_page, "mod_admin")
    }
  )
})
