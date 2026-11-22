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
      expect_match(user_page, "btn_start_tutorial")
      expect_match(user_page, "nav-tabs")
      expect_false(grepl("nav-pills", user_page))
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
      expect_match(admin_page, "btn_start_tutorial")

      # 8. On-demand Getting Started tutorial lifecycle (start, tab sync, language refresh, close)
      expect_false(tutorial_active())
      session$setInputs(btn_start_tutorial = 1L)
      session$flushReact()
      expect_true(tutorial_active())

      expect_no_error({
        session$setInputs(tutorial_nav_request = list(
          main_tab = "tab_admin",
          sub_tab_id = "mod_admin-admin_subtabs",
          sub_tab = "tab_sources",
          nonce = 1L
        ))
        session$flushReact()
      })

      session$setInputs(selected_ui_lang = "en")
      session$flushReact()
      expect_equal(current_lang(), "en")
      expect_true(tutorial_active())

      session$setInputs(tutorial_closed = 1L)
      session$flushReact()
      expect_false(tutorial_active())
    }
  )
})

test_that("university platform theme and bundled CSS define explicit offline tokens without external CDNs", {
  # Verifies deterministic local-vs-web styling contracts:
  # 1) coding_manager_theme() pins Bootstrap 5 with bootswatch = NULL and university navy/slate Sass tokens
  # 2) coding_manager_css() defines fixed 1280px shell geometry, underlined tabs, and offline system fonts
  # 3) Neither bundled CSS nor rendered coding_manager_ui() references remote CDN or Google Fonts URLs
  theme <- CodingManageR:::coding_manager_theme()
  expect_s3_class(theme, "bs_theme")
  expect_equal(as.character(bslib::theme_version(theme)), "5")

  if (exists("theme_bootswatch", envir = asNamespace("bslib"), inherits = FALSE)) {
    expect_null(bslib::theme_bootswatch(theme))
  }

  theme_vars <- bslib::bs_get_variables(theme, c("primary", "secondary", "body-bg"))
  expect_match(theme_vars[["primary"]], "^#1b365d$", ignore.case = TRUE)
  expect_match(theme_vars[["secondary"]], "^#526071$", ignore.case = TRUE)
  expect_match(theme_vars[["body-bg"]], "^#f6f8fa$", ignore.case = TRUE)

  css_html <- ui_to_html(CodingManageR:::coding_manager_css())
  expect_match(css_html, "<style")
  expect_match(css_html, "--cm-navy:\\s*#1B365D")
  expect_match(css_html, "--cm-bg-canvas:\\s*#F6F8FA")
  expect_match(css_html, "--cm-max-width:\\s*1280px")
  expect_match(css_html, "\\.cm-shell")
  expect_match(css_html, "\\.cm-header")
  expect_match(css_html, "\\.cm-kpi-card")
  expect_match(css_html, "\\.cm-case-text")
  expect_match(css_html, "\\.cm-session-banner")
  expect_match(css_html, "\\.nav-tabs")
  expect_false(grepl("https?://|fonts\\.googleapis|@import\\s+url", css_html, ignore.case = TRUE))

  root_html <- ui_to_html(coding_manager_ui())
  expect_match(root_html, "cm-root-container")
  expect_match(root_html, "cm-app")
  expect_match(root_html, "cm-shell")
  expect_match(root_html, "cm_start_tutorial")
  expect_false(grepl("fonts\\.googleapis", root_html, ignore.case = TRUE))
})

test_that("getting started tutorial builds role-aware bilingual tracks and offline spotlight assets", {
  # Verifies:
  # 1) Admin track contains all 3 workflow parts (12 steps) across setup, coding, and admin tools
  # 2) Standard coder track contains only Part 2 (4 steps) with zero admin tabs exposed
  # 3) English and German localization produce distinct non-empty titles, bodies, and tips
  # 4) Bundled spotlight CSS/JS assets contain zero external CDN URLs
  admin_en <- CodingManageR:::build_tutorial_payload(role = "admin", lang = "en")
  expect_true(admin_en$is_admin)
  expect_equal(admin_en$total_parts, 3L)
  expect_length(admin_en$parts, 3L)
  expect_length(admin_en$steps, 12L)
  admin_tabs <- vapply(admin_en$steps, function(s) s$main_tab, character(1))
  expect_true(all(c("tab_admin", "tab_training", "tab_dashboard", "tab_codings") %in% admin_tabs))

  admin_de <- CodingManageR:::build_tutorial_payload(role = "admin", lang = "de", preserve_step = TRUE)
  expect_true(admin_de$preserve_step)
  expect_equal(admin_de$lang, "de")
  expect_length(admin_de$steps, 12L)
  expect_false(identical(admin_en$steps[[1]]$title, admin_de$steps[[1]]$title))
  expect_true(all(vapply(admin_de$steps, function(s) nzchar(s$title) && nzchar(s$body) && nzchar(s$tip), logical(1))))

  coder_en <- CodingManageR:::build_tutorial_payload(role = "user", lang = "en")
  expect_false(coder_en$is_admin)
  expect_equal(coder_en$total_parts, 1L)
  expect_length(coder_en$steps, 4L)
  coder_tabs <- vapply(coder_en$steps, function(s) s$main_tab, character(1))
  expect_false("tab_admin" %in% coder_tabs)
  expect_setequal(unique(coder_tabs), c("tab_dashboard", "tab_training", "tab_codings"))

  coder_de <- CodingManageR:::build_tutorial_payload(role = "user", lang = "de")
  expect_length(coder_de$steps, 4L)
  expect_false(identical(coder_en$steps[[1]]$title, coder_de$steps[[1]]$title))

  assets_html <- ui_to_html(CodingManageR:::coding_manager_tutorial_assets())
  expect_match(assets_html, "cm-tour-spotlight")
  expect_match(assets_html, "cm-tour-tooltip")
  expect_match(assets_html, "cm_start_tutorial")
  expect_match(assets_html, "cm_close_tutorial")
  expect_match(assets_html, "tutorial_nav_request")
  expect_false(grepl("https?://|fonts\\.googleapis|@import\\s+url", assets_html, ignore.case = TRUE))
})

test_that("coding_manager_server initializes self-managed SQLite pools, empty catalogs, and error fallbacks cleanly", {
  # Verifies:
  # 1) Self-managed SQLite startup when CodingManageR.db_con is NULL (pool creation, table setup,
  #    seed_empty = TRUE, and onSessionEnded pool teardown) via options and direct-argument wrapper
  # 2) Empty scale_definitions catalog returning zero-length kategorie_spalten() & kategorien_liste()
  # 3) Unauthenticated tutorial guards and dashboard subtab tutorial_nav_request routing
  # 4) Direct db_con argument wrapper and scales_catalog() tryCatch error fallback when table is missing
  tmp_sqlite <- tempfile(pattern = "cm_shell_self_", fileext = ".sqlite")
  tmp_sqlite_direct <- tempfile(pattern = "cm_shell_direct_", fileext = ".sqlite")
  withr::defer(unlink(c(
    tmp_sqlite, paste0(tmp_sqlite, "-wal"), paste0(tmp_sqlite, "-shm"),
    tmp_sqlite_direct, paste0(tmp_sqlite_direct, "-wal"), paste0(tmp_sqlite_direct, "-shm")
  ), force = TRUE))

  withr::with_options(
    list(
      CodingManageR.db_con = NULL,
      CodingManageR.db_config = NULL,
      CodingManageR.sqlite_path = tmp_sqlite,
      CodingManageR.seed_empty = TRUE
    ),
    withr::with_envvar(
      c(MYSQL_HOST = "", MYSQL_DB_HOST = "", MYSQL_USER = "", MYSQL_DB_USER = "", SQLITE_PATH = ""),
      {
        suppressMessages(
          shiny::testServer(
            coding_manager_server,
            {
              session$flushReact()
              expect_true(owns_connection)
              expect_true(file.exists(tmp_sqlite))

              # Unauthenticated tutorial triggers return silently
              session$setInputs(btn_start_tutorial = 1L, tutorial_nav_request = list(main_tab = "tab_dashboard"))
              session$flushReact()
              expect_false(tutorial_active())

              # Empty seed creates 0 scales -> catalog reactives return character(0)
              expect_length(kategorie_spalten(), 0L)
              expect_length(kategorien_liste(), 0L)

              # Sign in as seeded admin and test dashboard subtab tutorial routing + non-list guard
              session$setInputs(login_username = "admin", login_token = "geheim123", btn_login = 1L)
              session$flushReact()
              expect_true(logged_in())

              expect_no_error({
                session$setInputs(tutorial_nav_request = "not_a_list")
                session$flushReact()
                session$setInputs(tutorial_nav_request = list(
                  main_tab = "tab_dashboard",
                  sub_tab_id = "mod_dashboard-dashboard_tabs",
                  sub_tab = "tab_scales_overview",
                  nonce = 2L
                ))
                session$flushReact()
              })
            }
          )
        )

        # Direct sqlite_path & seed_empty parameter path via wrapper (no args passed to testServer)
        suppressMessages(
          shiny::testServer(
            function(input, output, session) {
              coding_manager_server(input, output, session, sqlite_path = tmp_sqlite_direct, seed_empty = TRUE)
            },
            {
              session$flushReact()
              expect_true(file.exists(tmp_sqlite_direct))
            }
          )
        )
      }
    )
  )

  # Missing scale_definitions table hits tryCatch error fallback (both direct db_con wrapper and options)
  con_broken <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_broken))

  expect_no_error(
    shiny::testServer(
      function(input, output, session) {
        coding_manager_server(input, output, session, db_con = con_broken)
      },
      {
        session$flushReact()
      }
    )
  )

  withr::with_options(
    list(CodingManageR.db_con = con_broken),
    shiny::testServer(
      coding_manager_server,
      {
        session$flushReact()
        expect_false(owns_connection)
        cat_df <- scales_catalog()
        expect_s3_class(cat_df, "data.frame")
        expect_equal(nrow(cat_df), 0L)
        expect_length(kategorie_spalten(), 0L)
        expect_length(kategorien_liste(), 0L)
      }
    )
  )
})


