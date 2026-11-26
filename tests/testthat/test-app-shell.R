# Note: This file was generated with the assistance of AI.

# ==============================================================================
# Application Shell & Authentication Server Tests
# Covers: R/app.R (coding_manager_ui, coding_manager_server, run_app_local,
#         run_app_server, run_app) and inst/shiny/app.R
# ==============================================================================

test_that("coding_manager_ui and run_app construct valid Shiny UI and shiny.appobj objects", {
  # Verifies coding_manager_ui() emits the root page_content container and browser locale script,
  # and run_app() returns a valid shiny.appobj configured with an isolated temporary SQLite path.
  ui_html <- ui_to_html(coding_manager_ui())
  expect_match(ui_html, "page_content")
  expect_match(ui_html, "browser_lang")
  expect_match(ui_html, "shiny:connected")
  expect_match(ui_html, "cm-login-card")

  tmp_sqlite <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(c(tmp_sqlite, paste0(tmp_sqlite, "-wal"), paste0(tmp_sqlite, "-shm")), force = TRUE))

  withr::local_options(list(shiny.maxRequestSize = NULL))

  app_obj <- run_app(sqlite_path = tmp_sqlite, empty = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
  expect_true(is.function(app_obj$onStart))

  dereg_default <- app_obj$onStart()
  if (is.function(dereg_default)) withr::defer(dereg_default())
  expect_equal(getOption("shiny.maxRequestSize"), 3 * 1024^3)

  custom_hook_called <- FALSE
  app_custom <- run_app(
    sqlite_path = tmp_sqlite,
    empty = FALSE,
    max_upload_size_gb = 1.5,
    onStart = function() {
      custom_hook_called <<- TRUE
    }
  )
  dereg_custom <- app_custom$onStart()
  if (is.function(dereg_custom)) withr::defer(dereg_custom())
  expect_equal(getOption("shiny.maxRequestSize"), 1.5 * 1024^3)
  expect_true(custom_hook_called)

  options(shiny.maxRequestSize = 12345)
  app_null <- run_app(sqlite_path = tmp_sqlite, empty = FALSE, max_upload_size_gb = NULL)
  app_null$onStart()
  expect_equal(getOption("shiny.maxRequestSize"), 12345)
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
      # 1. Initial unauthenticated screen shows login controls, bundled hex logo, and dynamic package version
      session$flushReact()
      expect_false(logged_in())
      login_html <- ui_to_html(output$page_content)
      pkg_ver <- CodingManageR:::coding_manager_version()
      expect_true(nzchar(pkg_ver))
      expect_match(login_html, "login_username")
      expect_match(login_html, "btn_login")
      expect_match(login_html, "cm-assets/logo\\.png")
      expect_match(login_html, "cm-brand-logo--login")
      expect_match(login_html, "cm-login-version")
      expect_match(login_html, paste0("CodingManageR Version ", gsub("\\.", "\\\\.", pkg_ver)))

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
      expect_match(user_page, "cm-assets/logo\\.png")
      expect_match(user_page, "cm-brand-logo")
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

test_that("run_app(empty = FALSE) never writes sample data into an existing SQLite database file", {
  # Verifies end-to-end via run_app()$serverFuncSource() and shiny::testServer():
  # 1) Existing SQLite file initialized with empty = TRUE (+ custom scale) emits a warning
  #    and receives 0 sample rows across all 9 tables when reopened with run_app(empty = FALSE)
  # 2) Existing schema-only SQLite file with 0 rows in all 9 tables also emits a warning
  #    and remains at 0 rows across all 9 tables under run_app(empty = FALSE)
  # 3) Brand-new, non-existent SQLite file under run_app(empty = FALSE) seeds the full
  #    demonstration dataset cleanly without warning
  existing_sqlite <- tempfile(pattern = "cm_existing_study_", fileext = ".sqlite")
  blank_existing_sqlite <- tempfile(pattern = "cm_existing_blank_", fileext = ".sqlite")
  fresh_demo_sqlite <- tempfile(pattern = "cm_fresh_demo_", fileext = ".sqlite")

  withr::defer(unlink(c(
    existing_sqlite, paste0(existing_sqlite, "-wal"), paste0(existing_sqlite, "-shm"),
    blank_existing_sqlite, paste0(blank_existing_sqlite, "-wal"), paste0(blank_existing_sqlite, "-shm"),
    fresh_demo_sqlite, paste0(fresh_demo_sqlite, "-wal"), paste0(fresh_demo_sqlite, "-shm")
  ), force = TRUE))

  all_tables <- c(
    "user_tokens", "scale_definitions", "sources",
    "user_source_permissions", "user_coding_source_permissions",
    "user_scale_permissions", "texts", "user_ratings", "coding_training"
  )

  count_all_tables <- function(db_file) {
    con_chk <- DBI::dbConnect(RSQLite::SQLite(), db_file)
    on.exit(DBI::dbDisconnect(con_chk), add = TRUE)
    vapply(all_tables, function(tbl) {
      as.integer(DBI::dbGetQuery(con_chk, sprintf("SELECT COUNT(*) FROM %s", tbl))[1, 1])
    }, integer(1))
  }

  # Prepare Scenario 1: existing SQLite study created with empty = TRUE + 1 custom scale
  con_setup <- DBI::dbConnect(RSQLite::SQLite(), existing_sqlite)
  create_all_tables(con_setup, verbose = FALSE)
  suppressMessages(seed_initial_data(con_setup, empty = TRUE))
  DBI::dbExecute(
    con_setup,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc) VALUES (1, 'Custom', 'Custom_Study_Scale', 3, 'Own scale')"
  )
  DBI::dbDisconnect(con_setup)
  expect_true(file.exists(existing_sqlite))

  # Prepare Scenario 2: existing SQLite file on disk with empty schema (0 rows in all 9 tables)
  con_blank <- DBI::dbConnect(RSQLite::SQLite(), blank_existing_sqlite)
  create_all_tables(con_blank, verbose = FALSE)
  DBI::dbDisconnect(con_blank)
  expect_true(file.exists(blank_existing_sqlite))

  withr::with_options(
    list(
      CodingManageR.db_con = NULL,
      CodingManageR.db_config = NULL,
      CodingManageR.sqlite_path = NULL,
      CodingManageR.seed_empty = NULL
    ),
    withr::with_envvar(
      c(MYSQL_HOST = "", MYSQL_DB_HOST = "", MYSQL_USER = "", MYSQL_DB_USER = "", SQLITE_PATH = ""),
      {
        # Scenario 1: run_app(empty = FALSE) on existing study SQLite file -> warning + 0 sample rows
        app_existing <- run_app(sqlite_path = existing_sqlite, empty = FALSE)
        expect_warning(
          suppressMessages(
            shiny::testServer(
              app_existing$serverFuncSource(),
              {
                session$flushReact()
              }
            )
          ),
          regexp = "Existing SQLite database file detected"
        )

        counts_1 <- count_all_tables(existing_sqlite)
        expect_equal(counts_1[["user_tokens"]], 2L)
        expect_equal(counts_1[["scale_definitions"]], 1L)
        expect_equal(counts_1[["sources"]], 0L)
        expect_equal(counts_1[["user_source_permissions"]], 0L)
        expect_equal(counts_1[["user_coding_source_permissions"]], 0L)
        expect_equal(counts_1[["user_scale_permissions"]], 0L)
        expect_equal(counts_1[["texts"]], 0L)
        expect_equal(counts_1[["user_ratings"]], 0L)
        expect_equal(counts_1[["coding_training"]], 0L)

        con_verify <- DBI::dbConnect(RSQLite::SQLite(), existing_sqlite)
        users_after <- tryCatch(
          DBI::dbGetQuery(con_verify, "SELECT username FROM user_tokens ORDER BY username ASC")$username,
          finally = NULL
        )
        scales_after <- tryCatch(
          DBI::dbGetQuery(con_verify, "SELECT name FROM scale_definitions")$name,
          finally = DBI::dbDisconnect(con_verify)
        )
        expect_equal(users_after, c("admin", "historic_user"))
        expect_equal(scales_after, "Custom_Study_Scale")

        # Scenario 2: run_app(empty = FALSE) on existing 0-row schema-only SQLite file -> warning + 0 rows
        app_blank <- run_app(sqlite_path = blank_existing_sqlite, empty = FALSE)
        expect_warning(
          suppressMessages(
            shiny::testServer(
              app_blank$serverFuncSource(),
              {
                session$flushReact()
              }
            )
          ),
          regexp = "Existing SQLite database file detected"
        )

        counts_2 <- count_all_tables(blank_existing_sqlite)
        expect_true(all(counts_2 == 0L))

        # Scenario 3: Positive control - brand-new SQLite file with run_app(empty = FALSE) seeds demo data
        expect_false(file.exists(fresh_demo_sqlite))
        app_fresh <- run_app(sqlite_path = fresh_demo_sqlite, empty = FALSE)
        expect_no_warning(
          suppressMessages(
            shiny::testServer(
              app_fresh$serverFuncSource(),
              {
                session$flushReact()
              }
            )
          )
        )

        counts_3 <- count_all_tables(fresh_demo_sqlite)
        expect_equal(counts_3[["user_tokens"]], 8L)
        expect_equal(counts_3[["scale_definitions"]], 3L)
        expect_equal(counts_3[["sources"]], 4L)
        expect_equal(counts_3[["texts"]], 34L)
        expect_equal(counts_3[["user_ratings"]], 34L)
        expect_equal(counts_3[["coding_training"]], 10L)
      }
    )
  )
})

test_that("run_app_local exposes minimal parameters and stays isolated in SQLite even when MYSQL_* env vars exist", {
  # Verifies:
  # 1) Formal parameter list is strictly limited to (sqlite_path = NULL, empty = TRUE)
  # 2) Returns a valid shiny.appobj with 3 GB shiny.maxRequestSize managed via onStart
  # 3) Ignores ambient MYSQL_HOST / MYSQL_USER environment variables and uses SQLite exclusively
  # 4) Protects existing SQLite files when empty = FALSE
  fmls <- formals(run_app_local)
  expect_identical(names(fmls), c("sqlite_path", "empty"))
  expect_null(fmls$sqlite_path)
  expect_true(isTRUE(fmls$empty))

  tmp_local <- tempfile(pattern = "cm_local_isolated_", fileext = ".sqlite")
  withr::defer(unlink(c(tmp_local, paste0(tmp_local, "-wal"), paste0(tmp_local, "-shm")), force = TRUE))

  withr::local_options(list(
    shiny.maxRequestSize = NULL,
    CodingManageR.db_con = NULL,
    CodingManageR.db_config = NULL,
    CodingManageR.sqlite_path = NULL,
    CodingManageR.seed_empty = NULL
  ))

  app_local <- run_app_local(sqlite_path = tmp_local, empty = TRUE)
  expect_s3_class(app_local, "shiny.appobj")
  dereg <- app_local$onStart()
  if (is.function(dereg)) withr::defer(dereg())
  expect_equal(getOption("shiny.maxRequestSize"), 3 * 1024^3)

  # Even with MYSQL_HOST and MYSQL_USER set to an unreachable host, run_app_local() uses SQLite
  withr::with_envvar(
    c(
      MYSQL_HOST = "192.0.2.1",
      MYSQL_PORT = "3306",
      MYSQL_USER = "ambient_user",
      MYSQL_PASSWORD = "ambient_password",
      MYSQL_DBNAME = "ambient_db",
      SQLITE_PATH = ""
    ),
    {
      expect_no_error(
        suppressMessages(
          shiny::testServer(
            app_local$serverFuncSource(),
            {
              session$flushReact()
            }
          )
        )
      )
      expect_true(file.exists(tmp_local))

      con_local <- DBI::dbConnect(RSQLite::SQLite(), tmp_local)
      expected_tables <- c(
        "user_tokens", "scale_definitions", "sources",
        "user_source_permissions", "user_coding_source_permissions",
        "user_scale_permissions", "texts", "user_ratings", "coding_training"
      )
      local_tables <- tryCatch(
        DBI::dbListTables(con_local),
        finally = NULL
      )
      local_users <- tryCatch(
        DBI::dbGetQuery(con_local, "SELECT username FROM user_tokens ORDER BY username ASC")$username,
        finally = DBI::dbDisconnect(con_local)
      )
      expect_true(all(expected_tables %in% local_tables))
      expect_equal(local_users, c("admin", "historic_user"))

      # Reopening the existing SQLite file with empty = FALSE protects existing data
      app_local_demo <- run_app_local(sqlite_path = tmp_local, empty = FALSE)
      expect_warning(
        suppressMessages(
          shiny::testServer(
            app_local_demo$serverFuncSource(),
            {
              session$flushReact()
            }
          )
        ),
        regexp = "Existing SQLite database file detected"
      )
    }
  )
})

test_that("run_app_server enforces strict MySQL/MariaDB mode without SQLite fallback and reads .Renviron options", {
  # Verifies:
  # 1) Returns a valid shiny.appobj and honors CODINGMANAGER_MAX_UPLOAD_GB & CODINGMANAGER_EMPTY
  # 2) Explicit arguments override .Renviron values and invalid env values warn and fall back cleanly
  # 3) Missing MySQL/MariaDB credentials stop with an actionable error and NEVER create a SQLite file
  #    (even if SQLITE_PATH is set in the environment)
  forbidden_sqlite <- tempfile(pattern = "cm_server_no_sqlite_", fileext = ".sqlite")
  withr::defer(unlink(c(forbidden_sqlite, paste0(forbidden_sqlite, "-wal"), paste0(forbidden_sqlite, "-shm")), force = TRUE))

  withr::local_options(list(
    shiny.maxRequestSize = NULL,
    CodingManageR.db_con = NULL,
    CodingManageR.db_config = NULL,
    CodingManageR.sqlite_path = NULL,
    CodingManageR.seed_empty = NULL
  ))

  # Default env-driven upload limit (3 GB) and custom CODINGMANAGER_MAX_UPLOAD_GB
  withr::with_envvar(c(CODINGMANAGER_MAX_UPLOAD_GB = "2.5", CODINGMANAGER_EMPTY = "FALSE"), {
    app_env <- run_app_server()
    expect_s3_class(app_env, "shiny.appobj")
    dereg_env <- app_env$onStart()
    if (is.function(dereg_env)) withr::defer(dereg_env())
    expect_equal(getOption("shiny.maxRequestSize"), 2.5 * 1024^3)

    # Explicit argument overrides env var
    app_explicit <- run_app_server(max_upload_size_gb = 4)
    dereg_exp <- app_explicit$onStart()
    if (is.function(dereg_exp)) withr::defer(dereg_exp())
    expect_equal(getOption("shiny.maxRequestSize"), 4 * 1024^3)
  })

  # CODINGMANAGER_MAX_UPLOAD_GB = "null" leaves shiny.maxRequestSize untouched
  options(shiny.maxRequestSize = 54321)
  withr::with_envvar(c(CODINGMANAGER_MAX_UPLOAD_GB = "null"), {
    app_null_env <- run_app_server()
    app_null_env$onStart()
    expect_equal(getOption("shiny.maxRequestSize"), 54321)
  })

  # Invalid env values emit clear warnings and fall back to safe defaults
  withr::with_envvar(c(CODINGMANAGER_EMPTY = "maybe", CODINGMANAGER_MAX_UPLOAD_GB = ""), {
    expect_warning(
      app_invalid_empty <- run_app_server(),
      regexp = "CODINGMANAGER_EMPTY"
    )
    expect_s3_class(app_invalid_empty, "shiny.appobj")
  })

  withr::with_envvar(c(CODINGMANAGER_EMPTY = "", CODINGMANAGER_MAX_UPLOAD_GB = "-5"), {
    expect_warning(
      app_invalid_upload <- run_app_server(),
      regexp = "CODINGMANAGER_MAX_UPLOAD_GB"
    )
    dereg_inv <- app_invalid_upload$onStart()
    if (is.function(dereg_inv)) withr::defer(dereg_inv())
    expect_equal(getOption("shiny.maxRequestSize"), 3 * 1024^3)
  })

  # Strict MySQL enforcement: missing credentials must stop cleanly without creating SQLite
  withr::with_envvar(
    c(
      MYSQL_HOST = "",
      MYSQL_DB_HOST = "",
      MYSQL_USER = "",
      MYSQL_DB_USER = "",
      SQLITE_PATH = forbidden_sqlite
    ),
    {
      app_srv <- run_app_server()
      expect_error(
        shiny::testServer(
          app_srv$serverFuncSource(),
          {
            session$flushReact()
          }
        ),
        regexp = "MYSQL_HOST.*SQLite fallback is disabled"
      )
      expect_false(file.exists(forbidden_sqlite))
    }
  )
})

test_that("bundled inst/shiny/app.R is a minimal set-once entrypoint calling run_app_server()", {
  # Verifies that the packaged Shiny Server entrypoint parses cleanly, invokes
  # CodingManageR::run_app_server() with no hardcoded arguments, and does not
  # reference SQLITE_PATH or sqlite_path.
  app_r_path <- system.file("shiny", "app.R", package = "CodingManageR")
  if (!nzchar(app_r_path)) {
    app_r_path <- testthat::test_path("..", "..", "inst", "shiny", "app.R")
  }
  expect_true(file.exists(app_r_path))

  exprs <- parse(file = app_r_path)
  expect_gte(length(exprs), 2L)

  src <- paste(readLines(app_r_path, warn = FALSE), collapse = "\n")
  expect_match(src, "CodingManageR::run_app_server\\(\\)")
  expect_false(grepl("SQLITE_PATH|sqlite_path|lokale_datenbank", src))
})

test_that("login screen renders dynamic CodingManageR version under subtitle exclusively before sign-in", {
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  withr::local_options(list(CodingManageR.db_con = con))

  ver <- CodingManageR:::coding_manager_version()
  expect_match(ver, "^[0-9]+\\.[0-9]+")

  shiny::testServer(
    coding_manager_server,
    {
      # Unauthenticated DE screen: version appears directly after subtitle inside login body
      session$flushReact()
      de_login <- ui_to_html(output$page_content)
      expect_match(de_login, "cm-login-version")
      expect_match(de_login, paste0("CodingManageR Version ", gsub("\\.", "\\\\.", ver)))
      sub_pos <- regexpr("Authentifizierung", de_login, fixed = TRUE)[1]
      ver_pos <- regexpr("cm-login-version", de_login, fixed = TRUE)[1]
      usr_pos <- regexpr("login_username", de_login, fixed = TRUE)[1]
      expect_true(sub_pos > 0L && ver_pos > sub_pos && usr_pos > ver_pos)

      # Unauthenticated EN screen preserves version label cleanly
      session$setInputs(selected_ui_lang = "en")
      session$flushReact()
      en_login <- ui_to_html(output$page_content)
      expect_match(en_login, "Authentication via relational database")
      expect_match(en_login, paste0("CodingManageR Version ", gsub("\\.", "\\\\.", ver)))

      # After admin sign-in: version badge is NOT shown in authenticated workspace, and Admin tab uses FA icon
      session$setInputs(login_username = "admin", login_token = "geheim123", btn_login = 1L)
      session$flushReact()
      expect_true(logged_in())
      authed_html <- ui_to_html(output$page_content)
      expect_false(grepl("cm-login-version", authed_html, fixed = TRUE))
      expect_match(authed_html, "fa-wrench")
      expect_match(authed_html, "Admin Console")
    }
  )
})




