# Note: This file was generated with the assistance of AI.

#' CodingManageR Shiny Application Entrypoint
#'
#' Provides CRAN-compliant exported functions [coding_manager_ui()],
#' [coding_manager_server()], [run_app_local()], [run_app_server()], and
#' [run_app()] to launch the qualitative coding and training portal either
#' locally (SQLite) or on a multi-user server (MySQL/MariaDB) without top-level
#' side effects.
#'
#' @name CodingManageR-app
#' @keywords internal
NULL

#' Resolve Active CodingManageR Package Version
#'
#' Dynamically retrieves the installed `CodingManageR` package version via
#' `utils::packageVersion()`, falling back to reading a local `DESCRIPTION`
#' file during source/development sessions so the login screen never relies on
#' hardcoded release strings.
#'
#' @return Character scalar package version (e.g., `"1.0.0"`).
#' @keywords internal
#' @noRd
coding_manager_version <- function() {
  ver <- tryCatch(
    as.character(utils::packageVersion("CodingManageR")),
    error = function(e) ""
  )
  if (nzchar(ver)) {
    return(ver)
  }

  desc_path <- "DESCRIPTION"
  if (file.exists(desc_path)) {
    dcf <- tryCatch(read.dcf(desc_path, fields = "Version"), error = function(e) NULL)
    if (!is.null(dcf) && length(dcf) > 0L && !is.na(dcf[1, 1]) && nzchar(trimws(dcf[1, 1]))) {
      return(trimws(as.character(dcf[1, 1])))
    }
  }

  "dev"
}

#' Build the Root Shiny UI for CodingManageR
#'
#' Constructs the top-level Bootstrap 5 / `bslib` page shell for CodingManageR,
#' including browser language auto-detection, interactive spotlight tutorial
#' assets, and the reactive `page_content` outlet that switches between the
#' sign-in screen and the authenticated multi-tab workspace.
#'
#' @param lang Default initial language code (`"de"` or `"en"`). Defaults to `"de"`.
#'
#' @return A Shiny UI tag list suitable for `shiny::shinyApp()`.
#' @examples
#' \dontrun{
#' ui <- coding_manager_ui(lang = "en")
#' }
#' @export
coding_manager_ui <- function(lang = "de") {
  init_lang <- detect_browser_language(lang, fallback = "de")
  coding_manager_register_assets()

  fluidPage(
    theme = coding_manager_theme(),
    title = "CodingManageR",
    tags$head(
      tags$title("CodingManageR"),
      tags$link(rel = "icon", type = "image/png", href = "cm-assets/favicon.png"),
      coding_manager_css(),
      coding_manager_tutorial_assets(),
      tags$script(HTML("
        (function() {
          function sendBrowserLang() {
            if (window.Shiny && typeof Shiny.setInputValue === 'function') {
              var navLang = (navigator.language || navigator.userLanguage || 'en');
              Shiny.setInputValue('browser_lang', navLang, {priority: 'event'});
            }
          }
          document.addEventListener('shiny:connected', sendBrowserLang);
          document.addEventListener('DOMContentLoaded', function() {
            if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) {
              sendBrowserLang();
            }
          });
          document.addEventListener('keydown', function(evt) {
            if (evt.key !== 'Enter') return;
            var target = evt.target;
            if (!target || !target.closest || !target.closest('.cm-login-card')) return;
            if (target.id === 'login_username' || target.id === 'login_token') {
              var loginBtn = document.getElementById('btn_login');
              if (loginBtn) {
                evt.preventDefault();
                loginBtn.click();
              }
            }
          });
        })();
      "))
    ),
    div(
      class = "container-fluid cm-root-container",
      div(
        class = "cm-app",
        div(
          class = "cm-shell",
          `data-initial-lang` = init_lang,
          uiOutput("page_content")
        )
      )
    )
  )
}

#' Shiny Server Function for CodingManageR
#'
#' Manages database connectivity, user authentication (including inactive
#' account lockout), bilingual UI localization (`de`/`en`), reactive scale
#' catalog synchronization, the on-demand role-aware Getting Started spotlight
#' tutorial (including automatic tab/subtab switching), and routing to the
#' Dashboard, Coding Training, Live Coding, and Admin Console Shiny modules.
#'
#' @param input Standard Shiny server `input` object.
#' @param output Standard Shiny server `output` object.
#' @param session Standard Shiny `session` object.
#' @param db_con Optional pre-existing DBI connection or `pool::dbPool` object.
#'   When supplied (e.g., during `shiny::testServer()` tests), the caller retains
#'   ownership and the connection is not automatically closed on session end.
#' @param sqlite_path Optional path to a local SQLite database file used when
#'   `db_con` is `NULL`.
#' @param config Optional named database configuration list passed to [get_db_pool()].
#' @param seed_empty Logical passed to [seed_initial_data()] on first startup.
#'   Defaults to `TRUE` (minimal admin account) unless overridden via options or argument.
#'   When `FALSE`, demonstration data is only seeded into a brand-new, empty
#'   database; existing SQLite files or populated databases are protected and
#'   emit a warning instead.
#' @param fallback_to_sqlite Logical passed to [get_db_pool()]. Defaults to `TRUE`.
#'   Set to `FALSE` (used by [run_app_server()]) to enforce strict MySQL/MariaDB
#'   connectivity without silently falling back to a local SQLite file.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' shiny::shinyApp(ui = coding_manager_ui(), server = coding_manager_server)
#' }
#' @export
coding_manager_server <- function(input, output, session,
                                  db_con = NULL,
                                  sqlite_path = NULL,
                                  config = NULL,
                                  seed_empty = NULL,
                                  fallback_to_sqlite = TRUE) {
  # Resolve database connection (injected for testing, passed via launcher options, or created fresh)
  opt_con <- getOption("CodingManageR.db_con", default = NULL)
  owns_connection <- FALSE

  active_con <- if (!is.null(db_con)) {
    db_con
  } else if (!is.null(opt_con)) {
    opt_con
  } else {
    cfg <- if (!is.null(config)) config else getOption("CodingManageR.db_config", default = list())
    if (is.null(cfg)) cfg <- list()
    eff_fallback <- isTRUE(fallback_to_sqlite)
    resolved_sqlite <- ""
    sqlite_file_existed <- FALSE
    if (eff_fallback) {
      eff_sqlite <- if (!is.null(sqlite_path) && length(sqlite_path) > 0L && nzchar(as.character(sqlite_path)[1])) {
        as.character(sqlite_path)[1]
      } else if (!is.null(cfg$sqlite_path) && length(cfg$sqlite_path) > 0L && nzchar(as.character(cfg$sqlite_path)[1])) {
        as.character(cfg$sqlite_path)[1]
      } else if (!is.null(cfg$sqlite_f) && length(cfg$sqlite_f) > 0L && nzchar(as.character(cfg$sqlite_f)[1])) {
        as.character(cfg$sqlite_f)[1]
      } else {
        getOption("CodingManageR.sqlite_path", default = Sys.getenv("SQLITE_PATH", unset = ""))
      }
      if (nzchar(eff_sqlite)) {
        cfg$sqlite_path <- eff_sqlite
      }
      resolved_sqlite <- resolve_sqlite_path(config = cfg, check_option = FALSE, create_dir = FALSE)
      sqlite_file_existed <- nzchar(resolved_sqlite) &&
        !identical(resolved_sqlite, ":memory:") &&
        file.exists(resolved_sqlite)
    }
    owns_connection <- TRUE
    con_new <- get_db_pool(config = cfg, fallback_to_sqlite = eff_fallback, force_pool = TRUE)
    create_all_tables(con_new, verbose = FALSE)
    eff_empty <- if (!is.null(seed_empty)) {
      isTRUE(seed_empty)
    } else {
      isTRUE(getOption("CodingManageR.seed_empty", default = TRUE))
    }
    if (!eff_empty && !is_mysql_connection(con_new) && isTRUE(sqlite_file_existed)) {
      warning(
        sprintf(
          "[CodingManageR] Existing SQLite database file detected ('%s'). Skipping demonstration data seeding (empty = FALSE) to protect existing data.",
          resolved_sqlite
        ),
        call. = FALSE
      )
    } else {
      seed_initial_data(con_new, empty = eff_empty)
    }
    con_new
  }

  if (owns_connection && !is.null(session)) {
    session$onSessionEnded(function() {
      close_db_pool(active_con)
    })
  }

  # Localization state
  current_lang <- reactiveVal("de")
  manual_lang_set <- reactiveVal(FALSE)

  observeEvent(input$browser_lang, {
    if (!isTRUE(manual_lang_set())) {
      detected <- detect_browser_language(input$browser_lang, fallback = "en")
      current_lang(detected)
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$selected_ui_lang, {
    chosen <- detect_browser_language(input$selected_ui_lang, fallback = "en")
    manual_lang_set(TRUE)
    current_lang(chosen)
  }, ignoreNULL = TRUE)

  # Authentication & session state
  logged_in       <- reactiveVal(FALSE)
  current_user    <- reactiveVal(NULL)
  current_user_id <- reactiveVal(NULL)
  current_role    <- reactiveVal(NULL)
  login_error_key <- reactiveVal(NULL)

  # Shared invalidation triggers & tutorial state
  data_trigger    <- reactiveVal(0L)
  scales_trigger  <- reactiveVal(0L)
  tutorial_active <- reactiveVal(FALSE)

  send_tutorial_msg <- function(type, message) {
    if (!is.null(session) && is.function(session$sendCustomMessage)) {
      tryCatch(session$sendCustomMessage(type, message), error = function(e) NULL)
    }
  }

  # Reactive coding scale catalog shared across Dashboard, Training, and Live Coding
  scales_catalog <- reactive({
    scales_trigger()
    data_trigger()
    tryCatch({
      dbGetQuery(
        active_con,
        "SELECT scale_id, short_label, name, max_value FROM scale_definitions ORDER BY scale_id ASC"
      )
    }, error = function(e) {
      data.frame(
        scale_id = integer(0),
        short_label = character(0),
        name = character(0),
        max_value = integer(0),
        stringsAsFactors = FALSE
      )
    })
  })

  kategorie_spalten <- reactive({
    df <- scales_catalog()
    if (nrow(df) == 0) return(character(0))
    setNames(as.character(df$scale_id), as.character(df$short_label))
  })

  kategorien_liste <- reactive({
    df <- scales_catalog()
    if (nrow(df) == 0) return(character(0))
    labels <- paste0(df$name, " (", df$short_label, " #", df$scale_id, ")")
    setNames(as.character(df$scale_id), labels)
  })

  # Sign-in validation handler
  observeEvent(input$btn_login, {
    lang <- current_lang()
    u_name <- if (is.null(input$login_username)) "" else trimws(as.character(input$login_username))
    u_tok  <- if (is.null(input$login_token)) "" else trimws(as.character(input$login_token))

    if (!nzchar(u_name) || !nzchar(u_tok)) {
      login_error_key("missing")
      return()
    }

    user_row <- tryCatch({
      dbGetQuery(
        active_con,
        "SELECT user_id, username, permissions, is_active FROM user_tokens WHERE username = ? AND token = ? LIMIT 1",
        params = list(u_name, u_tok)
      )
    }, error = function(e) data.frame())

    if (nrow(user_row) == 0) {
      login_error_key("invalid")
      return()
    }

    if (as.integer(user_row$is_active[1]) != 1L) {
      login_error_key("inactive")
      return()
    }

    login_error_key(NULL)
    current_user(as.character(user_row$username[1]))
    current_user_id(as.integer(user_row$user_id[1]))
    current_role(as.character(user_row$permissions[1]))
    logged_in(TRUE)
    data_trigger(data_trigger() + 1L)
    scales_trigger(scales_trigger() + 1L)
    showNotification(tr("auth.welcome_back", lang, as.character(user_row$username[1])), type = "message")
  })

  # Manual data refresh handler
  observeEvent(input$btn_refresh_view, {
    lang <- current_lang()
    data_trigger(data_trigger() + 1L)
    scales_trigger(scales_trigger() + 1L)
    showNotification(tr("header.refresh_notification", lang), type = "message")
  })

  # Sign-out handler
  observeEvent(input$btn_logout, {
    lang <- current_lang()
    tutorial_active(FALSE)
    send_tutorial_msg("cm_close_tutorial", list(reason = "logout"))
    logged_in(FALSE)
    current_user(NULL)
    current_user_id(NULL)
    current_role(NULL)
    login_error_key(NULL)
    showNotification(tr("header.logout_notification", lang), type = "message")
  })

  # On-demand Getting Started spotlight tutorial launch handler
  observeEvent(input$btn_start_tutorial, {
    if (!isTRUE(logged_in())) return()
    tutorial_active(TRUE)
    payload <- build_tutorial_payload(
      role = current_role(),
      lang = current_lang(),
      preserve_step = FALSE
    )
    send_tutorial_msg("cm_start_tutorial", payload)
  })

  # Track tutorial close events from the browser overlay
  observeEvent(input$tutorial_closed, {
    tutorial_active(FALSE)
  }, ignoreNULL = TRUE)

  # Keep tutorial localized copy synced if user switches language while tour is open
  observeEvent(current_lang(), {
    if (isTRUE(logged_in()) && isTRUE(tutorial_active())) {
      payload <- build_tutorial_payload(
        role = current_role(),
        lang = current_lang(),
        preserve_step = TRUE
      )
      send_tutorial_msg("cm_start_tutorial", payload)
    }
  }, ignoreInit = TRUE)

  # Sync Shiny tabsets when the spotlight tutorial steps across tabs (RBAC-enforced)
  observeEvent(input$tutorial_nav_request, {
    if (!isTRUE(logged_in())) return()
    req_nav <- input$tutorial_nav_request
    if (!is.list(req_nav)) return()

    is_admin <- identical(current_role(), "admin")
    allowed_main <- c("tab_dashboard", "tab_training", "tab_codings", if (is_admin) "tab_admin")
    if (!is.null(req_nav$main_tab) && is.character(req_nav$main_tab) && req_nav$main_tab %in% allowed_main) {
      updateTabsetPanel(session, "main_nav_tabs", selected = req_nav$main_tab)
    }

    if (!is.null(req_nav$sub_tab_id) && !is.null(req_nav$sub_tab) &&
        is.character(req_nav$sub_tab_id) && is.character(req_nav$sub_tab)) {
      allowed_dash <- c("tab_progress", "tab_scales_overview")
      allowed_admin <- c(
        "tab_users", "tab_texts", "tab_sources", "tab_scales",
        "tab_reliability", "tab_coding_search", "tab_data", "tab_diagnose"
      )
      if (identical(req_nav$sub_tab_id, "mod_dashboard-dashboard_tabs") && req_nav$sub_tab %in% allowed_dash) {
        updateTabsetPanel(session, "mod_dashboard-dashboard_tabs", selected = req_nav$sub_tab)
      } else if (is_admin && identical(req_nav$sub_tab_id, "mod_admin-admin_subtabs") && req_nav$sub_tab %in% allowed_admin) {
        updateTabsetPanel(session, "mod_admin-admin_subtabs", selected = req_nav$sub_tab)
      }
    }
  }, ignoreNULL = TRUE)

  # Localized authentication error alert
  output$login_error_ui <- renderUI({
    err_code <- login_error_key()
    if (is.null(err_code)) return(NULL)
    lang <- current_lang()

    if (identical(err_code, "missing")) {
      div(class = "alert alert-warning mt-3 mb-0", tr("auth.err_missing_fields", lang))
    } else if (identical(err_code, "inactive")) {
      div(
        class = "alert alert-danger mt-3 mb-0",
        tags$strong(tr("auth.err_inactive_account", lang)),
        tags$p(class = "mb-0 mt-1 small", tr("auth.inactive_details", lang))
      )
    } else {
      div(class = "alert alert-danger mt-3 mb-0", tr("auth.err_invalid_credentials", lang))
    }
  })

  # Register all child Shiny module servers
  dashboardServer(
    id = "mod_dashboard",
    db_con = active_con,
    user_id_reactive = current_user_id,
    username_reactive = current_user,
    trigger_reactive = data_trigger,
    kategorie_spalten = kategorie_spalten,
    scales_trigger = scales_trigger,
    current_lang = current_lang
  )

  schulungServer(
    id = "mod_training",
    db_con = active_con,
    user_id_reactive = current_user_id,
    username_reactive = current_user,
    kategorie_spalten = kategorie_spalten,
    kategorien_liste = kategorien_liste,
    trigger_parent = data_trigger,
    scales_trigger = scales_trigger,
    current_lang = current_lang
  )

  kodierungenServer(
    id = "mod_codings",
    db_con = active_con,
    user_id_reactive = current_user_id,
    username_reactive = current_user,
    kategorie_spalten = kategorie_spalten,
    kategorien_liste = kategorien_liste,
    trigger_parent = data_trigger,
    scales_trigger = scales_trigger,
    current_lang = current_lang
  )

  adminServer(
    id = "mod_admin",
    db_con = active_con,
    current_user = current_user,
    current_user_id = current_user_id,
    scales_trigger = scales_trigger,
    current_lang = current_lang,
    current_role = current_role
  )

  # Main root page router (Login Screen vs Authenticated Workspace)
  output$page_content <- renderUI({
    lang <- current_lang()

    if (!isTRUE(logged_in())) {
      return(
        div(
          class = "cm-login-wrap",
          div(
            class = "cm-login-card",
            div(
              class = "cm-login-header",
              div(
                class = "cm-brand-wrap",
                tags$img(
                  src = "cm-assets/logo.png",
                  class = "cm-brand-logo cm-brand-logo--login",
                  alt = "CodingManageR logo"
                ),
                tags$span(class = "cm-brand", "CodingManageR")
              ),
              language_selector_ui("login_language", current_lang = lang)
            ),
            div(
              class = "cm-login-body",
              h4(class = "mb-1", tr("auth.login_title", lang)),
              p(class = "text-muted small mb-1", tr("auth.subtitle", lang)),
              p(
                class = "cm-login-version font-monospace",
                tr("auth.version_label", lang, coding_manager_version())
              ),
              textInput(
                "login_username",
                label = tr("auth.username", lang),
                placeholder = tr("auth.username_placeholder", lang),
                width = "100%"
              ),
              passwordInput(
                "login_token",
                label = tr("auth.token", lang),
                placeholder = tr("auth.token_placeholder", lang),
                width = "100%"
              ),
              actionButton(
                "btn_login",
                label = tr("auth.login_btn", lang),
                icon = icon("sign-in-alt"),
                class = "btn btn-primary w-100 mt-2"
              ),
              uiOutput("login_error_ui"),
              tags$p(
                class = "text-muted text-center small mt-3 mb-0",
                style = "font-size: 0.78rem;",
                tr("auth.license_footer", lang)
              )
            )
          )
        )
      )
    }

    u_name <- current_user()
    u_id   <- current_user_id()
    u_role <- current_role()
    is_admin <- identical(u_role, "admin")

    active_main_tab <- isolate(input$main_nav_tabs)
    valid_main_tabs <- c("tab_dashboard", "tab_training", "tab_codings", if (is_admin) "tab_admin")
    if (is.null(active_main_tab) || !(active_main_tab %in% valid_main_tabs)) {
      active_main_tab <- "tab_dashboard"
    }

    tabs <- list(
      tabPanel(
        title = tagList(icon("chart-line"), " ", tr("nav.dashboard", lang)),
        value = "tab_dashboard",
        dashboardUI("mod_dashboard", lang = lang)
      ),
      tabPanel(
        title = tagList(icon("graduation-cap"), " ", tr("nav.training", lang)),
        value = "tab_training",
        schulungUI("mod_training")
      ),
      tabPanel(
        title = tagList(icon("pen-to-square"), " ", tr("nav.coding", lang)),
        value = "tab_codings",
        kodierungenUI("mod_codings")
      )
    )

    if (is_admin) {
      tabs[[length(tabs) + 1L]] <- tabPanel(
        title = tagList(icon("wrench"), " ", tr("nav.admin", lang)),
        value = "tab_admin",
        adminUI("mod_admin", lang = lang)
      )
    }

    tagList(
      tags$header(
        class = "cm-header",
        div(
          class = "cm-header-left",
          div(
            class = "cm-brand-wrap",
            tags$img(
              src = "cm-assets/logo.png",
              class = "cm-brand-logo",
              alt = "CodingManageR logo"
            ),
            tags$span(class = "cm-brand", "CodingManageR")
          ),
          div(
            class = "cm-header-meta",
            tags$span(class = "font-monospace", tr("header.user_badge", lang, u_name, as.character(u_id))),
            tags$span(class = "cm-header-meta-sep", "\u00b7"),
            tags$span(tr("header.role_badge", lang, u_role))
          )
        ),
        div(
          class = "cm-header-actions",
          actionButton(
            "btn_start_tutorial",
            label = tr("header.tutorial_btn", lang),
            icon = icon("compass"),
            class = "btn btn-sm btn-outline-primary",
            title = tr("header.tutorial_tooltip", lang)
          ),
          actionButton(
            "btn_refresh_view",
            label = tr("header.refresh_btn", lang),
            icon = icon("rotate"),
            class = "btn btn-sm btn-outline-secondary"
          ),
          language_selector_ui("header_language", current_lang = lang),
          actionButton(
            "btn_logout",
            label = tr("header.logout_btn", lang),
            icon = icon("sign-out-alt"),
            class = "btn btn-sm btn-outline-danger"
          )
        )
      ),
      div(
        class = "cm-main-nav",
        do.call(
          tabsetPanel,
          c(list(id = "main_nav_tabs", selected = active_main_tab, type = "tabs"), tabs)
        )
      )
    )
  })

  invisible(NULL)
}

#' Parse Optional Logical Environment Variable
#'
#' @param var_name Name of the environment variable to inspect.
#' @param default Logical fallback returned when unset or invalid.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
parse_env_logical <- function(var_name, default = TRUE) {
  raw <- trimws(Sys.getenv(var_name, unset = ""))
  if (!nzchar(raw)) return(isTRUE(default))
  low <- tolower(raw)
  if (low %in% c("false", "0", "no", "f", "off")) return(FALSE)
  if (low %in% c("true", "1", "yes", "t", "on")) return(TRUE)
  warning(
    sprintf(
      "[CodingManageR] Ignoring invalid value '%s' for environment variable '%s'; using default (%s).",
      raw, var_name, as.character(isTRUE(default))
    ),
    call. = FALSE
  )
  isTRUE(default)
}

#' Parse Optional Upload Limit Environment Variable (in GB)
#'
#' @param var_name Name of the environment variable to inspect.
#' @param default Numeric fallback (in GB) returned when unset or invalid.
#' @return Positive numeric scalar or `NULL`.
#' @keywords internal
#' @noRd
parse_env_upload_gb <- function(var_name, default = 3) {
  raw <- trimws(Sys.getenv(var_name, unset = ""))
  if (!nzchar(raw)) return(default)
  low <- tolower(raw)
  if (low %in% c("null", "none", "off")) return(NULL)
  val <- suppressWarnings(as.numeric(raw))
  if (!is.na(val) && is.finite(val) && val > 0) {
    return(val)
  }
  warning(
    sprintf(
      "[CodingManageR] Ignoring invalid value '%s' for environment variable '%s'; using default (%s GB).",
      raw, var_name, as.character(default)
    ),
    call. = FALSE
  )
  default
}

#' Internal Shiny App Factory Shared by All Launchers
#'
#' @param sqlite_path Optional SQLite file path.
#' @param config Optional database configuration list.
#' @param empty Logical passed to [seed_initial_data()].
#' @param max_upload_size_gb Positive numeric upload limit in GB or `NULL`.
#' @param fallback_to_sqlite Logical controlling SQLite fallback.
#' @param options Optional list forwarded to `shiny::shinyApp()`.
#' @param ... Additional arguments forwarded to `shiny::shinyApp()`.
#' @return A `shiny.appobj` object.
#' @keywords internal
#' @noRd
build_coding_manager_app <- function(sqlite_path = NULL,
                                     config = NULL,
                                     empty = TRUE,
                                     max_upload_size_gb = 3,
                                     fallback_to_sqlite = TRUE,
                                     options = list(),
                                     ...) {
  app_server <- function(input, output, session) {
    coding_manager_server(
      input = input,
      output = output,
      session = session,
      sqlite_path = sqlite_path,
      config = config,
      seed_empty = empty,
      fallback_to_sqlite = fallback_to_sqlite
    )
  }

  dots <- list(...)
  user_on_start <- dots$onStart
  dots$onStart <- NULL

  app_on_start <- function() {
    deregister <- NULL
    if (!is.null(max_upload_size_gb) &&
        is.numeric(max_upload_size_gb) &&
        length(max_upload_size_gb) == 1L &&
        is.finite(max_upload_size_gb) &&
        max_upload_size_gb > 0) {
      prev_max_req <- getOption("shiny.maxRequestSize")
      options(shiny.maxRequestSize = as.numeric(max_upload_size_gb) * 1024^3)
      deregister <- shiny::onStop(function() {
        options(shiny.maxRequestSize = prev_max_req)
      })
    }
    if (is.function(user_on_start)) {
      user_on_start()
    }
    invisible(deregister)
  }

  do.call(
    shiny::shinyApp,
    c(
      list(
        ui = coding_manager_ui(),
        server = app_server,
        onStart = app_on_start,
        options = options
      ),
      dots
    )
  )
}

#' Launch CodingManageR Locally (Single-User SQLite Mode)
#'
#' Constructs and returns a `shiny.appobj` strictly configured for local,
#' single-user workstation use with a local SQLite database file. Any
#' `MYSQL_HOST` or `MYSQL_USER` environment variables present on the machine
#' are ignored so local sessions never connect accidentally to a remote server.
#'
#' To manage multiple independent local projects on one computer, supply a
#' distinct `sqlite_path` file for each study (each `.sqlite` file stores its
#' own scales, sources, case texts, user accounts, ratings, and training logs).
#' Re-passing the same `sqlite_path` reopens that project. When `sqlite_path`
#' is `NULL` (and no `SQLITE_PATH` environment variable is set), a single
#' default database is stored under
#' `tools::R_user_dir("CodingManageR", which = "data")`. Always stop the
#' running Shiny app before copying, renaming, moving, or archiving a `.sqlite`
#' file so SQLite merges its Write-Ahead Log (`-wal`/`-shm`) cleanly.
#'
#' For multi-user server deployments backed by MySQL/MariaDB, use
#' [run_app_server()] instead.
#'
#' @param sqlite_path Optional file path to a local SQLite database. Passing a
#'   different `.sqlite` path creates or reopens a separate, isolated project;
#'   the parent directory must already exist.
#' @param empty Logical passed to [seed_initial_data()] when initializing a fresh
#'   database. Defaults to `TRUE` (creates only the default `admin` and
#'   `historic_user` accounts). When `FALSE`, demonstration data is only inserted
#'   into a brand-new, empty database; existing SQLite files or databases that
#'   already contain rows are left untouched and trigger a warning.
#'
#' @return A `shiny.appobj` object that launches the local Shiny portal when
#'   printed or run interactively.
#' @seealso [run_app_server()], [run_app()]
#' @examples
#' \dontrun{
#' # Launch the default local SQLite project
#' run_app_local()
#'
#' # Launch separate local projects in isolated SQLite files
#' run_app_local(sqlite_path = "study_a.sqlite")
#' run_app_local(sqlite_path = "study_b.sqlite")
#'
#' # Launch a temporary demo session with sample data
#' run_app_local(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
#' }
#' @export
run_app_local <- function(sqlite_path = NULL, empty = TRUE) {
  build_coding_manager_app(
    sqlite_path = sqlite_path,
    config = list(force_sqlite = TRUE),
    empty = isTRUE(empty),
    max_upload_size_gb = 3,
    fallback_to_sqlite = TRUE,
    options = list()
  )
}

#' Launch CodingManageR on a Multi-User Server (MySQL/MariaDB Mode)
#'
#' Constructs and returns a `shiny.appobj` configured strictly for multi-user
#' server deployments (such as Shiny Server Open Source) backed by a central
#' MySQL or MariaDB database pool.
#'
#' Unlike [run_app_local()] and [run_app()], `run_app_server()` permanently
#' disables the SQLite fallback (`fallback_to_sqlite = FALSE`). If MySQL/MariaDB
#' credentials are missing or the database server cannot be reached, startup
#' stops with an actionable error message rather than silently creating a local
#' SQLite file on the server.
#'
#' On Shiny Server, create your `app.R` entrypoint **once** with a zero-argument
#' call to `CodingManageR::run_app_server()` (or copy the bundled file via
#' `system.file("shiny", "app.R", package = "CodingManageR")`) and store all
#' database and runtime settings in `.Renviron`:
#'
#' - Required database variables: `MYSQL_HOST`, `MYSQL_USER`, and `MYSQL_DBNAME`
#'   (plus optional `MYSQL_PORT` and `MYSQL_PASSWORD`). If an `.Renviron` file
#'   exists in the application directory, missing variables are loaded
#'   automatically at startup without overwriting already set environment
#'   variables.
#' - Optional runtime variables: `CODINGMANAGER_EMPTY` (`TRUE`/`FALSE`, default
#'   `TRUE`) and `CODINGMANAGER_MAX_UPLOAD_GB` (positive number in GB or
#'   `"null"`, default `3`).
#'
#' Because all configuration resides in `.Renviron` and all logic resides inside
#' the package, future updates of **CodingManageR** require only updating the R
#' package and restarting Shiny Server -- no manual edits to `app.R` are needed.
#'
#' @param config Optional named list of MySQL/MariaDB connection parameters
#'   passed to [get_db_pool()] (`host`, `port`, `user`, `password`, `dbname`,
#'   `minSize`, `maxSize`, `idleTimeout`). When `NULL` (default, recommended for
#'   server deployments), credentials are read from `.Renviron`.
#' @param empty Logical passed to [seed_initial_data()] when initializing a
#'   brand-new database. Defaults to `TRUE`. When omitted, the optional
#'   `CODINGMANAGER_EMPTY` environment variable is consulted before falling back
#'   to `TRUE`.
#' @param max_upload_size_gb Positive numeric maximum file upload size in
#'   gigabytes applied via `options(shiny.maxRequestSize = ...)` while the app
#'   runs (restored cleanly on app shutdown). Defaults to `3` (3 GB). When
#'   omitted, the optional `CODINGMANAGER_MAX_UPLOAD_GB` environment variable is
#'   consulted before falling back to `3`. Set to `NULL` to leave the existing
#'   `shiny.maxRequestSize` option unchanged.
#' @param options Optional list of options forwarded to `shiny::shinyApp()`.
#' @param ... Additional arguments forwarded to `shiny::shinyApp()`.
#'
#' @return A `shiny.appobj` object suitable for returning from a Shiny Server
#'   `app.R` entrypoint.
#' @seealso [run_app_local()], [run_app()]
#' @examples
#' \dontrun{
#' # Minimal, set-once Shiny Server app.R (reads credentials from .Renviron)
#' CodingManageR::run_app_server()
#'
#' # Explicit programmatic MySQL/MariaDB configuration
#' CodingManageR::run_app_server(
#'   config = list(
#'     host = "127.0.0.1",
#'     port = 3306L,
#'     user = "shiny_user",
#'     password = "secret",
#'     dbname = "kodierer_portal"
#'   )
#' )
#' }
#' @export
run_app_server <- function(config = NULL,
                           empty = TRUE,
                           max_upload_size_gb = 3,
                           options = list(),
                           ...) {
  load_local_renviron()

  eff_empty <- if (missing(empty)) {
    parse_env_logical("CODINGMANAGER_EMPTY", default = empty)
  } else {
    isTRUE(empty)
  }

  eff_upload_gb <- if (missing(max_upload_size_gb)) {
    parse_env_upload_gb("CODINGMANAGER_MAX_UPLOAD_GB", default = max_upload_size_gb)
  } else {
    max_upload_size_gb
  }

  cfg <- if (is.null(config)) list() else as.list(config)
  cfg$force_sqlite <- FALSE

  build_coding_manager_app(
    sqlite_path = NULL,
    config = cfg,
    empty = eff_empty,
    max_upload_size_gb = eff_upload_gb,
    fallback_to_sqlite = FALSE,
    options = options,
    ...
  )
}

#' Launch the CodingManageR Shiny Application (Backward-Compatible Wrapper)
#'
#' Constructs and returns a `shiny.appobj` configured automatically for either
#' pooled MySQL/MariaDB storage (when MySQL credentials are supplied) or local
#' single-user SQLite storage (with automatic fallback).
#'
#' For new scripts and deployments, prefer the explicit entrypoints
#' [run_app_local()] (minimal local SQLite launcher) and [run_app_server()]
#' (strict MySQL/MariaDB server launcher without SQLite fallback). `run_app()`
#' is retained for full backward compatibility with existing code.
#'
#' @param sqlite_path Optional file path to a local SQLite database. Passing a
#'   different `.sqlite` path creates or reopens a separate, isolated project;
#'   the parent directory must already exist.
#' @param config Optional named list of database configuration parameters passed
#'   to [get_db_pool()].
#' @param empty Logical passed to [seed_initial_data()] when initializing a fresh
#'   database. Defaults to `TRUE` (creates only the default `admin` and
#'   `historic_user` accounts). When `FALSE`, demonstration data is only inserted
#'   into a brand-new, empty database; existing SQLite files or databases that
#'   already contain rows are left untouched and trigger a warning.
#' @param max_upload_size_gb Positive numeric maximum file upload size in
#'   gigabytes applied via `options(shiny.maxRequestSize = ...)` while the app
#'   runs (restored cleanly on app shutdown). Defaults to `3` (3 GB). Set to
#'   `NULL` to leave the session's existing `shiny.maxRequestSize` option
#'   unchanged.
#' @param options Optional list of options forwarded to `shiny::shinyApp()`.
#' @param ... Additional arguments forwarded to `shiny::shinyApp()`.
#'
#' @return A `shiny.appobj` object that launches the Shiny portal when printed
#'   or returned from `app.R`.
#' @seealso [run_app_local()], [run_app_server()]
#' @examples
#' \dontrun{
#' # Preferred explicit launchers:
#' run_app_local(sqlite_path = "study_a.sqlite")
#' run_app_server()
#'
#' # Backward-compatible wrapper:
#' app <- run_app(sqlite_path = tempfile(fileext = ".sqlite"), empty = FALSE)
#' app
#' }
#' @export
run_app <- function(sqlite_path = NULL,
                    config = NULL,
                    empty = TRUE,
                    max_upload_size_gb = 3,
                    options = list(),
                    ...) {
  build_coding_manager_app(
    sqlite_path = sqlite_path,
    config = config,
    empty = empty,
    max_upload_size_gb = max_upload_size_gb,
    fallback_to_sqlite = TRUE,
    options = options,
    ...
  )
}
