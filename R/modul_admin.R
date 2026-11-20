# ==============================================================================
# ADMIN MODULE FOR R SHINY (HORIZONTAL TABS WITH 25-ITEM PAGINATION)
# ==============================================================================
# File: modul_admin.R
# Includes:
#  Tab 1: User Management (CRUD for 'user_tokens')
#         - Pagination: up to 25 users per page with first/prev/next/last & jump
#         - Integrated quick search by username, role, and active status
#         - Status control: is_active (1 = active, 0 = inactive)
#         - 1-click toggle button in table & modal toggle
#         - Self-deactivation and self-deletion protection for active admin
#         - User creation with validation & token generator
#         - Permission matrices for sources and coding scales per coder
#  Tab 2: Case Texts & Source Assignment ('texts' & 'sources')
#         - Pagination: up to 25 texts per page with full navigation
#         - Integrated full-text and source search
#         - Overview of all case texts with source assignment modal
#  Tab 3: Source Management ('sources')
#         - Complete source directory searchable by ID, short ref, citation, license
#         - Modals for adding, editing, and deleting scholarly sources
#  Tab 4: Scale Management ('scale_definitions')
#         - Overview, quick search, creation & protected editing (max_value locked)
#  Tab 5: Reliability & Coding Progress of All Users
#         - Monitoring of all coders: production codings & training reliabilities
#         - Scale filter, metric switcher, ggplot2 comparison chart & drilldown modal
#  Tab 6: Targeted Coding Search & Value Correction ('user_ratings')
#  Tab 7: Data & Import/Export
#         - Consolidated master data export with scale/source/user filters
#         - Master data CSV upload with strict schema validation & overwrite guard
#         - Raw table and template CSV downloads
#  Tab 8: Database Diagnostics & Integrity Audit (all 9 tables)
# ==============================================================================

# Dependencies (shiny, DBI, ggplot2) are imported via NAMESPACE

# ==============================================================================
# ADMIN MODULE UI
# ==============================================================================

#' Admin Module UI
#'
#' Creates the Shiny UI for the administration panel covering user management,
#' case texts, source definitions, scale definitions, reliability monitoring,
#' coding search/correction, data import/export, and database diagnostics.
#'
#' @param id Character string specifying the Shiny module namespace identifier.
#' @param lang Character string specifying the initial UI language code
#'   (`"en"` or `"de"`).
#' @param selected_subtab Character string selecting the initially active admin
#'   subtab (one of `"tab_users"`, `"tab_texts"`, `"tab_sources"`,
#'   `"tab_scales"`, `"tab_reliability"`, `"tab_coding_search"`, `"tab_data"`,
#'   or `"tab_diagnose"`).
#'
#' @return A `shiny.tag` (`fluidPage`) representing the admin interface.
#'
#' @examples
#' if (interactive()) {
#'   ui <- adminUI("mod_admin", lang = "en")
#' }
#'
#' @export
adminUI <- function(id, lang = "en", selected_subtab = "tab_users") {
  ns <- NS(id)
  valid_subtabs <- c("tab_users", "tab_texts", "tab_sources", "tab_scales", "tab_reliability", "tab_coding_search", "tab_data", "tab_diagnose")
  active_subtab <- if (!is.null(selected_subtab) && selected_subtab %in% valid_subtabs) selected_subtab else "tab_users"

  fluidPage(
    h2(paste0("\U0001f6e0\ufe0f ", tr("admin.title", lang))),
    p(tr("admin.subtitle", lang)),
    br(),

    tabsetPanel(
      id = ns("admin_subtabs"),
      selected = active_subtab,
      type = "tabs",

      # ------------------------------------------------------------------------
      # TAB 1: USER MANAGEMENT (user_tokens) - WITH 25-USER PAGINATION
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("users"), tr("admin.tab_users", lang)),
        value = "tab_users",
        br(),
        wellPanel(
          style = "border-left: 5px solid #2c3e50; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(class = "d-flex justify-content-between align-items-center mb-3",
              div(
                h4(style = "margin: 0; color: #2c3e50;", paste0("\U0001f464 ", tr("admin.users.title", lang))),
                p(class = "text-muted", style = "margin: 4px 0 0 0;", tr("admin.users.subtitle", lang))
              ),
              actionButton(ns("btn_open_add_user"), tr("admin.users.btn_add", lang),
                           icon = icon("user-plus"), class = "btn-success")
          ),
          hr(),

          fluidRow(
            column(width = 6,
                   div(class = "input-group mb-3",
                       span(class = "input-group-text", icon("search")),
                       textInput(ns("user_search_query"), label = NULL, placeholder = tr("admin.users.search_placeholder", lang), width = "100%")
                   )
            ),
            column(width = 6,
                   div(class = "text-end text-muted small pt-2",
                       uiOutput(ns("users_counter_badge"))
                   )
            )
          ),

          uiOutput(ns("users_table_container")),
          br(),
          uiOutput(ns("users_pagination_controls"))
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 2: CASE TEXTS & SOURCE ASSIGNMENT (texts & sources) - WITH 25-ITEM PAGINATION
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("file-lines"), tr("admin.tab_texts", lang)),
        value = "tab_texts",
        br(),
        wellPanel(
          style = "border-left: 5px solid #3498db; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(class = "d-flex justify-content-between align-items-center mb-3",
              div(
                h4(style = "margin: 0; color: #2c3e50;", paste0("\U0001f4d1 ", tr("admin.texts.title", lang))),
                p(class = "text-muted", style = "margin: 4px 0 0 0;", tr("admin.texts.subtitle", lang))
              )
          ),
          hr(),

          fluidRow(
            column(width = 6,
                   div(class = "input-group mb-3",
                       span(class = "input-group-text", icon("search")),
                       textInput(ns("texts_search_query"), label = NULL, placeholder = tr("admin.texts.search_placeholder", lang), width = "100%")
                   )
            ),
            column(width = 6,
                   div(class = "text-end text-muted small pt-2",
                       uiOutput(ns("texts_counter_badge"))
                   )
            )
          ),

          uiOutput(ns("texts_management_table_container")),
          br(),
          uiOutput(ns("texts_pagination_controls"))
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 3: SOURCE MANAGEMENT (sources)
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("book"), tr("admin.tab_sources", lang)),
        value = "tab_sources",
        br(),
        wellPanel(
          style = "border-left: 5px solid #34495e; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(class = "d-flex flex-wrap justify-content-between align-items-center mb-3 gap-2",
              div(
                h4(style = "margin: 0; color: #2c3e50;", tr("admin.sources.title", lang)),
                p(class = "text-muted", style = "margin: 4px 0 0 0;", tr("admin.sources.subtitle", lang))
              ),
              actionButton(ns("btn_open_add_source"), tr("admin.sources.btn_add", lang),
                           icon = icon("plus"), class = "btn-success")
          ),
          hr(),

          fluidRow(
            column(width = 6,
                   div(class = "input-group mb-3",
                       span(class = "input-group-text", icon("search")),
                       textInput(ns("source_search_query"), label = NULL, placeholder = tr("admin.sources.search_placeholder", lang), width = "100%")
                   )
            ),
            column(width = 6,
                   div(class = "text-end text-muted small pt-2",
                       uiOutput(ns("sources_counter_text"))
                   )
            )
          ),

          uiOutput(ns("sources_table_container"))
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 4: SCALE MANAGEMENT
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("sliders-h"), tr("admin.tab_scales", lang)),
        value = "tab_scales",
        br(),
        wellPanel(
          style = "border-left: 5px solid #3498db; background-color: #ffffff;",
          fluidRow(
            column(width = 8,
                   h4(icon("ruler-combined", class = "me-2 text-primary"), tr("admin.scales.title", lang)),
                   p(class = "text-muted mb-0", tr("admin.scales.subtitle", lang))
            ),
            column(width = 4, class = "text-end align-self-center",
                   actionButton(ns("btn_open_add_scale"), tr("admin.scales.btn_add", lang), icon = icon("plus"), class = "btn-success")
            )
          ),
          hr(),

          fluidRow(
            column(width = 6,
                   div(class = "input-group mb-3",
                       span(class = "input-group-text", icon("search")),
                       textInput(ns("scale_search_query"), label = NULL, placeholder = tr("admin.scales.search_placeholder", lang), width = "100%")
                   )
            ),
            column(width = 6,
                   div(class = "text-end text-muted small pt-2",
                       uiOutput(ns("scales_counter_text"))
                   )
            )
          ),

          uiOutput(ns("scales_table_container"))
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 5: RELIABILITY & CODING PROGRESS OF ALL USERS
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("chart-line"), tr("admin.tab_reliability", lang)),
        value = "tab_reliability",
        br(),
        wellPanel(
          style = "border-left: 5px solid #2980b9; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(class = "d-flex flex-wrap justify-content-between align-items-center mb-3 gap-2",
              div(
                h4(style = "margin: 0; color: #2c3e50;", icon("chart-bar", class = "me-2 text-primary"), tr("admin.reliability.title", lang)),
                p(class = "text-muted", style = "margin: 4px 0 0 0;", tr("admin.reliability.subtitle", lang))
              ),
              actionButton(ns("btn_refresh_reliability"), tr("admin.reliability.btn_refresh", lang),
                           icon = icon("rotate"), class = "btn-outline-primary btn-sm")
          ),
          hr(),

          uiOutput(ns("admin_reliability_kpis")),
          br(),

          fluidRow(
            column(width = 4,
                   div(class = "input-group mb-3",
                       span(class = "input-group-text", icon("search")),
                       textInput(ns("rel_search_query"), label = NULL, placeholder = tr("admin.reliability.search_placeholder", lang), width = "100%")
                   )
            ),
            column(width = 4,
                   selectInput(ns("rel_scale_select"), label = NULL,
                               choices = setNames("all", tr("admin.reliability.all_dimensions", lang)),
                               selected = "all", width = "100%")
            ),
            column(width = 4,
                   selectInput(ns("rel_metric_select"), label = NULL,
                               choices = get_reliability_metric_choices(lang, include_all_core = FALSE),
                               selected = "cohens_kappa", width = "100%")
            )
          ),

          div(class = "card p-3 mb-4 bg-light border",
              div(class = "d-flex justify-content-between align-items-center mb-2",
                  h5(style = "margin:0; font-size: 0.95rem; font-weight: 600; color: #2c3e50;",
                     icon("chart-column", class = "me-1 text-primary"), tr("admin.reliability.chart_card_title", lang)),
                  span(class = "text-muted small", tr("admin.reliability.chart_bench_note", lang))
              ),
              plotOutput(ns("admin_reliability_comparison_plot"), height = "320px")
          ),

          uiOutput(ns("admin_reliability_table_container"))
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 6: TARGETED CODING SEARCH & VALUE CORRECTION (user_ratings)
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("search"), uiOutput(ns("tab_coding_search_title"), inline = TRUE)),
        value = "tab_coding_search",
        br(),
        wellPanel(
          style = "background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          uiOutput(ns("coding_search_header_ui")),
          hr(),
          fluidRow(
            column(
              width = 3,
              selectInput(
                ns("coding_search_scale_filter"),
                label = tr("admin.filter_scale_label", lang),
                choices = setNames("all", tr("admin.filter_all_scales", lang)),
                selected = "all",
                width = "100%"
              )
            ),
            column(
              width = 3,
              selectInput(
                ns("coding_search_type_filter"),
                label = tr("admin.filter_type_label", lang),
                choices = setNames(
                  c("all", "kodierung", "gold_standard", "schulung"),
                  c(
                    tr("admin.filter_type_all", lang),
                    tr("admin.filter_type_kodierung", lang),
                    tr("admin.filter_type_gold", lang),
                    tr("admin.filter_type_schulung", lang)
                  )
                ),
                selected = "all",
                width = "100%"
              )
            ),
            column(
              width = 4,
              div(
                style = "margin-top: 25px;",
                textInput(
                  ns("coding_search_query"),
                  label = NULL,
                  placeholder = tr("admin.coding_search_placeholder", lang),
                  width = "100%"
                )
              )
            ),
            column(
              width = 2,
              div(
                style = "margin-top: 25px;",
                actionButton(
                  ns("btn_coding_search_reset"),
                  label = tr("common.reset", lang),
                  icon = icon("rotate-left"),
                  class = "btn btn-outline-secondary w-100"
                )
              )
            )
          ),
          div(
            class = "d-flex justify-content-end mb-2",
            uiOutput(ns("coding_search_counter_ui"))
          ),
          uiOutput(ns("coding_search_table_container")),
          br(),
          uiOutput(ns("coding_search_pagination_controls"))
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 7: DATA & IMPORT/EXPORT
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("database"), tr("admin.tab_data", lang)),
        value = "tab_data",
        br(),
        wellPanel(
          style = "border-left: 5px solid #18bc9c; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(class = "d-flex flex-wrap justify-content-between align-items-center mb-2 gap-2",
              div(
                h4(style = "margin: 0; color: #16a085;", paste0("\U0001f4ca ", tr("admin.export_master_title", lang))),
                p(class = "text-muted", style = "margin: 4px 0 0 0;", tr("admin.export_master_desc", lang))
              )
          ),
          hr(),

          fluidRow(
            column(width = 4,
              div(class = "p-3 rounded border bg-light h-100 d-flex flex-column justify-content-between",
                div(
                  div(class = "d-flex justify-content-between align-items-center mb-2",
                    tags$strong(class = "text-dark small", icon("sliders-h", class = "me-1 text-primary"), tr("admin.scales_dimension", lang)),
                    uiOutput(ns("export_scales_status_badge"))
                  ),
                  p(class = "text-muted small mb-2", tr("admin.scales_card_desc", lang)),
                  uiOutput(ns("export_scales_counter_ui"))
                ),
                div(class = "mt-3 pt-2 border-top",
                  actionButton(ns("btn_open_modal_export_scales"), tr("admin.scales_btn", lang), icon = icon("sliders-h"), class = "btn btn-outline-primary btn-sm w-100")
                )
              )
            ),

            column(width = 4,
              div(class = "p-3 rounded border bg-light h-100 d-flex flex-column justify-content-between",
                div(
                  div(class = "d-flex justify-content-between align-items-center mb-2",
                    tags$strong(class = "text-dark small", icon("book", class = "me-1 text-info"), tr("admin.sources_dimension", lang)),
                    uiOutput(ns("export_sources_status_badge"))
                  ),
                  p(class = "text-muted small mb-2", tr("admin.sources_card_desc", lang)),
                  uiOutput(ns("export_sources_counter_ui"))
                ),
                div(class = "mt-3 pt-2 border-top",
                  actionButton(ns("btn_open_modal_export_sources"), tr("admin.sources_btn", lang), icon = icon("book"), class = "btn btn-outline-info btn-sm w-100")
                )
              )
            ),

            column(width = 4,
              div(class = "p-3 rounded border bg-light h-100 d-flex flex-column justify-content-between",
                div(
                  div(class = "d-flex justify-content-between align-items-center mb-2",
                    tags$strong(class = "text-dark small", icon("users", class = "me-1 text-success"), tr("admin.users_dimension", lang)),
                    uiOutput(ns("export_users_status_badge"))
                  ),
                  p(class = "text-muted small mb-2", tr("admin.users_card_desc", lang)),
                  uiOutput(ns("export_users_counter_ui"))
                ),
                div(class = "mt-3 pt-2 border-top",
                  actionButton(ns("btn_open_modal_export_users"), tr("admin.users_btn", lang), icon = icon("users"), class = "btn btn-outline-success btn-sm w-100")
                )
              )
            )
          ),
          br(),

          div(class = "p-3 rounded border bg-light d-flex flex-column sm:flex-row justify-content-between align-items-center gap-3",
            div(
              uiOutput(ns("export_filter_summary_badge")),
              p(class = "text-muted small mb-0 mt-1",
                icon("info-circle", class = "me-1 text-info"),
                tr("admin.export_filter_note", lang))
            ),
            div(class = "shrink-0",
              downloadButton(ns("download_master_dataset_btn"), tr("admin.download_csv", lang), class = "btn btn-success font-monospace")
            )
          )
        ),
        br(),

        wellPanel(
          style = "border-left: 5px solid #28a745; background-color: #ffffff;",
          h4(paste0("\U0001f4e5 ", tr("admin.upload_title", lang))),
          p(tr("admin.upload_desc", lang)),
          br(),
          fluidRow(
            column(width = 4,
                   fileInput(ns("upload_scales_file"), tr("admin.upload_slot_scales", lang),
                             accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")),
                   p(tags$small(class = "text-muted", tr("admin.upload_csv_hint", lang))),
                   downloadButton(ns("download_template_scales"), tr("admin.upload_btn_template", lang),
                                  icon = icon("file-csv"), class = "btn-sm btn-outline-secondary w-100 mb-3")
            ),
            column(width = 4,
                   fileInput(ns("upload_texts_file"), tr("admin.upload_slot_texts", lang),
                             accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")),
                   p(tags$small(class = "text-muted", tr("admin.upload_csv_hint", lang))),
                   downloadButton(ns("download_template_texts"), tr("admin.upload_btn_template", lang),
                                  icon = icon("file-csv"), class = "btn-sm btn-outline-secondary w-100 mb-3")
            ),
            column(width = 4,
                   fileInput(ns("upload_sources_file"), tr("admin.upload_slot_sources", lang),
                             accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")),
                   p(tags$small(class = "text-muted", tr("admin.upload_csv_hint", lang))),
                   downloadButton(ns("download_template_sources"), tr("admin.upload_btn_template", lang),
                                  icon = icon("file-csv"), class = "btn-sm btn-outline-secondary w-100 mb-3")
            )
          ),
          fluidRow(
            column(width = 6,
                   fileInput(ns("upload_ratings_file"), tr("admin.upload_slot_ratings", lang),
                             accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")),
                   p(tags$small(class = "text-muted", tr("admin.upload_csv_hint", lang))),
                   downloadButton(ns("download_template_ratings"), tr("admin.upload_btn_template", lang),
                                  icon = icon("file-csv"), class = "btn-sm btn-outline-secondary w-100 mb-2")
            ),
            column(width = 6,
                   fileInput(ns("upload_training_file"), tr("admin.upload_slot_training", lang),
                             accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")),
                   p(tags$small(class = "text-muted", tr("admin.upload_csv_hint", lang))),
                   downloadButton(ns("download_template_training"), tr("admin.upload_btn_template", lang),
                                  icon = icon("file-csv"), class = "btn-sm btn-outline-secondary w-100 mb-2")
            )
          )
        ),
        br(),

        fluidRow(
          column(width = 6,
                 wellPanel(
                   h5(tr("admin.raw_master_title", lang)),
                   hr(),
                   div(class = "d-flex justify-content-between align-items-center mb-2",
                       span(tr("admin.raw_label_scales", lang)),
                       downloadButton(ns("download_scales_btn"), tr("admin.raw_download_btn", lang), class = "btn-sm btn-secondary")
                   ),
                   div(class = "d-flex justify-content-between align-items-center mb-2",
                       span(tr("admin.raw_label_texts", lang)),
                       downloadButton(ns("download_texts_btn"), tr("admin.raw_download_btn", lang), class = "btn-sm btn-secondary")
                   ),
                   div(class = "d-flex justify-content-between align-items-center",
                       span(tr("admin.raw_label_sources", lang)),
                       downloadButton(ns("download_sources_btn"), tr("admin.raw_download_btn", lang), class = "btn-sm btn-secondary")
                   )
                 )
          ),
          column(width = 6,
                 wellPanel(
                   h5(tr("admin.raw_results_title", lang)),
                   hr(),
                   div(class = "d-flex justify-content-between align-items-center mb-2",
                       span(tr("admin.raw_label_ratings", lang)),
                       downloadButton(ns("download_ratings_btn"), tr("admin.raw_download_btn", lang), class = "btn-sm btn-primary")
                   ),
                   div(class = "d-flex justify-content-between align-items-center",
                       span(tr("admin.raw_label_training", lang)),
                       downloadButton(ns("download_training_log_btn"), tr("admin.raw_download_btn", lang), class = "btn-sm btn-primary")
                   )
                 )
          )
        )
      ),

      # ------------------------------------------------------------------------
      # TAB 8: DATABASE DIAGNOSTICS & INTEGRITY AUDIT (9 TABLES)
      # ------------------------------------------------------------------------
      tabPanel(
        title = tagList(icon("stethoscope"), tr("admin.tab_diagnostics", lang)),
        value = "tab_diagnose",
        br(),
        wellPanel(
          style = "border-left: 5px solid #0d9488; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          div(class = "d-flex flex-wrap justify-content-between align-items-center mb-3 gap-2",
              div(
                h4(style = "margin: 0; color: #0f766e;", paste0("\U0001fa7a ", tr("admin.diagnostics_title", lang))),
                p(class = "text-muted", style = "margin: 4px 0 0 0;", tr("admin.diagnostics_desc", lang))
              ),
              actionButton(ns("btn_refresh_diagnostics"), tr("admin.diag_btn_refresh_now", lang),
                           icon = icon("sync"), class = "btn-outline-primary")
          ),
          hr(),

          uiOutput(ns("db_diag_summary_cards")),
          br(),

          h5(class = "fw-bold text-dark mb-2", icon("table", class = "me-2 text-primary"), tr("admin.diag_table_overview_title", lang)),
          uiOutput(ns("db_diag_tables_overview")),
          br(),

          h5(class = "fw-bold text-dark mb-2", icon("shield-alt", class = "me-2 text-success"), tr("admin.diag_audit_title", lang)),
          uiOutput(ns("db_diag_integrity_audit"))
        )
      )
    )
  )
}

# ==============================================================================
# ADMIN MODULE SERVER
# ==============================================================================

#' Admin Module Server
#'
#' Implements the server-side administration workflows for managing users and
#' permissions, case texts, bibliographic sources, coding scales, team
#' reliability dashboards, targeted coding search and value edits, CSV data
#' import/export, and relational database integrity diagnostics.
#'
#' @param id Character string specifying the Shiny module namespace identifier.
#' @param db_con Active `DBIConnection` or `pool::Pool` database handle.
#' @param current_user Optional reactive expression or character scalar
#'   identifying the currently signed-in administrator's username.
#' @param current_user_id Optional reactive expression or integer scalar
#'   identifying the currently signed-in administrator's `user_id`.
#' @param scales_trigger Optional `reactiveVal` incremented whenever scale
#'   definitions are added or modified so other modules refresh dynamically.
#' @param current_lang Reactive expression or character scalar providing the
#'   active UI language code (`"de"` or `"en"`).
#'
#' @return The return value of `shiny::moduleServer()`.
#'
#' @examples
#' \dontrun{
#' adminServer(
#'   id = "mod_admin",
#'   db_con = db_con,
#'   current_user = reactive("admin"),
#'   current_user_id = reactive(1L),
#'   scales_trigger = reactiveVal(0L)
#' )
#' }
#'
#' @export
adminServer <- function(id, db_con, current_user = NULL, current_user_id = NULL, scales_trigger = NULL, current_lang = reactiveVal("de")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    get_lang <- function() {
      if (is.reactive(current_lang) || is.function(current_lang)) current_lang() else as.character(current_lang)
    }

    # Shared cross-tab reactive triggers
    user_table_trigger   <- reactiveVal(0)
    texts_table_trigger  <- reactiveVal(0)
    sources_trigger      <- reactiveVal(0)
    scales_table_trigger <- reactiveVal(0)
    ratings_edit_trigger <- reactiveVal(0)

    # Local reactive state kept directly in adminServer() frame for testServer()
    current_page_users         <- reactiveVal(1)
    current_page_texts         <- reactiveVal(1)
    current_page_coding_search <- reactiveVal(1L)
    pending_import             <- reactiveVal(NULL)

    aktiver_admin <- reactive({
      if (is.null(current_user)) {
        return("admin")
      } else if (is.reactive(current_user)) {
        return(current_user())
      } else {
        return(as.character(current_user))
      }
    })

    aktiver_admin_id <- reactive({
      if (!is.null(current_user_id)) {
        val <- if (is.reactive(current_user_id)) current_user_id() else current_user_id
        if (!is.null(val) && !is.na(val) && val != "") {
          return(as.integer(val))
        }
      }
      u_name <- aktiver_admin()
      res <- tryCatch({
        dbGetQuery(db_con, "SELECT user_id FROM user_tokens WHERE username = ?", params = list(u_name))$user_id
      }, error = function(e) integer(0))
      if (length(res) > 0 && !is.na(res[1])) as.integer(res[1]) else 1L
    })

    # Initialize user_source_permissions, user_coding_source_permissions, and user_scale_permissions
    tryCatch({
      t_stamp <- if (is_mysql_connection(db_con)) "DATETIME DEFAULT CURRENT_TIMESTAMP" else "TEXT DEFAULT CURRENT_TIMESTAMP"
      dbExecute(db_con, paste0("
        CREATE TABLE IF NOT EXISTS user_source_permissions (
          user_id INTEGER NOT NULL,
          source_id INTEGER NOT NULL,
          erstellt_am ", t_stamp, ",
          PRIMARY KEY (user_id, source_id)
        )
      "))
      dbExecute(db_con, paste0("
        CREATE TABLE IF NOT EXISTS user_coding_source_permissions (
          user_id INTEGER NOT NULL,
          source_id INTEGER NOT NULL,
          erstellt_am ", t_stamp, ",
          PRIMARY KEY (user_id, source_id)
        )
      "))
      dbExecute(db_con, paste0("
        CREATE TABLE IF NOT EXISTS user_scale_permissions (
          user_id INTEGER NOT NULL,
          scale_id INTEGER NOT NULL,
          erstellt_am ", t_stamp, ",
          PRIMARY KEY (user_id, scale_id)
        )
      "))
    }, error = function(e) NULL)

    ctx <- list(
      ns                         = ns,
      db_con                     = db_con,
      scales_trigger             = scales_trigger,
      get_lang                   = get_lang,
      aktiver_admin              = aktiver_admin,
      aktiver_admin_id           = aktiver_admin_id,
      user_table_trigger         = user_table_trigger,
      texts_table_trigger        = texts_table_trigger,
      sources_trigger            = sources_trigger,
      scales_table_trigger       = scales_table_trigger,
      ratings_edit_trigger       = ratings_edit_trigger,
      current_page_users         = current_page_users,
      current_page_texts         = current_page_texts,
      current_page_coding_search = current_page_coding_search,
      pending_import             = pending_import
    )

    # Register modular subtab server handlers (Tabs 1-8)
    admin_register_users_table_server(input, output, session, ctx)
    admin_register_users_crud_server(input, output, session, ctx)
    admin_register_texts_server(input, output, session, ctx)
    admin_register_sources_server(input, output, session, ctx)
    admin_register_scales_server(input, output, session, ctx)
    admin_register_import_export_server(input, output, session, ctx)
    admin_register_master_export_server(input, output, session, ctx)
    admin_register_reliability_server(input, output, session, ctx)
    admin_register_coding_search_server(input, output, session, ctx)
    db_diagnostics_report <- admin_register_diagnostics_server(input, output, session, ctx)

    invisible(db_diagnostics_report)
  })
}
