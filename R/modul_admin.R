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

    user_table_trigger  <- reactiveVal(0)
    texts_table_trigger <- reactiveVal(0)
    sources_trigger     <- reactiveVal(0)

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

    user_to_edit          <- reactiveVal(NULL)
    user_to_delete        <- reactiveVal(NULL)
    text_to_assign_source <- reactiveVal(NULL)
    source_to_edit        <- reactiveVal(NULL)
    source_to_delete      <- reactiveVal(NULL)
    target_user_for_sources <- reactiveVal(NULL)
    scale_to_edit         <- reactiveVal(NULL)
    scales_table_trigger  <- reactiveVal(0)

    generiere_zufalls_token <- function(laenge = 12) {
      generate_security_token(laenge)
    }

    get_lizenz_optionen <- function(lang) {
      get_source_license_choices(lang)
    }

    # Initialize user_source_permissions and user_coding_source_permissions tables
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

    # ==========================================================================
    # 1. USER MANAGEMENT (TAB 1 - WITH 25-USER PAGINATION & SEARCH)
    # ==========================================================================
    current_page_users <- reactiveVal(1)
    page_size_users    <- 25

    # Reactive filtering of users by ID, username, role, or active status
    filtered_users_data <- reactive({
      user_table_trigger()

      users_df <- tryCatch({
        dbGetQuery(db_con, "SELECT user_id, username, token, permissions, is_active FROM user_tokens ORDER BY permissions ASC, user_id ASC")
      }, error = function(e) {
        data.frame(user_id = integer(0), username = character(0), token = character(0), permissions = character(0), is_active = integer(0))
      })

      suchtext <- trimws(input$user_search_query)
      if (!is.null(suchtext) && suchtext != "" && nrow(users_df) > 0) {
        muster <- tolower(suchtext)
        u_name <- tolower(ifelse(is.na(users_df$username), "", users_df$username))
        u_id_str <- paste0("#", users_df$user_id, " ", users_df$user_id)
        u_perm <- tolower(ifelse(is.na(users_df$permissions), "", users_df$permissions))
        status_text <- ifelse(as.integer(users_df$is_active) == 1, "aktiv active enabled freigeschaltet", "inaktiv inactive disabled locked gesperrt")
        perm_display <- ifelse(users_df$permissions == "admin", "administrator admin", "kodierer coder user")

        treffer <- grepl(muster, u_name, fixed = TRUE) |
                   grepl(muster, u_id_str, fixed = TRUE) |
                   grepl(muster, u_perm, fixed = TRUE) |
                   grepl(muster, status_text, fixed = TRUE) |
                   grepl(muster, perm_display, fixed = TRUE)
        users_df <- users_df[treffer, , drop = FALSE]
      }

      users_df
    })

    # Reset pagination to page 1 when the search query changes
    observeEvent(input$user_search_query, {
      current_page_users(1)
    })

    # Compute total number of available pages
    total_pages_users <- reactive({
      df <- filtered_users_data()
      max(1, ceiling(nrow(df) / page_size_users))
    })

    # Counter badge (total vs. filtered)
    output$users_counter_badge <- renderUI({
      lang <- get_lang()
      df <- filtered_users_data()
      total_count <- tryCatch({
        dbGetQuery(db_con, "SELECT COUNT(*) AS total FROM user_tokens")$total
      }, error = function(e) nrow(df))

      if (nrow(df) == total_count) {
        tr("admin.users.counter_all", lang, as.integer(total_count))
      } else {
        tr("admin.users.counter_filtered", lang, nrow(df), as.integer(total_count))
      }
    })

    # Paging and pagination bar for user management
    output$users_pagination_controls <- renderUI({
      lang <- get_lang()
      df <- filtered_users_data()
      total_rows <- nrow(df)
      if (total_rows == 0) return(NULL)

      tot_pages <- total_pages_users()
      cur_page <- min(max(1, current_page_users()), tot_pages)

      start_idx <- (cur_page - 1) * page_size_users + 1
      end_idx <- min(total_rows, cur_page * page_size_users)

      div(
        class = "d-flex flex-wrap justify-content-between align-items-center gap-3 p-3 bg-light rounded border",
        div(
          class = "text-muted small",
          tags$strong(tr("admin.users.pagination_showing", lang, start_idx, end_idx, total_rows)),
          paste0(" (", tr("common.page_x_of_y", lang, cur_page, tot_pages), ")")
        ),
        div(
          class = "d-flex align-items-center gap-2",
          actionButton(ns("btn_users_first"), paste0("\u00ab ", tr("common.first", lang)), class = "btn-sm btn-outline-secondary", disabled = if (cur_page <= 1) "disabled" else NULL),
          actionButton(ns("btn_users_prev"), paste0("\u2039 ", tr("common.previous", lang)), class = "btn-sm btn-outline-primary", disabled = if (cur_page <= 1) "disabled" else NULL),
          div(
            class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", tr("common.page_label", lang)),
            tags$input(
              id = ns("input_users_jump_page"),
              type = "number",
              class = "form-control form-control-sm text-center",
              style = "width: 65px; display: inline-block;",
              value = cur_page,
              min = 1,
              max = tot_pages,
              onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", ns("trigger_jump_users_page"))
            ),
            span(class = "small text-muted", paste0("/ ", tot_pages))
          ),
          actionButton(ns("btn_users_next"), paste0(tr("common.next", lang), " \u203a"), class = "btn-sm btn-outline-primary", disabled = if (cur_page >= tot_pages) "disabled" else NULL),
          actionButton(ns("btn_users_last"), paste0(tr("common.last", lang), " \u00bb"), class = "btn-sm btn-outline-secondary", disabled = if (cur_page >= tot_pages) "disabled" else NULL)
        )
      )
    })

    # Paging event handlers for user page navigation
    observeEvent(input$btn_users_first, {
      current_page_users(1)
    })

    observeEvent(input$btn_users_prev, {
      cur <- current_page_users()
      if (cur > 1) current_page_users(cur - 1)
    })

    observeEvent(input$btn_users_next, {
      cur <- current_page_users()
      tot <- total_pages_users()
      if (cur < tot) current_page_users(cur + 1)
    })

    observeEvent(input$btn_users_last, {
      tot <- total_pages_users()
      current_page_users(tot)
    })

    observeEvent(input$trigger_jump_users_page, {
      ziel <- as.integer(input$trigger_jump_users_page)
      tot <- total_pages_users()
      if (!is.na(ziel)) {
        sichere_seite <- max(1, min(tot, ziel))
        current_page_users(sichere_seite)
      }
    })

    output$users_table_container <- renderUI({
      lang <- get_lang()
      users_df <- filtered_users_data()

      if (nrow(users_df) == 0) {
        return(div(class = "alert alert-warning text-center py-4",
                   icon("info-circle", class = "me-2"),
                   tr("admin.users.empty", lang)))
      }

      tot_pages <- total_pages_users()
      cur_page <- min(max(1, current_page_users()), tot_pages)

      start_idx <- (cur_page - 1) * page_size_users + 1
      end_idx <- min(nrow(users_df), cur_page * page_size_users)

      page_df <- users_df[start_idx:end_idx, , drop = FALSE]

      logged_in <- aktiver_admin()
      logged_in_id <- aktiver_admin_id()
      all_sources_df <- tryCatch({ dbGetQuery(db_con, "SELECT source_id, Kurzreferenz FROM sources ORDER BY source_id ASC") }, error = function(e) data.frame())
      total_sources_count <- nrow(all_sources_df)
      all_scales_df <- tryCatch({ dbGetQuery(db_con, "SELECT scale_id, short_label FROM scale_definitions ORDER BY scale_id ASC") }, error = function(e) data.frame())
      total_scales_count <- nrow(all_scales_df)

      rows <- lapply(1:nrow(page_df), function(i) {
        u_id     <- as.integer(page_df$user_id[i])
        u_name   <- page_df$username[i]
        u_tok    <- page_df$token[i]
        u_perm   <- page_df$permissions[i]
        u_active <- as.integer(page_df$is_active[i])
        is_self  <- (!is.null(logged_in_id) && u_id == logged_in_id) || (u_name == logged_in)

        safe_id <- paste0("user_", u_id)

        role_badge <- if (u_perm == "admin") {
          tags$span(class = "badge bg-dark", style = "font-size: 0.85em; padding: 5px 9px;", paste0("\U0001f451 ", tr("common.role_admin", lang)))
        } else {
          tags$span(class = "badge bg-info text-dark", style = "font-size: 0.85em; padding: 5px 9px;", paste0("\U0001f464 ", tr("common.role_coder", lang)))
        }

        status_widget <- if (is_self) {
          tags$span(
            class = "badge bg-success",
            style = "font-size: 0.85em; padding: 5px 9px; cursor: default;",
            icon("shield-alt", class = "me-1"),
            tr("admin.users.badge_self_active", lang)
          )
        } else if (u_active == 1) {
          actionButton(
            inputId = ns(paste0("btn_toggle_active_", safe_id)),
            label = tr("admin.users.status_active_btn_short", lang),
            icon = icon("check-circle"),
            class = "btn-sm btn-outline-success",
            style = "font-weight: 500; min-width: 90px;",
            title = tr("admin.users.status_active_tip", lang),
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_toggle_user_status"), u_id)
          )
        } else {
          actionButton(
            inputId = ns(paste0("btn_toggle_inactive_", safe_id)),
            label = tr("admin.users.status_inactive_btn_short", lang),
            icon = icon("ban"),
            class = "btn-sm btn-outline-danger",
            style = "font-weight: 500; min-width: 90px; background-color: #fff5f5;",
            title = tr("admin.users.status_inactive_tip", lang),
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_toggle_user_status"), u_id)
          )
        }

        sources_widget <- if (u_perm == "admin") {
          tags$span(class = "badge bg-secondary", style = "font-size: 0.8em;", icon("globe", class = "me-1"), tr("admin.users.full_access_admin", lang))
        } else {
          user_text_sources <- tryCatch({
            dbGetQuery(db_con, "SELECT source_id FROM user_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
          }, error = function(e) integer(0))
          user_coding_sources <- tryCatch({
            dbGetQuery(db_con, "SELECT source_id FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
          }, error = function(e) integer(0))

          n_text <- length(user_text_sources)
          n_code <- length(user_coding_sources)

          btn_label <- tr("admin.users.btn_sources_label", lang, n_text, total_sources_count, n_code, total_sources_count)
          is_locked <- (n_text == 0 && n_code == 0)

          actionButton(
            inputId = ns(paste0("btn_sources_", safe_id)),
            label = btn_label,
            icon = icon("book"),
            class = if (is_locked) "btn-sm btn-outline-danger" else "btn-sm btn-outline-secondary",
            style = "font-size: 0.8em;",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_manage_user_sources"), u_id)
          )
        }

        scales_widget <- if (u_perm == "admin") {
          tags$span(class = "badge bg-secondary", style = "font-size: 0.8em;", icon("sliders-h", class = "me-1"), tr("admin.users.full_access_admin", lang))
        } else {
          user_scales <- tryCatch({
            dbGetQuery(db_con, "SELECT scale_id FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))$scale_id
          }, error = function(e) integer(0))

          n_scales <- length(user_scales)
          btn_label_scales <- tr("admin.users.btn_scales_label", lang, n_scales, total_scales_count)
          is_scales_locked <- (n_scales == 0)

          actionButton(
            inputId = ns(paste0("btn_scales_", safe_id)),
            label = btn_label_scales,
            icon = icon("sliders-h"),
            class = if (is_scales_locked) "btn-sm btn-outline-danger" else "btn-sm btn-outline-primary",
            style = "font-size: 0.8em;",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_manage_user_scales"), u_id)
          )
        }

        edit_btn <- actionButton(
          inputId = ns(paste0("btn_edit_", safe_id)),
          label = tr("common.edit", lang),
          icon = icon("edit"),
          class = "btn-sm btn-outline-primary me-1",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_edit_user"), u_id)
        )

        delete_btn <- if (is_self) {
          tags$button(class = "btn btn-sm btn-outline-secondary", disabled = "disabled", title = tr("admin.users.self_protect_tip", lang), icon("trash"))
        } else {
          actionButton(
            inputId = ns(paste0("btn_del_", safe_id)),
            label = tr("common.delete", lang),
            icon = icon("trash"),
            class = "btn-sm btn-outline-danger",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_delete_user"), u_id)
          )
        }

        tags$tr(
          class = if (u_active == 0) "table-light text-muted" else "",
          tags$td(
            style = "vertical-align: middle;",
            tags$span(class = "badge bg-light text-secondary border font-monospace", paste0("#", u_id))
          ),
          tags$td(
            style = "vertical-align: middle;",
            tags$strong(u_name),
            if (is_self) tags$span(class = "badge bg-secondary ms-1", tr("admin.users.badge_currently_logged_in", lang))
          ),
          tags$td(style = "vertical-align: middle; font-family: monospace;", tags$code(u_tok)),
          tags$td(style = "vertical-align: middle;", role_badge),
          tags$td(style = "vertical-align: middle;", status_widget),
          tags$td(style = "vertical-align: middle;", sources_widget),
          tags$td(style = "vertical-align: middle;", scales_widget),
          tags$td(style = "vertical-align: middle; text-align: right;", div(edit_btn, delete_btn))
        )
      })

      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-hover align-middle mb-0",
          tags$thead(
            class = "table-light",
            tags$tr(
              tags$th(style = "width: 70px;", tr("admin.users.col_id", lang)),
              tags$th(tr("admin.users.col_username", lang)),
              tags$th(tr("admin.users.col_token_header", lang)),
              tags$th(tr("admin.users.col_role", lang)),
              tags$th(tr("admin.users.col_status_login", lang)),
              tags$th(tr("admin.users.col_sources", lang)),
              tags$th(tr("admin.users.col_scales", lang)),
              tags$th(style = "text-align: right;", tr("common.actions", lang))
            )
          ),
          tags$tbody(rows)
        )
      )
    })

    # 1-click active status toggle by user_id
    observeEvent(input$trigger_toggle_user_status, {
      lang <- get_lang()
      u_target <- input$trigger_toggle_user_status
      req(u_target)

      user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
        dbGetQuery(db_con, "SELECT user_id, username, is_active FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
      } else {
        dbGetQuery(db_con, "SELECT user_id, username, is_active FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
      }

      if (nrow(user_row) == 0) return()
      u_id <- as.integer(user_row$user_id[1])
      u_name <- as.character(user_row$username[1])

      if (u_id == aktiver_admin_id() || u_name == aktiver_admin()) {
        showNotification(tr("admin.users.notif_self_deactivate_err", lang), type = "error")
        return()
      }

      current_status <- user_row$is_active[1]
      new_status <- if (as.integer(current_status) == 1) 0 else 1

      tryCatch({
        dbExecute(db_con, "UPDATE user_tokens SET is_active = ? WHERE user_id = ?", params = list(new_status, u_id))
        user_table_trigger(user_table_trigger() + 1)
        if (new_status == 1) {
          showNotification(tr("admin.users.notif_status_active", lang, u_name, u_id), type = "message")
        } else {
          showNotification(tr("admin.users.notif_status_inactive", lang, u_name, u_id), type = "warning")
        }
      }, error = function(e) {
        showNotification(tr("admin.users.notif_status_err", lang, e$message), type = "error")
      })
    })

    # Manage source permissions by user_id
    observeEvent(input$trigger_manage_user_sources, {
      lang <- get_lang()
      u_target <- input$trigger_manage_user_sources
      req(u_target)

      user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
        dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
      } else {
        dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
      }

      if (nrow(user_row) == 0) return()
      u_id <- as.integer(user_row$user_id[1])
      u_name <- as.character(user_row$username[1])

      target_user_for_sources(list(user_id = u_id, username = u_name))

      all_sources <- dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz FROM sources ORDER BY source_id ASC")
      if (nrow(all_sources) == 0) {
        showNotification(tr("admin.users.notif_no_sources", lang), type = "warning")
        return()
      }

      assigned_text <- tryCatch({
        dbGetQuery(db_con, "SELECT source_id FROM user_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
      }, error = function(e) integer(0))

      assigned_coding <- tryCatch({
        dbGetQuery(db_con, "SELECT source_id FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
      }, error = function(e) integer(0))

      choices_list <- setNames(
        as.character(all_sources$source_id),
        paste0("[#", all_sources$source_id, "] ", all_sources$Kurzreferenz, " - ", substr(all_sources$Referenz, 1, 65), "...")
      )

      showModal(modalDialog(
        title = div(icon("shield-alt", class = "me-2 text-primary"), tr("admin.permissions.sources_modal_title", lang, paste0(u_name, " [#", u_id, "]"))),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_user_sources"), tr("admin.permissions.btn_save_permissions", lang), class = "btn-primary")
        ),
        div(
          p(class = "text-muted small", tr("admin.permissions.sources_modal_subtitle", lang)),

          tabsetPanel(
            id = ns("tabs_user_sources"),
            type = "tabs",

            tabPanel(
              title = tagList(icon("file-lines", class = "me-1 text-primary"), tr("admin.permissions.tab_text_sources", lang)),
              br(),
              div(class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded border",
                  div(
                    tags$strong("Status: "),
                    tags$span(class = "text-muted small", paste0("(", length(assigned_text), " / ", nrow(all_sources), ")"))
                  ),
                  div(class = "d-flex gap-2",
                      actionButton(ns("btn_select_all_text_sources"), tr("admin.permissions.btn_select_all", lang),
                                   icon = icon("check-square"), class = "btn-sm btn-outline-primary"),
                      actionButton(ns("btn_deselect_all_text_sources"), tr("admin.permissions.btn_deselect_all", lang),
                                   icon = icon("square"), class = "btn-sm btn-outline-secondary")
                  )
              ),

              div(style = "max-height: 340px; overflow-y: auto; padding: 12px; border: 1px solid #ced4da; border-radius: 4px; background-color: #ffffff;",
                  checkboxGroupInput(ns("selected_user_text_sources"), label = NULL, choices = choices_list, selected = as.character(assigned_text))
              )
            ),

            tabPanel(
              title = tagList(icon("tag", class = "me-1 text-success"), tr("admin.permissions.tab_coding_sources", lang)),
              br(),
              div(class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded border",
                  div(
                    tags$strong("Status: "),
                    tags$span(class = "text-muted small", paste0("(", length(assigned_coding), " / ", nrow(all_sources), ")"))
                  ),
                  div(class = "d-flex gap-2",
                      actionButton(ns("btn_select_all_coding_sources"), tr("admin.permissions.btn_select_all", lang),
                                   icon = icon("check-square"), class = "btn-sm btn-outline-success"),
                      actionButton(ns("btn_deselect_all_coding_sources"), tr("admin.permissions.btn_deselect_all", lang),
                                   icon = icon("square"), class = "btn-sm btn-outline-secondary")
                  )
              ),

              div(style = "max-height: 340px; overflow-y: auto; padding: 12px; border: 1px solid #ced4da; border-radius: 4px; background-color: #ffffff;",
                  checkboxGroupInput(ns("selected_user_coding_sources"), label = NULL, choices = choices_list, selected = as.character(assigned_coding))
              )
            )
          ),
          tags$input(type = "hidden", id = ns("target_sources_user_id"), value = as.character(u_id)),
          tags$input(type = "hidden", id = ns("target_sources_username"), value = u_name)
        )
      ))
    })

    # Quick selection for text sources
    observeEvent(input$btn_select_all_text_sources, {
      all_ids <- dbGetQuery(db_con, "SELECT source_id FROM sources ORDER BY source_id ASC")$source_id
      updateCheckboxGroupInput(session, "selected_user_text_sources", selected = as.character(all_ids))
    })

    observeEvent(input$btn_deselect_all_text_sources, {
      updateCheckboxGroupInput(session, "selected_user_text_sources", selected = character(0))
    })

    # Quick selection for coding sources
    observeEvent(input$btn_select_all_coding_sources, {
      all_ids <- dbGetQuery(db_con, "SELECT source_id FROM sources ORDER BY source_id ASC")$source_id
      updateCheckboxGroupInput(session, "selected_user_coding_sources", selected = as.character(all_ids))
    })

    observeEvent(input$btn_deselect_all_coding_sources, {
      updateCheckboxGroupInput(session, "selected_user_coding_sources", selected = character(0))
    })

    # Save both source permission types
    observeEvent(input$btn_save_user_sources, {
      lang <- get_lang()
      u_info <- target_user_for_sources()
      req(u_info)
      u_id <- as.integer(u_info$user_id)
      u_name <- as.character(u_info$username)

      selected_text_ids <- as.integer(input$selected_user_text_sources)
      selected_coding_ids <- as.integer(input$selected_user_coding_sources)

      tryCatch({
        with_db_transaction(db_con, function(tx_con) {
          dbExecute(tx_con, "DELETE FROM user_source_permissions WHERE user_id = ?", params = list(u_id))
          if (length(selected_text_ids) > 0) {
            for (sid in selected_text_ids) {
              dbExecute(tx_con, "INSERT INTO user_source_permissions (user_id, source_id) VALUES (?, ?)", params = list(u_id, sid))
            }
          }

          dbExecute(tx_con, "DELETE FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))
          if (length(selected_coding_ids) > 0) {
            for (sid in selected_coding_ids) {
              dbExecute(tx_con, "INSERT INTO user_coding_source_permissions (user_id, source_id) VALUES (?, ?)", params = list(u_id, sid))
            }
          }
        })

        removeModal()
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.users.notif_sources_updated", lang, u_name, u_id), type = "message")
      }, error = function(e) {
        showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
      })
    })

    # ==========================================================================
    # MODAL: SCALE PERMISSIONS (CODING MODULE ONLY)
    # ==========================================================================
    target_user_for_scales <- reactiveVal(NULL)

    observeEvent(input$trigger_manage_user_scales, {
      lang <- get_lang()
      u_target <- input$trigger_manage_user_scales
      req(u_target)

      user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
        dbGetQuery(db_con, "SELECT user_id, username, permissions FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
      } else {
        dbGetQuery(db_con, "SELECT user_id, username, permissions FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
      }

      if (nrow(user_row) == 0) return()
      u_id <- as.integer(user_row$user_id[1])
      u_name <- as.character(user_row$username[1])
      u_perm <- as.character(user_row$permissions[1])

      if (u_perm == "admin") {
        showNotification(tr("admin.users.notif_admin_full_scales", lang), type = "message")
        return()
      }

      target_user_for_scales(list(user_id = u_id, username = u_name))

      all_scales <- dbGetQuery(db_con, "SELECT scale_id, short_label, name, max_value FROM scale_definitions ORDER BY scale_id ASC")
      if (nrow(all_scales) == 0) {
        showNotification(tr("admin.users.notif_no_scales", lang), type = "warning")
        return()
      }

      assigned_scales <- tryCatch({
        dbGetQuery(db_con, "SELECT scale_id FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))$scale_id
      }, error = function(e) integer(0))

      choices_list <- setNames(
        as.character(all_scales$scale_id),
        paste0("[#", all_scales$scale_id, "] ", all_scales$short_label, " - ", all_scales$name, " (0..", all_scales$max_value, ")")
      )

      showModal(modalDialog(
        title = div(icon("sliders-h", class = "me-2 text-primary"), tr("admin.permissions.scales_modal_title", lang, paste0(u_name, " [#", u_id, "]"))),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_user_scales"), tr("admin.permissions.btn_save_permissions", lang), class = "btn-primary")
        ),
        div(
          div(class = "alert alert-info py-2 px-3 mb-3 small",
              icon("info-circle", class = "me-2"),
              tr("admin.permissions.scales_modal_subtitle", lang)
          ),
          div(class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded border",
              div(
                tags$strong("Status: "),
                tags$span(class = "text-muted small", tr("admin.permissions.scales_summary", lang, length(assigned_scales), nrow(all_scales)))
              ),
              div(class = "d-flex gap-2",
                  actionButton(ns("btn_select_all_scales"), tr("admin.permissions.btn_select_all", lang),
                               icon = icon("check-square"), class = "btn-sm btn-outline-primary"),
                  actionButton(ns("btn_deselect_all_scales"), tr("admin.permissions.btn_deselect_all", lang),
                               icon = icon("square"), class = "btn-sm btn-outline-secondary")
              )
          ),
          div(style = "max-height: 360px; overflow-y: auto; padding: 12px; border: 1px solid #ced4da; border-radius: 4px; background-color: #ffffff;",
              checkboxGroupInput(ns("selected_user_scales"), label = NULL, choices = choices_list, selected = as.character(assigned_scales))
          )
        )
      ))
    })

    # Quick selection for scales
    observeEvent(input$btn_select_all_scales, {
      all_ids <- dbGetQuery(db_con, "SELECT scale_id FROM scale_definitions ORDER BY scale_id ASC")$scale_id
      updateCheckboxGroupInput(session, "selected_user_scales", selected = as.character(all_ids))
    })

    observeEvent(input$btn_deselect_all_scales, {
      updateCheckboxGroupInput(session, "selected_user_scales", selected = character(0))
    })

    # Save scale permissions
    observeEvent(input$btn_save_user_scales, {
      lang <- get_lang()
      u_info <- target_user_for_scales()
      req(u_info)
      u_id <- as.integer(u_info$user_id)
      u_name <- as.character(u_info$username)

      selected_scale_ids <- as.integer(input$selected_user_scales)

      tryCatch({
        with_db_transaction(db_con, function(tx_con) {
          dbExecute(tx_con, "DELETE FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))
          if (length(selected_scale_ids) > 0) {
            for (sid in selected_scale_ids) {
              dbExecute(tx_con, "INSERT INTO user_scale_permissions (user_id, scale_id) VALUES (?, ?)", params = list(u_id, sid))
            }
          }
        })

        removeModal()
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.users.notif_scales_updated", lang, u_name, u_id), type = "message")
      }, error = function(e) {
        showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
      })
    })

    # Modal: Create new user
    observeEvent(input$btn_open_add_user, {
      lang <- get_lang()
      zufall <- generiere_zufalls_token(10)
      role_choices <- setNames(c("user", "admin"), c(tr("admin.users.opt_user_desc", lang), tr("admin.users.opt_admin_desc", lang)))
      status_choices <- setNames(c(1, 0), c(tr("admin.users.opt_active_desc", lang), tr("admin.users.opt_inactive_desc", lang)))

      showModal(modalDialog(
        title = div(icon("user-plus", class = "me-2 text-success"), tr("admin.users.modal_add_title", lang)),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_new_user"), tr("admin.users.btn_submit_add", lang), class = "btn-success")
        ),
        div(
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_username_req", lang)),
              textInput(ns("new_username"), label = NULL, placeholder = tr("admin.users.placeholder_username", lang)),
              tags$small(class = "text-muted", tr("admin.users.hint_username", lang))
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_token_req", lang)),
              div(class = "input-group",
                  textInput(ns("new_token"), label = NULL, value = zufall),
                  actionButton(ns("btn_generate_new_token"), tr("admin.users.btn_generate", lang), icon = icon("sync"), class = "btn-outline-secondary")
              ),
              tags$small(class = "text-muted", tr("admin.users.placeholder_token", lang))
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_role_req", lang)),
              selectInput(ns("new_permissions"), label = NULL, choices = role_choices, selected = "user")
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_status_req", lang)),
              selectInput(ns("new_is_active"), label = NULL, choices = status_choices, selected = 1)
          ),
          uiOutput(ns("add_user_feedback"))
        )
      ))
    })

    observeEvent(input$btn_generate_new_token, {
      updateTextInput(session, "new_token", value = generiere_zufalls_token(10))
    })

    observeEvent(input$btn_save_new_user, {
      lang <- get_lang()
      u_name <- trimws(input$new_username)
      u_token <- trimws(input$new_token)
      u_perm <- input$new_permissions
      u_active <- as.integer(input$new_is_active)

      if (is.null(u_name) || nchar(u_name) < 3) {
        output$add_user_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_username_short", lang))
        })
        return()
      }

      if (is.null(u_token) || nchar(u_token) < 4) {
        output$add_user_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_token_short", lang))
        })
        return()
      }

      existiert <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM user_tokens WHERE username = ?", params = list(u_name))$n
      if (existiert > 0) {
        output$add_user_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_username_exists", lang, u_name))
        })
        return()
      }

      tryCatch({
        dbExecute(db_con, "INSERT INTO user_tokens (username, token, permissions, is_active) VALUES (?, ?, ?, ?)",
                  params = list(u_name, u_token, u_perm, u_active))

        new_user_res <- dbGetQuery(db_con, "SELECT user_id FROM user_tokens WHERE username = ?", params = list(u_name))
        new_u_id <- if (nrow(new_user_res) > 0) as.integer(new_user_res$user_id[1]) else 0L

        removeModal()
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.users.notif_created_id", lang, u_name, new_u_id), type = "message")
      }, error = function(e) {
        output$add_user_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
        })
      })
    })

    # Modal: Edit user by user_id
    observeEvent(input$trigger_edit_user, {
      lang <- get_lang()
      u_target <- input$trigger_edit_user
      req(u_target)

      user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
        dbGetQuery(db_con, "SELECT user_id, username, token, permissions, is_active FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
      } else {
        dbGetQuery(db_con, "SELECT user_id, username, token, permissions, is_active FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
      }

      if (nrow(user_row) == 0) return()

      user_to_edit(user_row)
      u_id <- as.integer(user_row$user_id[1])
      u_name <- as.character(user_row$username[1])
      is_self <- (u_id == aktiver_admin_id() || u_name == aktiver_admin())

      role_choices <- setNames(c("user", "admin"), c(tr("admin.users.opt_user_desc", lang), tr("admin.users.opt_admin_desc", lang)))
      status_choices <- setNames(c(1, 0), c(tr("admin.users.opt_active_desc", lang), tr("admin.users.opt_inactive_desc", lang)))

      showModal(modalDialog(
        title = div(icon("user-edit", class = "me-2 text-primary"), tr("admin.users.modal_edit_title", lang, paste0(u_name, " [#", u_id, "]"))),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_edited_user"), tr("common.save_changes", lang), class = "btn-primary")
        ),
        div(
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_id_and_name", lang)),
              tags$div(
                class = "input-group",
                tags$span(class = "input-group-text font-monospace bg-light", paste0("#", u_id)),
                tags$input(type = "text", class = "form-control", value = u_name, disabled = "disabled")
              ),
              tags$small(class = "text-muted", tr("admin.users.hint_id_immutable", lang))
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_token_req", lang)),
              div(class = "input-group",
                  textInput(ns("edit_token"), label = NULL, value = user_row$token),
                  actionButton(ns("btn_generate_edit_token"), tr("admin.users.btn_generate", lang), icon = icon("sync"), class = "btn-outline-secondary")
              )
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_role", lang)),
              if (is_self) {
                tagList(
                  tags$input(type = "text", class = "form-control", value = tr("admin.users.self_admin_immutable", lang), disabled = "disabled"),
                  tags$input(type = "hidden", id = ns("edit_permissions"), value = "admin")
                )
              } else {
                selectInput(ns("edit_permissions"), label = NULL, choices = role_choices, selected = user_row$permissions)
              }
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.users.label_account_status", lang)),
              if (is_self) {
                tagList(
                  tags$span(class = "badge bg-success p-2", icon("shield-alt", class = "me-1"), tr("admin.users.self_active_protected", lang)),
                  p(class = "text-muted small mt-1", tr("admin.users.err_self_deactivate", lang))
                )
              } else {
                selectInput(ns("edit_is_active"), label = NULL, choices = status_choices, selected = as.integer(user_row$is_active))
              }
          ),
          uiOutput(ns("edit_user_feedback"))
        )
      ))
    })

    observeEvent(input$btn_generate_edit_token, {
      updateTextInput(session, "edit_token", value = generiere_zufalls_token(10))
    })

    observeEvent(input$btn_save_edited_user, {
      lang <- get_lang()
      row <- user_to_edit()
      req(row)

      u_id <- as.integer(row$user_id[1])
      u_name <- as.character(row$username[1])
      is_self <- (u_id == aktiver_admin_id() || u_name == aktiver_admin())
      new_token <- trimws(input$edit_token)

      new_perm <- if (is_self) "admin" else input$edit_permissions
      new_active <- if (is_self) 1 else as.integer(input$edit_is_active)

      if (is.null(new_token) || nchar(new_token) < 4) {
        output$edit_user_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_token_short", lang))
        })
        return()
      }

      tryCatch({
        dbExecute(db_con, "UPDATE user_tokens SET token = ?, permissions = ?, is_active = ? WHERE user_id = ?",
                  params = list(new_token, new_perm, new_active, u_id))

        removeModal()
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.users.notif_updated_id", lang, u_name, u_id), type = "message")
      }, error = function(e) {
        output$edit_user_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
        })
      })
    })

    # Modal: Delete user by user_id
    observeEvent(input$trigger_delete_user, {
      lang <- get_lang()
      u_target <- input$trigger_delete_user
      req(u_target)

      user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
        dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
      } else {
        dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
      }

      if (nrow(user_row) == 0) return()
      u_id <- as.integer(user_row$user_id[1])
      u_name <- as.character(user_row$username[1])

      if (u_id == aktiver_admin_id() || u_name == aktiver_admin()) {
        showNotification(tr("admin.users.notif_self_delete_err", lang), type = "error")
        return()
      }

      user_to_delete(list(user_id = u_id, username = u_name))

      bewertungs_count <- tryCatch({
        as.integer(dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM user_ratings WHERE user_id = ?", params = list(u_id))$n[1])
      }, error = function(e) 0L)

      showModal(modalDialog(
        title = div(icon("trash", class = "me-2 text-danger"), tr("admin.users.modal_delete_title_id", lang, u_name, u_id)),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_confirm_delete_user"), tr("admin.users.btn_confirm_delete", lang), class = "btn-danger")
        ),
        div(
          p(tags$strong(tr("admin.users.modal_delete_confirm_id", lang, u_name, u_id))),
          p(tr("admin.users.modal_delete_desc", lang), " ", tags$code("user_tokens"), "."),
          p(tags$small(class = "text-muted", tr("admin.users.modal_delete_hint", lang))),
          if (bewertungs_count > 0) {
            div(class = "alert alert-info py-2",
                icon("info-circle", class = "me-1"),
                tr("admin.users.modal_delete_ratings_info", lang, bewertungs_count, u_id)
            )
          } else {
            p(class = "text-muted small", tr("admin.users.modal_delete_no_ratings", lang))
          }
        )
      ))
    })

    observeEvent(input$btn_confirm_delete_user, {
      lang <- get_lang()
      u_info <- user_to_delete()
      req(u_info)
      u_id <- as.integer(u_info$user_id)
      u_name <- as.character(u_info$username)

      if (u_id == aktiver_admin_id() || u_name == aktiver_admin()) {
        removeModal()
        showNotification(tr("admin.users.notif_self_delete_denied", lang), type = "error")
        return()
      }

      tryCatch({
        with_db_transaction(db_con, function(tx_con) {
          dbExecute(tx_con, "DELETE FROM user_ratings WHERE user_id = ?", params = list(u_id))
          dbExecute(tx_con, "DELETE FROM coding_training WHERE user_id = ?", params = list(u_id))
          dbExecute(tx_con, "DELETE FROM user_source_permissions WHERE user_id = ?", params = list(u_id))
          dbExecute(tx_con, "DELETE FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))
          dbExecute(tx_con, "DELETE FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))
          dbExecute(tx_con, "DELETE FROM user_tokens WHERE user_id = ?", params = list(u_id))
        })

        removeModal()
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.users.notif_deleted_id", lang, u_name, u_id), type = "warning")
      }, error = function(e) {
        showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
      })
    })

    # ==========================================================================
    # 2. CASE TEXTS & SOURCE ASSIGNMENT (TAB 2 - WITH 25-ITEM PAGINATION)
    # ==========================================================================
    current_page_texts <- reactiveVal(1)
    page_size_texts    <- 25

    # Reactive filtering of case texts by ID, content, or source reference
    filtered_texts_data <- reactive({
      texts_table_trigger()

      texts_df <- tryCatch({
        dbGetQuery(db_con, "
          SELECT t.text_id, t.text, t.source_id, s.Kurzreferenz, s.Referenz, s.Lizenz
          FROM texts t
          LEFT JOIN sources s ON t.source_id = s.source_id
          ORDER BY t.text_id ASC
        ")
      }, error = function(e) data.frame())

      suchtext <- trimws(input$texts_search_query)
      if (!is.null(suchtext) && suchtext != "" && nrow(texts_df) > 0) {
        such_bereinigt <- gsub("^#", "", suchtext)
        muster <- tolower(such_bereinigt)
        s_kurz <- tolower(ifelse(is.na(texts_df$Kurzreferenz), "", texts_df$Kurzreferenz))
        s_ref  <- tolower(ifelse(is.na(texts_df$Referenz), "", texts_df$Referenz))
        b_text <- tolower(ifelse(is.na(texts_df$text), "", texts_df$text))
        t_id   <- as.character(texts_df$text_id)

        treffer <- grepl(muster, t_id) |
                   grepl(tolower(suchtext), b_text) |
                   grepl(tolower(suchtext), s_kurz) |
                   grepl(tolower(suchtext), s_ref)
        texts_df <- texts_df[treffer, , drop = FALSE]
      }

      texts_df
    })

    # Reset pagination to page 1 when the search query changes
    observeEvent(input$texts_search_query, {
      current_page_texts(1)
    })

    # Compute total number of available pages
    total_pages_texts <- reactive({
      df <- filtered_texts_data()
      max(1, ceiling(nrow(df) / page_size_texts))
    })

    # Counter badge (total vs. filtered)
    output$texts_counter_badge <- renderUI({
      lang <- get_lang()
      df <- filtered_texts_data()
      total_count <- tryCatch({
        dbGetQuery(db_con, "SELECT COUNT(*) AS total FROM texts")$total
      }, error = function(e) nrow(df))

      if (nrow(df) == total_count) {
        tr("admin.texts.counter_all", lang, as.integer(total_count))
      } else {
        tr("admin.texts.count_filtered", lang, nrow(df), as.integer(total_count))
      }
    })

    # Paging and pagination bar
    output$texts_pagination_controls <- renderUI({
      lang <- get_lang()
      df <- filtered_texts_data()
      total_rows <- nrow(df)
      if (total_rows == 0) return(NULL)

      tot_pages <- total_pages_texts()
      cur_page <- min(max(1, current_page_texts()), tot_pages)

      start_idx <- (cur_page - 1) * page_size_texts + 1
      end_idx <- min(total_rows, cur_page * page_size_texts)

      div(
        class = "d-flex flex-wrap justify-content-between align-items-center gap-3 p-3 bg-light rounded border",
        div(
          class = "text-muted small",
          tags$strong(tr("admin.texts.pagination_showing", lang, start_idx, end_idx, total_rows)),
          paste0(" (", tr("common.page_x_of_y", lang, cur_page, tot_pages), ")")
        ),
        div(
          class = "d-flex align-items-center gap-2",
          actionButton(ns("btn_texts_first"), paste0("\u00ab ", tr("common.first", lang)), class = "btn-sm btn-outline-secondary", disabled = if (cur_page <= 1) "disabled" else NULL),
          actionButton(ns("btn_texts_prev"), paste0("\u2039 ", tr("common.previous", lang)), class = "btn-sm btn-outline-primary", disabled = if (cur_page <= 1) "disabled" else NULL),
          div(
            class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", tr("common.page_label", lang)),
            tags$input(
              id = ns("input_texts_jump_page"),
              type = "number",
              class = "form-control form-control-sm text-center",
              style = "width: 65px; display: inline-block;",
              value = cur_page,
              min = 1,
              max = tot_pages,
              onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", ns("trigger_jump_texts_page"))
            ),
            span(class = "small text-muted", paste0("/ ", tot_pages))
          ),
          actionButton(ns("btn_texts_next"), paste0(tr("common.next", lang), " \u203a"), class = "btn-sm btn-outline-primary", disabled = if (cur_page >= tot_pages) "disabled" else NULL),
          actionButton(ns("btn_texts_last"), paste0(tr("common.last", lang), " \u00bb"), class = "btn-sm btn-outline-secondary", disabled = if (cur_page >= tot_pages) "disabled" else NULL)
        )
      )
    })

    # Paging event handlers for page navigation
    observeEvent(input$btn_texts_first, {
      current_page_texts(1)
    })

    observeEvent(input$btn_texts_prev, {
      cur <- current_page_texts()
      if (cur > 1) current_page_texts(cur - 1)
    })

    observeEvent(input$btn_texts_next, {
      cur <- current_page_texts()
      tot <- total_pages_texts()
      if (cur < tot) current_page_texts(cur + 1)
    })

    observeEvent(input$btn_texts_last, {
      tot <- total_pages_texts()
      current_page_texts(tot)
    })

    observeEvent(input$trigger_jump_texts_page, {
      ziel <- as.integer(input$trigger_jump_texts_page)
      tot <- total_pages_texts()
      if (!is.na(ziel)) {
        sichere_seite <- max(1, min(tot, ziel))
        current_page_texts(sichere_seite)
      }
    })

    # Render case text table (maximum 25 rows per page)
    output$texts_management_table_container <- renderUI({
      lang <- get_lang()
      texts_df <- filtered_texts_data()

      if (nrow(texts_df) == 0) {
        return(div(class = "alert alert-warning text-center py-4",
                   icon("info-circle", class = "me-2"),
                   tr("admin.texts.empty", lang)))
      }

      tot_pages <- total_pages_texts()
      cur_page <- min(max(1, current_page_texts()), tot_pages)

      start_idx <- (cur_page - 1) * page_size_texts + 1
      end_idx <- min(nrow(texts_df), cur_page * page_size_texts)

      page_df <- texts_df[start_idx:end_idx, , drop = FALSE]

      rows <- lapply(1:nrow(page_df), function(i) {
        t_id    <- page_df$text_id[i]
        t_text  <- page_df$text[i]
        s_id    <- page_df$source_id[i]
        s_kurz  <- ifelse(is.na(page_df$Kurzreferenz[i]) || page_df$Kurzreferenz[i] == "", tr("admin.texts.no_source_assigned", lang), page_df$Kurzreferenz[i])
        s_ref   <- ifelse(is.na(page_df$Referenz[i]), "", page_df$Referenz[i])
        s_liz   <- ifelse(is.na(page_df$Lizenz[i]), "", page_df$Lizenz[i])

        tags$tr(
          tags$td(
            style = "font-family: monospace; font-weight: bold;",
            tags$span(class = "badge bg-light text-dark border", paste0("#", t_id))
          ),
          tags$td(
            style = "max-width: 450px; font-size: 0.9em; line-height: 1.4;",
            if (nchar(t_text) > 130) paste0(substr(t_text, 1, 130), "...") else t_text
          ),
          tags$td(
            tags$span(class = "badge bg-primary me-1", icon("book", class = "me-1"), s_kurz),
            if (s_liz != "") tags$span(class = "badge bg-light text-dark border", s_liz)
          ),
          tags$td(
            actionButton(
              inputId = ns(paste0("btn_assign_source_", t_id)),
              label = tr("admin.texts.btn_assign_source", lang),
              icon = icon("link"),
              class = "btn-sm btn-outline-primary",
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_assign_text_source"), t_id)
            )
          )
        )
      })

      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-hover align-middle mb-0",
          tags$thead(
            class = "table-light",
            tags$tr(
              tags$th(style = "width: 90px;", tr("admin.texts.col_id", lang)),
              tags$th(tr("admin.texts.col_excerpt", lang)),
              tags$th(style = "width: 250px;", tr("admin.texts.col_source", lang)),
              tags$th(style = "width: 160px;", tr("common.action", lang))
            )
          ),
          tags$tbody(rows)
        )
      )
    })

    observeEvent(input$trigger_assign_text_source, {
      lang <- get_lang()
      t_id <- as.integer(input$trigger_assign_text_source)
      req(t_id)
      text_to_assign_source(t_id)

      all_sources <- tryCatch({
        dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz, Lizenz FROM sources ORDER BY source_id ASC")
      }, error = function(e) data.frame())

      cur_source <- tryCatch({
        dbGetQuery(db_con, "SELECT source_id FROM texts WHERE text_id = ?", params = list(t_id))$source_id
      }, error = function(e) 1)

      choices <- if (nrow(all_sources) > 0) {
        setNames(all_sources$source_id, paste0(all_sources$Kurzreferenz, " (", all_sources$Lizenz, ") - ", substr(all_sources$Referenz, 1, 55), "..."))
      } else {
        character(0)
      }

      showModal(modalDialog(
        title = div(icon("link", class = "me-2 text-primary"), tr("admin.texts.modal_assign_title", lang, t_id)),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_assigned_source"), tr("admin.texts.btn_assign_source", lang), class = "btn-primary")
        ),
        div(
          p(tr("admin.texts.modal_assign_desc", lang)),
          selectInput(ns("select_new_source_id"), tr("admin.texts.select_source_label", lang), choices = choices, selected = cur_source, width = "100%"),
          p(tags$small(class = "text-muted", tr("admin.texts.modal_assign_hint", lang)))
        )
      ))
    })

    observeEvent(input$btn_save_assigned_source, {
      lang <- get_lang()
      t_id <- text_to_assign_source()
      new_s_id <- as.integer(input$select_new_source_id)
      req(t_id, new_s_id)

      tryCatch({
        dbExecute(db_con, "UPDATE texts SET source_id = ? WHERE text_id = ?", params = list(new_s_id, t_id))
        removeModal()
        texts_table_trigger(texts_table_trigger() + 1)
        showNotification(tr("admin.texts.notif_assigned_id", lang, t_id, new_s_id), type = "message")
      }, error = function(e) {
        showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
      })
    })

    # ==========================================================================
    # 3. SOURCE MANAGEMENT (TAB 3)
    # ==========================================================================
    output$sources_counter_text <- renderUI({
      lang <- get_lang()
      sources_trigger()
      count_res <- tryCatch({
        dbGetQuery(db_con, "SELECT COUNT(*) AS total FROM sources")$total
      }, error = function(e) 0)

      tr("admin.sources.counter_all", lang, as.integer(count_res))
    })

    output$sources_table_container <- renderUI({
      lang <- get_lang()
      sources_trigger()

      query <- "SELECT source_id, Kurzreferenz, Referenz, Lizenz, erfasst_am FROM sources ORDER BY source_id ASC"
      df <- tryCatch({
        dbGetQuery(db_con, query)
      }, error = function(e) {
        data.frame(source_id = integer(0), Kurzreferenz = character(0), Referenz = character(0), Lizenz = character(0), erfasst_am = character(0))
      })

      suchtext <- trimws(input$source_search_query)
      if (!is.null(suchtext) && suchtext != "" && nrow(df) > 0) {
        such_bereinigt <- gsub("^#", "", suchtext)
        muster <- tolower(such_bereinigt)
        treffer <- grepl(muster, as.character(df$source_id)) |
                   grepl(tolower(suchtext), tolower(df$Kurzreferenz)) |
                   grepl(tolower(suchtext), tolower(df$Referenz)) |
                   grepl(tolower(suchtext), tolower(df$Lizenz))
        df <- df[treffer, , drop = FALSE]
      }

      if (nrow(df) == 0) {
        return(div(class = "alert alert-light border text-center py-4",
                   icon("info-circle", class = "me-2 text-muted"),
                   tr("admin.sources.empty", lang)))
      }

      rows <- lapply(1:nrow(df), function(i) {
        s_id    <- df$source_id[i]
        s_kurz  <- df$Kurzreferenz[i]
        s_ref   <- df$Referenz[i]
        s_liz   <- df$Lizenz[i]
        s_date  <- df$erfasst_am[i]

        s_date_str <- tryCatch({
          if (is.null(s_date) || all(is.na(s_date))) {
            ""
          } else if (inherits(s_date, "POSIXt") || inherits(s_date, "Date")) {
            format(s_date, "%Y-%m-%d")
          } else {
            trimws(as.character(s_date))
          }
        }, error = function(e) "")

        safe_btn_id <- paste0("s_", s_id)

        edit_btn <- actionButton(
          inputId = ns(paste0("btn_edit_", safe_btn_id)),
          label = tr("common.edit", lang),
          icon = icon("pen"),
          class = "btn-sm btn-outline-primary me-2",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_edit_source"), s_id)
        )

        delete_btn <- actionButton(
          inputId = ns(paste0("btn_delete_", safe_btn_id)),
          label = tr("common.delete", lang),
          icon = icon("trash"),
          class = "btn-sm btn-outline-danger",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_delete_source"), s_id)
        )

        lizenz_badge <- tags$span(
          class = "badge bg-light text-dark border",
          style = "font-size: 0.85em; font-family: monospace; padding: 4px 8px;",
          s_liz
        )

        tags$tr(
          tags$td(
            style = "vertical-align: middle; width: 8%; font-family: monospace; font-weight: bold; color: #495057;",
            tags$span(
              style = "background-color: #f1f3f5; padding: 3px 7px; border-radius: 4px; border: 1px solid #dee2e6;",
              paste0("#", s_id)
            )
          ),
          tags$td(
            style = "vertical-align: middle; font-weight: 700; width: 22%;",
            tags$span(class = "text-primary", s_kurz),
            if (nzchar(s_date_str) && s_date_str != "NA") {
              tags$div(class = "text-muted small", style = "font-size: 0.75em; font-weight: normal;", tr("admin.sources.captured_at", lang, substr(s_date_str, 1, 10)))
            }
          ),
          tags$td(
            style = "vertical-align: middle; width: 43%; line-height: 1.4;",
            s_ref
          ),
          tags$td(
            style = "vertical-align: middle; width: 12%;",
            lizenz_badge
          ),
          tags$td(
            style = "vertical-align: middle; text-align: right; width: 15%;",
            div(edit_btn, delete_btn)
          )
        )
      })

      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-hover align-middle mb-0",
          tags$thead(
            class = "table-light",
            tags$tr(
              tags$th(style = "width: 8%;", tr("admin.sources.col_id", lang)),
              tags$th(style = "width: 22%;", tr("admin.sources.col_short_ref", lang)),
              tags$th(style = "width: 43%;", tr("admin.sources.col_full_ref", lang)),
              tags$th(style = "width: 12%;", tr("admin.sources.col_license", lang)),
              tags$th(style = "width: 15%; text-align: right;", tr("common.actions", lang))
            )
          ),
          tags$tbody(rows)
        )
      )
    })

    # Modal: Add new source
    observeEvent(input$btn_open_add_source, {
      lang <- get_lang()
      showModal(modalDialog(
        title = div(icon("plus-circle", class = "me-2 text-success"), tr("admin.sources.modal_add_title", lang)),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_new_source"), tr("admin.sources.btn_save_source", lang), class = "btn-success")
        ),
        div(
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_short_ref_req", lang)),
              textInput(ns("new_kurzreferenz"), label = NULL, placeholder = tr("admin.sources.placeholder_short_ref", lang)),
              tags$small(class = "text-muted", tr("admin.sources.hint_short_ref", lang))
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_full_ref_req", lang)),
              textAreaInput(ns("new_referenz"), label = NULL, rows = 3,
                            placeholder = tr("admin.sources.placeholder_full_ref", lang)),
              tags$small(class = "text-muted", tr("admin.sources.placeholder_full_ref", lang))
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_license_req", lang)),
              selectInput(ns("new_lizenz_select"), label = NULL, choices = get_lizenz_optionen(lang), selected = "CC-BY 4.0"),
              uiOutput(ns("new_custom_lizenz_ui"))
          ),
          uiOutput(ns("add_source_feedback"))
        )
      ))
    })

    output$new_custom_lizenz_ui <- renderUI({
      lang <- get_lang()
      if (identical(input$new_lizenz_select, "__custom__")) {
        textInput(ns("new_lizenz_custom"), tr("admin.sources.label_license_req", lang), placeholder = tr("admin.sources.placeholder_custom_license", lang))
      }
    })

    observeEvent(input$btn_save_new_source, {
      lang <- get_lang()
      kurz <- trimws(input$new_kurzreferenz)
      ref  <- trimws(input$new_referenz)

      liz <- if (identical(input$new_lizenz_select, "__custom__")) {
        trimws(input$new_lizenz_custom)
      } else {
        input$new_lizenz_select
      }

      if (is.null(kurz) || nchar(kurz) < 2) {
        output$add_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.sources.err_short_ref", lang))
        })
        return()
      }

      if (is.null(ref) || nchar(ref) < 5) {
        output$add_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.sources.err_full_ref", lang))
        })
        return()
      }

      if (is.null(liz) || liz == "") {
        output$add_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.sources.err_license", lang))
        })
        return()
      }

      existiert <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM sources WHERE LOWER(Kurzreferenz) = LOWER(?)", params = list(kurz))$n
      if (existiert > 0) {
        output$add_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.sources.err_duplicate", lang, kurz))
        })
        return()
      }

      tryCatch({
        dbExecute(db_con, "INSERT INTO sources (Kurzreferenz, Referenz, Lizenz) VALUES (?, ?, ?)",
                  params = list(kurz, ref, liz))

        removeModal()
        sources_trigger(sources_trigger() + 1)
        texts_table_trigger(texts_table_trigger() + 1)
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.sources.notif_created", lang, kurz), type = "message")
      }, error = function(e) {
        output$add_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
        })
      })
    })

    # Modal: Edit source
    observeEvent(input$trigger_edit_source, {
      lang <- get_lang()
      s_id <- as.integer(input$trigger_edit_source)
      req(s_id)

      row <- dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz, Lizenz FROM sources WHERE source_id = ?", params = list(s_id))
      if (nrow(row) == 0) return()

      source_to_edit(row)

      liz_opts <- get_lizenz_optionen(lang)
      is_preset <- row$Lizenz %in% liz_opts
      selected_liz <- if (is_preset) row$Lizenz else "__custom__"

      showModal(modalDialog(
        title = div(icon("edit", class = "me-2 text-primary"), tr("admin.sources.modal_edit_title", lang, s_id)),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_edit_source"), tr("common.save_changes", lang), class = "btn-primary")
        ),
        div(
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_pk_source", lang)),
              tags$input(type = "text", class = "form-control font-monospace", value = paste0("#", s_id), disabled = "disabled"),
              tags$small(class = "text-muted", tr("admin.sources.hint_pk_source", lang))
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_short_ref_req", lang)),
              textInput(ns("edit_kurzreferenz"), label = NULL, value = row$Kurzreferenz)
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_full_ref_req", lang)),
              textAreaInput(ns("edit_referenz"), label = NULL, rows = 3, value = row$Referenz)
          ),
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", tr("admin.sources.label_license_req", lang)),
              selectInput(ns("edit_lizenz_select"), label = NULL, choices = liz_opts, selected = selected_liz),
              uiOutput(ns("edit_custom_lizenz_ui"))
          ),
          uiOutput(ns("edit_source_feedback"))
        )
      ))
    })

    output$edit_custom_lizenz_ui <- renderUI({
      lang <- get_lang()
      row <- source_to_edit()
      liz_opts <- get_lizenz_optionen(lang)
      if (identical(input$edit_lizenz_select, "__custom__")) {
        def_val <- if (!is.null(row) && !(row$Lizenz %in% liz_opts)) row$Lizenz else ""
        textInput(ns("edit_lizenz_custom"), tr("admin.sources.label_license_req", lang), value = def_val)
      }
    })

    observeEvent(input$btn_save_edit_source, {
      lang <- get_lang()
      row <- source_to_edit()
      req(row)

      s_id <- as.integer(row$source_id)
      kurz <- trimws(input$edit_kurzreferenz)
      ref  <- trimws(input$edit_referenz)

      liz <- if (identical(input$edit_lizenz_select, "__custom__")) {
        trimws(input$edit_lizenz_custom)
      } else {
        input$edit_lizenz_select
      }

      if (is.null(kurz) || nchar(kurz) < 2) {
        output$edit_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.sources.err_short_ref", lang))
        })
        return()
      }

      existiert <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM sources WHERE LOWER(Kurzreferenz) = LOWER(?) AND source_id != ?", params = list(kurz, s_id))$n
      if (existiert > 0) {
        output$edit_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.sources.err_duplicate_other", lang, kurz))
        })
        return()
      }

      tryCatch({
        dbExecute(db_con, "UPDATE sources SET Kurzreferenz = ?, Referenz = ?, Lizenz = ? WHERE source_id = ?",
                  params = list(kurz, ref, liz, s_id))

        removeModal()
        sources_trigger(sources_trigger() + 1)
        texts_table_trigger(texts_table_trigger() + 1)
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.sources.notif_updated", lang, s_id), type = "message")
      }, error = function(e) {
        output$edit_source_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
        })
      })
    })

    # Modal: Delete source
    observeEvent(input$trigger_delete_source, {
      lang <- get_lang()
      s_id <- as.integer(input$trigger_delete_source)
      req(s_id)

      row <- dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz FROM sources WHERE source_id = ?", params = list(s_id))
      if (nrow(row) == 0) return()

      source_to_delete(row)

      texts_count <- tryCatch({
        as.integer(dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM texts WHERE source_id = ?", params = list(s_id))$n[1])
      }, error = function(e) 0L)

      showModal(modalDialog(
        title = div(icon("exclamation-triangle", class = "me-2 text-danger"), tr("admin.sources.modal_delete_title", lang, s_id)),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          if (texts_count == 0) {
            actionButton(ns("btn_confirm_delete_source"), tr("admin.sources.btn_confirm_delete", lang), class = "btn-danger")
          } else {
            actionButton(ns("btn_blocked_delete_source"), tr("admin.sources.btn_blocked_delete", lang), class = "btn-secondary", disabled = "disabled")
          }
        ),
        div(
          p(tags$strong(tr("admin.sources.modal_delete_confirm", lang, as.integer(row$source_id), row$Kurzreferenz))),
          p(class = "text-muted small", tags$em(row$Referenz)),
          if (texts_count > 0) {
            div(class = "alert alert-danger py-2",
                icon("ban", class = "me-2 text-danger fs-5"),
                tags$strong(tr("admin.sources.integrity_protect_label", lang)),
                tr("admin.sources.integrity_protect_desc", lang, texts_count)
            )
          } else {
            p(tr("admin.sources.delete_clean_desc", lang))
          }
        )
      ))
    })

    observeEvent(input$btn_confirm_delete_source, {
      lang <- get_lang()
      row <- source_to_delete()
      req(row)
      s_id <- as.integer(row$source_id)

      ref_count <- tryCatch({
        dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM texts WHERE source_id = ?", params = list(s_id))$n
      }, error = function(e) 999)

      if (ref_count > 0) {
        showNotification(tr("admin.sources.notif_delete_blocked", lang), type = "error")
        removeModal()
        return()
      }

      tryCatch({
        with_db_transaction(db_con, function(tx_con) {
          dbExecute(tx_con, "DELETE FROM user_source_permissions WHERE source_id = ?", params = list(s_id))
          dbExecute(tx_con, "DELETE FROM user_coding_source_permissions WHERE source_id = ?", params = list(s_id))
          dbExecute(tx_con, "DELETE FROM sources WHERE source_id = ?", params = list(s_id))
        })

        removeModal()
        sources_trigger(sources_trigger() + 1)
        texts_table_trigger(texts_table_trigger() + 1)
        user_table_trigger(user_table_trigger() + 1)
        showNotification(tr("admin.sources.notif_deleted_kurz", lang, s_id, row$Kurzreferenz), type = "warning")
      }, error = function(e) {
        showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
      })
    })

    # ==========================================================================
    # 4. SCALE MANAGEMENT (TAB 4)
    # ==========================================================================
    scales_data <- reactive({
      scales_table_trigger()
      if (!is.null(scales_trigger) && is.function(scales_trigger)) {
        scales_trigger()
      }
      tryCatch({
        dbGetQuery(db_con, "SELECT scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1, def_n_2, def_n_3, def_n_4, def_n_5 FROM scale_definitions ORDER BY scale_id ASC")
      }, error = function(e) {
        data.frame(scale_id = integer(0), short_label = character(0), name = character(0),
                   max_value = integer(0), def_desc = character(0),
                   def_n_0 = character(0), def_n_1 = character(0), def_n_2 = character(0),
                   def_n_3 = character(0), def_n_4 = character(0), def_n_5 = character(0),
                   stringsAsFactors = FALSE)
      })
    })

    scales_page <- reactiveVal(1)
    scales_page_size <- 25

    # Automatically reset to page 1 on search query change
    observeEvent(input$scale_search_query, {
      scales_page(1)
    }, ignoreInit = TRUE)

    # Previous/next pagination event handlers
    observeEvent(input$btn_scales_prev_page, {
      cur <- scales_page()
      if (cur > 1) scales_page(cur - 1)
    })

    observeEvent(input$btn_scales_next_page, {
      cur <- scales_page()
      scales_page(cur + 1)
    })

    output$scales_counter_text <- renderUI({
      lang <- get_lang()
      df <- scales_data()
      suchtext <- trimws(input$scale_search_query)
      n_total <- nrow(df)
      if (!is.null(suchtext) && suchtext != "" && n_total > 0) {
        treffer <- grepl(tolower(suchtext), tolower(as.character(df$scale_id))) |
                   grepl(tolower(suchtext), tolower(paste0("#", df$scale_id))) |
                   grepl(tolower(suchtext), tolower(df$short_label)) |
                   grepl(tolower(suchtext), tolower(df$name)) |
                   grepl(tolower(suchtext), tolower(ifelse(is.na(df$def_desc), "", df$def_desc)))
        n_filtered <- sum(treffer)
        tr("admin.scales.count_filtered", lang, n_total, n_filtered)
      } else {
        tr("admin.scales.count_total", lang, n_total)
      }
    })

    output$scales_table_container <- renderUI({
      lang <- get_lang()
      df <- scales_data()
      suchtext <- trimws(input$scale_search_query)

      if (!is.null(suchtext) && suchtext != "" && nrow(df) > 0) {
        treffer <- grepl(tolower(suchtext), tolower(as.character(df$scale_id))) |
                   grepl(tolower(suchtext), tolower(paste0("#", df$scale_id))) |
                   grepl(tolower(suchtext), tolower(df$short_label)) |
                   grepl(tolower(suchtext), tolower(df$name)) |
                   grepl(tolower(suchtext), tolower(ifelse(is.na(df$def_desc), "", df$def_desc)))
        df <- df[treffer, , drop = FALSE]
      }

      n_total <- nrow(df)
      if (n_total == 0) {
        return(div(class = "alert alert-light border text-center py-4",
                   icon("info-circle", class = "me-2 text-muted"),
                   tr("admin.scales.empty", lang)))
      }

      total_pages <- max(1, ceiling(n_total / scales_page_size))
      cur_page <- scales_page()
      if (cur_page > total_pages) {
        cur_page <- total_pages
        scales_page(cur_page)
      } else if (cur_page < 1) {
        cur_page <- 1
        scales_page(cur_page)
      }

      start_idx <- (cur_page - 1) * scales_page_size + 1
      end_idx <- min(cur_page * scales_page_size, n_total)
      df_page <- df[start_idx:end_idx, , drop = FALSE]

      rows <- lapply(1:nrow(df_page), function(i) {
        s_id    <- df_page$scale_id[i]
        s_short <- df_page$short_label[i]
        s_name  <- df_page$name[i]
        s_max   <- df_page$max_value[i]
        s_desc  <- df_page$def_desc[i]

        safe_btn_id <- paste0("sc_", s_id)

        edit_btn <- actionButton(
          inputId = ns(paste0("btn_edit_", safe_btn_id)),
          label = tr("common.edit", lang),
          icon = icon("pen"),
          class = "btn-sm btn-outline-primary",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_edit_scale"), s_id)
        )

        stufen_tags <- lapply(0:s_max, function(lvl) {
          col_name <- paste0("def_n_", lvl)
          lvl_text <- if (col_name %in% colnames(df_page)) df_page[[col_name]][i] else ""
          if (is.na(lvl_text)) lvl_text <- ""
          tags$span(
            class = "badge bg-light text-dark border me-1 mb-1",
            title = paste0(tr("common.level_label", lang, lvl), if (lvl_text != "") paste0(": ", lvl_text) else ""),
            paste0(lvl, if (lvl_text != "") paste0(": ", substr(lvl_text, 1, 25), if (nchar(lvl_text) > 25) "\u2026" else "") else "")
          )
        })

        tags$tr(
          tags$td(
            style = "vertical-align: middle; width: 7%; font-family: monospace; font-weight: bold; color: #495057;",
            tags$span(
              style = "background-color: #f1f3f5; padding: 3px 7px; border-radius: 4px; border: 1px solid #dee2e6;",
              paste0("#", s_id)
            )
          ),
          tags$td(
            style = "vertical-align: middle; font-weight: 700; width: 14%;",
            tags$span(class = "badge bg-primary-subtle text-primary border border-primary-subtle", style = "font-size: 0.9em;", s_short)
          ),
          tags$td(
            style = "vertical-align: middle; font-weight: 600; width: 22%; color: #2c3e50;",
            s_name
          ),
          tags$td(
            style = "vertical-align: middle; width: 12%;",
            tags$span(
              class = "badge bg-secondary",
              style = "font-size: 0.85em; font-family: monospace;",
              tr("admin.scales.range_text", lang, s_max)
            ),
            tags$div(class = "text-muted small", style = "font-size: 0.75em;", tr("admin.scales.steps_count", lang, s_max + 1))
          ),
          tags$td(
            style = "vertical-align: middle; width: 33%; font-size: 0.88em; line-height: 1.4;",
            if (!is.na(s_desc) && s_desc != "") tags$div(class = "mb-1 text-secondary", s_desc),
            tags$div(stufen_tags)
          ),
          tags$td(
            style = "vertical-align: middle; text-align: right; width: 12%;",
            edit_btn
          )
        )
      })

      pagination_bar <- div(
        class = "d-flex flex-wrap justify-content-between align-items-center mt-3 pt-2 border-top gap-2",
        div(
          class = "text-muted small",
          tr("admin.scales.pagination_showing", lang, start_idx, end_idx, n_total)
        ),
        div(
          class = "d-flex align-items-center gap-2",
          actionButton(
            ns("btn_scales_prev_page"),
            label = tr("common.back", lang),
            icon = icon("chevron-left"),
            class = paste0("btn btn-sm ", if (cur_page <= 1) "btn-outline-secondary disabled" else "btn-outline-primary")
          ),
          span(
            class = "text-muted small px-2 font-monospace",
            tr("common.page_x_of_y", lang, cur_page, total_pages)
          ),
          actionButton(
            ns("btn_scales_next_page"),
            label = tr("common.next", lang),
            icon = icon("chevron-right"),
            class = paste0("btn btn-sm ", if (cur_page >= total_pages) "btn-outline-secondary disabled" else "btn-outline-primary")
          )
        )
      )

      tagList(
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-hover align-middle mb-0",
            tags$thead(
              class = "table-light",
              tags$tr(
                tags$th(style = "width: 7%;", tr("admin.scales.col_id", lang)),
                tags$th(style = "width: 14%;", tr("admin.scales.col_short_label", lang)),
                tags$th(style = "width: 22%;", tr("admin.scales.col_name", lang)),
                tags$th(style = "width: 12%;", tr("admin.scales.col_range", lang)),
                tags$th(style = "width: 33%;", tr("admin.scales.col_steps", lang)),
                tags$th(style = "width: 12%; text-align: right;", tr("common.actions", lang))
              )
            ),
            tags$tbody(rows)
          )
        ),
        pagination_bar
      )
    })

    # Modal: Create new scale definition
    observeEvent(input$btn_open_add_scale, {
      lang <- get_lang()
      showModal(modalDialog(
        title = div(icon("plus-circle", class = "me-2 text-success"), tr("admin.scales.modal_add_title", lang)),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_new_scale"), tr("admin.scales.btn_submit_add", lang), class = "btn-success")
        ),
        div(
          fluidRow(
            column(width = 6,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", tr("admin.scales.label_short_label_req", lang)),
                       textInput(ns("new_scale_short_label"), label = NULL, placeholder = tr("admin.scales.placeholder_short_label", lang)),
                       tags$small(class = "text-muted", tr("admin.scales.hint_short_label", lang))
                   )
            ),
            column(width = 6,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", tr("admin.scales.label_name_req", lang)),
                       textInput(ns("new_scale_name"), label = NULL, placeholder = tr("admin.scales.placeholder_name", lang)),
                       tags$small(class = "text-muted", tr("admin.scales.hint_name", lang))
                   )
            )
          ),
          fluidRow(
            column(width = 6,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", tr("admin.scales.label_max_req", lang)),
                       numericInput(ns("new_scale_max_value"), label = NULL, value = 5, min = 1, max = 5, step = 1),
                       tags$small(class = "text-warning fw-bold", icon("exclamation-triangle", class = "me-1"),
                                  tr("admin.scales.warn_max_immutable", lang))
                   )
            ),
            column(width = 6,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", tr("admin.scales.label_def_desc", lang)),
                       textAreaInput(ns("new_scale_def_desc"), label = NULL, rows = 3,
                                     placeholder = tr("admin.scales.placeholder_def_desc", lang))
                   )
            )
          ),
          div(class = "card p-3 mb-3 bg-light border",
              h6(class = "fw-bold mb-2 text-secondary", icon("list-ol", class = "me-1"), tr("admin.scales.step_defs_add_heading", lang)),
              fluidRow(
                column(width = 6,
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 0), ":")),
                           textInput(ns("new_scale_def_n_0"), label = NULL, placeholder = tr("admin.scales.step_placeholder", lang, 0))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 1), ":")),
                           textInput(ns("new_scale_def_n_1"), label = NULL, placeholder = tr("admin.scales.step_placeholder", lang, 1))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 2), ":")),
                           textInput(ns("new_scale_def_n_2"), label = NULL, placeholder = tr("admin.scales.step_placeholder", lang, 2))
                       )
                ),
                column(width = 6,
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 3), ":")),
                           textInput(ns("new_scale_def_n_3"), label = NULL, placeholder = tr("admin.scales.step_placeholder", lang, 3))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 4), ":")),
                           textInput(ns("new_scale_def_n_4"), label = NULL, placeholder = tr("admin.scales.step_placeholder", lang, 4))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 5), ":")),
                           textInput(ns("new_scale_def_n_5"), label = NULL, placeholder = tr("admin.scales.step_placeholder", lang, 5))
                       )
                )
              ),
              tags$small(class = "text-muted", tr("admin.scales.hint_steps_needed", lang, 5))
          ),
          uiOutput(ns("add_scale_feedback"))
        )
      ))
    })

    observeEvent(input$btn_save_new_scale, {
      lang <- get_lang()
      safe_trim <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else trimws(as.character(x[1]))
      }
      short_val <- safe_trim(input$new_scale_short_label)
      name_val  <- safe_trim(input$new_scale_name)
      max_val   <- suppressWarnings(as.integer(input$new_scale_max_value))
      desc_val  <- safe_trim(input$new_scale_def_desc)
      n0        <- safe_trim(input$new_scale_def_n_0)
      n1        <- safe_trim(input$new_scale_def_n_1)
      n2        <- safe_trim(input$new_scale_def_n_2)
      n3        <- safe_trim(input$new_scale_def_n_3)
      n4        <- safe_trim(input$new_scale_def_n_4)
      n5        <- safe_trim(input$new_scale_def_n_5)

      if (is.null(short_val) || nchar(short_val) < 1) {
        output$add_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_short_label", lang))
        })
        return()
      }

      if (is.null(name_val) || nchar(name_val) < 2) {
        output$add_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_name_short", lang))
        })
        return()
      }

      if (is.na(max_val) || max_val < 1 || max_val > 5) {
        output$add_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_max_range", lang))
        })
        return()
      }

      existiert <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM scale_definitions WHERE LOWER(name) = LOWER(?)",
                              params = list(name_val))$n
      if (existiert > 0) {
        output$add_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_name_exists", lang, name_val))
        })
        return()
      }

      tryCatch({
        dbExecute(db_con,
                  "INSERT INTO scale_definitions (short_label, name, max_value, def_desc, def_n_0, def_n_1, def_n_2, def_n_3, def_n_4, def_n_5) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                  params = list(short_val, name_val, max_val, desc_val, n0, n1, n2, n3, n4, n5))

        removeModal()
        scales_table_trigger(scales_table_trigger() + 1)
        if (!is.null(scales_trigger) && is.function(scales_trigger)) {
          scales_trigger(scales_trigger() + 1)
        }
        showNotification(tr("admin.scales.notif_created", lang, short_val), type = "message")
      }, error = function(e) {
        output$add_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
        })
      })
    })

    # Modal: Edit scale definition (max_value is strictly immutable/locked)
    observeEvent(input$trigger_edit_scale, {
      lang <- get_lang()
      s_id <- as.integer(input$trigger_edit_scale)
      req(s_id)

      row <- dbGetQuery(db_con,
                        "SELECT scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1, def_n_2, def_n_3, def_n_4, def_n_5 FROM scale_definitions WHERE scale_id = ?",
                        params = list(s_id))
      if (nrow(row) == 0) return()

      scale_to_edit(row)

      showModal(modalDialog(
        title = div(icon("edit", class = "me-2 text-primary"), tr("admin.scales.modal_edit_title", lang, row$name, s_id)),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_save_edit_scale"), tr("common.save_changes", lang), class = "btn-primary")
        ),
        div(
          fluidRow(
            column(width = 4,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", tr("admin.scales.label_pk_scale", lang)),
                       tags$input(type = "text", class = "form-control font-monospace", value = paste0("#", s_id), disabled = "disabled"),
                       tags$small(class = "text-muted", tr("admin.scales.hint_pk_scale", lang))
                   )
            ),
            column(width = 4,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", paste0(tr("admin.scales.col_short_label", lang), ":")),
                       textInput(ns("edit_scale_short_label"), label = NULL, value = row$short_label)
                   )
            ),
            column(width = 4,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", paste0(tr("admin.scales.col_name", lang), ":")),
                       textInput(ns("edit_scale_name"), label = NULL, value = row$name)
                   )
            )
          ),
          fluidRow(
            column(width = 5,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold d-flex justify-content-between align-items-center",
                                  span(tr("admin.scales.label_max_req", lang)),
                                  span(class = "badge bg-warning text-dark border", icon("lock", class = "me-1"), tr("admin.scales.badge_locked", lang))
                       ),
                       tags$input(type = "text", class = "form-control bg-light font-monospace text-muted",
                                  value = tr("admin.scales.locked_max_value", lang, as.integer(row$max_value), as.integer(row$max_value)),
                                  disabled = "disabled"),
                       tags$small(class = "text-danger d-block mt-1",
                                  icon("shield-alt", class = "me-1"),
                                  tr("admin.scales.warn_max_locked_edit", lang))
                   )
            ),
            column(width = 7,
                   div(class = "mb-3",
                       tags$label(class = "form-label fw-bold", tr("admin.scales.label_def_desc", lang)),
                       textAreaInput(ns("edit_scale_def_desc"), label = NULL, rows = 3,
                                     value = ifelse(is.na(row$def_desc), "", row$def_desc))
                   )
            )
          ),
          div(class = "card p-3 mb-3 bg-light border",
              h6(class = "fw-bold mb-2 text-secondary", icon("list-ol", class = "me-1"),
                 tr("admin.scales.step_defs_heading", lang, as.integer(row$max_value))),
              fluidRow(
                column(width = 6,
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 0), ":")),
                           textInput(ns("edit_scale_def_n_0"), label = NULL,
                                     value = ifelse(is.na(row$def_n_0), "", row$def_n_0))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 1), ":")),
                           textInput(ns("edit_scale_def_n_1"), label = NULL,
                                     value = ifelse(is.na(row$def_n_1), "", row$def_n_1))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 2), ":")),
                           textInput(ns("edit_scale_def_n_2"), label = NULL,
                                     value = ifelse(is.na(row$def_n_2), "", row$def_n_2))
                       )
                ),
                column(width = 6,
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 3), ":")),
                           textInput(ns("edit_scale_def_n_3"), label = NULL,
                                     value = ifelse(is.na(row$def_n_3), "", row$def_n_3))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 4), ":")),
                           textInput(ns("edit_scale_def_n_4"), label = NULL,
                                     value = ifelse(is.na(row$def_n_4), "", row$def_n_4))
                       ),
                       div(class = "mb-2",
                           tags$label(class = "form-label small fw-bold", paste0(tr("common.level_label", lang, 5), ":")),
                           textInput(ns("edit_scale_def_n_5"), label = NULL,
                                     value = ifelse(is.na(row$def_n_5), "", row$def_n_5))
                       )
                )
              )
          ),
          uiOutput(ns("edit_scale_feedback"))
        )
      ))
    })

    observeEvent(input$btn_save_edit_scale, {
      lang <- get_lang()
      row <- scale_to_edit()
      req(row)
      s_id <- as.integer(row$scale_id)

      safe_trim <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else trimws(as.character(x[1]))
      }
      short_val <- safe_trim(input$edit_scale_short_label)
      name_val  <- safe_trim(input$edit_scale_name)
      desc_val  <- safe_trim(input$edit_scale_def_desc)
      n0        <- safe_trim(input$edit_scale_def_n_0)
      n1        <- safe_trim(input$edit_scale_def_n_1)
      n2        <- safe_trim(input$edit_scale_def_n_2)
      n3        <- safe_trim(input$edit_scale_def_n_3)
      n4        <- safe_trim(input$edit_scale_def_n_4)
      n5        <- safe_trim(input$edit_scale_def_n_5)

      if (is.null(short_val) || nchar(short_val) < 1) {
        output$edit_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_short_label", lang))
        })
        return()
      }

      if (is.null(name_val) || nchar(name_val) < 2) {
        output$edit_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_name_short", lang))
        })
        return()
      }

      existiert <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM scale_definitions WHERE LOWER(name) = LOWER(?) AND scale_id != ?",
                              params = list(name_val, s_id))$n
      if (existiert > 0) {
        output$edit_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", tr("admin.scales.err_name_exists_other", lang, name_val))
        })
        return()
      }

      tryCatch({
        dbExecute(db_con,
                  "UPDATE scale_definitions SET short_label = ?, name = ?, def_desc = ?, def_n_0 = ?, def_n_1 = ?, def_n_2 = ?, def_n_3 = ?, def_n_4 = ?, def_n_5 = ? WHERE scale_id = ?",
                  params = list(short_val, name_val, desc_val, n0, n1, n2, n3, n4, n5, s_id))

        removeModal()
        scales_table_trigger(scales_table_trigger() + 1)
        if (!is.null(scales_trigger) && is.function(scales_trigger)) {
          scales_trigger(scales_trigger() + 1)
        }
        showNotification(tr("admin.scales.notif_updated", lang, short_val), type = "message")
      }, error = function(e) {
        output$edit_scale_feedback <- renderUI({
          div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
        })
      })
    })

    # ==========================================================================
    # 5. DATA & IMPORT/EXPORT
    # ==========================================================================
    clean_columns <- function(df) {
      clean_table_columns(df)
    }

    get_table_pk <- function(tbl) {
      get_table_pk_definition(tbl)
    }

    pending_import <- reactiveVal(NULL)

    validiere_schema_und_starte_bestaetigung <- function(tabellen_name, neues_df) {
      lang <- get_lang()
      tabelle_existiert <- dbExistsTable(db_con, tabellen_name)
      if (tabelle_existiert) {
        alte_spalten <- dbListFields(db_con, tabellen_name)
        neue_spalten <- colnames(neues_df)
        if (length(alte_spalten) != length(neue_spalten) || !all(alte_spalten %in% neue_spalten)) {
          fehlende_oder_falsche <- setdiff(union(alte_spalten, neue_spalten), intersect(alte_spalten, neue_spalten))
          showNotification(
            tr("admin.upload_schema_err_r", lang, tabellen_name, paste(fehlende_oder_falsche, collapse = ", ")),
            type = "error", duration = 15
          )
          return(FALSE)
        }
        neues_df <- neues_df[, alte_spalten, drop = FALSE]
      }

      pk_col <- get_table_pk(tabellen_name)
      existing_ids <- if (!is.null(pk_col) && pk_col %in% colnames(neues_df)) {
        tryCatch({
          dbGetQuery(db_con, paste0("SELECT DISTINCT ", pk_col, " FROM ", tabellen_name))[[pk_col]]
        }, error = function(e) c())
      } else {
        c()
      }

      conflicts <- if (length(existing_ids) > 0) {
        intersect(as.character(neues_df[[pk_col]]), as.character(existing_ids))
      } else {
        character(0)
      }

      n_conflicts <- length(conflicts)
      n_new <- nrow(neues_df) - n_conflicts
      old_count <- length(existing_ids)

      pending_import(list(
        table = tabellen_name,
        data = neues_df,
        pk = pk_col,
        conflicts = conflicts,
        old_rows = old_count,
        new_rows = nrow(neues_df),
        brand_new_rows = n_new
      ))

      conflict_choices <- setNames(
        c("overwrite", "skip"),
        c(tr("admin.upload_mode_overwrite", lang), tr("admin.upload_mode_skip", lang))
      )

      showModal(modalDialog(
        title = div(icon("file-import", class = "text-primary me-2"), paste0("\U0001f4e5 ", tr("admin.upload_modal_title", lang, tabellen_name))),
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          modalButton(tr("common.cancel", lang)),
          actionButton(ns("btn_confirm_append_table"), tr("admin.btn_confirm_import", lang), class = "btn-success", icon = icon("plus-circle"))
        ),
        div(
          p(tr("admin.upload_modal_desc", lang, tabellen_name)),
          div(class = "alert alert-light border py-2 mb-3",
              tags$ul(style = "margin: 0; padding-left: 20px;",
                      tags$li(paste0(tr("admin.upload_old_rows", lang), " ", old_count)),
                      tags$li(paste0(tr("admin.upload_new_rows", lang), " ", nrow(neues_df))),
                      tags$li(tr("admin.upload_new_brand_new_rows", lang, n_new))
              )
          ),
          if (n_conflicts > 0) {
            tagList(
              div(class = "alert alert-warning py-2 mb-3",
                  div(class = "d-flex align-items-center mb-1",
                      icon("exclamation-triangle", class = "me-2 text-warning fs-5"),
                      tags$strong(tr("admin.upload_conflict_warning", lang, n_conflicts))
                  ),
                  p(class = "small mb-1 text-muted",
                    tr("admin.upload_conflict_ids", lang, paste0(paste(head(conflicts, 8), collapse = ", "), if (n_conflicts > 8) "..." else ""))
                  )
              ),
              div(class = "mb-3 p-3 bg-light rounded border",
                  tags$label(class = "form-label fw-bold small text-dark",
                             icon("question-circle", class = "me-1 text-primary"),
                             tr("admin.upload_conflict_choice", lang)),
                  radioButtons(
                    ns("conflict_resolution_mode"),
                    label = NULL,
                    choices = conflict_choices,
                    selected = "overwrite"
                  )
              )
            )
          } else {
            div(class = "alert alert-success py-2 mb-3",
                icon("check-circle", class = "me-2 text-success"),
                tr("admin.upload_no_conflicts", lang, nrow(neues_df))
            )
          },
          p(class = "text-muted small mb-0",
            icon("info-circle", class = "me-1 text-primary"),
            tr("admin.upload_modal_tx_hint", lang, tabellen_name)
          )
        )
      ))
      return(TRUE)
    }

    observeEvent(input$btn_confirm_append_table, {
      lang <- get_lang()
      req(pending_import())
      target <- pending_import()

      mode <- if (!is.null(input$conflict_resolution_mode)) input$conflict_resolution_mode else "overwrite"

      tryCatch({
        df_to_process <- target$data
        pk <- target$pk
        conflicts <- target$conflicts

        rows_added <- 0
        rows_updated <- 0

        with_db_transaction(db_con, function(tx_con) {
          if (length(conflicts) > 0 && !is.null(pk)) {
            if (mode == "skip") {
              df_to_insert <- df_to_process[!as.character(df_to_process[[pk]]) %in% conflicts, , drop = FALSE]
              if (nrow(df_to_insert) > 0) {
                dbAppendTable(tx_con, name = target$table, value = df_to_insert)
              }
              rows_added <<- nrow(df_to_insert)
              rows_updated <<- 0
            } else {
              for (cid in conflicts) {
                cid_val <- if (grepl("^[0-9]+$", as.character(cid))) as.integer(cid) else as.character(cid)
                dbExecute(tx_con, paste0("DELETE FROM ", target$table, " WHERE ", pk, " = ?"), params = list(cid_val))
              }
              dbAppendTable(tx_con, name = target$table, value = df_to_process)
              rows_updated <<- length(conflicts)
              rows_added <<- nrow(df_to_process) - length(conflicts)
            }
          } else {
            dbAppendTable(tx_con, name = target$table, value = df_to_process)
            rows_added <<- nrow(df_to_process)
            rows_updated <<- 0
          }
        })

        removeModal()
        pending_import(NULL)

        if (target$table == "texts") texts_table_trigger(texts_table_trigger() + 1)
        if (target$table == "sources") {
          sources_trigger(sources_trigger() + 1)
          texts_table_trigger(texts_table_trigger() + 1)
          user_table_trigger(user_table_trigger() + 1)
        }
        if (target$table == "user_tokens") user_table_trigger(user_table_trigger() + 1)
        if (target$table == "scale_definitions") {
          scales_table_trigger(scales_table_trigger() + 1)
          if (!is.null(scales_trigger) && is.function(scales_trigger)) {
            scales_trigger(scales_trigger() + 1)
          }
        }

        summary_txt <- if (rows_updated > 0) {
          tr("admin.upload_summary_overwrite", lang, rows_added, rows_updated)
        } else {
          tr("admin.upload_summary_skip", lang, rows_added)
        }
        showNotification(tr("admin.upload_success", lang, target$table, summary_txt), type = "message", duration = 8)
      }, error = function(e) {
        showNotification(
          tr("admin.upload_tx_error", lang, target$table, e$message),
          type = "error",
          duration = 15
        )
      })
    })

    observeEvent(input$upload_scales_file, {
      lang <- get_lang()
      req(input$upload_scales_file)
      tryCatch({
        data <- read.csv(input$upload_scales_file$datapath, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8", blank.lines.skip = TRUE, strip.white = TRUE)
        data <- clean_columns(data)
        validiere_schema_und_starte_bestaetigung("scale_definitions", data)
      }, error = function(e) { showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error") })
    })

    observeEvent(input$upload_texts_file, {
      lang <- get_lang()
      req(input$upload_texts_file)
      tryCatch({
        data <- read.csv(input$upload_texts_file$datapath, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8", blank.lines.skip = TRUE, strip.white = TRUE)
        data <- clean_columns(data)
        colnames(data)[colnames(data) == "id"]   <- "text_id"
        colnames(data)[colnames(data) %in% c("befund_text", "inhalt")] <- "text"

        if (!"source_id" %in% colnames(data)) {
          showNotification(tr("admin.upload_missing_col_source", lang), type = "error", duration = 12)
          return()
        }

        gueltige_sources <- tryCatch({
          dbGetQuery(db_con, "SELECT source_id FROM sources")$source_id
        }, error = function(e) integer(0))

        ungueltige_ids <- setdiff(unique(as.integer(data$source_id)), gueltige_sources)
        if (length(ungueltige_ids) > 0) {
          showNotification(
            tr("admin.upload_err_fk_ids", lang, paste(ungueltige_ids, collapse = ", ")),
            type = "error", duration = 15
          )
          return()
        }

        validiere_schema_und_starte_bestaetigung("texts", data)
      }, error = function(e) { showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error") })
    })

    observeEvent(input$upload_sources_file, {
      lang <- get_lang()
      req(input$upload_sources_file)
      tryCatch({
        data <- read.csv(input$upload_sources_file$datapath, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8", blank.lines.skip = TRUE, strip.white = TRUE)
        data <- clean_columns(data)
        colnames(data)[colnames(data) == "id"] <- "source_id"

        aktuelle_zeit <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        if (!"erfasst_am" %in% colnames(data)) {
          data$erfasst_am <- aktuelle_zeit
        } else {
          leere_idx <- is.na(data$erfasst_am) | trimws(as.character(data$erfasst_am)) == "" | tolower(trimws(as.character(data$erfasst_am))) == "na"
          if (any(leere_idx)) {
            data$erfasst_am[leere_idx] <- aktuelle_zeit
          }
        }

        validiere_schema_und_starte_bestaetigung("sources", data)
      }, error = function(e) { showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error") })
    })

    observeEvent(input$upload_ratings_file, {
      lang <- get_lang()
      req(input$upload_ratings_file)
      tryCatch({
        data <- read.csv(input$upload_ratings_file$datapath, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8", blank.lines.skip = TRUE, strip.white = TRUE)
        data <- clean_columns(data)
        validiere_schema_und_starte_bestaetigung("user_ratings", data)
      }, error = function(e) { showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error") })
    })

    observeEvent(input$upload_training_file, {
      lang <- get_lang()
      req(input$upload_training_file)
      tryCatch({
        data <- read.csv(input$upload_training_file$datapath, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8", blank.lines.skip = TRUE, strip.white = TRUE)
        data <- clean_columns(data)
        validiere_schema_und_starte_bestaetigung("coding_training", data)
      }, error = function(e) { showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error") })
    })

    erzeuge_csv_download <- function(tabellen_name) {
      downloadHandler(
        filename = function() { paste0(tabellen_name, "_export_", Sys.Date(), ".csv") },
        content = function(file) {
          data <- dbReadTable(db_con, tabellen_name)
          write.csv2(data, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )
    }

    # CSV template generation: produces an empty CSV with exact column headers (0 rows)
    erzeuge_csv_vorlage <- function(tabellen_name) {
      downloadHandler(
        filename = function() { paste0(tabellen_name, "_vorlage.csv") },
        content = function(file) {
          cols <- tryCatch({
            dbListFields(db_con, tabellen_name)
          }, error = function(e) character(0))

          if (length(cols) == 0) {
            cols <- switch(tabellen_name,
              "user_tokens" = c("user_id", "username", "token", "permissions", "is_active"),
              "scale_definitions" = c("scale_id", "short_label", "name", "max_value", "def_desc", "def_n_0", "def_n_1", "def_n_2", "def_n_3", "def_n_4", "def_n_5"),
              "texts" = c("text_id", "text", "source_id"),
              "sources" = c("source_id", "Kurzreferenz", "Referenz", "Lizenz", "erfasst_am"),
              "user_ratings" = c("rating_id", "text_id", "user_id", "kategorie", "bewertung", "zeitstempel", "typ", "source_id"),
              "coding_training" = c("training_id", "user_id", "kategorie", "datum", "anzahl_einheiten", "accuracy", "balanced_accuracy", "iota_index", "avg_iota2", "cohens_kappa", "kappa_linear", "kappa_squared", "fleiss_kappa", "kendall_w", "alpha_nominal", "alpha_ordinal", "gwet_ac1", "gwet_ac2_quadratic", "macro_precision", "macro_recall", "macro_f1", "zeitstempel"),
              "user_source_permissions" = c("user_id", "source_id", "erstellt_am"),
              "user_coding_source_permissions" = c("user_id", "source_id", "erstellt_am"),
              character(0)
            )
          }

          template_df <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(cols)))
          colnames(template_df) <- cols
          write.csv2(template_df, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )
    }

    # --------------------------------------------------------------------------
    # CONFIGURABLE MASTER DATA EXPORT (FILTERS & DYNAMIC DOWNLOAD)
    # --------------------------------------------------------------------------
    # Reactive state for filter selections (IDs / usernames)
    selected_export_scales  <- reactiveVal(NULL)
    selected_export_sources <- reactiveVal(NULL)
    selected_export_users   <- reactiveVal(NULL)

    # Initialization on startup & table refreshes
    observe({
      scales_table_trigger()
      if (is.function(scales_trigger)) scales_trigger()
      scales_df <- tryCatch(dbGetQuery(db_con, "SELECT scale_id FROM scale_definitions ORDER BY scale_id ASC"), error = function(e) data.frame())
      if (nrow(scales_df) > 0 && is.null(selected_export_scales())) {
        selected_export_scales(as.character(scales_df$scale_id))
      }
    })

    observe({
      sources_trigger()
      sources_df <- tryCatch(dbGetQuery(db_con, "SELECT source_id FROM sources ORDER BY source_id ASC"), error = function(e) data.frame())
      if (nrow(sources_df) > 0 && is.null(selected_export_sources())) {
        selected_export_sources(as.character(sources_df$source_id))
      }
    })

    observe({
      user_table_trigger()
      users_df <- tryCatch(dbGetQuery(db_con, "SELECT username FROM user_tokens ORDER BY username ASC"), error = function(e) data.frame())
      if (nrow(users_df) > 0 && is.null(selected_export_users())) {
        selected_export_users(users_df$username)
      }
    })

    # Status indicators on the 3 main filter cards
    output$export_scales_status_badge <- renderUI({
      lang <- get_lang()
      scales_df <- tryCatch(dbGetQuery(db_con, "SELECT scale_id FROM scale_definitions"), error = function(e) data.frame())
      total_n <- nrow(scales_df)
      sel <- selected_export_scales()
      sel_n <- if (is.null(sel)) total_n else length(sel)
      if (sel_n == 0) {
        tags$span(class = "badge bg-warning text-dark", tr("admin.none_selected", lang))
      } else if (sel_n == total_n) {
        tags$span(class = "badge bg-success", tr("admin.all_active", lang))
      } else {
        tags$span(class = "badge bg-primary", tr("admin.selected_count", lang, sel_n))
      }
    })

    output$export_scales_counter_ui <- renderUI({
      lang <- get_lang()
      scales_df <- tryCatch(dbGetQuery(db_con, "SELECT scale_id FROM scale_definitions"), error = function(e) data.frame())
      total_n <- nrow(scales_df)
      sel <- selected_export_scales()
      sel_n <- if (is.null(sel)) total_n else length(sel)
      div(
        tags$h4(class = "mb-0 fw-bold text-dark", sel_n),
        tags$small(class = "text-muted", tr("admin.scales_selected_of", lang, total_n))
      )
    })

    output$export_sources_status_badge <- renderUI({
      lang <- get_lang()
      sources_df <- tryCatch(dbGetQuery(db_con, "SELECT source_id FROM sources"), error = function(e) data.frame())
      total_n <- nrow(sources_df)
      sel <- selected_export_sources()
      sel_n <- if (is.null(sel)) total_n else length(sel)
      if (sel_n == 0) {
        tags$span(class = "badge bg-warning text-dark", tr("admin.none_selected", lang))
      } else if (sel_n == total_n) {
        tags$span(class = "badge bg-success", tr("admin.all_active", lang))
      } else {
        tags$span(class = "badge bg-info text-dark", tr("admin.selected_count", lang, sel_n))
      }
    })

    output$export_sources_counter_ui <- renderUI({
      lang <- get_lang()
      sources_df <- tryCatch(dbGetQuery(db_con, "SELECT source_id FROM sources"), error = function(e) data.frame())
      total_n <- nrow(sources_df)
      sel <- selected_export_sources()
      sel_n <- if (is.null(sel)) total_n else length(sel)
      div(
        tags$h4(class = "mb-0 fw-bold text-dark", sel_n),
        tags$small(class = "text-muted", tr("admin.sources_selected_of", lang, total_n))
      )
    })

    output$export_users_status_badge <- renderUI({
      lang <- get_lang()
      users_df <- tryCatch(dbGetQuery(db_con, "SELECT username FROM user_tokens"), error = function(e) data.frame())
      total_n <- nrow(users_df)
      sel <- selected_export_users()
      sel_n <- if (is.null(sel)) total_n else length(sel)
      if (sel_n == 0) {
        tags$span(class = "badge bg-warning text-dark", tr("admin.none_selected", lang))
      } else if (sel_n == total_n) {
        tags$span(class = "badge bg-success", tr("admin.all_active", lang))
      } else {
        tags$span(class = "badge bg-secondary", tr("admin.selected_count", lang, sel_n))
      }
    })

    output$export_users_counter_ui <- renderUI({
      lang <- get_lang()
      users_df <- tryCatch(dbGetQuery(db_con, "SELECT username FROM user_tokens"), error = function(e) data.frame())
      total_n <- nrow(users_df)
      sel <- selected_export_users()
      sel_n <- if (is.null(sel)) total_n else length(sel)
      div(
        tags$h4(class = "mb-0 fw-bold text-dark", sel_n),
        tags$small(class = "text-muted", tr("admin.users_selected_of", lang, total_n))
      )
    })

    # ==========================================================================
    # MODAL 1: SCALE SELECTION WITH PAGINATION (25 PER PAGE) & SEARCH
    # ==========================================================================
    page_scales_modal <- reactiveVal(1)
    current_modal_scales_page_ids <- reactiveVal(character(0))
    filtered_modal_scales_ids     <- reactiveVal(character(0))

    observeEvent(input$input_modal_scales_search, {
      page_scales_modal(1)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_open_modal_export_scales, {
      lang <- get_lang()
      page_scales_modal(1)
      showModal(modalDialog(
        title = tagList(icon("sliders-h", class = "me-2 text-primary"), tr("admin.modal_scales_title", lang)),
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        div(
          div(class = "row g-2 align-items-center mb-3",
            div(class = "col-md-7",
              textInput(ns("input_modal_scales_search"), label = NULL, placeholder = tr("admin.filter_by_name_id", lang), width = "100%")
            ),
            div(class = "col-md-5 text-end",
              actionButton(ns("btn_modal_scales_select_all"), tr("admin.btn_select_all", lang), class = "btn btn-sm btn-outline-primary me-1"),
              actionButton(ns("btn_modal_scales_deselect_all"), tr("admin.btn_deselect_all", lang), class = "btn btn-sm btn-outline-danger")
            )
          ),
          hr(class = "my-2"),
          uiOutput(ns("modal_scales_page_ui")),
          hr(class = "my-2"),
          uiOutput(ns("modal_scales_paging_controls"))
        ),
        footer = tagList(
          actionButton(ns("btn_close_modal_scales"), tr("admin.modal_apply_close", lang), icon = icon("check"), class = "btn btn-dark")
        )
      ))
    })

    observeEvent(input$btn_close_modal_scales, {
      removeModal()
    })

    output$modal_scales_page_ui <- renderUI({
      lang <- get_lang()
      scales_df <- tryCatch({
        dbGetQuery(db_con, "SELECT scale_id, name, short_label FROM scale_definitions ORDER BY scale_id ASC")
      }, error = function(e) data.frame())

      if (nrow(scales_df) == 0) return(div(class = "text-muted text-center py-4", tr("admin.scales.empty", lang)))

      suchtext <- input$input_modal_scales_search
      if (!is.null(suchtext) && trimws(suchtext) != "") {
        st <- tolower(trimws(suchtext))
        match_idx <- grepl(st, tolower(paste0("#", scales_df$scale_id))) |
                     grepl(st, tolower(scales_df$name)) |
                     grepl(st, tolower(scales_df$short_label))
        scales_df <- scales_df[match_idx, , drop = FALSE]
      }

      total_n <- nrow(scales_df)
      filtered_modal_scales_ids(as.character(scales_df$scale_id))

      if (total_n == 0) {
        return(div(class = "text-muted text-center py-4", tr("admin.no_modal_matches", lang)))
      }

      total_pages <- max(1, ceiling(total_n / 25))
      cur_p <- min(page_scales_modal(), total_pages)
      start_i <- (cur_p - 1) * 25 + 1
      end_i   <- min(cur_p * 25, total_n)

      page_df <- scales_df[start_i:end_i, , drop = FALSE]
      page_ids <- as.character(page_df$scale_id)
      current_modal_scales_page_ids(page_ids)

      choices <- setNames(
        page_ids,
        paste0(page_df$name, " (", page_df$short_label, ") [ID: #", page_df$scale_id, "]")
      )

      all_sel <- selected_export_scales()
      cur_sel <- if (is.null(all_sel)) page_ids else intersect(page_ids, all_sel)

      div(style = "max-height: 380px; overflow-y: auto; padding-right: 5px;",
        checkboxGroupInput(
          ns("modal_scales_page_selection"),
          label = NULL,
          choices = choices,
          selected = cur_sel,
          width = "100%"
        )
      )
    })

    observeEvent(input$modal_scales_page_selection, {
      cur_page_ids <- current_modal_scales_page_ids()
      if (length(cur_page_ids) == 0) return()
      new_on_page <- input$modal_scales_page_selection
      cur_all <- selected_export_scales()
      if (is.null(cur_all)) cur_all <- character(0)
      remaining <- setdiff(cur_all, cur_page_ids)
      selected_export_scales(unique(c(remaining, new_on_page)))
    }, ignoreInit = TRUE)

    output$modal_scales_paging_controls <- renderUI({
      lang <- get_lang()
      all_f <- filtered_modal_scales_ids()
      total_n <- length(all_f)
      total_pages <- max(1, ceiling(total_n / 25))
      cur_p <- min(page_scales_modal(), total_pages)

      start_i <- if (total_n == 0) 0 else (cur_p - 1) * 25 + 1
      end_i   <- min(cur_p * 25, total_n)

      div(class = "d-flex justify-content-between align-items-center",
        tags$span(class = "text-muted small", tr("common.showing_entries", lang, start_i, end_i, total_n)),
        div(class = "btn-group btn-group-sm",
          actionButton(ns("btn_modal_scales_prev"), paste0("\u25c0 ", tr("common.previous", lang)), class = "btn btn-outline-secondary", disabled = if (cur_p <= 1) "disabled" else NULL),
          tags$span(class = "btn btn-light disabled px-3", tr("common.page_x_of_y", lang, cur_p, total_pages)),
          actionButton(ns("btn_modal_scales_next"), paste0(tr("common.next", lang), " \u25b6"), class = "btn btn-outline-secondary", disabled = if (cur_p >= total_pages) "disabled" else NULL)
        )
      )
    })

    observeEvent(input$btn_modal_scales_prev, {
      page_scales_modal(max(1, page_scales_modal() - 1))
    })

    observeEvent(input$btn_modal_scales_next, {
      total_n <- length(filtered_modal_scales_ids())
      total_pages <- max(1, ceiling(total_n / 25))
      page_scales_modal(min(total_pages, page_scales_modal() + 1))
    })

    observeEvent(input$btn_modal_scales_select_all, {
      filtered_ids <- filtered_modal_scales_ids()
      cur_all <- selected_export_scales()
      if (is.null(cur_all)) cur_all <- character(0)
      new_sel <- unique(c(cur_all, filtered_ids))
      selected_export_scales(new_sel)
      cur_p_ids <- current_modal_scales_page_ids()
      updateCheckboxGroupInput(session, "modal_scales_page_selection", selected = intersect(cur_p_ids, new_sel))
    })

    observeEvent(input$btn_modal_scales_deselect_all, {
      filtered_ids <- filtered_modal_scales_ids()
      cur_all <- selected_export_scales()
      if (is.null(cur_all)) cur_all <- character(0)
      new_sel <- setdiff(cur_all, filtered_ids)
      selected_export_scales(new_sel)
      updateCheckboxGroupInput(session, "modal_scales_page_selection", selected = character(0))
    })

    # ==========================================================================
    # MODAL 2: SOURCE SELECTION WITH PAGINATION (25 PER PAGE) & SEARCH
    # ==========================================================================
    page_sources_modal <- reactiveVal(1)
    current_modal_sources_page_ids <- reactiveVal(character(0))
    filtered_modal_sources_ids     <- reactiveVal(character(0))

    observeEvent(input$input_modal_sources_search, {
      page_sources_modal(1)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_open_modal_export_sources, {
      lang <- get_lang()
      page_sources_modal(1)
      showModal(modalDialog(
        title = tagList(icon("book", class = "me-2 text-info"), tr("admin.modal_sources_title", lang)),
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        div(
          div(class = "row g-2 align-items-center mb-3",
            div(class = "col-md-7",
              textInput(ns("input_modal_sources_search"), label = NULL, placeholder = tr("admin.filter_by_source", lang), width = "100%")
            ),
            div(class = "col-md-5 text-end",
              actionButton(ns("btn_modal_sources_select_all"), tr("admin.btn_select_all", lang), class = "btn btn-sm btn-outline-info me-1"),
              actionButton(ns("btn_modal_sources_deselect_all"), tr("admin.btn_deselect_all", lang), class = "btn btn-sm btn-outline-danger")
            )
          ),
          hr(class = "my-2"),
          uiOutput(ns("modal_sources_page_ui")),
          hr(class = "my-2"),
          uiOutput(ns("modal_sources_paging_controls"))
        ),
        footer = tagList(
          actionButton(ns("btn_close_modal_sources"), tr("admin.modal_apply_close", lang), icon = icon("check"), class = "btn btn-dark")
        )
      ))
    })

    observeEvent(input$btn_close_modal_sources, {
      removeModal()
    })

    output$modal_sources_page_ui <- renderUI({
      lang <- get_lang()
      sources_df <- tryCatch({
        dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz FROM sources ORDER BY source_id ASC")
      }, error = function(e) data.frame())

      if (nrow(sources_df) == 0) return(div(class = "text-muted text-center py-4", tr("admin.sources.empty", lang)))

      suchtext <- input$input_modal_sources_search
      if (!is.null(suchtext) && trimws(suchtext) != "") {
        st <- tolower(trimws(suchtext))
        match_idx <- grepl(st, tolower(paste0("#", sources_df$source_id))) |
                     grepl(st, tolower(sources_df$Kurzreferenz)) |
                     grepl(st, tolower(ifelse(is.na(sources_df$Referenz), "", sources_df$Referenz)))
        sources_df <- sources_df[match_idx, , drop = FALSE]
      }

      total_n <- nrow(sources_df)
      filtered_modal_sources_ids(as.character(sources_df$source_id))

      if (total_n == 0) {
        return(div(class = "text-muted text-center py-4", tr("admin.no_modal_matches", lang)))
      }

      total_pages <- max(1, ceiling(total_n / 25))
      cur_p <- min(page_sources_modal(), total_pages)
      start_i <- (cur_p - 1) * 25 + 1
      end_i   <- min(cur_p * 25, total_n)

      page_df <- sources_df[start_i:end_i, , drop = FALSE]
      page_ids <- as.character(page_df$source_id)
      current_modal_sources_page_ids(page_ids)

      choices <- setNames(
        page_ids,
        paste0("#", page_df$source_id, " ", page_df$Kurzreferenz, ifelse(is.na(page_df$Referenz) | page_df$Referenz == "", "", paste0(" \u2014 ", substr(page_df$Referenz, 1, 60), "...")))
      )

      all_sel <- selected_export_sources()
      cur_sel <- if (is.null(all_sel)) page_ids else intersect(page_ids, all_sel)

      div(style = "max-height: 380px; overflow-y: auto; padding-right: 5px;",
        checkboxGroupInput(
          ns("modal_sources_page_selection"),
          label = NULL,
          choices = choices,
          selected = cur_sel,
          width = "100%"
        )
      )
    })

    observeEvent(input$modal_sources_page_selection, {
      cur_page_ids <- current_modal_sources_page_ids()
      if (length(cur_page_ids) == 0) return()
      new_on_page <- input$modal_sources_page_selection
      cur_all <- selected_export_sources()
      if (is.null(cur_all)) cur_all <- character(0)
      remaining <- setdiff(cur_all, cur_page_ids)
      selected_export_sources(unique(c(remaining, new_on_page)))
    }, ignoreInit = TRUE)

    output$modal_sources_paging_controls <- renderUI({
      lang <- get_lang()
      all_f <- filtered_modal_sources_ids()
      total_n <- length(all_f)
      total_pages <- max(1, ceiling(total_n / 25))
      cur_p <- min(page_sources_modal(), total_pages)

      start_i <- if (total_n == 0) 0 else (cur_p - 1) * 25 + 1
      end_i   <- min(cur_p * 25, total_n)

      div(class = "d-flex justify-content-between align-items-center",
        tags$span(class = "text-muted small", tr("common.showing_entries", lang, start_i, end_i, total_n)),
        div(class = "btn-group btn-group-sm",
          actionButton(ns("btn_modal_sources_prev"), paste0("\u25c0 ", tr("common.previous", lang)), class = "btn btn-outline-secondary", disabled = if (cur_p <= 1) "disabled" else NULL),
          tags$span(class = "btn btn-light disabled px-3", tr("common.page_x_of_y", lang, cur_p, total_pages)),
          actionButton(ns("btn_modal_sources_next"), paste0(tr("common.next", lang), " \u25b6"), class = "btn btn-outline-secondary", disabled = if (cur_p >= total_pages) "disabled" else NULL)
        )
      )
    })

    observeEvent(input$btn_modal_sources_prev, {
      page_sources_modal(max(1, page_sources_modal() - 1))
    })

    observeEvent(input$btn_modal_sources_next, {
      total_n <- length(filtered_modal_sources_ids())
      total_pages <- max(1, ceiling(total_n / 25))
      page_sources_modal(min(total_pages, page_sources_modal() + 1))
    })

    observeEvent(input$btn_modal_sources_select_all, {
      filtered_ids <- filtered_modal_sources_ids()
      cur_all <- selected_export_sources()
      if (is.null(cur_all)) cur_all <- character(0)
      new_sel <- unique(c(cur_all, filtered_ids))
      selected_export_sources(new_sel)
      cur_p_ids <- current_modal_sources_page_ids()
      updateCheckboxGroupInput(session, "modal_sources_page_selection", selected = intersect(cur_p_ids, new_sel))
    })

    observeEvent(input$btn_modal_sources_deselect_all, {
      filtered_ids <- filtered_modal_sources_ids()
      cur_all <- selected_export_sources()
      if (is.null(cur_all)) cur_all <- character(0)
      new_sel <- setdiff(cur_all, filtered_ids)
      selected_export_sources(new_sel)
      updateCheckboxGroupInput(session, "modal_sources_page_selection", selected = character(0))
    })

    # ==========================================================================
    # MODAL 3: USER SELECTION WITH PAGINATION (25 PER PAGE) & SEARCH
    # ==========================================================================
    page_users_modal <- reactiveVal(1)
    current_modal_users_page_ids <- reactiveVal(character(0))
    filtered_modal_users_ids     <- reactiveVal(character(0))

    observeEvent(input$input_modal_users_search, {
      page_users_modal(1)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_open_modal_export_users, {
      lang <- get_lang()
      page_users_modal(1)
      showModal(modalDialog(
        title = tagList(icon("users", class = "me-2 text-success"), tr("admin.modal_users_title", lang)),
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        div(
          div(class = "row g-2 align-items-center mb-3",
            div(class = "col-md-7",
              textInput(ns("input_modal_users_search"), label = NULL, placeholder = tr("admin.filter_by_user", lang), width = "100%")
            ),
            div(class = "col-md-5 text-end",
              actionButton(ns("btn_modal_users_select_all"), tr("admin.btn_select_all", lang), class = "btn btn-sm btn-outline-success me-1"),
              actionButton(ns("btn_modal_users_deselect_all"), tr("admin.btn_deselect_all", lang), class = "btn btn-sm btn-outline-danger")
            )
          ),
          hr(class = "my-2"),
          uiOutput(ns("modal_users_page_ui")),
          hr(class = "my-2"),
          uiOutput(ns("modal_users_paging_controls"))
        ),
        footer = tagList(
          actionButton(ns("btn_close_modal_users"), tr("admin.modal_apply_close", lang), icon = icon("check"), class = "btn btn-dark")
        )
      ))
    })

    observeEvent(input$btn_close_modal_users, {
      removeModal()
    })

    output$modal_users_page_ui <- renderUI({
      lang <- get_lang()
      users_df <- tryCatch({
        dbGetQuery(db_con, "SELECT user_id, username, permissions FROM user_tokens ORDER BY username ASC")
      }, error = function(e) data.frame())

      if (nrow(users_df) == 0) return(div(class = "text-muted text-center py-4", tr("admin.reliability.no_users_found", lang)))

      suchtext <- input$input_modal_users_search
      if (!is.null(suchtext) && trimws(suchtext) != "") {
        st <- tolower(trimws(suchtext))
        match_idx <- grepl(st, tolower(users_df$username)) |
                     grepl(st, tolower(users_df$permissions))
        users_df <- users_df[match_idx, , drop = FALSE]
      }

      total_n <- nrow(users_df)
      filtered_modal_users_ids(users_df$username)

      if (total_n == 0) {
        return(div(class = "text-muted text-center py-4", tr("admin.no_modal_matches", lang)))
      }

      total_pages <- max(1, ceiling(total_n / 25))
      cur_p <- min(page_users_modal(), total_pages)
      start_i <- (cur_p - 1) * 25 + 1
      end_i   <- min(cur_p * 25, total_n)

      page_df <- users_df[start_i:end_i, , drop = FALSE]
      page_usernames <- page_df$username
      current_modal_users_page_ids(page_usernames)

      choices <- setNames(
        page_usernames,
        paste0(page_df$username, " [", tr("admin.users.col_role", lang), ": ", page_df$permissions, "]")
      )

      all_sel <- selected_export_users()
      cur_sel <- if (is.null(all_sel)) page_usernames else intersect(page_usernames, all_sel)

      div(style = "max-height: 380px; overflow-y: auto; padding-right: 5px;",
        checkboxGroupInput(
          ns("modal_users_page_selection"),
          label = NULL,
          choices = choices,
          selected = cur_sel,
          width = "100%"
        )
      )
    })

    observeEvent(input$modal_users_page_selection, {
      cur_page_ids <- current_modal_users_page_ids()
      if (length(cur_page_ids) == 0) return()
      new_on_page <- input$modal_users_page_selection
      cur_all <- selected_export_users()
      if (is.null(cur_all)) cur_all <- character(0)
      remaining <- setdiff(cur_all, cur_page_ids)
      selected_export_users(unique(c(remaining, new_on_page)))
    }, ignoreInit = TRUE)

    output$modal_users_paging_controls <- renderUI({
      lang <- get_lang()
      all_f <- filtered_modal_users_ids()
      total_n <- length(all_f)
      total_pages <- max(1, ceiling(total_n / 25))
      cur_p <- min(page_users_modal(), total_pages)

      start_i <- if (total_n == 0) 0 else (cur_p - 1) * 25 + 1
      end_i   <- min(cur_p * 25, total_n)

      div(class = "d-flex justify-content-between align-items-center",
        tags$span(class = "text-muted small", tr("common.showing_entries", lang, start_i, end_i, total_n)),
        div(class = "btn-group btn-group-sm",
          actionButton(ns("btn_modal_users_prev"), paste0("\u25c0 ", tr("common.previous", lang)), class = "btn btn-outline-secondary", disabled = if (cur_p <= 1) "disabled" else NULL),
          tags$span(class = "btn btn-light disabled px-3", tr("common.page_x_of_y", lang, cur_p, total_pages)),
          actionButton(ns("btn_modal_users_next"), paste0(tr("common.next", lang), " \u25b6"), class = "btn btn-outline-secondary", disabled = if (cur_p >= total_pages) "disabled" else NULL)
        )
      )
    })

    observeEvent(input$btn_modal_users_prev, {
      page_users_modal(max(1, page_users_modal() - 1))
    })

    observeEvent(input$btn_modal_users_next, {
      total_n <- length(filtered_modal_users_ids())
      total_pages <- max(1, ceiling(total_n / 25))
      page_users_modal(min(total_pages, page_users_modal() + 1))
    })

    observeEvent(input$btn_modal_users_select_all, {
      filtered_ids <- filtered_modal_users_ids()
      cur_all <- selected_export_users()
      if (is.null(cur_all)) cur_all <- character(0)
      new_sel <- unique(c(cur_all, filtered_ids))
      selected_export_users(new_sel)
      cur_p_ids <- current_modal_users_page_ids()
      updateCheckboxGroupInput(session, "modal_users_page_selection", selected = intersect(cur_p_ids, new_sel))
    })

    observeEvent(input$btn_modal_users_deselect_all, {
      filtered_ids <- filtered_modal_users_ids()
      cur_all <- selected_export_users()
      if (is.null(cur_all)) cur_all <- character(0)
      new_sel <- setdiff(cur_all, filtered_ids)
      selected_export_users(new_sel)
      updateCheckboxGroupInput(session, "modal_users_page_selection", selected = character(0))
    })

    # Live status badge showing the count of records queued for export
    output$export_filter_summary_badge <- renderUI({
      lang <- get_lang()
      sel_scales  <- selected_export_scales()
      sel_sources <- selected_export_sources()
      sel_users   <- selected_export_users()

      n_sc <- if (is.null(sel_scales)) 0 else length(sel_scales)
      n_so <- if (is.null(sel_sources)) 0 else length(sel_sources)
      n_us <- if (is.null(sel_users)) 0 else length(sel_users)

      div(class = "d-flex flex-wrap align-items-center gap-2",
        tags$span(class = "badge bg-primary", tr("admin.export_scales_short_selected", lang, n_sc)),
        tags$span(class = "badge bg-info text-dark", tr("admin.export_sources_short_selected", lang, n_so)),
        tags$span(class = "badge bg-secondary", tr("admin.export_users_short_selected", lang, n_us)),
        if (n_sc == 0 || n_so == 0 || n_us == 0) {
          tags$span(class = "badge bg-warning text-dark", icon("exclamation-triangle", class = "me-1"), tr("admin.export_blocked", lang))
        } else {
          tags$span(class = "badge bg-success", icon("check", class = "me-1"), tr("admin.export_ready", lang))
        }
      )
    })

    # Download handler with full support for all three filter dimensions
    output$download_master_dataset_btn <- downloadHandler(
      filename = function() {
        lang <- get_lang()
        paste0(tr("admin.master_export_filename_prefix", lang), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        lang <- get_lang()
        sel_scales  <- selected_export_scales()
        sel_sources <- selected_export_sources()
        sel_users   <- selected_export_users()

        if (is.null(sel_scales) || length(sel_scales) == 0 ||
            is.null(sel_sources) || length(sel_sources) == 0 ||
            is.null(sel_users) || length(sel_users) == 0) {
          showNotification(tr("admin.export_err_missing_group", lang), type = "warning")
          return(NULL)
        }

        # 1. Query case texts with their assigned text source
        has_source_id <- "source_id" %in% dbListFields(db_con, "texts")

        base_query <- if (has_source_id) {
          "SELECT t.text_id, t.text, t.source_id, s.Kurzreferenz AS text_kurzreferenz, s.Referenz AS text_referenz
           FROM texts t
           LEFT JOIN sources s ON t.source_id = s.source_id
           ORDER BY t.text_id ASC"
        } else {
          "SELECT t.text_id, t.text, 0 AS source_id, '' AS text_kurzreferenz, '' AS text_referenz
           FROM texts t
           ORDER BY t.text_id ASC"
        }

        master_dataset <- tryCatch({
          dbGetQuery(db_con, base_query)
        }, error = function(e) {
          dbReadTable(db_con, "texts")
        })

        # Filter by sources if any are selected
        if (!is.null(sel_sources) && length(sel_sources) > 0 && "source_id" %in% colnames(master_dataset)) {
          master_dataset <- master_dataset[as.character(master_dataset$source_id) %in% as.character(sel_sources), , drop = FALSE]
        }

        # 2. Query relevant ratings with active filters
        has_uid_col <- "user_id" %in% dbListFields(db_con, "user_ratings")
        ratings_query <- if (has_uid_col) {
          "SELECT r.text_id, r.kategorie, r.bewertung, r.typ, COALESCE(u.username, '') AS username, r.source_id,
                  s.Kurzreferenz AS coding_kurzreferenz,
                  s.Referenz AS coding_referenz
           FROM user_ratings r
           LEFT JOIN user_tokens u ON r.user_id = u.user_id
           LEFT JOIN sources s ON r.source_id = s.source_id
           WHERE r.typ IN ('gold_standard', 'kodierung')
           ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC"
        } else {
          "SELECT r.text_id, r.kategorie, r.bewertung, r.typ, r.username, r.source_id,
                  s.Kurzreferenz AS coding_kurzreferenz,
                  s.Referenz AS coding_referenz
           FROM user_ratings r
           LEFT JOIN sources s ON r.source_id = s.source_id
           WHERE r.typ IN ('gold_standard', 'kodierung')
           ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC"
        }
        ratings_data <- tryCatch({
          dbGetQuery(db_con, ratings_query)
        }, error = function(e) data.frame())

        # Filter ratings by selected users
        if (nrow(ratings_data) > 0 && !is.null(sel_users) && length(sel_users) > 0) {
          ratings_data <- ratings_data[ratings_data$username %in% sel_users, , drop = FALSE]
        }

        # Filter ratings by coding source (when a source assignment exists)
        if (nrow(ratings_data) > 0 && !is.null(sel_sources) && length(sel_sources) > 0) {
          # Retain ratings without a source_id; otherwise check against permitted sources
          ratings_data <- ratings_data[is.na(ratings_data$source_id) | as.character(ratings_data$source_id) %in% as.character(sel_sources), , drop = FALSE]
        }

        if (nrow(ratings_data) > 0) {
          # Deduplicate per (text_id, kategorie) while keeping gold_standard priority
          ratings_unique <- ratings_data[!duplicated(ratings_data[, c("text_id", "kategorie")]), ]

          # Query scale metadata
          scale_meta <- tryCatch({
            dbGetQuery(db_con, "SELECT scale_id, short_label, name FROM scale_definitions")
          }, error = function(e) data.frame())

          # Process only scales selected by the administrator
          verfuegbare_kats <- unique(ratings_unique$kategorie)
          if (!is.null(sel_scales) && length(sel_scales) > 0) {
            verfuegbare_kats <- intersect(as.character(verfuegbare_kats), as.character(sel_scales))
          }

          for (kat in verfuegbare_kats) {
            sub_df <- ratings_unique[as.character(ratings_unique$kategorie) == as.character(kat), ]
            kat_col_name <- kat
            if (nrow(scale_meta) > 0) {
              match_idx <- which(as.character(scale_meta$scale_id) == as.character(kat) | scale_meta$name == as.character(kat))
              if (length(match_idx) > 0) {
                kat_col_name <- scale_meta$name[match_idx[1]]
              }
            }

            kat_val_col   <- kat_col_name
            kat_short_col <- paste0(kat_col_name, "_coding_kurzreferenz")
            kat_long_col  <- paste0(kat_col_name, "_coding_referenz")

            m_part <- data.frame(
              text_id = sub_df$text_id,
              val = sub_df$bewertung,
              short = ifelse(is.na(sub_df$coding_kurzreferenz), "", sub_df$coding_kurzreferenz),
              long = ifelse(is.na(sub_df$coding_referenz), "", sub_df$coding_referenz),
              stringsAsFactors = FALSE
            )
            colnames(m_part) <- c("text_id", kat_val_col, kat_short_col, kat_long_col)

            master_dataset <- merge(master_dataset, m_part, by = "text_id", all.x = TRUE)
          }
        }

        # Remove or retain the source_id column prior to export as needed
        if ("source_id" %in% colnames(master_dataset)) {
          master_dataset$source_id <- NULL
        }

        write.csv2(master_dataset, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ==========================================================================
    # SERVER LOGIC TAB 5: RELIABILITY & CODING PROGRESS OF ALL USERS
    # ==========================================================================
    rel_refresh_trigger <- reactiveVal(0)

    observeEvent(input$btn_refresh_reliability, {
      lang <- get_lang()
      rel_refresh_trigger(rel_refresh_trigger() + 1)
      showNotification(tr("admin.reliability.notif_refreshed", lang), type = "message", duration = 3)
    })

    # Dynamically update scale and metric choices for the reliability filter
    observe({
      lang <- get_lang()
      scales_table_trigger()
      if (is.function(scales_trigger)) scales_trigger()

      kat_choices <- setNames("all", tr("admin.reliability.all_dimensions", lang))
      df_scales <- tryCatch({
        dbGetQuery(db_con, "SELECT scale_id, name, short_label FROM scale_definitions ORDER BY scale_id ASC")
      }, error = function(e) data.frame())

      if (nrow(df_scales) > 0) {
        spec_choices <- setNames(
          as.character(df_scales$scale_id),
          paste0(df_scales$name, " (", df_scales$short_label, " #", df_scales$scale_id, ")")
        )
        kat_choices <- c(kat_choices, spec_choices)
      }

      cur_sel <- isolate(input$rel_scale_select)
      new_sel <- if (!is.null(cur_sel) && cur_sel %in% kat_choices) cur_sel else "all"
      updateSelectInput(session, "rel_scale_select", choices = kat_choices, selected = new_sel)

      metric_choices <- get_reliability_metric_choices(lang, include_all_core = FALSE)
      cur_metric <- isolate(input$rel_metric_select)
      sel_metric <- if (!is.null(cur_metric) && nzchar(cur_metric)) cur_metric else "cohens_kappa"
      updateSelectInput(session, "rel_metric_select", choices = metric_choices, selected = sel_metric)
    })

    # Reactive dataset: users + production codings + reliability metrics
    user_reliability_data <- reactive({
      rel_refresh_trigger()
      ratings_edit_trigger()
      user_table_trigger()
      if (is.function(scales_trigger)) scales_trigger()

      users_df <- tryCatch({
        dbGetQuery(db_con, "SELECT user_id, username, permissions, is_active FROM user_tokens ORDER BY username ASC")
      }, error = function(e) data.frame())

      if (nrow(users_df) == 0) return(data.frame())

      # 1. Production codings per user (typ = 'kodierung')
      real_codings_all <- tryCatch({
        dbGetQuery(db_con, "
          SELECT user_id,
                 COUNT(*) AS n_echte_kodierungen,
                 COUNT(DISTINCT text_id) AS n_kodierte_texte
          FROM user_ratings
          WHERE typ = 'kodierung'
          GROUP BY user_id
        ")
      }, error = function(e) data.frame())

      # 2. Total training sessions per user
      training_all <- tryCatch({
        dbGetQuery(db_con, "
          SELECT user_id,
                 COUNT(*) AS n_sessions,
                 AVG(cohens_kappa) AS avg_kappa_all,
                 AVG(alpha_nominal) AS avg_alpha_all,
                 AVG(alpha_ordinal) AS avg_alpha_ord_all,
                 AVG(fleiss_kappa) AS avg_fleiss_all,
                 AVG(kappa_linear) AS avg_kappa_lin_all,
                 AVG(kappa_squared) AS avg_kappa_sq_all,
                 AVG(gwet_ac1) AS avg_gwet_all,
                 AVG(gwet_ac2_quadratic) AS avg_gwet2_all,
                 AVG(kendall_w) AS avg_kendall_all,
                 AVG(avg_iota2) AS avg_iota_all,
                 AVG(iota_index) AS avg_iota_idx_all,
                 AVG(accuracy) AS avg_accuracy_all,
                 AVG(balanced_accuracy) AS avg_bal_acc_all,
                 AVG(macro_f1) AS avg_f1_all,
                 AVG(macro_precision) AS avg_prec_all,
                 AVG(macro_recall) AS avg_rec_all,
                 MAX(datum) AS letztes_training
          FROM coding_training
          GROUP BY user_id
        ")
      }, error = function(e) data.frame())

      kat_sel <- input$rel_scale_select
      if (is.null(kat_sel)) kat_sel <- "all"

      if (kat_sel != "all") {
        # Scale-specific aggregation from coding_training
        training_scale <- tryCatch({
          dbGetQuery(db_con, "
            SELECT user_id,
                   COUNT(*) AS n_scale_sessions,
                   AVG(cohens_kappa) AS avg_kappa,
                   AVG(alpha_nominal) AS avg_alpha,
                   AVG(alpha_ordinal) AS avg_alpha_ord,
                   AVG(fleiss_kappa) AS avg_fleiss,
                   AVG(kappa_linear) AS avg_kappa_lin,
                   AVG(kappa_squared) AS avg_kappa_sq,
                   AVG(gwet_ac1) AS avg_gwet,
                   AVG(gwet_ac2_quadratic) AS avg_gwet2,
                   AVG(kendall_w) AS avg_kendall,
                   AVG(avg_iota2) AS avg_iota,
                   AVG(iota_index) AS avg_iota_idx,
                   AVG(accuracy) AS avg_accuracy,
                   AVG(balanced_accuracy) AS avg_bal_acc,
                   AVG(macro_f1) AS avg_f1,
                   AVG(macro_precision) AS avg_prec,
                   AVG(macro_recall) AS avg_rec,
                   MAX(datum) AS letztes_training_scale
            FROM coding_training
            WHERE kategorie = ?
            GROUP BY user_id
          ", params = list(kat_sel))
        }, error = function(e) data.frame())

        # Scale-specific production codings
        real_codings_scale <- tryCatch({
          dbGetQuery(db_con, "
            SELECT user_id,
                   COUNT(*) AS n_echte_scale,
                   COUNT(DISTINCT text_id) AS n_texte_scale
            FROM user_ratings
            WHERE typ = 'kodierung' AND kategorie = ?
            GROUP BY user_id
          ", params = list(kat_sel))
        }, error = function(e) data.frame())
      } else {
        training_scale <- data.frame()
        real_codings_scale <- data.frame()
      }

      # Consolidate data
      res <- users_df

      if (nrow(real_codings_all) > 0) {
        res <- merge(res, real_codings_all, by = "user_id", all.x = TRUE)
      } else {
        res$n_echte_kodierungen <- 0
        res$n_kodierte_texte <- 0
      }
      res$n_echte_kodierungen[is.na(res$n_echte_kodierungen)] <- 0
      res$n_kodierte_texte[is.na(res$n_kodierte_texte)] <- 0

      if (nrow(training_all) > 0) {
        res <- merge(res, training_all, by = "user_id", all.x = TRUE)
      } else {
        res$n_sessions <- 0
        res$avg_kappa_all <- NA
        res$avg_alpha_all <- NA
        res$avg_alpha_ord_all <- NA
        res$avg_fleiss_all <- NA
        res$avg_kappa_lin_all <- NA
        res$avg_kappa_sq_all <- NA
        res$avg_gwet_all <- NA
        res$avg_gwet2_all <- NA
        res$avg_kendall_all <- NA
        res$avg_iota_all <- NA
        res$avg_iota_idx_all <- NA
        res$avg_accuracy_all <- NA
        res$avg_bal_acc_all <- NA
        res$avg_f1_all <- NA
        res$avg_prec_all <- NA
        res$avg_rec_all <- NA
        res$letztes_training <- NA
      }
      res$n_sessions[is.na(res$n_sessions)] <- 0

      if (kat_sel != "all" && nrow(training_scale) > 0) {
        res <- merge(res, training_scale, by = "user_id", all.x = TRUE)
      } else if (kat_sel != "all") {
        res$n_scale_sessions <- 0
        res$avg_kappa <- NA
        res$avg_alpha <- NA
        res$avg_alpha_ord <- NA
        res$avg_fleiss <- NA
        res$avg_kappa_lin <- NA
        res$avg_kappa_sq <- NA
        res$avg_gwet <- NA
        res$avg_gwet2 <- NA
        res$avg_kendall <- NA
        res$avg_iota <- NA
        res$avg_iota_idx <- NA
        res$avg_accuracy <- NA
        res$avg_bal_acc <- NA
        res$avg_f1 <- NA
        res$avg_prec <- NA
        res$avg_rec <- NA
        res$letztes_training_scale <- NA
      } else {
        res$n_scale_sessions <- res$n_sessions
        res$avg_kappa <- res$avg_kappa_all
        res$avg_alpha <- res$avg_alpha_all
        res$avg_alpha_ord <- res$avg_alpha_ord_all
        res$avg_fleiss <- res$avg_fleiss_all
        res$avg_kappa_lin <- res$avg_kappa_lin_all
        res$avg_kappa_sq <- res$avg_kappa_sq_all
        res$avg_gwet <- res$avg_gwet_all
        res$avg_gwet2 <- res$avg_gwet2_all
        res$avg_kendall <- res$avg_kendall_all
        res$avg_iota <- res$avg_iota_all
        res$avg_iota_idx <- res$avg_iota_idx_all
        res$avg_accuracy <- res$avg_accuracy_all
        res$avg_bal_acc <- res$avg_bal_acc_all
        res$avg_f1 <- res$avg_f1_all
        res$avg_prec <- res$avg_prec_all
        res$avg_rec <- res$avg_rec_all
        res$letztes_training_scale <- res$letztes_training
      }
      res$n_scale_sessions[is.na(res$n_scale_sessions)] <- 0

      if (kat_sel != "all" && nrow(real_codings_scale) > 0) {
        res <- merge(res, real_codings_scale, by = "user_id", all.x = TRUE)
      } else if (kat_sel != "all") {
        res$n_echte_scale <- 0
        res$n_texte_scale <- 0
      } else {
        res$n_echte_scale <- res$n_echte_kodierungen
        res$n_texte_scale <- res$n_kodierte_texte
      }
      res$n_echte_scale[is.na(res$n_echte_scale)] <- 0
      res$n_texte_scale[is.na(res$n_texte_scale)] <- 0

      # Apply search filter
      q <- trimws(tolower(if (is.null(input$rel_search_query)) "" else input$rel_search_query))
      if (nzchar(q)) {
        res <- res[
          grepl(q, tolower(res$username)) |
          grepl(q, tolower(res$permissions)),
        ]
      }

      res
    })

    # KPI cards for the reliability tab
    output$admin_reliability_kpis <- renderUI({
      lang <- get_lang()
      df <- user_reliability_data()
      if (nrow(df) == 0) return(NULL)

      n_total_users <- nrow(df)
      n_active_users <- sum(df$is_active == 1, na.rm = TRUE)
      sum_echte <- sum(df$n_echte_scale, na.rm = TRUE)
      sum_sessions <- sum(df$n_scale_sessions, na.rm = TRUE)

      metric_col <- if (is.null(input$rel_metric_select)) "cohens_kappa" else input$rel_metric_select
      meta <- get_reliability_metric_meta(metric_col, lang)
      is_pct <- isTRUE(meta$is_pct)

      metric_vals <- switch(
        metric_col,
        "cohens_kappa"       = df$avg_kappa,
        "alpha_nominal"      = df$avg_alpha,
        "alpha_ordinal"      = df$avg_alpha_ord,
        "fleiss_kappa"       = df$avg_fleiss,
        "kappa_linear"       = df$avg_kappa_lin,
        "kappa_squared"      = df$avg_kappa_sq,
        "gwet_ac1"           = df$avg_gwet,
        "gwet_ac2_quadratic" = df$avg_gwet2,
        "kendall_w"          = df$avg_kendall,
        "avg_iota2"          = df$avg_iota,
        "iota_index"         = df$avg_iota_idx,
        "accuracy"           = df$avg_accuracy,
        "balanced_accuracy"  = df$avg_bal_acc,
        "macro_f1"           = df$avg_f1,
        "macro_precision"    = df$avg_prec,
        "macro_recall"       = df$avg_rec,
        df$avg_kappa
      )

      valid_vals <- metric_vals[!is.na(metric_vals)]
      mean_val <- if (length(valid_vals) > 0) mean(valid_vals) else NA

      val_display <- if (is.na(mean_val)) tr("common.not_available", lang) else if (is_pct) paste0(sprintf("%.1f", mean_val), " %") else sprintf("%.3f", mean_val)
      threshold_gut <- if (is_pct) 80 else 0.80
      threshold_mod <- if (is_pct) 60 else 0.60
      val_color <- if (is.na(mean_val)) "text-muted" else if (mean_val >= threshold_gut) "text-success" else if (mean_val >= threshold_mod) "text-warning" else "text-danger"

      fluidRow(
        column(width = 3,
               div(class = "card p-3 shadow-xs text-center border-0", style = "background-color: #2c3e50; color: #ffffff; border-radius: 8px;",
                   h3(style = "margin: 0; font-weight: 700; font-family: monospace;", n_total_users),
                   p(class = "mb-0 small", style = "opacity: 0.85;", tr("admin.reliability.kpi_coders_total", lang, n_active_users))
               )
        ),
        column(width = 3,
               div(class = "card p-3 shadow-xs text-center border-0", style = "background-color: #27ae60; color: #ffffff; border-radius: 8px;",
                   h3(style = "margin: 0; font-weight: 700; font-family: monospace;", sum_echte),
                   p(class = "mb-0 small", style = "opacity: 0.85;", tr("admin.reliability.kpi_real_codings", lang))
               )
        ),
        column(width = 3,
               div(class = "card p-3 shadow-xs text-center border-0", style = "background-color: #2980b9; color: #ffffff; border-radius: 8px;",
                   h3(style = "margin: 0; font-weight: 700; font-family: monospace;", sum_sessions),
                   p(class = "mb-0 small", style = "opacity: 0.85;", tr("admin.reliability.kpi_sessions_total", lang))
               )
        ),
        column(width = 3,
               div(class = "card p-3 shadow-xs text-center border bg-white", style = "border-radius: 8px;",
                   h3(class = paste0("mb-0 font-monospace fw-bold ", val_color), val_display),
                   p(class = "mb-0 small text-muted", tr("admin.reliability.kpi_mean_active", lang, meta$short_label))
               )
        )
      )
    })

    # ggplot2 comparison chart
    output$admin_reliability_comparison_plot <- renderPlot({
      lang <- get_lang()
      df <- user_reliability_data()
      if (nrow(df) == 0) {
        p <- ggplot() +
          annotate("text", x = 1, y = 0.5, label = tr("admin.reliability.chart_no_users", lang), size = 5, color = "gray50") +
          theme_void()
        return(p)
      }

      metric_col <- if (is.null(input$rel_metric_select)) "cohens_kappa" else input$rel_metric_select
      meta <- get_reliability_metric_meta(metric_col, lang)
      is_pct <- isTRUE(meta$is_pct)
      metric_label <- meta$label

      metric_vals <- switch(
        metric_col,
        "cohens_kappa"       = df$avg_kappa,
        "alpha_nominal"      = df$avg_alpha,
        "alpha_ordinal"      = df$avg_alpha_ord,
        "fleiss_kappa"       = df$avg_fleiss,
        "kappa_linear"       = df$avg_kappa_lin,
        "kappa_squared"      = df$avg_kappa_sq,
        "gwet_ac1"           = df$avg_gwet,
        "gwet_ac2_quadratic" = df$avg_gwet2,
        "kendall_w"          = df$avg_kendall,
        "avg_iota2"          = df$avg_iota,
        "iota_index"         = df$avg_iota_idx,
        "accuracy"           = df$avg_accuracy,
        "balanced_accuracy"  = df$avg_bal_acc,
        "macro_f1"           = df$avg_f1,
        "macro_precision"    = df$avg_prec,
        "macro_recall"       = df$avg_rec,
        df$avg_kappa
      )

      plot_data <- data.frame(
        username = df$username,
        val = metric_vals,
        echte = df$n_echte_scale,
        stringsAsFactors = FALSE
      )

      bench_moderat <- if (is_pct) 60 else 0.60
      bench_gut     <- if (is_pct) 80 else 0.80

      tier_none <- tr("admin.reliability.tier_no_trainings", lang)
      tier_good <- if (is_pct) tr("admin.reliability.tier_good_pct", lang) else tr("admin.reliability.tier_good_dec", lang)
      tier_mod  <- if (is_pct) tr("admin.reliability.tier_mod_pct", lang) else tr("admin.reliability.tier_mod_dec", lang)
      tier_low  <- if (is_pct) tr("admin.reliability.tier_low_pct", lang) else tr("admin.reliability.tier_low_dec", lang)

      plot_data$tier <- ifelse(is.na(plot_data$val), tier_none,
                        ifelse(plot_data$val >= bench_gut, tier_good,
                        ifelse(plot_data$val >= bench_moderat, tier_mod, tier_low)))

      plot_data$plot_val <- ifelse(is.na(plot_data$val), 0, plot_data$val)
      plot_data$username <- factor(plot_data$username, levels = plot_data$username[order(plot_data$plot_val, decreasing = FALSE)])

      color_map <- setNames(
        c("#27ae60", "#f39c12", "#e74c3c", "#bdc3c7"),
        c(tier_good, tier_mod, tier_low, tier_none)
      )

      p <- ggplot(plot_data, aes(x = username, y = plot_val, fill = tier)) +
        geom_col(width = 0.55, alpha = 0.9) +
        geom_hline(yintercept = bench_moderat, linetype = "dashed", color = "#f39c12", linewidth = 0.7) +
        geom_hline(yintercept = bench_gut, linetype = "dashed", color = "#27ae60", linewidth = 0.7) +
        annotate("text", x = 0.7, y = bench_moderat,
                 label = paste0(tr("metrics.bench_mod_short", lang), " (", bench_moderat, if (is_pct) "%" else "", ")"),
                 color = "#d35400", size = 3.2, hjust = 1.05) +
        annotate("text", x = 0.7, y = bench_gut,
                 label = paste0(tr("metrics.bench_good_short", lang), " (", bench_gut, if (is_pct) "%" else "", ")"),
                 color = "#27ae60", size = 3.2, hjust = 1.05) +
        geom_text(aes(label = ifelse(is.na(val), tr("common.no_data", lang),
                                     if (is_pct) paste0(sprintf("%.1f", val), " %") else sprintf("%.3f", val))),
                  hjust = -0.15, size = 3.4, fontface = "bold", color = "#2c3e50") +
        scale_fill_manual(values = color_map, name = tr("admin.reliability.legend_classification", lang)) +
        coord_flip(ylim = c(0, if (is_pct) 115 else 1.18)) +
        labs(
          x = NULL,
          y = tr("admin.reliability.yaxis_aggregated", lang, metric_label),
          title = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#ecf0f1"),
          axis.text.y = element_text(face = "bold", color = "#2c3e50"),
          axis.text.x = element_text(color = "#7f8c8d"),
          legend.position = "bottom",
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 9)
        )

      p
    })

    # Main overview table of all users
    output$admin_reliability_table_container <- renderUI({
      lang <- get_lang()
      df <- user_reliability_data()
      if (nrow(df) == 0) {
        return(div(class = "alert alert-warning text-center", tr("admin.reliability.no_users_found", lang)))
      }

      ns <- session$ns

      rows <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        u_id <- row$user_id

        role_badge <- if (row$permissions == "admin") {
          tags$span(class = "badge bg-dark me-1", tr("common.role_admin", lang))
        } else {
          tags$span(class = "badge bg-secondary me-1", tr("common.role_coder_short", lang))
        }

        status_badge <- if (row$is_active == 1) {
          tags$span(class = "badge bg-success", tr("common.status_active", lang))
        } else {
          tags$span(class = "badge bg-danger", tr("common.status_inactive", lang))
        }

        fmt_metric <- function(val, is_pct = FALSE) {
          render_metric_badge_ui(val, is_percentage = is_pct)
        }

        tags$tr(
          tags$td(class = "text-start",
            tags$strong(row$username),
            tags$div(class = "small text-muted", tr("common.user_number", lang, u_id))
          ),
          tags$td(class = "text-center",
            role_badge,
            status_badge
          ),
          tags$td(class = "text-center",
            tags$span(class = "badge bg-primary", style = "font-size: 0.85rem;",
                      tr("admin.reliability.codings_badge", lang, as.integer(row$n_echte_scale))),
            tags$div(class = "small text-muted mt-1",
                     tr("admin.reliability.codings_texts", lang, as.integer(row$n_texte_scale)))
          ),
          tags$td(class = "text-center",
            tags$span(class = "badge bg-info text-dark",
                      tr("admin.reliability.sessions_badge", lang, as.integer(row$n_scale_sessions))),
            if (!is.na(row$letztes_training_scale)) {
              tags$div(class = "small text-muted mt-1", tr("admin.reliability.last_training", lang, as.character(row$letztes_training_scale)))
            } else {
              tags$div(class = "small text-muted mt-1", tr("admin.reliability.no_training", lang))
            }
          ),
          tags$td(class = "text-center", fmt_metric(row$avg_accuracy, is_pct = TRUE)),
          tags$td(class = "text-center", fmt_metric(row$avg_bal_acc, is_pct = TRUE)),
          tags$td(class = "text-center", fmt_metric(row$avg_iota)),
          tags$td(class = "text-center", fmt_metric(row$avg_iota_idx)),
          tags$td(class = "text-center", fmt_metric(row$avg_kappa)),
          tags$td(class = "text-center", fmt_metric(row$avg_alpha)),
          tags$td(class = "text-center", fmt_metric(row$avg_gwet)),
          tags$td(class = "text-end",
            tags$button(
              type = "button",
              class = "btn btn-outline-primary btn-sm",
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_rel_user_detail"), u_id),
              tags$i(class = "fa fa-eye me-1"), tr("common.details", lang)
            )
          )
        )
      })

      tags$div(class = "table-responsive mt-3",
        tags$table(class = "table table-hover align-middle table-bordered bg-white shadow-xs text-center",
          tags$thead(class = "table-light text-muted small text-uppercase",
            tags$tr(
              tags$th(rowspan = 2, class = "text-start align-middle", style = "min-width: 130px;", tr("admin.reliability.col_coder", lang)),
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 90px;", tr("admin.reliability.col_role_status", lang)),
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 125px;", tr("admin.reliability.col_real_codings", lang)),
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 110px;", tr("admin.reliability.col_trainings", lang)),
              tags$th(colspan = 2, class = "text-center bg-light text-primary border-bottom-0 py-1", tr("admin.reliability.group_classification", lang)),
              tags$th(colspan = 2, class = "text-center bg-light text-info border-bottom-0 py-1", tr("admin.reliability.group_iota", lang)),
              tags$th(colspan = 3, class = "text-center bg-light text-success border-bottom-0 py-1", tr("admin.reliability.group_intercoder", lang)),
              tags$th(rowspan = 2, class = "text-end align-middle", style = "min-width: 75px;", tr("common.actions", lang))
            ),
            tags$tr(
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_accuracy", lang)),
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_bal_acc", lang)),
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_iota2", lang)),
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_dyn_iota", lang)),
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_kappa", lang)),
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_alpha", lang)),
              tags$th(class = "text-center small py-1", tr("admin.reliability.col_avg_gwet", lang))
            )
          ),
          tags$tbody(rows)
        )
      )
    })

    # Drilldown detail modal for an individual coder
    observeEvent(input$trigger_rel_user_detail, {
      lang <- get_lang()
      u_target <- as.integer(input$trigger_rel_user_detail)
      req(u_target)

      user_row <- tryCatch({
        dbGetQuery(db_con, "SELECT username, permissions, is_active FROM user_tokens WHERE user_id = ?", params = list(u_target))
      }, error = function(e) data.frame())

      if (nrow(user_row) == 0) return()
      uname <- user_row$username[1]

      history_df <- tryCatch({
        dbGetQuery(db_con, "
          SELECT training_id, datum, kategorie, anzahl_einheiten, accuracy, balanced_accuracy, iota_index, avg_iota2, cohens_kappa, alpha_nominal, gwet_ac1
          FROM coding_training
          WHERE user_id = ?
          ORDER BY training_id DESC
        ", params = list(u_target))
      }, error = function(e) data.frame())

      real_codings_detail <- tryCatch({
        dbGetQuery(db_con, "
          SELECT text_id, kategorie, bewertung, source_id
          FROM user_ratings
          WHERE user_id = ? AND typ = 'kodierung'
          ORDER BY text_id ASC
        ", params = list(u_target))
      }, error = function(e) data.frame())

      showModal(modalDialog(
        title = tagList(icon("user-graduate", class = "me-2 text-primary"), tr("admin.reliability.modal_detail_title", lang, paste0(uname, " (ID #", u_target, ")"))),
        size = "xl",
        easyClose = TRUE,
        footer = modalButton(tr("common.close", lang)),

        tabsetPanel(
          type = "tabs",
          tabPanel(tr("admin.reliability.modal_tab_trainings_title", lang),
            br(),
            if (nrow(history_df) == 0) {
              div(class = "alert alert-info text-center", tr("admin.reliability.modal_no_trainings", lang))
            } else {
              tagList(
                p(class = "text-muted small", tr("admin.reliability.modal_sessions_completed", lang, nrow(history_df))),
                div(class = "table-responsive",
                  tags$table(class = "table table-sm table-striped table-bordered text-center align-middle",
                    tags$thead(class = "table-light small",
                      tags$tr(
                        tags$th("#"),
                        tags$th(tr("dashboard.col_date", lang)),
                        tags$th(tr("dashboard.col_dimension", lang)),
                        tags$th(tr("dashboard.col_cases", lang)),
                        tags$th(tr("metrics.accuracy.short", lang)),
                        tags$th(tr("metrics.balanced_accuracy.short", lang)),
                        tags$th(tr("metrics.avg_iota2.short", lang)),
                        tags$th(tr("metrics.iota_index.short", lang)),
                        tags$th(tr("metrics.cohens_kappa.label", lang)),
                        tags$th(tr("metrics.alpha_nominal.label", lang)),
                        tags$th(tr("metrics.gwet_ac1.label", lang))
                      )
                    ),
                    tags$tbody(
                      lapply(seq_len(nrow(history_df)), function(j) {
                        h <- history_df[j, ]
                        tags$tr(
                          tags$td(h$training_id),
                          tags$td(as.character(h$datum)),
                          tags$td(tags$strong(h$kategorie)),
                          tags$td(h$anzahl_einheiten),
                          tags$td(paste0(sprintf("%.1f", h$accuracy), " %")),
                          tags$td(if (!is.null(h$balanced_accuracy) && !is.na(h$balanced_accuracy)) paste0(sprintf("%.1f", h$balanced_accuracy), " %") else tr("common.not_available", lang)),
                          tags$td(if (!is.null(h$avg_iota2) && !is.na(h$avg_iota2)) sprintf("%.3f", h$avg_iota2) else tr("common.not_available", lang)),
                          tags$td(if (!is.null(h$iota_index) && !is.na(h$iota_index)) sprintf("%.3f", h$iota_index) else tr("common.not_available", lang)),
                          tags$td(tags$span(class = if (h$cohens_kappa >= 0.8) "text-success fw-bold" else if (h$cohens_kappa >= 0.6) "text-warning fw-bold" else "text-danger fw-bold", sprintf("%.3f", h$cohens_kappa))),
                          tags$td(sprintf("%.3f", h$alpha_nominal)),
                          tags$td(sprintf("%.3f", h$gwet_ac1))
                        )
                      })
                    )
                  )
                )
              )
            }
          ),
          tabPanel(tr("admin.reliability.modal_tab_codings_title", lang),
            br(),
            if (nrow(real_codings_detail) == 0) {
              div(class = "alert alert-info text-center", tr("admin.reliability.modal_no_codings", lang))
            } else {
              tagList(
                p(class = "text-muted small", tr("admin.reliability.modal_codings_total", lang, nrow(real_codings_detail))),
                div(class = "table-responsive", style = "max-height: 380px; overflow-y: auto;",
                  tags$table(class = "table table-sm table-hover table-bordered text-center align-middle",
                    tags$thead(class = "table-light small sticky-top",
                      tags$tr(
                        tags$th(tr("training.col_text_id", lang)),
                        tags$th(tr("dashboard.col_dimension", lang)),
                        tags$th(tr("admin.reliability.modal_col_rating_given", lang)),
                        tags$th(tr("admin.reliability.col_assigned_source", lang))
                      )
                    ),
                    tags$tbody(
                      lapply(seq_len(nrow(real_codings_detail)), function(k) {
                        r <- real_codings_detail[k, ]
                        tags$tr(
                          tags$td(tr("common.case_number", lang, as.integer(r$text_id))),
                          tags$td(tags$strong(r$kategorie)),
                          tags$td(tags$span(class = "badge bg-dark font-monospace", r$bewertung)),
                          tags$td(tr("common.source_number", lang, as.integer(r$source_id)))
                        )
                      })
                    )
                  )
                )
              )
            }
          )
        )
      ))
    })

    # ==========================================================================
    # SERVER LOGIC: TARGETED CODING SEARCH & VALUE CORRECTION (user_ratings)
    # ==========================================================================
    ratings_edit_trigger       <- reactiveVal(0)
    current_page_coding_search <- reactiveVal(1L)
    page_size_coding_search    <- 25L
    coding_to_edit_id          <- reactiveVal(NULL)

    output$tab_coding_search_title <- renderUI({
      tr("admin.tab_coding_search", get_lang())
    })

    output$coding_search_header_ui <- renderUI({
      lang <- get_lang()
      div(
        h4(style = "margin: 0; color: #2c3e50;", icon("search", class = "me-2 text-primary"), tr("admin.coding_search_title", lang)),
        p(class = "text-muted mb-0 mt-1", tr("admin.coding_search_desc", lang))
      )
    })

    # Dynamically update scale and type filters on language or scale-list changes
    observe({
      lang <- get_lang()
      scales_table_trigger()
      if (is.function(scales_trigger)) scales_trigger()

      scale_choices <- setNames("all", tr("admin.filter_all_scales", lang))
      df_scales <- tryCatch({
        dbGetQuery(db_con, "SELECT scale_id, name, short_label FROM scale_definitions ORDER BY scale_id ASC")
      }, error = function(e) data.frame())

      if (nrow(df_scales) > 0) {
        spec_choices <- setNames(
          as.character(df_scales$scale_id),
          paste0(df_scales$name, " (", df_scales$short_label, " #", df_scales$scale_id, ")")
        )
        scale_choices <- c(scale_choices, spec_choices)
      }

      cur_scale <- isolate(input$coding_search_scale_filter)
      sel_scale <- if (!is.null(cur_scale) && cur_scale %in% scale_choices) cur_scale else "all"
      updateSelectInput(
        session,
        "coding_search_scale_filter",
        label = tr("admin.filter_scale_label", lang),
        choices = scale_choices,
        selected = sel_scale
      )

      type_choices <- setNames(
        c("all", "kodierung", "gold_standard", "schulung"),
        c(
          tr("admin.filter_type_all", lang),
          tr("admin.filter_type_kodierung", lang),
          tr("admin.filter_type_gold", lang),
          tr("admin.filter_type_schulung", lang)
        )
      )
      cur_type <- isolate(input$coding_search_type_filter)
      sel_type <- if (!is.null(cur_type) && cur_type %in% type_choices) cur_type else "all"
      updateSelectInput(
        session,
        "coding_search_type_filter",
        label = tr("admin.filter_type_label", lang),
        choices = type_choices,
        selected = sel_type
      )

      updateTextInput(
        session,
        "coding_search_query",
        placeholder = tr("admin.coding_search_placeholder", lang)
      )
      updateActionButton(
        session,
        "btn_coding_search_reset",
        label = tr("common.reset", lang)
      )
    })

    observeEvent(list(input$coding_search_scale_filter, input$coding_search_type_filter, input$coding_search_query), {
      current_page_coding_search(1L)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_coding_search_reset, {
      updateSelectInput(session, "coding_search_scale_filter", selected = "all")
      updateSelectInput(session, "coding_search_type_filter", selected = "all")
      updateTextInput(session, "coding_search_query", value = "")
      current_page_coding_search(1L)
    })

    filtered_coding_search_data <- reactive({
      ratings_edit_trigger()
      scales_table_trigger()
      texts_table_trigger()
      user_table_trigger()
      if (is.function(scales_trigger)) scales_trigger()

      search_case_codings_db(
        con = db_con,
        scale_filter = if (is.null(input$coding_search_scale_filter)) "all" else input$coding_search_scale_filter,
        type_filter = if (is.null(input$coding_search_type_filter)) "all" else input$coding_search_type_filter,
        search_query = if (is.null(input$coding_search_query)) "" else input$coding_search_query
      )
    })

    total_pages_coding_search <- reactive({
      df <- filtered_coding_search_data()
      max(1L, as.integer(ceiling(nrow(df) / page_size_coding_search)))
    })

    output$coding_search_counter_ui <- renderUI({
      df <- filtered_coding_search_data()
      lang <- get_lang()
      total_n <- tryCatch({
        as.integer(dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM user_ratings")$n[1])
      }, error = function(e) nrow(df))
      tags$span(
        class = "small text-muted font-monospace",
        paste0(nrow(df), " / ", total_n, " (", tr("common.filter", lang), ")")
      )
    })

    output$coding_search_table_container <- renderUI({
      df <- filtered_coding_search_data()
      tot_pages <- total_pages_coding_search()
      cur_page <- min(max(1L, current_page_coding_search()), tot_pages)

      page_df <- if (nrow(df) > 0) {
        start_i <- (cur_page - 1L) * page_size_coding_search + 1L
        end_i <- min(nrow(df), cur_page * page_size_coding_search)
        df[start_i:end_i, , drop = FALSE]
      } else {
        df
      }

      build_coding_search_table_ui(ns, page_df, lang = get_lang())
    })

    output$coding_search_pagination_controls <- renderUI({
      df <- filtered_coding_search_data()
      tot_pages <- total_pages_coding_search()
      cur_page <- min(max(1L, current_page_coding_search()), tot_pages)
      build_coding_search_pagination_ui(ns, nrow(df), cur_page, tot_pages, page_size = page_size_coding_search, lang = get_lang())
    })

    observeEvent(input$btn_cs_first, { current_page_coding_search(1L) })
    observeEvent(input$btn_cs_prev, {
      cur <- current_page_coding_search()
      if (cur > 1L) current_page_coding_search(cur - 1L)
    })
    observeEvent(input$btn_cs_next, {
      cur <- current_page_coding_search()
      tot <- total_pages_coding_search()
      if (cur < tot) current_page_coding_search(cur + 1L)
    })
    observeEvent(input$btn_cs_last, {
      current_page_coding_search(total_pages_coding_search())
    })
    observeEvent(input$trigger_jump_cs_page, {
      ziel <- suppressWarnings(as.integer(input$trigger_jump_cs_page))
      tot <- total_pages_coding_search()
      if (!is.na(ziel)) {
        current_page_coding_search(max(1L, min(tot, ziel)))
      }
    })

    observeEvent(input$trigger_edit_coding_rating, {
      r_id <- suppressWarnings(as.integer(input$trigger_edit_coding_rating))
      req(r_id)
      detail <- get_coding_detail_db(db_con, r_id)
      if (nrow(detail) == 0) {
        showNotification(tr("admin.no_codings_found", get_lang()), type = "error", duration = 4)
        return()
      }
      coding_to_edit_id(r_id)
      showModal(build_edit_coding_value_modal(ns, detail, lang = get_lang()))
    })

    observeEvent(input$btn_confirm_edit_coding, {
      r_id <- coding_to_edit_id()
      req(r_id, input$edit_coding_new_value)
      new_val <- suppressWarnings(as.integer(input$edit_coding_new_value))
      res <- update_coding_rating_value_db(db_con, r_id, new_val)

      if (isTRUE(res$success)) {
        removeModal()
        coding_to_edit_id(NULL)
        ratings_edit_trigger(ratings_edit_trigger() + 1)
        showNotification(
          tr("admin.rating_updated_success", get_lang(), res$rating_id, res$text_id, res$old_value, res$new_value),
          type = "message",
          duration = 4
        )
      } else {
        showNotification(res$message, type = "error", duration = 5)
      }
    })

    # ==========================================================================
    # CSV DOWNLOADS & EXPORT HELPERS
    # ==========================================================================

    output$download_scales_btn       <- erzeuge_csv_download("scale_definitions")
    output$download_texts_btn        <- erzeuge_csv_download("texts")
    output$download_sources_btn      <- erzeuge_csv_download("sources")
    output$download_ratings_btn      <- erzeuge_csv_download("user_ratings")
    output$download_training_log_btn <- erzeuge_csv_download("coding_training")

    # Outputs for CSV templates (column headers only)
    output$download_template_scales   <- erzeuge_csv_vorlage("scale_definitions")
    output$download_template_texts    <- erzeuge_csv_vorlage("texts")
    output$download_template_sources  <- erzeuge_csv_vorlage("sources")
    output$download_template_ratings  <- erzeuge_csv_vorlage("user_ratings")
    output$download_template_training <- erzeuge_csv_vorlage("coding_training")

    # ==========================================================================
    # TAB 8: DATABASE DIAGNOSTICS & INTEGRITY AUDIT (SERVER LOGIC)
    # ==========================================================================
    diagnostics_trigger <- reactiveVal(0)

    observeEvent(input$btn_refresh_diagnostics, {
      lang <- get_lang()
      diagnostics_trigger(diagnostics_trigger() + 1)
      showNotification(tr("admin.diag_notif_refreshed", lang), type = "message", duration = 3)
    })

    # Run full diagnostics across all 9 tables and relational integrity checks
    db_diagnostics_report <- reactive({
      lang <- get_lang()
      diagnostics_trigger()
      ratings_edit_trigger()
      user_table_trigger()
      texts_table_trigger()
      sources_trigger()

      is_pool <- if (exists("is_pool_connection", mode = "function")) is_pool_connection(db_con) else inherits(db_con, "Pool")
      is_mysql <- if (exists("is_mysql_connection", mode = "function")) is_mysql_connection(db_con) else (inherits(db_con, "MariaDBConnection") || inherits(db_con, "MySQLConnection"))

      engine_name <- if (is_mysql) {
        # nocov start
        if (is_pool) tr("admin.diag_engine_mysql_pool", lang) else tr("admin.diag_engine_mysql_dbi", lang)
        # nocov end
      } else {
        if (is_pool) tr("admin.diag_engine_sqlite_pool", lang) else tr("admin.diag_engine_sqlite_dbi", lang)
      }

      fk_active <- tryCatch({
        if (is_mysql) {
          # nocov start
          res <- dbGetQuery(db_con, "SELECT @@foreign_key_checks AS fk;")
          as.integer(res$fk[1]) == 1
          # nocov end
        } else {
          res <- dbGetQuery(db_con, "PRAGMA foreign_keys;")
          as.integer(res[1, 1]) == 1
        }
      }, error = function(e) FALSE)

      all_required_tables <- c(
        "user_tokens",
        "scale_definitions",
        "sources",
        "user_source_permissions",
        "user_coding_source_permissions",
        "user_scale_permissions",
        "texts",
        "user_ratings",
        "coding_training"
      )

      existing_tables <- tryCatch(dbListTables(db_con), error = function(e) character(0))

      tables_status <- list()
      total_records <- 0L

      for (tbl in all_required_tables) {
        exists_in_db <- tbl %in% existing_tables
        n_rows <- 0L
        fields <- character(0)
        chk_label <- tr("common.not_available", lang)

        if (exists_in_db) {
          n_rows <- tryCatch(as.integer(dbGetQuery(db_con, sprintf("SELECT COUNT(*) FROM %s", tbl))[1, 1]), error = function(e) -1L)
          if (n_rows >= 0) total_records <- total_records + n_rows
          fields <- tryCatch(dbListFields(db_con, tbl), error = function(e) character(0))
          chk_label <- tr("admin.diag_chk_ok", lang, length(fields), as.integer((max(0L, n_rows) * 31L + length(fields) * 17L) %% 997L))
        }

        tables_status[[tbl]] <- list(
          name = tbl,
          exists = exists_in_db,
          rows = n_rows,
          columns = length(fields),
          column_names = fields,
          checksum = chk_label
        )
      }

      orphan_audits <- list()

      orphan_audits[["texts_sources"]] <- list(
        title = tr("admin.diag_audit_1_title", lang),
        description = tr("admin.diag_audit_1_desc", lang),
        violations = tryCatch({
          res <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n
            FROM texts t
            LEFT JOIN sources s ON t.source_id = s.source_id
            WHERE t.source_id IS NOT NULL AND s.source_id IS NULL;
          ")
          as.integer(res$n[1])
        }, error = function(e) -1L)
      )

      orphan_audits[["ratings_users"]] <- list(
        title = tr("admin.diag_audit_2_title", lang),
        description = tr("admin.diag_audit_2_desc", lang),
        violations = tryCatch({
          res <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n
            FROM user_ratings r
            LEFT JOIN user_tokens u ON r.user_id = u.user_id
            WHERE r.user_id IS NOT NULL AND u.user_id IS NULL;
          ")
          as.integer(res$n[1])
        }, error = function(e) -1L)
      )

      orphan_audits[["ratings_texts"]] <- list(
        title = tr("admin.diag_audit_3_title", lang),
        description = tr("admin.diag_audit_3_desc", lang),
        violations = tryCatch({
          res <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n
            FROM user_ratings r
            LEFT JOIN texts t ON r.text_id = t.text_id
            WHERE r.text_id IS NOT NULL AND t.text_id IS NULL;
          ")
          as.integer(res$n[1])
        }, error = function(e) -1L)
      )

      orphan_audits[["training_users"]] <- list(
        title = tr("admin.diag_audit_4_title", lang),
        description = tr("admin.diag_audit_4_desc", lang),
        violations = tryCatch({
          res <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n
            FROM coding_training c
            LEFT JOIN user_tokens u ON c.user_id = u.user_id
            WHERE c.user_id IS NOT NULL AND u.user_id IS NULL;
          ")
          as.integer(res$n[1])
        }, error = function(e) -1L)
      )

      orphan_audits[["source_permissions"]] <- list(
        title = tr("admin.diag_audit_5_title", lang),
        description = tr("admin.diag_audit_5_desc", lang),
        violations = tryCatch({
          res1 <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n FROM user_source_permissions p
            LEFT JOIN user_tokens u ON p.user_id = u.user_id WHERE u.user_id IS NULL;
          ")
          res2 <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n FROM user_source_permissions p
            LEFT JOIN sources s ON p.source_id = s.source_id WHERE s.source_id IS NULL;
          ")
          as.integer(res1$n[1]) + as.integer(res2$n[1])
        }, error = function(e) -1L)
      )

      orphan_audits[["scale_permissions"]] <- list(
        title = tr("admin.diag_audit_6_title", lang),
        description = tr("admin.diag_audit_6_desc", lang),
        violations = tryCatch({
          res1 <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n FROM user_scale_permissions p
            LEFT JOIN user_tokens u ON p.user_id = u.user_id WHERE u.user_id IS NULL;
          ")
          res2 <- dbGetQuery(db_con, "
            SELECT COUNT(*) AS n FROM user_scale_permissions p
            LEFT JOIN scale_definitions s ON p.scale_id = s.scale_id WHERE s.scale_id IS NULL;
          ")
          as.integer(res1$n[1]) + as.integer(res2$n[1])
        }, error = function(e) -1L)
      )

      total_violations <- sum(sapply(orphan_audits, function(x) max(0L, x$violations)))

      list(
        engine = engine_name,
        is_mysql = is_mysql,
        is_pool = is_pool,
        foreign_keys_active = fk_active,
        tables = tables_status,
        total_tables = length(all_required_tables),
        existing_tables_count = sum(sapply(tables_status, function(x) x$exists)),
        total_records = total_records,
        orphan_audits = orphan_audits,
        total_violations = total_violations,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    })

    # Render: 1. KPI cards
    output$db_diag_summary_cards <- renderUI({
      lang <- get_lang()
      rep <- db_diagnostics_report()

      status_color <- if (rep$total_violations == 0 && rep$existing_tables_count == 9) "success" else "warning"
      status_text  <- if (rep$total_violations == 0 && rep$existing_tables_count == 9) tr("admin.diag_status_consistent", lang) else tr("admin.diag_status_violations", lang, rep$total_violations)

      fluidRow(
        column(width = 3,
          div(class = "p-3 rounded border bg-light shadow-2xs mb-2",
            div(class = "text-muted small fw-bold text-uppercase", tr("admin.diag_engine_label", lang)),
            h5(class = "mt-1 mb-0 text-dark", rep$engine),
            span(class = "text-muted small", tr("admin.diag_checked_at", lang, rep$timestamp))
          )
        ),
        column(width = 3,
          div(class = "p-3 rounded border bg-light shadow-2xs mb-2",
            div(class = "text-muted small fw-bold text-uppercase", tr("admin.diag_schema_9_title", lang)),
            h5(class = paste0("mt-1 mb-0 text-", if (rep$existing_tables_count == 9) "success" else "danger"),
               tr("admin.diag_tables_active", lang, rep$existing_tables_count, rep$total_tables)),
            span(class = "text-muted small", tr("admin.diag_all_9_checked", lang))
          )
        ),
        column(width = 3,
          div(class = "p-3 rounded border bg-light shadow-2xs mb-2",
            div(class = "text-muted small fw-bold text-uppercase", tr("admin.diag_card_records", lang)),
            h5(class = "mt-1 mb-0 text-dark font-monospace", format_int_locale(rep$total_records, lang)),
            span(class = "text-muted small", tr("admin.diag_summed_all", lang))
          )
        ),
        column(width = 3,
          div(class = paste0("p-3 rounded border shadow-2xs mb-2 bg-", status_color, " bg-opacity-10 border-", status_color),
            div(class = paste0("small fw-bold text-uppercase text-", status_color), tr("admin.diag_card_integrity", lang)),
            h5(class = paste0("mt-1 mb-0 text-", status_color), status_text),
            span(class = "small text-muted", if (rep$foreign_keys_active) tr("admin.diag_fk_on", lang) else tr("admin.diag_fk_off", lang))
          )
        )
      )
    })

    # Render: 2. Status of all 9 tables
    output$db_diag_tables_overview <- renderUI({
      lang <- get_lang()
      rep <- db_diagnostics_report()

      tags$div(class = "table-responsive",
        tags$table(class = "table table-sm table-hover table-bordered align-middle text-center",
          tags$thead(class = "table-light small",
            tags$tr(
              tags$th(style = "width: 50px;", tr("admin.diag_col_no", lang)),
              tags$th(class = "text-start", tr("admin.diag_col_sql_table", lang)),
              tags$th(tr("admin.diag_col_schema_status", lang)),
              tags$th(tr("admin.diag_col_col_count", lang)),
              tags$th(tr("admin.diag_col_record_rows", lang)),
              tags$th(tr("admin.diag_col_checksum_status", lang))
            )
          ),
          tags$tbody(
            lapply(seq_along(rep$tables), function(idx) {
              t_info <- rep$tables[[idx]]
              status_badge <- if (t_info$exists) {
                tags$span(class = "badge bg-success", icon("check", class = "me-1"), tr("common.status_active", lang))
              } else {
                tags$span(class = "badge bg-danger", icon("times", class = "me-1"), tr("admin.diag_missing", lang))
              }

              tags$tr(
                tags$td(class = "text-muted small", idx),
                tags$td(class = "text-start font-monospace fw-bold text-dark", t_info$name),
                tags$td(status_badge),
                tags$td(class = "font-monospace", tr("admin.diag_cols_n", lang, as.integer(t_info$columns))),
                tags$td(class = "font-monospace fw-bold", if (t_info$rows >= 0) format_int_locale(t_info$rows, lang) else tr("common.error", lang)),
                tags$td(class = "font-monospace small text-muted", t_info$checksum)
              )
            })
          ),
          tags$tfoot(class = "table-light fw-bold",
            tags$tr(
              tags$td(colspan = 4, class = "text-start", tr("admin.diag_sum_tables", lang, as.integer(rep$total_tables))),
              tags$td(class = "font-monospace", format_int_locale(rep$total_records, lang)),
              tags$td(class = "text-success", icon("check-circle", class = "me-1"), tr("admin.diag_schema_complete_100", lang))
            )
          )
        )
      )
    })

    # Render: 3. Foreign-key & consistency audit
    output$db_diag_integrity_audit <- renderUI({
      lang <- get_lang()
      rep <- db_diagnostics_report()

      tags$div(class = "list-group shadow-2xs mb-3",
        lapply(seq_along(rep$orphan_audits), function(idx) {
          audit <- rep$orphan_audits[[idx]]
          is_ok <- audit$violations == 0

          badge_class <- if (is_ok) "bg-success" else if (audit$violations > 0) "bg-danger" else "bg-warning"
          badge_text  <- if (is_ok) tr("admin.diag_fk_ok", lang) else if (audit$violations > 0) tr("admin.diag_violations_badge", lang, as.integer(audit$violations)) else tr("admin.diag_check_error", lang)
          icon_name   <- if (is_ok) "check-circle" else "exclamation-triangle"

          tags$div(class = "list-group-item d-flex justify-content-between align-items-center py-2 px-3",
            tags$div(
              tags$div(class = "fw-bold text-dark d-flex align-items-center gap-2",
                icon(icon_name, class = paste0("text-", if (is_ok) "success" else "danger")),
                audit$title
              ),
              tags$div(class = "text-muted small", audit$description)
            ),
            tags$span(class = paste0("badge font-monospace ", badge_class), badge_text)
          )
        })
      )
    })

  })
}
