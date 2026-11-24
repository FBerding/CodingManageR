# Note: This file was generated with the assistance of AI.

# ==============================================================================
# CODING MODULE FOR R SHINY (FLEXIBLE CASE SELECTION: RANDOM VS. TEXT SOURCE)
# ==============================================================================
# Dependencies (shiny, DBI) are imported via NAMESPACE

# ==============================================================================
# MODULE UI
# ==============================================================================

#' Coding Module UI
#'
#' Creates the Shiny UI container for the interactive case coding workspace.
#'
#' @param id Character string specifying the Shiny module namespace identifier.
#'
#' @return A `shiny.tag` (`uiOutput`) rendering the coding workspace.
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(kodierungenUI("mod_codings"))
#' }
#'
#' @export
kodierungenUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("kodier_bereich"))
}

# ==============================================================================
# MODULE SERVER
# ==============================================================================

#' Coding Module Server
#'
#' Implements the server-side workflow for production case coding, including
#' scale and text source selection, permission filtering, random or
#' source-scoped unrated case retrieval, rating persistence, and reference
#' example inspection.
#'
#' @param id Character string specifying the Shiny module namespace identifier.
#' @param db_con Active `DBIConnection` or `pool::Pool` database handle.
#' @param user_id_reactive Optional reactive expression returning the active
#'   user's integer ID.
#' @param username_reactive Reactive expression returning the active username.
#' @param kategorie_spalten Reactive expression returning a character vector of
#'   configured scale identifiers or column names.
#' @param kategorien_liste Reactive expression returning a named character
#'   vector of available coding scales (`scale_id` values with display names).
#' @param trigger_parent A `reactiveVal` incremented whenever new codings are
#'   saved so dependent modules refresh automatically.
#' @param scales_trigger Optional `reactiveVal` triggered when scale definitions
#'   change in the admin panel.
#' @param current_lang Reactive expression or character scalar providing the
#'   active UI language code (`"de"` or `"en"`).
#'
#' @return The return value of `shiny::moduleServer()`.
#'
#' @examples
#' \dontrun{
#' kodierungenServer(
#'   id = "mod_codings",
#'   db_con = db_con,
#'   user_id_reactive = reactive(1L),
#'   username_reactive = reactive("admin"),
#'   kategorie_spalten = reactive(character(0)),
#'   kategorien_liste = reactive(c("Scale 1" = "1")),
#'   trigger_parent = reactiveVal(0L)
#' )
#' }
#'
#' @export
kodierungenServer <- function(id, db_con, user_id_reactive = NULL, username_reactive, kategorie_spalten, kategorien_liste, trigger_parent, scales_trigger = NULL, current_lang = reactiveVal("de")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    get_lang <- function() {
      if (is.reactive(current_lang) || is.function(current_lang)) current_lang() else as.character(current_lang)
    }

    get_kategorie_spalten <- reactive({
      if (!is.null(scales_trigger) && is.function(scales_trigger)) scales_trigger()
      kategorie_spalten()
    })

    get_permitted_kategorien_liste <- reactive({
      if (!is.null(scales_trigger) && is.function(scales_trigger)) scales_trigger()
      all_choices <- kategorien_liste()
      u_info <- get_user_info()

      if (identical(u_info$permissions, "admin")) {
        return(all_choices)
      }

      permitted_ids <- tryCatch({
        as.character(dbGetQuery(db_con, "SELECT scale_id FROM user_scale_permissions WHERE user_id = ?", params = list(u_info$id))$scale_id)
      }, error = function(e) character(0))

      if (length(permitted_ids) == 0) {
        return(character(0))
      }

      all_choices[as.character(all_choices) %in% permitted_ids]
    })

    get_kategorien_liste <- reactive({
      get_permitted_kategorien_liste()
    })

    observe({
      lang <- get_lang()
      choices <- get_permitted_kategorien_liste()
      current_sel <- isolate(input$gewaehlte_kategorie)
      new_sel <- if (!is.null(current_sel) && current_sel %in% choices) {
        current_sel
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        NULL
      }
      if (length(choices) == 0) {
        empty_choice <- setNames("", tr("coding.no_scales_unlocked", lang))
        updateSelectInput(session, "gewaehlte_kategorie", choices = empty_choice, selected = "")
      } else {
        updateSelectInput(session, "gewaehlte_kategorie", choices = choices, selected = new_sel)
      }
    })

    modus_aktiv <- reactiveVal(FALSE)
    aktueller_fall <- reactiveVal(NULL)
    aktive_quelle_info <- reactiveVal(NULL)
    aktive_text_quelle_info <- reactiveVal(NULL)
    aktiver_filter_modus <- reactiveVal("alle")
    aktive_text_quelle_id <- reactiveVal(NULL)
    max_bewertung_wert <- reactiveVal(3)
    beschreibung_text <- reactiveVal("")
    verbleibende_faelle <- reactiveVal(0)

    get_user_info <- function() {
      get_user_info_db(db_con, username_reactive, user_id_reactive)
    }

    get_rand_fn <- function() {
      get_db_random_function(db_con)
    }

    ermittle_erlaubte_quellen <- function(u_id = NULL, ...) {
      if (is.null(u_id) || !is.numeric(u_id)) {
        u_info <- get_user_info()
        u_id <- u_info$id
      }
      u_info <- get_user_info_db(db_con, username_reactive, u_id)
      is_admin <- identical(u_info$permissions, "admin")
      if (is_admin) return(NULL)
      get_user_permitted_sources_db(db_con, u_id, is_admin = FALSE)
    }

    ermittle_erlaubte_kodier_quellen <- function(u_id = NULL) {
      if (is.null(u_id) || !is.numeric(u_id)) {
        u_info <- get_user_info()
        u_id <- u_info$id
      }
      u_info <- get_user_info_db(db_con, username_reactive, u_id)
      is_admin <- identical(u_info$permissions, "admin")
      if (is_admin) return(NULL)
      get_user_permitted_coding_sources_db(db_con, u_id, is_admin = FALSE)
    }

    zaehle_offene_faelle <- function(kat_name, u_id = NULL, f_modus = "alle", s_id = NULL) {
      if (is.null(u_id) || !is.numeric(u_id)) {
        u_info <- get_user_info()
        u_id <- u_info$id
      }
      erlaubte_sids <- ermittle_erlaubte_quellen(u_id)

      if (!is.null(erlaubte_sids) && length(erlaubte_sids) == 0) {
        return(0)
      }

      base_sql <- "
        SELECT COUNT(*) AS offene_anzahl
        FROM texts t
        LEFT JOIN user_ratings r ON t.text_id = r.text_id
          AND r.kategorie = ?
          AND r.typ IN ('kodierung', 'gold_standard')
        WHERE r.rating_id IS NULL
      "

      params_list <- list(kat_name)

      if (!is.null(erlaubte_sids)) {
        in_clause <- paste(as.integer(erlaubte_sids), collapse = ",")
        base_sql <- paste0(base_sql, sprintf(" AND t.source_id IN (%s)", in_clause))
      }

      if (f_modus == "quelle" && !is.null(s_id) && !is.na(s_id)) {
        base_sql <- paste0(base_sql, " AND t.source_id = ?")
        params_list <- c(params_list, list(as.integer(s_id)))
      }

      res <- dbGetQuery(db_con, base_sql, params = params_list)
      if (nrow(res) > 0) as.integer(res$offene_anzahl[1]) else 0
    }

    ziehe_unbewerteten_fall <- function(kat_name, u_id = NULL, f_modus = "alle", s_id = NULL) {
      if (is.null(u_id) || !is.numeric(u_id)) {
        u_info <- get_user_info()
        u_id <- u_info$id
      }
      n_offen <- zaehle_offene_faelle(kat_name, u_id, f_modus, s_id)
      verbleibende_faelle(n_offen)

      if (n_offen == 0) return(NULL)

      erlaubte_sids <- ermittle_erlaubte_quellen(u_id)
      rand_fn <- get_rand_fn()

      main_sql <- "
        SELECT t.text_id, t.text, t.source_id,
               s.Kurzreferenz AS text_kurzreferenz,
               s.Referenz AS text_referenz,
               s.Lizenz AS text_lizenz
        FROM texts t
        LEFT JOIN user_ratings r ON t.text_id = r.text_id
          AND r.kategorie = ?
          AND r.typ IN ('kodierung', 'gold_standard')
        LEFT JOIN sources s ON t.source_id = s.source_id
        WHERE r.rating_id IS NULL
      "

      params_list <- list(kat_name)

      if (!is.null(erlaubte_sids)) {
        in_clause <- paste(as.integer(erlaubte_sids), collapse = ",")
        main_sql <- paste0(main_sql, sprintf(" AND t.source_id IN (%s)", in_clause))
      }

      if (f_modus == "quelle" && !is.null(s_id) && !is.na(s_id)) {
        main_sql <- paste0(main_sql, sprintf(" AND t.source_id = ? ORDER BY %s LIMIT 1", rand_fn))
        params_list <- c(params_list, list(as.integer(s_id)))
      } else {
        main_sql <- paste0(main_sql, sprintf(" ORDER BY %s LIMIT 1", rand_fn))
      }

      dbGetQuery(db_con, main_sql, params = params_list)
    }

    output$kodier_bereich <- renderUI({
      lang <- get_lang()
      k_liste <- get_kategorien_liste()
      sel_kat <- isolate(input$gewaehlte_kategorie)
      if (is.null(sel_kat) || !(sel_kat %in% k_liste)) {
        sel_kat <- if (length(k_liste) > 0) k_liste[1] else NULL
      }

      if (!modus_aktiv()) {
        sources_df <- tryCatch({
          dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz, Lizenz FROM sources ORDER BY source_id ASC")
        }, error = function(e) data.frame())

        hat_quellen <- nrow(sources_df) > 0
        u_info <- get_user_info()
        erlaubte_sids <- ermittle_erlaubte_quellen(u_info$id)
        erlaubte_kodier_sids <- ermittle_erlaubte_kodier_quellen(u_info$id)

        filter_sources_df <- if (!is.null(erlaubte_sids)) {
          sources_df[sources_df$source_id %in% erlaubte_sids, ]
        } else {
          sources_df
        }

        choices_filter_quellen <- if (nrow(filter_sources_df) > 0) {
          labels <- paste0("[#", filter_sources_df$source_id, "] ", filter_sources_df$Kurzreferenz, " - ", substr(filter_sources_df$Referenz, 1, 60), "...")
          setNames(as.character(filter_sources_df$source_id), labels)
        } else {
          character(0)
        }

        kodier_sources_df <- if (!is.null(erlaubte_kodier_sids)) {
          sources_df[sources_df$source_id %in% erlaubte_kodier_sids, ]
        } else {
          sources_df
        }

        choices_kodier_quellen <- if (nrow(kodier_sources_df) > 0) {
          labels <- paste0("[#", kodier_sources_df$source_id, "] ", kodier_sources_df$Kurzreferenz, " - ", substr(kodier_sources_df$Referenz, 1, 60), "...")
          setNames(as.character(kodier_sources_df$source_id), labels)
        } else {
          character(0)
        }

        sel_f_modus <- isolate(input$fall_filter_modus)
        if (is.null(sel_f_modus) || !(sel_f_modus %in% c("alle", "quelle"))) sel_f_modus <- "alle"

        sel_t_src <- isolate(input$gewaehlte_text_quelle)
        if (is.null(sel_t_src) || !(sel_t_src %in% choices_filter_quellen)) {
          sel_t_src <- if (length(choices_filter_quellen) > 0) choices_filter_quellen[1] else NULL
        }

        sel_c_src <- isolate(input$gewaehlte_quelle)
        if (is.null(sel_c_src) || !(sel_c_src %in% choices_kodier_quellen)) {
          sel_c_src <- if (length(choices_kodier_quellen) > 0) choices_kodier_quellen[1] else NULL
        }

        radio_choices <- setNames(
          c("alle", "quelle"),
          c(tr("coding.all_text_sources", lang, length(choices_filter_quellen)), tr("coding.select_text_source_label", lang))
        )

        fluidRow(
          column(
            width = 8,
            div(
              class = "cm-surface",
              h4(class = "mb-1", tr("coding.config_card_title", lang)),
              p(class = "text-muted small mb-3", tr("coding.config_desc", lang)),
              if (!is.null(erlaubte_sids) && length(erlaubte_sids) == 0) {
                div(class = "alert alert-danger", icon("exclamation-triangle", class = "me-1"),
                    " ", tr("coding.no_permission_desc", lang))
              },
              if (!is.null(erlaubte_kodier_sids) && length(erlaubte_kodier_sids) == 0) {
                div(class = "alert alert-danger", icon("ban", class = "me-1"),
                    " ", tr("coding.no_coding_sources_err", lang))
              },
              hr(),

              selectInput(ns("gewaehlte_kategorie"), tr("coding.select_scale_label", lang),
                          choices = k_liste, selected = sel_kat, width = "100%"),

              div(
                class = "cm-surface-subtle my-3",
                h5(class = "mb-1", tr("coding.select_mode_label", lang)),
                p(class = "text-muted small mb-2", tr("coding.subtitle", lang)),
                radioButtons(
                  ns("fall_filter_modus"),
                  label = NULL,
                  choices = radio_choices,
                  selected = sel_f_modus,
                  width = "100%"
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'quelle'", ns("fall_filter_modus")),
                  div(
                    class = "ps-3 pe-2 pt-2 border-top",
                    if (length(choices_filter_quellen) > 0) {
                      selectInput(ns("gewaehlte_text_quelle"), tr("coding.select_text_source_label", lang),
                                  choices = choices_filter_quellen, selected = sel_t_src, width = "100%")
                    } else {
                      p(class = "text-danger small mb-0", tr("coding.no_permission_desc", lang))
                    }
                  )
                )
              ),

              if (length(choices_kodier_quellen) > 0) {
                div(
                  class = "mb-3",
                  tags$label(class = "fw-semibold mb-1", tr("coding.select_coding_source_label", lang)),
                  selectInput(ns("gewaehlte_quelle"), label = NULL,
                              choices = choices_kodier_quellen, selected = sel_c_src, width = "100%")
                )
              } else {
                div(
                  class = "alert alert-warning mb-3",
                  h5(class = "mb-1", tr("coding.no_coding_sources_err", lang)),
                  p(class = "mb-0", tr("coding.no_permission_desc", lang))
                )
              },

              uiOutput(ns("konfiguration_status_ui"))
            )
          )
        )
      } else {
        fall <- aktueller_fall()
        kodier_quelle <- aktive_quelle_info()
        text_quelle <- aktive_text_quelle_info()
        f_modus <- aktiver_filter_modus()
        slider_val <- isolate(input$bewertung_slider)
        if (is.null(slider_val) || is.na(slider_val)) slider_val <- 0

        k_spalten <- get_kategorie_spalten()
        kat_lbl <- if (length(k_spalten) > 0) names(k_spalten)[which(k_spalten == sel_kat)[1]] else NULL
        if (is.na(kat_lbl) || is.null(kat_lbl) || kat_lbl == "") kat_lbl <- paste0("ID #", sel_kat)

        div(
          class = "cm-module-section",
          div(
            class = "cm-session-banner",
            div(
              class = "d-flex flex-wrap justify-content-between align-items-start gap-3",
              div(
                h5(class = "mb-0", tr("training.scale_badge", lang, kat_lbl)),
                div(
                  class = "cm-session-meta",
                  if (f_modus == "alle") {
                    tags$span(class = "badge bg-secondary", icon("random", class = "me-1"), tr("coding.case_random", lang))
                  } else {
                    tags$span(icon("filter", class = "me-1"), paste0("[#", text_quelle$source_id, "] ", text_quelle$Kurzreferenz))
                  },
                  if (!is.null(fall) && nrow(fall) > 0 && !is.null(fall$text_kurzreferenz)) {
                    tagList(
                      tags$span("\u00b7"),
                      tags$span(icon("file-lines", class = "me-1"), paste0(tr("coding.source_info", lang), " ", fall$text_kurzreferenz))
                    )
                  },
                  if (!is.null(kodier_quelle)) {
                    tagList(
                      tags$span("\u00b7"),
                      tags$span(icon("bookmark", class = "me-1"), paste0(tr("coding.coding_source_info", lang), " [#", kodier_quelle$source_id, "] ", kodier_quelle$Kurzreferenz))
                    )
                  }
                )
              ),
              div(
                class = "text-end",
                span(class = "text-muted small d-block", tr("dashboard.col_total", lang)),
                h4(class = "cm-session-counter", verbleibende_faelle())
              )
            ),
            if (!is.null(kodier_quelle)) {
              div(
                class = "border-top pt-2 mt-2 text-muted small",
                tags$em(paste(tr("coding.coding_source_info", lang), kodier_quelle$Referenz))
              )
            }
          ),

          if (is.null(fall) || nrow(fall) == 0) {
            div(
              class = "cm-surface",
              h4(class = "mb-2", tr("coding.all_done_title", lang)),
              p(class = "text-muted mb-3", tr("coding.no_more_cases", lang)),
              actionButton(ns("back_to_select_btn"), tr("coding.btn_back_config", lang), class = "btn btn-primary")
            )
          } else {
            fluidRow(
              column(
                width = 8,
                div(
                  class = "cm-surface",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-2",
                    h5(class = "mb-0", tr("coding.text_content", lang)),
                    tags$span(class = "text-muted small font-monospace", tr("common.case_number", lang, as.integer(fall$text_id)))
                  ),
                  hr(),
                  div(class = "cm-case-text", fall$text),
                  hr(),
                  div(
                    class = "d-flex flex-wrap justify-content-between align-items-center text-muted small gap-2",
                    span(paste(tr("common.case_number", lang, as.integer(fall$text_id)), "\u00b7", tr("coding.source_info", lang), ifelse(is.null(fall$text_kurzreferenz) || is.na(fall$text_kurzreferenz), tr("common.none", lang), fall$text_kurzreferenz))),
                    span(tr("coding.title", lang))
                  )
                )
              ),
              column(
                width = 4,
                div(
                  class = "cm-surface",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-2",
                    h5(class = "mb-0", tr("coding.ratings_header", lang)),
                    actionButton(ns("btn_beispiele"), tr("coding.btn_show_examples", lang), icon = icon("book-open"), class = "btn btn-sm btn-outline-primary")
                  ),
                  hr(),
                  sliderInput(ns("bewertung_slider"), tr("coding.slider_label", lang, kat_lbl, max_bewertung_wert()),
                              min = 0, max = max_bewertung_wert(), value = slider_val, step = 1),
                  div(
                    class = "cm-surface-subtle mt-3 mb-3",
                    tags$div(class = "small fw-semibold text-muted mb-1", tr("examples.definition_label", lang)),
                    p(class = "mb-0 fw-semibold", style = "font-size: 0.92rem;", textOutput(ns("wert_definition")))
                  ),
                  div(
                    class = "cm-surface-subtle",
                    tags$div(class = "small fw-semibold text-muted mb-1", tr("examples.general_def", lang)),
                    p(class = "mb-0 text-muted", style = "font-size: 0.88rem;", textOutput(ns("beschreibung_text_ui")))
                  )
                )
              )
            )
          },

          if (!is.null(fall) && nrow(fall) > 0) {
            fluidRow(
              column(width = 4, actionButton(ns("save_next_btn"), tr("coding.btn_save_and_next", lang), icon = icon("check"), class = "btn btn-primary w-100")),
              column(width = 3, actionButton(ns("back_to_select_btn"), tr("coding.btn_back_config", lang), class = "btn btn-outline-secondary w-100"))
            )
          }
        )
      }
    })

    output$konfiguration_status_ui <- renderUI({
      if (!is.null(trigger_parent) && is.function(trigger_parent)) trigger_parent()
      modus_aktiv()
      lang <- get_lang()
      u_info <- get_user_info()
      permitted_scales <- get_permitted_kategorien_liste()
      has_scale_perms <- (length(permitted_scales) > 0)

      if (!has_scale_perms) {
        return(tagList(
          div(class = "alert alert-warning py-3",
              icon("lock", class = "me-2 text-warning fs-5"),
              tags$strong(paste0(tr("coding.no_scales_unlocked", lang), ": ")),
              tr("coding.no_permission_desc", lang)
          ),
          actionButton(ns("start_kodierung_disabled_btn"),
                       tr("coding.no_scales_unlocked", lang),
                       icon = icon("ban"), class = "btn-secondary w-100 btn-lg", disabled = "disabled")
        ))
      }

      req(input$gewaehlte_kategorie)
      f_modus <- ifelse(is.null(input$fall_filter_modus), "alle", input$fall_filter_modus)
      t_quelle_id <- if (f_modus == "quelle" && !is.null(input$gewaehlte_text_quelle)) as.integer(input$gewaehlte_text_quelle) else NULL

      offene_anzahl <- zaehle_offene_faelle(input$gewaehlte_kategorie, u_info$id, f_modus, t_quelle_id)
      erlaubte_kodier_sids <- ermittle_erlaubte_kodier_quellen(u_info$id)
      has_coding_source_perms <- is.null(erlaubte_kodier_sids) || length(erlaubte_kodier_sids) > 0

      has_sources <- tryCatch({
        dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM sources")$n > 0
      }, error = function(e) FALSE)

      can_start <- (offene_anzahl > 0 && has_sources && has_coding_source_perms && !is.null(input$gewaehlte_quelle) && input$gewaehlte_quelle != "")
      k_spalten <- get_kategorie_spalten()
      kat_lbl <- if (length(k_spalten) > 0) names(k_spalten)[which(k_spalten == input$gewaehlte_kategorie)[1]] else NULL
      if (is.na(kat_lbl) || is.null(kat_lbl) || kat_lbl == "") kat_lbl <- paste0("ID #", input$gewaehlte_kategorie)

      tagList(
        if (offene_anzahl > 0) {
          div(class = "alert alert-info py-2",
              icon("info-circle", class = "me-1"),
              tr("coding.pool_status_available", lang, kat_lbl, as.integer(offene_anzahl), as.integer(offene_anzahl))
          )
        } else {
          div(class = "alert alert-warning py-2",
              icon("exclamation-triangle", class = "me-1"),
              tr("coding.err_no_cases", lang)
          )
        },
        if (can_start) {
          actionButton(ns("start_kodierung_btn"), tr("coding.btn_start_coding", lang),
                       icon = icon("play"), class = "btn-success w-100 btn-lg")
        } else {
          disabled_reason <- if (!has_sources) {
            tr("coding.no_coding_sources_err", lang)
          } else if (!has_coding_source_perms || is.null(input$gewaehlte_quelle) || input$gewaehlte_quelle == "") {
            tr("coding.no_coding_sources_err", lang)
          } else {
            tr("coding.err_no_cases", lang)
          }
          actionButton(ns("start_kodierung_disabled_btn"),
                       disabled_reason,
                       icon = icon("ban"), class = "btn-secondary w-100 btn-lg", disabled = "disabled")
        },
        actionButton(ns("btn_beispiele_config"), paste0("\U0001f4a1 ", tr("coding.btn_show_examples", lang)), class = "btn-outline-info w-100 mt-2")
      )
    })

    output$wert_definition <- renderText({
      lang <- get_lang()
      req(input$bewertung_slider, input$gewaehlte_kategorie)
      ziel_spalte <- paste0("def_n_", as.integer(input$bewertung_slider))

      res <- tryCatch({
        dbGetQuery(db_con, sprintf("SELECT %s FROM scale_definitions WHERE scale_id = ?", ziel_spalte), params = list(as.integer(input$gewaehlte_kategorie)))
      }, error = function(e) data.frame())

      if (nrow(res) > 0 && !is.na(res[[1]]) && res[[1]] != "") {
        as.character(res[[1]])
      } else {
        tr("common.no_description", lang)
      }
    })

    output$beschreibung_text_ui <- renderText({
      lang <- get_lang()
      val <- beschreibung_text()
      if (is.null(val) || is.na(val) || !nzchar(trimws(val))) tr("common.no_description", lang) else val
    })

    observeEvent(input$gewaehlte_kategorie, {
      req(input$gewaehlte_kategorie)
      meta_res <- tryCatch({
        dbGetQuery(db_con, "SELECT max_value, def_desc FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(input$gewaehlte_kategorie)))
      }, error = function(e) data.frame())

      if (nrow(meta_res) > 0 && !is.na(meta_res$max_value[1])) {
        m_val <- as.integer(meta_res$max_value[1])
        max_bewertung_wert(m_val)
        desc <- if (is.na(meta_res$def_desc[1]) || meta_res$def_desc[1] == "") "" else as.character(meta_res$def_desc[1])
        beschreibung_text(desc)
        updateSliderInput(session, "bewertung_slider", max = m_val, value = 0)
      }
    })

    observeEvent(input$start_kodierung_btn, {
      req(input$gewaehlte_kategorie, input$gewaehlte_quelle)

      f_modus <- ifelse(is.null(input$fall_filter_modus), "alle", input$fall_filter_modus)
      t_quelle_id <- if (f_modus == "quelle" && !is.null(input$gewaehlte_text_quelle)) as.integer(input$gewaehlte_text_quelle) else NULL

      aktiver_filter_modus(f_modus)
      aktive_text_quelle_id(t_quelle_id)

      if (!is.null(t_quelle_id)) {
        t_q_df <- dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz, Lizenz FROM sources WHERE source_id = ?", params = list(t_quelle_id))
        if (nrow(t_q_df) > 0) aktive_text_quelle_info(t_q_df[1, ])
      } else {
        aktive_text_quelle_info(NULL)
      }

      kodier_quelle_id <- as.integer(input$gewaehlte_quelle)
      q_df <- dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz, Lizenz FROM sources WHERE source_id = ?", params = list(kodier_quelle_id))
      if (nrow(q_df) > 0) {
        aktive_quelle_info(q_df[1, ])
      }

      meta_res <- tryCatch({
        dbGetQuery(db_con, "SELECT max_value, def_desc FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(input$gewaehlte_kategorie)))
      }, error = function(e) data.frame())

      if (nrow(meta_res) > 0 && !is.na(meta_res$max_value[1])) {
        m_val <- as.integer(meta_res$max_value[1])
        max_bewertung_wert(m_val)
        desc <- if (is.na(meta_res$def_desc[1]) || meta_res$def_desc[1] == "") "" else as.character(meta_res$def_desc[1])
        beschreibung_text(desc)
        updateSliderInput(session, "bewertung_slider", max = m_val, value = 0)
      }

      u_info <- get_user_info()
      fall_daten <- ziehe_unbewerteten_fall(input$gewaehlte_kategorie, u_info$id, f_modus, t_quelle_id)
      aktueller_fall(fall_daten)
      modus_aktiv(TRUE)
    })

    observeEvent(input$save_next_btn, {
      lang <- get_lang()
      fall <- aktueller_fall()
      req(fall, input$bewertung_slider, input$gewaehlte_quelle)

      kodier_quelle_id <- as.integer(input$gewaehlte_quelle)
      u_info <- get_user_info()

      dbExecute(db_con, "
        INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id)
        VALUES (?, ?, ?, ?, 'kodierung', ?);
      ", params = list(u_info$id, fall$text_id, input$gewaehlte_kategorie, as.integer(input$bewertung_slider), kodier_quelle_id))

      trigger_parent(trigger_parent() + 1)

      fall_daten <- ziehe_unbewerteten_fall(input$gewaehlte_kategorie, u_info$id, aktiver_filter_modus(), aktive_text_quelle_id())
      aktueller_fall(fall_daten)
      updateSliderInput(session, "bewertung_slider", value = 0)
      showNotification(tr("coding.save_success", lang, as.integer(fall$text_id)), type = "message")
    })

    observeEvent(input$back_to_select_btn, {
      modus_aktiv(FALSE)
      aktueller_fall(NULL)
      aktive_quelle_info(NULL)
      aktive_text_quelle_info(NULL)
    })

    beispiel_offset <- reactiveVal(0)

    oeffne_beispiel_modal <- function() {
      lang <- get_lang()
      k_liste <- get_kategorien_liste()
      kat_init <- if (!is.null(input$gewaehlte_kategorie) && input$gewaehlte_kategorie %in% k_liste) {
        input$gewaehlte_kategorie
      } else if (length(k_liste) > 0) {
        k_liste[1]
      } else {
        ""
      }
      showModal(modalDialog(
        title = tags$div(tags$strong(tr("examples.modal_title", lang)),
                         tags$br(),
                         tags$small(class = "text-muted", tr("examples.example_label", lang))),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("btn_anderes_beispiel"), tr("coding.btn_skip", lang), icon = icon("random"), class = "btn-primary"),
          modalButton(tr("examples.btn_close", lang))
        ),
        div(
          fluidRow(
            column(width = 6,
                   selectInput(ns("modal_beispiel_kategorie"), tr("admin.filter_scale_label", lang),
                               choices = k_liste, selected = kat_init)
            ),
            column(width = 6,
                   uiOutput(ns("modal_stufen_selector"))
            )
          ),
          hr(),
          uiOutput(ns("modal_beispiel_content"))
        )
      ))
    }

    observeEvent(input$btn_beispiele_config, { oeffne_beispiel_modal() })
    observeEvent(input$btn_beispiele, { oeffne_beispiel_modal() })
    observeEvent(input$btn_anderes_beispiel, { beispiel_offset(beispiel_offset() + 1) })

    output$modal_stufen_selector <- renderUI({
      lang <- get_lang()
      req(input$modal_beispiel_kategorie)
      kat <- input$modal_beispiel_kategorie
      max_v <- 3

      df_m <- tryCatch({
        dbGetQuery(db_con, "SELECT max_value FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(kat)))
      }, error = function(e) data.frame())

      if (nrow(df_m) > 0 && !is.na(df_m$max_value[1])) {
        max_v <- as.integer(df_m$max_value[1])
      }

      lvl_labels <- vapply(0:max_v, function(x) tr("common.level_n", lang, as.integer(x)), character(1))
      selectInput(ns("modal_gewaehlte_stufe"), tr("admin.col_rating_value", lang),
                  choices = setNames(0:max_v, lvl_labels), selected = 0)
    })

    output$modal_beispiel_content <- renderUI({
      lang <- get_lang()
      req(input$modal_beispiel_kategorie, input$modal_gewaehlte_stufe)
      beispiel_offset()
      kat <- input$modal_beispiel_kategorie
      st <- as.integer(input$modal_gewaehlte_stufe)
      rand_fn <- get_rand_fn()

      cnt_df <- dbGetQuery(db_con, "
        SELECT COUNT(DISTINCT t.text_id) AS anz
        FROM texts t
        JOIN user_ratings r ON t.text_id = r.text_id
        WHERE r.typ IN ('kodierung', 'gold_standard') AND r.kategorie = ? AND r.bewertung = ?
      ", params = list(kat, st))
      n_verfuegbar <- if (nrow(cnt_df) > 0) as.integer(cnt_df$anz[1]) else 0

      if (n_verfuegbar == 0) {
        div(class = "alert alert-warning text-center",
            h5(tr("examples.no_example_text", lang)),
            p(tr("admin.no_codings_found", lang))
        )
      } else {
        bsp <- dbGetQuery(db_con, sprintf("
          SELECT DISTINCT t.text_id, t.text, t.source_id, r.typ AS bewertung_typ,
                 s.Kurzreferenz AS text_kurzreferenz,
                 s.Referenz AS text_referenz
          FROM texts t
          JOIN user_ratings r ON t.text_id = r.text_id
          LEFT JOIN sources s ON t.source_id = s.source_id
          WHERE r.typ IN ('kodierung', 'gold_standard') AND r.kategorie = ? AND r.bewertung = ?
          ORDER BY %s LIMIT 1
        ", rand_fn), params = list(kat, st))

        is_gold <- !is.null(bsp$bewertung_typ) && bsp$bewertung_typ[1] == "gold_standard"
        typ_label <- if (is_gold) tr("admin.filter_type_gold", lang) else tr("admin.filter_type_kodierung", lang)
        typ_class <- if (is_gold) "badge bg-warning text-dark me-2" else "badge bg-primary me-2"

        tagList(
          div(class = "d-flex justify-content-between align-items-center mb-2",
              div(
                tags$span(class = "badge bg-success p-2 me-2", paste0(tr("common.level_n", lang, st), " \u00b7 ", n_verfuegbar)),
                tags$span(class = paste(typ_class, "p-2"), typ_label)
              ),
              tags$span(class = "text-muted small", tr("common.case_number", lang, as.integer(bsp$text_id[1])))
          ),
          div(
            class = "cm-case-text",
            style = "max-height: 260px;",
            bsp$text[1]
          ),
          br(),
          div(class = "card bg-light p-2",
              tags$strong(tr("coding.source_info", lang)),
              if (!is.null(bsp$source_id) && !is.na(bsp$source_id[1])) {
                tags$span(class = "text-primary", paste0("[#", bsp$source_id[1], "] ", bsp$text_kurzreferenz[1], " - ", bsp$text_referenz[1]))
              } else {
                tags$span(class = "text-muted", tr("admin.texts.no_source_assigned", lang))
              }
          )
        )
      }
    })
  })
}
