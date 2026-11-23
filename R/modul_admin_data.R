# ==============================================================================
# ADMIN MODULE SERVER HELPERS: DATA IMPORT, CSV EXPORTS & MASTER DATASET (TAB 7)
# ==============================================================================
# Internal subtab server registrars extracted from adminServer() to keep each
# function compact and avoid quadratic AST instrumentation overhead under covr.
# ==============================================================================

#' Register Admin Tab 7 CSV Import, Schema Validation & Table Download Logic
#'
#' Registers reactive outputs, file upload observers, schema confirmation
#' modals, raw table CSV downloads, and empty CSV template download handlers.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_import_export_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  scales_trigger <- ctx$scales_trigger
  get_lang <- ctx$get_lang
  user_table_trigger <- ctx$user_table_trigger
  texts_table_trigger <- ctx$texts_table_trigger
  sources_trigger <- ctx$sources_trigger
  scales_table_trigger <- ctx$scales_table_trigger
  pending_import <- ctx$pending_import

  clean_columns <- function(df) {
    clean_table_columns(df)
  }

  get_table_pk <- function(tbl) {
    get_table_pk_definition(tbl)
  }


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

  invisible(NULL)
}

#' Register Admin Tab 7 Configurable Master Dataset Export Server Logic
#'
#' Registers the three paginated filter modals (scales, sources, users),
#' filter summary badges, reset observer, and consolidated master dataset
#' CSV download handler.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_master_export_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  scales_trigger <- ctx$scales_trigger
  get_lang <- ctx$get_lang
  user_table_trigger <- ctx$user_table_trigger
  sources_trigger <- ctx$sources_trigger
  scales_table_trigger <- ctx$scales_table_trigger

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
        notice_msg <- tr("admin.export_err_missing_group", lang)
        showNotification(notice_msg, type = "warning")
        write.csv2(
          data.frame(notice = notice_msg, stringsAsFactors = FALSE),
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
        return(invisible(NULL))
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

  invisible(NULL)
}
