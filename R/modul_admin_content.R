# Note: This file was generated with the assistance of AI.

# ==============================================================================
# ADMIN MODULE SERVER HELPERS: TEXTS, SOURCES & SCALES (TABS 2-4)
# ==============================================================================
# Internal subtab server registrars extracted from adminServer() to keep each
# function compact and avoid quadratic AST instrumentation overhead under covr.
# ==============================================================================

#' Register Admin Tab 2 Case Texts & Source Assignment Server Logic
#'
#' Registers reactive outputs and observers for the paginated case text table
#' and source assignment modal inside the admin module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_texts_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  get_lang <- ctx$get_lang
  is_admin_authorized <- if (is.function(ctx$is_admin_authorized)) ctx$is_admin_authorized else function() TRUE
  texts_table_trigger <- ctx$texts_table_trigger
  current_page_texts <- ctx$current_page_texts
  text_to_assign_source <- reactiveVal(NULL)

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
          style = "font-family: var(--cm-font-mono); font-weight: bold;",
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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

  invisible(NULL)
}

#' Register Admin Tab 3 Bibliographic Source Management Server Logic
#'
#' Registers reactive outputs and observers for listing, creating, editing,
#' and deleting bibliographic sources inside the admin module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_sources_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  get_lang <- ctx$get_lang
  is_admin_authorized <- if (is.function(ctx$is_admin_authorized)) ctx$is_admin_authorized else function() TRUE
  user_table_trigger <- ctx$user_table_trigger
  texts_table_trigger <- ctx$texts_table_trigger
  sources_trigger <- ctx$sources_trigger
  source_to_edit   <- reactiveVal(NULL)
  source_to_delete <- reactiveVal(NULL)

  get_lizenz_optionen <- function(lang) {
    get_source_license_choices(lang)
  }

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
        style = "font-size: 0.85em; font-family: var(--cm-font-mono); padding: 4px 8px;",
        s_liz
      )

      tags$tr(
        tags$td(
          style = "vertical-align: middle; width: 8%; font-family: var(--cm-font-mono); font-weight: bold; color: #495057;",
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
    if (!is_admin_authorized()) return()
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
        tags$p(class = "text-muted small mb-2", icon("copyright", class = "me-1"), tr("admin.sources.copyright_notice", lang)),
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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
        tags$p(class = "text-muted small mb-2", icon("copyright", class = "me-1"), tr("admin.sources.copyright_notice", lang)),
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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

  invisible(NULL)
}

#' Register Admin Tab 4 Coding Scale Management Server Logic
#'
#' Registers reactive outputs and observers for viewing, creating, and editing
#' ordinal coding scale definitions inside the admin module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_scales_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  scales_trigger <- ctx$scales_trigger
  get_lang <- ctx$get_lang
  is_admin_authorized <- if (is.function(ctx$is_admin_authorized)) ctx$is_admin_authorized else function() TRUE
  scales_table_trigger <- ctx$scales_table_trigger
  scale_to_edit <- reactiveVal(NULL)

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
          style = "vertical-align: middle; width: 7%; font-family: var(--cm-font-mono); font-weight: bold; color: #495057;",
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
          style = "vertical-align: middle; font-weight: 600; width: 22%; color: var(--cm-ink);",
          s_name
        ),
        tags$td(
          style = "vertical-align: middle; width: 12%;",
          tags$span(
            class = "badge bg-secondary",
            style = "font-size: 0.85em; font-family: var(--cm-font-mono);",
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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
    if (!is_admin_authorized()) return()
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

  invisible(NULL)
}
