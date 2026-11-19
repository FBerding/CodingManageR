#' @title User Interface Components and Modal Builders for Coding Portal
#' @description Modular Shiny UI constructors including standardized KPI cards,
#' metric status badges, scale example dialogs, and pagination controls.
#' @author AI Studio Coding Engine
#' @keywords shiny ui components widgets
NULL

# shiny is imported via NAMESPACE

#' Render Standardized KPI Stat Card
#'
#' @description Generates a consistent Bootstrap card widget for dashboard and administrative KPIs.
#'
#' @param title Character string describing the metric (e.g. "Echte Kodierungen insgesamt").
#' @param value Numeric or character representation of the primary metric value.
#' @param subtitle Optional character string providing extra context or time window.
#' @param bg_class Character string defining the Bootstrap background color class (e.g. \code{"bg-dark"}, \code{"bg-success"}, \code{"bg-info"}).
#' @param width Integer column width according to Bootstrap 12-column grid. Default is 4.
#'
#' @return A Shiny \code{column} UI tag containing the styled stat card.
#'
#' @export
render_kpi_card_ui <- function(title, value, subtitle = NULL, bg_class = "bg-primary", width = 4) {
  column(
    width = width,
    div(
      class = paste("card text-white mb-3 shadow-sm", bg_class),
      div(
        class = "card-body text-center p-3",
        h3(class = "card-title mb-1 font-monospace fw-bold", as.character(value)),
        p(class = "card-text small mb-0 opacity-75", title),
        if (!is.null(subtitle) && subtitle != "") {
          div(class = "text-white-50 text-xs mt-1", subtitle)
        } else {
          NULL
        }
      )
    )
  )
}

#' Render Metric Status Badge
#'
#' @description Generates a color-coded Bootstrap/HTML badge based on reliability thresholds.
#'
#' @param val Numeric metric value or character representation.
#' @param is_percentage Logical; if TRUE, interprets values on a 0-100 scale, else 0.0-1.0.
#' @param thresh_high Numeric upper threshold for excellent/green styling. Default 0.80 (or 80.0).
#' @param thresh_mid Numeric middle threshold for moderate/warning styling. Default 0.60 (or 60.0).
#'
#' @return A Shiny \code{tags$span} UI element with formatted badge markup.
#'
#' @export
render_metric_badge_ui <- function(val, is_percentage = FALSE, thresh_high = NULL, thresh_mid = NULL) {
  if (is.null(val) || is.na(val) || val == "" || val == "NA") {
    return(tags$span(class = "text-muted", "—"))
  }

  num <- suppressWarnings(as.numeric(val))
  if (is.na(num)) return(tags$span(class = "text-muted", "—"))

  t_high <- if (!is.null(thresh_high)) thresh_high else if (is_percentage) 80.0 else 0.80
  t_mid  <- if (!is.null(thresh_mid)) thresh_mid else if (is_percentage) 60.0 else 0.60

  badge_cls <- "badge bg-success"
  if (num < t_mid) {
    badge_cls <- "badge bg-danger"
  } else if (num < t_high) {
    badge_cls <- "badge bg-warning text-dark"
  }

  disp_text <- if (is_percentage) {
    paste0(formatC(num, digits = 1, format = "f"), " %")
  } else {
    formatC(num, digits = 3, format = "f")
  }

  tags$span(class = paste(badge_cls, "font-monospace"), disp_text)
}

#' Build Modal Dialog for Coding Scale Definitions and Stage Examples
#'
#' @description Constructs a Shiny modalDialog displaying full scale descriptions,
#' stage boundaries, and concrete text examples for all levels 0 through max_value.
#'
#' @param scale_df A 1-row data frame from \code{scale_definitions}.
#' @param active_level Optional integer highlighting a specific score level (e.g. 0..4).
#'
#' @return A Shiny \code{modalDialog} object ready to be passed to \code{showModal()}.
#'
#' @export
build_scale_examples_modal <- function(scale_df, active_level = NULL, lang = "en") {
  if (is.null(scale_df) || nrow(scale_df) == 0) {
    return(modalDialog(
      title = tr("examples.modal_title", lang),
      p(class = "text-muted", tr("admin.scales.empty", lang)),
      easyClose = TRUE,
      footer = modalButton(tr("common.close", lang))
    ))
  }

  s_name <- if (!is.null(scale_df$name) && !is.na(scale_df$name)) scale_df$name[1] else "Skala"
  s_label <- if (!is.null(scale_df$short_label) && !is.na(scale_df$short_label)) scale_df$short_label[1] else ""
  s_desc <- if (!is.null(scale_df$def_desc) && !is.na(scale_df$def_desc) && nzchar(trimws(scale_df$def_desc[1]))) scale_df$def_desc[1] else tr("common.no_description", lang)
  max_v <- if (!is.null(scale_df$max_value) && !is.na(scale_df$max_value)) as.integer(scale_df$max_value[1]) else 2L

  level_blocks <- lapply(0:max_v, function(lvl) {
    col_name <- paste0("bsp_", lvl)
    ex_text <- if (col_name %in% colnames(scale_df)) scale_df[[col_name]][1] else NULL
    if (is.null(ex_text) || is.na(ex_text) || ex_text == "") ex_text <- tr("examples.no_example_text", lang)

    is_highlighted <- !is.null(active_level) && as.integer(active_level) == lvl
    border_style <- if (is_highlighted) "border-left: 4px solid #059669; background-color: #f0fdf4;" else "border-left: 3px solid #cbd5e1; background-color: #f8fafc;"

    div(
      class = "p-3 mb-2 rounded border shadow-xs",
      style = border_style,
      div(
        class = "d-flex justify-content-between align-items-center mb-1",
        span(class = "fw-bold", tr("examples.level_heading", lang, lvl)),
        if (is_highlighted) span(class = "badge bg-success", tr("examples.active_selection", lang)) else NULL
      ),
      p(class = "text-secondary small mb-0 font-monospace", ex_text)
    )
  })

  modalDialog(
    title = tagList(icon("book-open", class = "me-2 text-primary"), paste0(s_name, " (", s_label, ")")),
    div(
      class = "mb-3 p-3 bg-light rounded border",
      h6(class = "fw-bold text-dark", tr("examples.general_def", lang)),
      p(class = "text-muted small mb-0", s_desc)
    ),
    h6(class = "fw-bold mb-2 text-dark", tr("examples.modal_title", lang)),
    do.call(div, level_blocks),
    size = "l",
    easyClose = TRUE,
    footer = modalButton(tr("examples.btn_close", lang))
  )
}

#' Build Modal Dialog for Editing a Case Coding Rating Value
#'
#' @description Constructs a localized Shiny \code{modalDialog} presenting read-only context
#' for an existing case coding (rating ID, case text, scale definition, coder, rating type,
#' and timestamp) alongside radio choices \code{0..max_value} enriched with stage descriptions
#' (\code{def_n_0..def_n_5}) so an administrator can adjust only the numeric rating score.
#'
#' @param ns A Shiny namespace function (\code{session$ns}) from the calling module server.
#' @param rating_row A 1-row \code{data.frame} returned by \code{\link{get_coding_detail_db}}.
#' @param lang Character language code (\code{"de"} or \code{"en"}) passed to \code{tr()}.
#'
#' @return A Shiny \code{modalDialog} object ready to be passed to \code{showModal()}.
#'
#' @examples
#' \dontrun{
#' detail <- get_coding_detail_db(con, 201)
#' showModal(build_edit_coding_value_modal(session$ns, detail, lang = "de"))
#' }
#' @export
build_edit_coding_value_modal <- function(ns, rating_row, lang = "en") {
  if (is.null(rating_row) || nrow(rating_row) == 0) {
    return(modalDialog(
      title = tr("admin.modal_edit_coding_title", lang),
      p(class = "text-muted", tr("admin.no_codings_found", lang)),
      easyClose = TRUE,
      footer = modalButton(tr("common.close", lang))
    ))
  }

  r_id      <- as.integer(rating_row$rating_id[1])
  t_id      <- as.integer(rating_row$text_id[1])
  cur_val   <- as.integer(rating_row$bewertung[1])
  coder     <- as.character(rating_row$username[1])
  r_type    <- as.character(rating_row$typ[1])
  tstamp    <- if (!is.null(rating_row$zeitstempel) && !is.na(rating_row$zeitstempel[1])) as.character(rating_row$zeitstempel[1]) else "—"
  case_txt  <- if (!is.null(rating_row$case_text) && !is.na(rating_row$case_text[1])) as.character(rating_row$case_text[1]) else ""
  s_name    <- if (!is.null(rating_row$scale_name) && !is.na(rating_row$scale_name[1])) as.character(rating_row$scale_name[1]) else as.character(rating_row$kategorie[1])
  s_short   <- if (!is.null(rating_row$scale_short_label) && !is.na(rating_row$scale_short_label[1])) as.character(rating_row$scale_short_label[1]) else ""
  s_desc    <- if (!is.null(rating_row$scale_def_desc) && !is.na(rating_row$scale_def_desc[1])) as.character(rating_row$scale_def_desc[1]) else ""
  max_v     <- if (!is.null(rating_row$scale_max_value) && !is.na(rating_row$scale_max_value[1])) as.integer(rating_row$scale_max_value[1]) else 5L
  if (is.na(max_v) || max_v < 0L) max_v <- 5L

  type_label <- switch(
    r_type,
    "kodierung"     = tr("admin.filter_type_kodierung", lang),
    "gold_standard" = tr("admin.filter_type_gold", lang),
    "schulung"      = tr("admin.filter_type_schulung", lang),
    r_type
  )

  levels_seq <- 0:max_v
  choice_names <- lapply(levels_seq, function(lvl) {
    col_n <- paste0("def_n_", lvl)
    lvl_desc <- if (col_n %in% colnames(rating_row) && !is.na(rating_row[[col_n]][1]) && nzchar(trimws(rating_row[[col_n]][1]))) {
      as.character(rating_row[[col_n]][1])
    } else {
      "—"
    }
    is_cur <- (!is.na(cur_val) && cur_val == lvl)
    tags$div(
      class = "d-flex flex-column py-1",
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$span(class = "fw-bold font-monospace text-dark", paste0(tr("admin.col_rating_value", lang), " ", lvl)),
        if (is_cur) tags$span(class = "badge bg-secondary", tr("admin.modal_current_value", lang)) else NULL
      ),
      tags$span(class = "small text-muted", lvl_desc)
    )
  })

  modalDialog(
    title = tagList(
      icon("edit", class = "me-2 text-primary"),
      paste0(tr("admin.modal_edit_coding_title", lang), " (#", r_id, ")")
    ),
    size = "l",
    easyClose = TRUE,
    div(
      class = "mb-3 p-3 bg-light rounded border",
      div(
        class = "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2 pb-2 border-bottom",
        tags$span(class = "small fw-bold text-uppercase text-muted", tr("admin.modal_case_context", lang)),
        tags$div(
          class = "small font-monospace text-muted",
          paste0("Rating #", r_id, " · ", tr("common.case_number", lang, t_id), " · ", coder, " · ", type_label, " · ", tstamp)
        )
      ),
      div(
        class = "mb-2",
        tags$strong(class = "text-dark small", paste0(s_name, if (nzchar(s_short) && s_short != s_name) paste0(" (", s_short, ")") else "")),
        if (nzchar(s_desc)) p(class = "text-muted small mb-0 mt-1", s_desc) else NULL
      ),
      div(
        class = "p-2 bg-white rounded border small text-dark",
        style = "max-height: 150px; overflow-y: auto; white-space: pre-wrap;",
        case_txt
      )
    ),
    div(
      class = "d-flex align-items-center justify-content-between mb-2 p-2 rounded border bg-white",
      tags$span(class = "small fw-bold text-muted", tr("admin.modal_current_value", lang)),
      tags$span(class = "badge bg-dark font-monospace fs-6", as.character(cur_val))
    ),
    radioButtons(
      inputId = ns("edit_coding_new_value"),
      label = tr("admin.modal_new_value", lang),
      choiceNames = choice_names,
      choiceValues = as.character(levels_seq),
      selected = as.character(cur_val),
      width = "100%"
    ),
    footer = tagList(
      modalButton(tr("common.cancel", lang)),
      actionButton(
        ns("btn_confirm_edit_coding"),
        tr("admin.modal_save_rating", lang),
        icon = icon("save"),
        class = "btn btn-primary"
      )
    )
  )
}

#' Build Paginated Table UI for Admin Coding Search and Correction
#'
#' @description Renders a responsive Bootstrap table displaying filtered coding records
#' (\code{user_ratings}) with Rating ID, case text preview, scale label, coder, rating type,
#' current score on scale, timestamp, and an edit action button triggering
#' \code{trigger_edit_coding_rating}.
#'
#' @param ns A Shiny namespace function (\code{session$ns}) from the calling module server.
#' @param page_df A sliced \code{data.frame} (up to 25 rows) returned by \code{\link{search_case_codings_db}}.
#' @param lang Character language code (\code{"de"} or \code{"en"}) passed to \code{tr()}.
#'
#' @return A Shiny UI tag containing either an empty-state alert or the responsive HTML table.
#'
#' @examples
#' \dontrun{
#' df <- search_case_codings_db(con)
#' build_coding_search_table_ui(session$ns, df[seq_len(min(25, nrow(df))), ], lang = "en")
#' }
#' @export
build_coding_search_table_ui <- function(ns, page_df, lang = "en") {
  if (is.null(page_df) || nrow(page_df) == 0) {
    return(div(
      class = "alert alert-info text-center py-4 mb-0",
      icon("search", class = "me-2"),
      tr("admin.no_codings_found", lang)
    ))
  }

  rows <- lapply(seq_len(nrow(page_df)), function(i) {
    r <- page_df[i, ]
    r_id <- as.integer(r$rating_id)
    t_id <- as.integer(r$text_id)
    raw_txt <- if (!is.na(r$case_text)) as.character(r$case_text) else ""
    preview <- if (nchar(raw_txt) > 110) paste0(substr(raw_txt, 1, 110), "...") else raw_txt
    s_name <- if (!is.na(r$scale_name)) as.character(r$scale_name) else as.character(r$kategorie)
    s_short <- if (!is.na(r$scale_short_label)) as.character(r$scale_short_label) else ""
    max_v <- if (!is.na(r$scale_max_value)) as.integer(r$scale_max_value) else 5L
    score_val <- as.integer(r$bewertung)
    coder <- if (!is.na(r$username)) as.character(r$username) else "—"
    r_type <- as.character(r$typ)
    tstamp <- if (!is.na(r$zeitstempel)) substr(as.character(r$zeitstempel), 1, 16) else "—"

    type_badge <- switch(
      r_type,
      "kodierung"     = tags$span(class = "badge bg-primary", tr("admin.filter_type_kodierung", lang)),
      "gold_standard" = tags$span(class = "badge bg-warning text-dark", tr("admin.filter_type_gold", lang)),
      "schulung"      = tags$span(class = "badge bg-info text-dark", tr("admin.filter_type_schulung", lang)),
      tags$span(class = "badge bg-secondary", r_type)
    )

    tags$tr(
      tags$td(class = "font-monospace text-muted small", paste0("#", r_id)),
      tags$td(
        tags$div(class = "fw-bold font-monospace small text-dark", tr("common.case_number", lang, t_id)),
        tags$div(class = "small text-muted", preview)
      ),
      tags$td(
        tags$div(class = "fw-semibold small text-dark", s_name),
        if (nzchar(s_short) && s_short != s_name) tags$div(class = "small font-monospace text-muted", s_short) else NULL
      ),
      tags$td(class = "small fw-semibold", coder),
      tags$td(class = "text-center", type_badge),
      tags$td(
        class = "text-center font-monospace",
        tags$span(class = "badge bg-dark fs-6", paste0(score_val, " / ", max_v))
      ),
      tags$td(class = "font-monospace small text-muted", tstamp),
      tags$td(
        class = "text-end",
        tags$button(
          type = "button",
          class = "btn btn-outline-primary btn-sm text-nowrap",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_edit_coding_rating"), r_id),
          tags$i(class = "fa fa-pen me-1"),
          tr("admin.btn_edit_rating", lang)
        )
      )
    )
  })

  tags$div(
    class = "table-responsive",
    tags$table(
      class = "table table-hover align-middle table-bordered bg-white mb-0",
      tags$thead(
        class = "table-light text-muted small text-uppercase",
        tags$tr(
          tags$th(style = "width: 90px;", tr("admin.col_rating_id", lang)),
          tags$th(tr("admin.col_case_text", lang)),
          tags$th(style = "width: 170px;", tr("admin.col_scale", lang)),
          tags$th(style = "width: 120px;", tr("admin.col_coder", lang)),
          tags$th(class = "text-center", style = "width: 130px;", tr("admin.col_type", lang)),
          tags$th(class = "text-center", style = "width: 110px;", tr("admin.col_rating_value", lang)),
          tags$th(style = "width: 145px;", tr("admin.col_timestamp", lang)),
          tags$th(class = "text-end", style = "width: 135px;", tr("common.actions", lang))
        )
      ),
      tags$tbody(rows)
    )
  )
}

#' Build Pagination Bar for Admin Coding Search Table
#'
#' @description Generates localized pagination controls (first, previous, direct page number
#' input, next, last) for the 25-row coding search table in \code{modul_admin.R}.
#'
#' @param ns A Shiny namespace function (\code{session$ns}) from the calling module server.
#' @param total_rows Integer total count of filtered coding rows.
#' @param cur_page Integer current 1-based page number.
#' @param tot_pages Integer total number of pages.
#' @param page_size Integer number of records per page. Default is 25.
#' @param lang Character language code (\code{"de"} or \code{"en"}) passed to \code{tr()}.
#'
#' @return A Shiny UI tag containing pagination controls, or \code{NULL} when \code{total_rows == 0}.
#'
#' @examples
#' \dontrun{
#' build_coding_search_pagination_ui(session$ns, total_rows = 48, cur_page = 1, tot_pages = 2, lang = "de")
#' }
#' @export
build_coding_search_pagination_ui <- function(ns, total_rows, cur_page, tot_pages, page_size = 25, lang = "en") {
  if (is.null(total_rows) || total_rows <= 0) return(NULL)

  start_idx <- (cur_page - 1L) * page_size + 1L
  end_idx <- min(total_rows, cur_page * page_size)

  div(
    class = "d-flex flex-wrap justify-content-between align-items-center gap-3 p-3 bg-light rounded border",
    div(
      class = "text-muted small",
      tags$strong(tr("common.showing_entries", lang, start_idx, end_idx, total_rows)),
      paste0(" (", tr("common.page_x_of_y", lang, cur_page, tot_pages), ")")
    ),
    div(
      class = "d-flex align-items-center gap-2",
      actionButton(ns("btn_cs_first"), "«", class = "btn-sm btn-outline-secondary", disabled = if (cur_page <= 1) "disabled" else NULL),
      actionButton(ns("btn_cs_prev"), paste0("‹ ", tr("common.previous", lang)), class = "btn-sm btn-outline-primary", disabled = if (cur_page <= 1) "disabled" else NULL),
      div(
        class = "d-flex align-items-center gap-1",
        tags$input(
          id = ns("input_cs_jump_page"),
          type = "number",
          class = "form-control form-control-sm text-center",
          style = "width: 65px; display: inline-block;",
          value = cur_page,
          min = 1,
          max = tot_pages,
          onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", ns("trigger_jump_cs_page"))
        ),
        span(class = "small text-muted", paste0("/ ", tot_pages))
      ),
      actionButton(ns("btn_cs_next"), paste0(tr("common.next", lang), " ›"), class = "btn-sm btn-outline-primary", disabled = if (cur_page >= tot_pages) "disabled" else NULL),
      actionButton(ns("btn_cs_last"), "»", class = "btn-sm btn-outline-secondary", disabled = if (cur_page >= tot_pages) "disabled" else NULL)
    )
  )
}

#' Format Integer with Locale-Aware Separators
#'
#' @description Formats an integer count using distinct thousands and decimal separators
#' matching the active UI language (\code{"de"} uses \code{"."}/\code{","};
#' \code{"en"} uses \code{","}/\code{"."}) so \code{prettyNum()} never emits warnings
#' about identical \code{big.mark} and \code{decimal.mark} characters.
#'
#' @param x Numeric or integer value to format.
#' @param lang Character language code (\code{"de"} or \code{"en"}). Default is \code{"en"}.
#'
#' @return A character string containing the formatted integer.
#'
#' @export
format_int_locale <- function(x, lang = "en") {
  if (is.null(x) || length(x) == 0) return("0")
  num <- suppressWarnings(as.numeric(x[1]))
  if (is.na(num)) return("0")
  is_de <- identical(tolower(as.character(lang[1])), "de")
  big_m <- if (is_de) "." else ","
  dec_m <- if (is_de) "," else "."
  format(round(num, 0), big.mark = big_m, decimal.mark = dec_m, trim = TRUE, scientific = FALSE)
}
