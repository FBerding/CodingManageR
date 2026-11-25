# Note: This file was generated with the assistance of AI.

# Dependencies (shiny, DBI, ggplot2) are imported via NAMESPACE

# ==============================================================================
# DASHBOARD MODULE UI
# ==============================================================================

#' Dashboard Module UI
#'
#' Creates the Shiny UI for the coder dashboard, displaying personal coding
#' progress, training reliability trends, open-case breakdowns, and an
#' interactive scale directory.
#'
#' @param id Character string specifying the Shiny module namespace identifier.
#' @param kategorie_spalten Optional character vector of scale column names
#'   retained for backward compatibility.
#' @param lang Character string specifying the initial UI language code
#'   (`"en"` or `"de"`).
#' @param selected_tab Character string selecting the initially active tab
#'   (`"tab_progress"` or `"tab_scales_overview"`).
#'
#' @return A `shiny.tag` (`fluidPage`) representing the dashboard interface.
#'
#' @examples
#' if (interactive()) {
#'   ui <- dashboardUI("mod_dashboard", lang = "en")
#' }
#'
#' @export
dashboardUI <- function(id, kategorie_spalten = NULL, lang = "en", selected_tab = "tab_progress") {
  ns <- NS(id)
  active_tab <- if (!is.null(selected_tab) && selected_tab %in% c("tab_progress", "tab_scales_overview")) selected_tab else "tab_progress"

  div(
    class = "cm-module-section",
    tabsetPanel(
      id = ns("dashboard_tabs"),
      selected = active_tab,
      type = "tabs",

      # TAB 1: LEARNING & WORK PROGRESS
      tabPanel(
        title = tagList(icon("chart-line"), " ", tr("dashboard.tab_progress", lang)),
        value = "tab_progress",
        div(
          class = "cm-page-intro",
          h3(tr("dashboard.personal_heading", lang)),
          p(tr("dashboard.subtitle", lang))
        ),

        # Separate KPI cards by rating mode/type
        fluidRow(
          uiOutput(ns("stat_card_total_kodierung")),
          uiOutput(ns("stat_card_total_schulung")),
          uiOutput(ns("stat_card_sessions"))
        ),

        # Performance across previous coding training sessions (coding_training)
        div(
          class = "cm-surface",
          div(
            class = "d-flex justify-content-between align-items-start mb-3 flex-wrap gap-3",
            div(
              h4(class = "mb-1", tr("dashboard.chart_card_title", lang)),
              p(class = "text-muted small mb-0", tr("dashboard.training_history_desc", lang))
            ),
            div(
              class = "d-flex gap-2 flex-wrap",
              div(
                style = "min-width: 220px;",
                selectInput(ns("dashboard_selected_kategorie"), tr("dashboard.category_filter_label", lang), choices = NULL)
              ),
              div(
                style = "min-width: 280px;",
                selectInput(
                  ns("dashboard_plot_metric"),
                  tr("dashboard.chart_metric_select_label", lang),
                  choices = get_reliability_metric_choices(lang, include_all_core = TRUE),
                  selected = "all_core"
                )
              )
            )
          ),
          uiOutput(ns("training_kpi_summary")),
          div(class = "my-3", plotOutput(ns("training_reliability_plot"), height = "360px")),
          h5(class = "mt-4 mb-2 text-secondary", icon("table", class = "me-1"), tr("dashboard.training_history_title", lang)),
          uiOutput(ns("training_performance_table"))
        ),

        # Breakdown by category, type, and remaining open cases
        div(
          class = "cm-surface",
          h4(class = "mb-1", tr("dashboard.category_breakdown_title", lang)),
          p(class = "text-muted small mb-3", tr("dashboard.subtitle", lang)),
          tableOutput(ns("kategorie_breakdown_table"))
        )
      ),

      # TAB 2: SCALE OVERVIEW & REQUIREMENTS
      tabPanel(
        title = tagList(icon("ruler-combined"), " ", tr("dashboard.tab_scales", lang)),
        value = "tab_scales_overview",
        div(
          class = "cm-surface",
          fluidRow(
            column(
              width = 8,
              h3(class = "mb-1", tr("dashboard.scales_title", lang)),
              p(class = "text-muted small mb-0", tr("dashboard.scales_desc", lang))
            ),
            column(
              width = 4,
              class = "text-end align-self-center",
              div(
                class = "text-muted small",
                uiOutput(ns("dashboard_scales_count"))
              )
            )
          ),
          hr(),

          # Quick search with ID and name filter plus reset button
          fluidRow(
            column(
              width = 7,
              div(
                class = "input-group mb-3",
                span(class = "input-group-text", icon("search")),
                textInput(ns("dashboard_scale_search"), label = NULL, placeholder = tr("dashboard.scales_search_placeholder", lang), width = "100%"),
                actionButton(ns("btn_reset_scale_search"), tr("common.reset", lang), icon = icon("times"), class = "btn btn-outline-secondary")
              )
            )
          ),

          # Card grid
          uiOutput(ns("dashboard_scales_cards"))
        )
      )
    )
  )
}

# ==============================================================================
# DASHBOARD MODULE SERVER
# ==============================================================================

#' Dashboard Module Server
#'
#' Implements the server-side logic for the coder dashboard, including personal
#' KPI counters, multi-metric training reliability visualizations, session
#' history tables, open-case counts, and searchable scale definition cards.
#'
#' @param id Character string specifying the Shiny module namespace identifier.
#' @param db_con Active `DBIConnection` or `pool::Pool` database handle.
#' @param user_id_reactive Optional reactive expression returning the active
#'   user's integer ID.
#' @param username_reactive Reactive expression returning the active username.
#' @param trigger_reactive Reactive trigger that invalidates dashboard outputs
#'   when ratings or training sessions are updated.
#' @param kategorie_spalten Reactive expression returning configured scale
#'   identifiers or column names.
#' @param scales_trigger Optional `reactiveVal` triggered when scale definitions
#'   change in the admin panel.
#' @param current_lang Reactive expression or character scalar providing the
#'   active UI language code (`"de"` or `"en"`).
#'
#' @return The return value of `shiny::moduleServer()`.
#'
#' @examples
#' \dontrun{
#' dashboardServer(
#'   id = "mod_dashboard",
#'   db_con = db_con,
#'   user_id_reactive = reactive(1L),
#'   username_reactive = reactive("admin"),
#'   trigger_reactive = reactiveVal(0L),
#'   kategorie_spalten = reactive(character(0))
#' )
#' }
#'
#' @export
dashboardServer <- function(id, db_con, user_id_reactive = NULL, username_reactive, trigger_reactive, kategorie_spalten, scales_trigger = NULL, current_lang = reactiveVal("de")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    get_lang <- function() {
      if (is.reactive(current_lang) || is.function(current_lang)) current_lang() else as.character(current_lang)
    }

    # Reactive helper for scale columns (purely reactive)
    get_kategorie_spalten <- reactive({
      if (!is.null(scales_trigger) && is.function(scales_trigger)) scales_trigger()
      kategorie_spalten()
    })

    # Live synchronization of static UI inputs on language change
    observe({
      lang <- get_lang()
      cur_metric <- isolate(input$dashboard_plot_metric)
      if (is.null(cur_metric) || cur_metric == "") cur_metric <- "all_core"
      updateSelectInput(
        session,
        "dashboard_plot_metric",
        label = tr("dashboard.chart_metric_select_label", lang),
        choices = get_reliability_metric_choices(lang, include_all_core = TRUE),
        selected = cur_metric
      )
      updateTextInput(
        session,
        "dashboard_scale_search",
        placeholder = tr("dashboard.scales_search_placeholder", lang)
      )
      updateActionButton(
        session,
        "btn_reset_scale_search",
        label = tr("common.reset", lang)
      )
    })

    # KPI CARD 1: Total production codings
    output$stat_card_total_kodierung <- renderUI({
      trigger_reactive()
      lang <- get_lang()
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)
      res <- dbGetQuery(db_con, "SELECT COUNT(*) AS total FROM user_ratings WHERE user_id = ? AND typ = 'kodierung'", params = list(u_info$id))
      render_kpi_card_ui(tr("dashboard.card_codings", lang), res$total, subtitle = tr("dashboard.kpi_codings_sub", lang), bg_class = "bg-dark", width = 4)
    })

    # KPI CARD 2: Total training codings
    output$stat_card_total_schulung <- renderUI({
      trigger_reactive()
      lang <- get_lang()
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)
      res <- dbGetQuery(db_con, "SELECT COUNT(*) AS total FROM user_ratings WHERE user_id = ? AND typ = 'schulung'", params = list(u_info$id))
      render_kpi_card_ui(tr("dashboard.card_trainings", lang), res$total, subtitle = tr("dashboard.kpi_trainings_sub", lang), bg_class = "bg-success", width = 4)
    })

    # KPI CARD 3: Completed training sessions
    output$stat_card_sessions <- renderUI({
      trigger_reactive()
      lang <- get_lang()
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)
      res <- dbGetQuery(db_con, "SELECT COUNT(*) AS anzahl FROM coding_training WHERE user_id = ?", params = list(u_info$id))
      render_kpi_card_ui(tr("dashboard.card_sessions", lang), res$anzahl, subtitle = tr("dashboard.kpi_sessions_sub", lang), bg_class = "bg-info", width = 4)
    })

    # Dynamically update category selection when scale_definitions or language changes
    observe({
      lang <- get_lang()
      k_spalten <- get_kategorie_spalten()
      if (length(k_spalten) > 0) {
        kat_ids <- unname(k_spalten)
        kat_labels <- names(k_spalten)
        labels_display <- paste0(kat_labels, " [#", kat_ids, "]")
        choices_kat <- setNames(kat_ids, labels_display)
        cur_sel <- isolate(input$dashboard_selected_kategorie)
        new_sel <- if (!is.null(cur_sel) && cur_sel %in% choices_kat) cur_sel else choices_kat[1]
        updateSelectInput(session, "dashboard_selected_kategorie", label = tr("dashboard.category_filter_label", lang), choices = choices_kat, selected = new_sel)
      } else {
        updateSelectInput(session, "dashboard_selected_kategorie", label = tr("dashboard.category_filter_label", lang), choices = character(0))
      }
    })

    # Aggregated metrics for the selected category from 'coding_training' (all 16 metrics)
    output$training_kpi_summary <- renderUI({
      trigger_reactive()
      lang <- get_lang()
      req(input$dashboard_selected_kategorie)
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)
      kat_sel <- input$dashboard_selected_kategorie
      selected_metric <- if (is.null(input$dashboard_plot_metric)) "all_core" else input$dashboard_plot_metric

      df <- dbGetQuery(db_con, "
        SELECT AVG(accuracy) AS avg_acc,
               AVG(balanced_accuracy) AS avg_bal_acc,
               AVG(avg_iota2) AS avg_iota,
               AVG(iota_index) AS avg_iota_idx,
               AVG(cohens_kappa) AS avg_kappa,
               AVG(alpha_nominal) AS avg_alpha,
               AVG(fleiss_kappa) AS avg_fleiss,
               AVG(gwet_ac1) AS avg_gwet,
               AVG(alpha_ordinal) AS avg_alpha_ord,
               AVG(kappa_linear) AS avg_kappa_lin,
               AVG(kappa_squared) AS avg_kappa_sq,
               AVG(gwet_ac2_quadratic) AS avg_gwet_ac2,
               AVG(kendall_w) AS avg_kendall,
               AVG(macro_f1) AS avg_f1,
               AVG(macro_precision) AS avg_prec,
               AVG(macro_recall) AS avg_rec,
               COUNT(*) AS n_sessions
        FROM coding_training
        WHERE user_id = ? AND kategorie = ?
      ", params = list(u_info$id, kat_sel))

      if (nrow(df) == 0 || is.na(df$n_sessions[1]) || df$n_sessions[1] == 0) {
        return(div(class = "alert alert-info", tr("dashboard.no_trainings_for_filter", lang)))
      }

      col_map <- list(
        cohens_kappa = "avg_kappa",
        alpha_nominal = "avg_alpha",
        fleiss_kappa = "avg_fleiss",
        gwet_ac1 = "avg_gwet",
        alpha_ordinal = "avg_alpha_ord",
        kappa_linear = "avg_kappa_lin",
        kappa_squared = "avg_kappa_sq",
        gwet_ac2_quadratic = "avg_gwet_ac2",
        kendall_w = "avg_kendall",
        avg_iota2 = "avg_iota",
        iota_index = "avg_iota_idx",
        accuracy = "avg_acc",
        balanced_accuracy = "avg_bal_acc",
        macro_f1 = "avg_f1",
        macro_precision = "avg_prec",
        macro_recall = "avg_rec"
      )

      eff_key <- if (selected_metric %in% names(col_map)) selected_metric else "accuracy"
      meta_sel <- get_reliability_metric_meta(eff_key, lang)
      raw_val <- df[[col_map[[eff_key]]]][1]
      disp_val <- format_metric_display(raw_val, is_percentage = meta_sel$is_pct)
      active_card_title <- tr("admin.reliability.kpi_mean_prefix", lang, meta_sel$short_label)

      # 5-KPI card layout: 4 core KPIs + 1 active metric
      div(class = "row g-2 mb-3",
        div(class = "col",
            div(class = "card h-100 bg-light border p-2 text-center shadow-sm",
                tags$small(class = "text-muted fw-bold", tr("admin.reliability.kpi_mean_prefix", lang, tr("metrics.cohens_kappa.short", lang))),
                tags$h4(class = "mb-0 text-primary fw-bold mt-1", format_metric_display(df$avg_kappa[1])),
                tags$small(class = "text-muted", tr("metrics.group_nominal", lang))
            )
        ),
        div(class = "col",
            div(class = "card h-100 bg-light border p-2 text-center shadow-sm",
                tags$small(class = "text-muted fw-bold", tr("admin.reliability.kpi_mean_prefix", lang, tr("metrics.alpha_nominal.short", lang))),
                tags$h4(class = "mb-0 text-success fw-bold mt-1", format_metric_display(df$avg_alpha[1])),
                tags$small(class = "text-muted", tr("metrics.alpha_nominal.short", lang))
            )
        ),
        div(class = "col",
            div(class = "card h-100 bg-light border p-2 text-center shadow-sm",
                tags$small(class = "text-muted fw-bold", tr("admin.reliability.kpi_mean_prefix", lang, tr("metrics.avg_iota2.short", lang))),
                tags$h4(class = "mb-0 text-warning fw-bold mt-1", format_metric_display(df$avg_iota[1])),
                tags$small(class = "text-muted", tr("admin.reliability.group_iota", lang))
            )
        ),
        div(class = "col",
            div(class = "card h-100 bg-light border p-2 text-center shadow-sm",
                tags$small(class = "text-muted fw-bold", tr("admin.reliability.kpi_mean_prefix", lang, tr("metrics.balanced_accuracy.short", lang))),
                tags$h4(class = "mb-0 text-info fw-bold mt-1", format_metric_display(df$avg_bal_acc[1], is_percentage = TRUE)),
                tags$small(class = "text-muted", tr("metrics.balanced_accuracy.short", lang))
            )
        ),
        div(class = "col",
            div(class = "card h-100 bg-white border border-primary p-2 text-center shadow-sm",
                tags$small(class = "text-primary fw-bold", active_card_title),
                tags$h4(class = "mb-0 text-dark fw-bold mt-1", disp_val),
                tags$small(class = "text-muted", meta_sel$desc)
            )
        )
      )
    })

    # Interactive reliability time series (ggplot2) for all 16 metrics or core comparison
    output$training_reliability_plot <- renderPlot({
      trigger_reactive()
      lang <- get_lang()
      req(input$dashboard_selected_kategorie)
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)
      kat_sel <- input$dashboard_selected_kategorie
      selected_metric <- if (is.null(input$dashboard_plot_metric)) "all_core" else input$dashboard_plot_metric

      df <- dbGetQuery(db_con, "
        SELECT training_id, datum,
               accuracy, balanced_accuracy,
               avg_iota2, iota_index,
               cohens_kappa, kappa_linear, kappa_squared, fleiss_kappa, kendall_w,
               alpha_nominal, alpha_ordinal, gwet_ac1, gwet_ac2_quadratic,
               macro_f1, macro_precision, macro_recall
        FROM coding_training
        WHERE user_id = ? AND kategorie = ?
        ORDER BY training_id ASC
      ", params = list(u_info$id, kat_sel))

      plot_font <- coding_manager_font_stacks()$plot_family

      if (nrow(df) == 0) {
        empty_lbl <- paste0(tr("dashboard.chart_empty_msg", lang), "\n", tr("dashboard.chart_empty_hint", lang, 16L))
        p <- ggplot() +
          annotate("text", x = 1, y = 0.5, label = empty_lbl, size = 5, color = "gray50", hjust = 0.5, family = plot_font) +
          theme_void(base_family = plot_font)
        return(p)
      }

      df$sitzung_nr <- seq_len(nrow(df))
      n_sitzungen <- nrow(df)

      # Visualization according to selected metric mode
      if (selected_metric %in% c("all_core", "all")) {
        lbl_kappa <- tr("metrics.cohens_kappa.label", lang)
        lbl_alpha <- tr("metrics.alpha_nominal.label", lang)
        lbl_gwet  <- tr("metrics.gwet_ac1.label", lang)
        lbl_iota  <- tr("metrics.avg_iota2.label", lang)

        plot_df <- rbind(
          data.frame(sitzung_nr = df$sitzung_nr, metric_name = lbl_kappa, value = df$cohens_kappa, stringsAsFactors = FALSE),
          data.frame(sitzung_nr = df$sitzung_nr, metric_name = lbl_alpha, value = df$alpha_nominal, stringsAsFactors = FALSE),
          data.frame(sitzung_nr = df$sitzung_nr, metric_name = lbl_gwet, value = df$gwet_ac1, stringsAsFactors = FALSE),
          data.frame(sitzung_nr = df$sitzung_nr, metric_name = lbl_iota, value = df$avg_iota2, stringsAsFactors = FALSE)
        )
        plot_df <- plot_df[!is.na(plot_df$value), ]

        color_palette <- setNames(
          c("#33418F", "#166534", "#475569", "#B45309"),
          c(lbl_kappa, lbl_alpha, lbl_gwet, lbl_iota)
        )

        min_val <- min(plot_df$value, na.rm = TRUE)
        y_lower <- if (is.finite(min_val) && min_val < 0) min(-0.2, min_val - 0.05) else 0

        p <- ggplot(plot_df, aes(x = sitzung_nr, y = value, color = metric_name, group = metric_name)) +
          geom_hline(yintercept = 0.80, linetype = "dashed", color = "#166534", alpha = 0.55, linewidth = 0.55) +
          geom_hline(yintercept = 0.60, linetype = "dashed", color = "#B45309", alpha = 0.55, linewidth = 0.55) +
          geom_hline(yintercept = 0.40, linetype = "dashed", color = "#991B1B", alpha = 0.55, linewidth = 0.55) +
          annotate("text", x = 0.55, y = 0.82, label = tr("dashboard.chart_bench_good_dec", lang), color = "#166534", size = 3.2, hjust = 0, family = plot_font) +
          annotate("text", x = 0.55, y = 0.62, label = tr("dashboard.chart_bench_mod_dec", lang), color = "#B45309", size = 3.2, hjust = 0, family = plot_font) +
          annotate("text", x = 0.55, y = 0.42, label = tr("metrics.bench_moderate", lang), color = "#991B1B", size = 3.2, hjust = 0, family = plot_font) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 3.5) +
          scale_color_manual(name = paste0(tr("dashboard.chart_legend_metric", lang), ":"), values = color_palette) +
          scale_y_continuous(limits = c(y_lower, 1.05), breaks = seq(0, 1, 0.2)) +
          labs(
            title = tr("dashboard.chart_plot_title_all", lang, n_sitzungen),
            subtitle = tr("dashboard.chart_plot_subtitle", lang, u_info$name, paste0("#", kat_sel)),
            x = tr("dashboard.chart_x_session", lang),
            y = tr("dashboard.chart_y_value", lang)
          )
      } else {
        cur_meta <- get_reliability_metric_meta(selected_metric, lang)
        df$val <- as.numeric(df[[selected_metric]])
        is_pct <- cur_meta$is_pct

        if (is_pct) {
          min_val <- min(df$val, na.rm = TRUE)
          y_lower <- if (is.finite(min_val) && min_val < 0) min(-10, min_val - 5) else 0

          p <- ggplot(df, aes(x = sitzung_nr, y = val)) +
            geom_hline(yintercept = 80, linetype = "dashed", color = "#27ae60", alpha = 0.6, linewidth = 0.6) +
            geom_hline(yintercept = 60, linetype = "dashed", color = "#f39c12", alpha = 0.6, linewidth = 0.6) +
            annotate("text", x = 0.55, y = 82, label = tr("dashboard.chart_bench_good_pct", lang), color = "#27ae60", size = 3.3, hjust = 0, family = plot_font) +
            annotate("text", x = 0.55, y = 62, label = tr("dashboard.chart_bench_mod_pct", lang), color = "#f39c12", size = 3.3, hjust = 0, family = plot_font) +
            geom_line(color = cur_meta$color, linewidth = 1.3) +
            geom_point(color = cur_meta$color, size = 4) +
            scale_y_continuous(limits = c(y_lower, 105), breaks = seq(0, 100, 20), labels = function(x) paste0(x, " %")) +
            labs(
              title = tr("dashboard.chart_plot_title_single", lang, cur_meta$label, n_sitzungen),
              subtitle = tr("dashboard.chart_plot_subtitle", lang, u_info$name, paste0("#", kat_sel)),
              x = tr("dashboard.chart_x_session", lang),
              y = tr("dashboard.chart_y_pct", lang)
            )
        } else {
          min_val <- min(df$val, na.rm = TRUE)
          y_lower <- if (is.finite(min_val) && min_val < 0) min(-0.2, min_val - 0.05) else 0

          p <- ggplot(df, aes(x = sitzung_nr, y = val)) +
            geom_hline(yintercept = 0.80, linetype = "dashed", color = "#27ae60", alpha = 0.6, linewidth = 0.6) +
            geom_hline(yintercept = 0.60, linetype = "dashed", color = "#f39c12", alpha = 0.6, linewidth = 0.6) +
            geom_hline(yintercept = 0.40, linetype = "dashed", color = "#e74c3c", alpha = 0.6, linewidth = 0.6) +
            annotate("text", x = 0.55, y = 0.82, label = tr("dashboard.chart_bench_good_dec", lang), color = "#27ae60", size = 3.3, hjust = 0, family = plot_font) +
            annotate("text", x = 0.55, y = 0.62, label = tr("dashboard.chart_bench_mod_dec", lang), color = "#f39c12", size = 3.3, hjust = 0, family = plot_font) +
            annotate("text", x = 0.55, y = 0.42, label = tr("metrics.bench_moderate", lang), color = "#e74c3c", size = 3.3, hjust = 0, family = plot_font) +
            geom_line(color = cur_meta$color, linewidth = 1.3) +
            geom_point(color = cur_meta$color, size = 4) +
            scale_y_continuous(limits = c(y_lower, 1.05), breaks = seq(0, 1, 0.2)) +
            labs(
              title = tr("dashboard.chart_plot_title_single", lang, cur_meta$label, n_sitzungen),
              subtitle = tr("dashboard.chart_plot_subtitle", lang, u_info$name, paste0("#", kat_sel)),
              x = tr("dashboard.chart_x_session", lang),
              y = tr("dashboard.chart_y_value", lang)
            )
        }

        if (n_sitzungen <= 15) {
          val_labels <- if (is_pct) sprintf("%.1f%%", df$val) else sprintf("%.3f", df$val)
          p <- p + geom_text(aes(label = val_labels), vjust = -1.2, size = 3.3, fontface = "bold", color = cur_meta$color, family = plot_font)
        }
      }

      x_breaks <- seq_len(n_sitzungen)
      x_labels <- vapply(x_breaks, function(x) tr("dashboard.chart_session_prefix", lang, x), character(1))

      p <- p +
        scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0.5, max(1.5, n_sitzungen + 0.5))) +
        coding_manager_plot_theme(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 13, color = "#1C234A", margin = margin(b = 4)),
          plot.subtitle = element_text(size = 10.5, color = "#525B70", margin = margin(b = 10)),
          axis.title.x = element_text(face = "bold", margin = margin(t = 10), color = "#334155"),
          axis.title.y = element_text(face = "bold", margin = margin(r = 10), color = "#334155"),
          axis.text = element_text(color = "#525B70"),
          axis.text.x = element_text(angle = if (n_sitzungen > 8) 35 else 0, hjust = if (n_sitzungen > 8) 1 else 0.5),
          panel.grid.major = element_line(color = "#E2E5F0", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.title = element_text(face = "bold", color = "#1C234A")
        )

      return(p)
    })

    # Detailed training session history table with two-level grouped header
    output$training_performance_table <- renderUI({
      trigger_reactive()
      lang <- get_lang()
      req(input$dashboard_selected_kategorie)
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)
      kat_sel <- input$dashboard_selected_kategorie

      df <- dbGetQuery(db_con, "
        SELECT training_id,
               datum,
               anzahl_einheiten,
               accuracy,
               balanced_accuracy,
               avg_iota2,
               iota_index,
               cohens_kappa,
               alpha_nominal,
               gwet_ac1
        FROM coding_training
        WHERE user_id = ? AND kategorie = ?
        ORDER BY training_id DESC
      ", params = list(u_info$id, kat_sel))

      if (nrow(df) == 0) {
        return(div(class = "alert alert-light border text-center text-muted py-3",
                   tr("dashboard.no_trainings_for_filter", lang)))
      }

      rows <- lapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        kappa_val <- if (!is.na(r$cohens_kappa)) as.numeric(r$cohens_kappa) else NA

        quality_meta <- classify_reliability_magnitude(kappa_val, lang = lang)
        quality_badge <- tags$span(class = paste("badge", quality_meta$css_class), quality_meta$label)

        datum_str <- if (!is.na(r$datum)) as.character(r$datum) else "\u2014"

        tags$tr(
          tags$td(class = "text-center fw-bold text-secondary", paste0("#", r$training_id)),
          tags$td(class = "text-center small text-muted", datum_str),
          tags$td(class = "text-center", tags$span(class = "badge bg-light text-dark border", paste(r$anzahl_einheiten, tr("dashboard.col_cases", lang)))),
          tags$td(class = "text-center", render_metric_badge_ui(r$accuracy, is_percentage = TRUE)),
          tags$td(class = "text-center", render_metric_badge_ui(r$balanced_accuracy, is_percentage = TRUE)),
          tags$td(class = "text-center", render_metric_badge_ui(r$avg_iota2)),
          tags$td(class = "text-center", render_metric_badge_ui(r$iota_index)),
          tags$td(class = "text-center", render_metric_badge_ui(r$cohens_kappa)),
          tags$td(class = "text-center", render_metric_badge_ui(r$alpha_nominal)),
          tags$td(class = "text-center", render_metric_badge_ui(r$gwet_ac1)),
          tags$td(class = "text-center", quality_badge)
        )
      })

      tags$div(class = "table-responsive mt-2",
        tags$table(class = "table table-hover align-middle table-bordered bg-white shadow-sm text-center",
          tags$thead(class = "table-light text-muted small text-uppercase",
            tags$tr(
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 80px;", "#"),
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 110px;", tr("dashboard.col_date", lang)),
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 90px;", tr("dashboard.col_cases", lang)),
              tags$th(colspan = 2, class = "text-center bg-light text-primary border-bottom-0 py-1", tr("admin.reliability.group_classification", lang)),
              tags$th(colspan = 2, class = "text-center bg-light text-warning border-bottom-0 py-1", tr("admin.reliability.group_iota", lang)),
              tags$th(colspan = 3, class = "text-center bg-light text-success border-bottom-0 py-1", tr("admin.reliability.group_intercoder", lang)),
              tags$th(rowspan = 2, class = "text-center align-middle", style = "min-width: 110px;", tr("dashboard.col_agreement", lang))
            ),
            tags$tr(
              tags$th(class = "text-center small py-1", tr("metrics.accuracy.short", lang)),
              tags$th(class = "text-center small py-1", tr("metrics.balanced_accuracy.short", lang)),
              tags$th(class = "text-center small py-1", tr("metrics.avg_iota2.short", lang)),
              tags$th(class = "text-center small py-1", tr("metrics.iota_index.short", lang)),
              tags$th(class = "text-center small py-1", tr("metrics.cohens_kappa.short", lang)),
              tags$th(class = "text-center small py-1", tr("metrics.alpha_nominal.short", lang)),
              tags$th(class = "text-center small py-1", tr("metrics.gwet_ac1.short", lang))
            )
          ),
          tags$tbody(rows)
        )
      )
    })

    # Table computing remaining open cases per category
    output$kategorie_breakdown_table <- renderTable({
      trigger_reactive()
      lang <- get_lang()
      req(username_reactive())

      total_res <- dbGetQuery(db_con, "SELECT COUNT(*) AS gesamt FROM texts")
      gesamt_faelle <- if (nrow(total_res) > 0) as.integer(total_res$gesamt[1]) else 0

      ergebnis_liste <- list()
      u_info <- get_user_info_db(db_con, username_reactive, user_id_reactive)

      k_spalten <- get_kategorie_spalten()
      for (i in seq_along(k_spalten)) {
        kat_short <- names(k_spalten)[i]
        kat_id    <- unname(k_spalten)[i]

        user_query <- "
          SELECT
            SUM(CASE WHEN typ = 'schulung' THEN 1 ELSE 0 END) AS schulung_n,
            SUM(CASE WHEN typ = 'kodierung' THEN 1 ELSE 0 END) AS eigene_kodierung_n
          FROM user_ratings
          WHERE user_id = ? AND kategorie = ?
        "
        user_res <- dbGetQuery(db_con, user_query, params = list(u_info$id, kat_id))

        schulung_n <- ifelse(is.na(user_res$schulung_n), 0, user_res$schulung_n)
        eigene_kodierung_n <- ifelse(is.na(user_res$eigene_kodierung_n), 0, user_res$eigene_kodierung_n)

        global_coded_res <- dbGetQuery(db_con, "
          SELECT COUNT(DISTINCT text_id) AS n_kodiert
          FROM user_ratings
          WHERE kategorie = ?
            AND typ IN ('kodierung', 'gold_standard')
        ", params = list(kat_id))
        gesamt_kodiert_n <- if (nrow(global_coded_res) > 0 && !is.na(global_coded_res$n_kodiert[1])) as.integer(global_coded_res$n_kodiert[1]) else 0

        noch_offen <- max(0, gesamt_faelle - gesamt_kodiert_n)

        row_df <- data.frame(
          c1 = paste0(kat_short, " [#", kat_id, "]"),
          c2 = schulung_n,
          c3 = eigene_kodierung_n,
          c4 = gesamt_kodiert_n,
          c5 = noch_offen,
          stringsAsFactors = FALSE
        )
        colnames(row_df) <- c(
          tr("dashboard.col_category_scale", lang),
          tr("dashboard.col_training_cases", lang),
          tr("dashboard.col_live_codings", lang),
          tr("dashboard.col_total", lang),
          tr("dashboard.col_remaining_open", lang)
        )
        ergebnis_liste[[kat_id]] <- row_df
      }

      if (length(ergebnis_liste) == 0) return(data.frame())
      do.call(rbind, ergebnis_liste)
    }, width = "100%", striped = TRUE, spacing = "m", align = "lcccc")

    # --------------------------------------------------------------------------
    # TAB 2: SCALE OVERVIEW & REQUIREMENTS (CARD VIEW & DETAILS)
    # --------------------------------------------------------------------------
    dashboard_scales_data <- reactive({
      if (!is.null(scales_trigger) && is.function(scales_trigger)) {
        scales_trigger()
      }
      trigger_reactive()
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

    observeEvent(input$btn_reset_scale_search, {
      updateTextInput(session, "dashboard_scale_search", value = "")
    })

    output$dashboard_scales_count <- renderUI({
      lang <- get_lang()
      df <- dashboard_scales_data()
      suchtext <- trimws(input$dashboard_scale_search)
      n_total <- nrow(df)
      if (!is.null(suchtext) && suchtext != "" && n_total > 0) {
        clean_query <- tolower(suchtext)
        raw_num <- gsub("^#", "", clean_query)
        is_num <- grepl("^[0-9]+$", raw_num)

        treffer <- grepl(clean_query, tolower(df$name), fixed = TRUE) |
                   grepl(clean_query, tolower(df$short_label), fixed = TRUE) |
                   grepl(clean_query, tolower(paste0("#", df$scale_id)), fixed = TRUE) |
                   grepl(clean_query, tolower(as.character(df$scale_id)), fixed = TRUE) |
                   (if (is_num) as.character(df$scale_id) == raw_num else FALSE)

        n_filtered <- sum(treffer)
        tr("dashboard.scales_count_filtered", lang, n_filtered, n_total)
      } else {
        tr("dashboard.scales_count_all", lang, n_total)
      }
    })

    output$dashboard_scales_cards <- renderUI({
      lang <- get_lang()
      df <- dashboard_scales_data()
      suchtext <- trimws(input$dashboard_scale_search)

      if (!is.null(suchtext) && suchtext != "" && nrow(df) > 0) {
        clean_query <- tolower(suchtext)
        raw_num <- gsub("^#", "", clean_query)
        is_num <- grepl("^[0-9]+$", raw_num)

        treffer <- grepl(clean_query, tolower(df$name), fixed = TRUE) |
                   grepl(clean_query, tolower(df$short_label), fixed = TRUE) |
                   grepl(clean_query, tolower(paste0("#", df$scale_id)), fixed = TRUE) |
                   grepl(clean_query, tolower(as.character(df$scale_id)), fixed = TRUE) |
                   (if (is_num) as.character(df$scale_id) == raw_num else FALSE)

        df <- df[treffer, , drop = FALSE]
      }

      if (nrow(df) == 0) {
        return(div(class = "alert alert-light border text-center py-4 my-3",
                   icon("info-circle", class = "me-2 text-muted"),
                   tr("dashboard.scales_empty", lang)))
      }

      cards <- lapply(1:nrow(df), function(i) {
        s_id    <- df$scale_id[i]
        s_short <- df$short_label[i]
        s_name  <- df$name[i]
        s_max   <- df$max_value[i]
        s_desc  <- df$def_desc[i]
        safe_id <- paste0("scale_card_", s_id)

        desc_display <- if (!is.na(s_desc) && s_desc != "") {
          if (nchar(s_desc) > 140) paste0(substr(s_desc, 1, 140), "...") else s_desc
        } else {
          tr("common.no_overarching_description", lang)
        }

        column(
          width = 6,
          class = "mb-4",
          div(
            class = "card h-100 border",
            style = "border-top: 3px solid var(--cm-navy) !important;",
            div(
              class = "card-header bg-white d-flex justify-content-between align-items-center py-2",
              tags$span(class = "badge bg-primary-subtle text-primary border border-primary-subtle px-2 py-1",
                        style = "font-size: 0.9em; font-weight: 700;", s_short),
              tags$span(class = "text-muted font-monospace small", paste0("#", s_id))
            ),
            div(
              class = "card-body d-flex flex-column",
              tags$h5(class = "card-title text-dark mb-2", s_name),
              div(
                class = "bg-light p-2 rounded mb-3 border",
                div(class = "d-flex justify-content-between align-items-center",
                    tags$span(class = "text-muted small", tr("dashboard.scales_range_label", lang)),
                    tags$span(class = "badge bg-dark font-monospace", tr("dashboard.scales_range_badge", lang, as.integer(s_max)))
                ),
                div(class = "d-flex justify-content-between align-items-center mt-1",
                    tags$span(class = "text-muted small", tr("dashboard.scales_levels_label", lang)),
                    tags$span(class = "text-secondary small fw-bold", tr("dashboard.scales_levels_value", lang, as.integer(s_max) + 1L, as.integer(s_max)))
                )
              ),
              p(class = "card-text text-muted small flex-grow-1", desc_display),
              div(
                class = "mt-3 pt-2 border-top text-end",
                actionButton(
                  inputId = ns(paste0("btn_details_", safe_id)),
                  label = tr("dashboard.scales_btn_details", lang),
                  icon = icon("list-ol"),
                  class = "btn-sm btn-outline-primary",
                  onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("show_scale_details"), s_id)
                )
              )
            )
          )
        )
      })

      fluidRow(cards)
    })

    # MODAL: Display rating level descriptions and detailed instructions for the selected scale
    observeEvent(input$show_scale_details, {
      lang <- get_lang()
      s_id <- as.integer(input$show_scale_details)
      req(s_id)

      row <- dbGetQuery(db_con,
                        "SELECT scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1, def_n_2, def_n_3, def_n_4, def_n_5 FROM scale_definitions WHERE scale_id = ?",
                        params = list(s_id))
      if (nrow(row) == 0) return()

      s_max <- as.integer(row$max_value)

      stufen_items <- lapply(0:s_max, function(lvl) {
        col_name <- paste0("def_n_", lvl)
        lvl_text <- if (col_name %in% colnames(row)) row[[col_name]] else ""
        if (is.na(lvl_text) || lvl_text == "") lvl_text <- paste0("(", tr("common.no_description", lang), ")")

        div(
          class = "p-3 mb-2 rounded border bg-light",
          div(
            class = "d-flex align-items-center mb-1",
            tags$span(class = "badge bg-primary me-2 font-monospace", style = "font-size: 0.9em;", tr("common.level_n", lang, as.integer(lvl))),
            tags$span(class = "fw-bold text-secondary",
                      if (lvl == 0) tr("common.level_min", lang)
                      else if (lvl == s_max) tr("common.level_max", lang, as.integer(s_max))
                      else "")
          ),
          p(class = "mb-0 text-dark", style = "font-size: 0.95em; line-height: 1.4;", lvl_text)
        )
      })

      showModal(modalDialog(
        title = div(
          icon("ruler-combined", class = "me-2 text-primary"),
          tr("dashboard.scales_modal_title", lang, as.character(row$name), as.character(row$short_label), as.integer(row$scale_id))
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton(tr("common.close", lang)),
        div(
          div(class = "alert alert-secondary py-2 mb-3",
              div(class = "d-flex justify-content-between align-items-center",
                  tags$span(tr("dashboard.scales_modal_banner", lang, as.integer(s_max), as.integer(s_max) + 1L)),
                  tags$span(class = "badge bg-dark font-monospace", tr("dashboard.scales_modal_shortlabel", lang, as.character(row$short_label)))
              )
          ),
          if (!is.na(row$def_desc) && row$def_desc != "") {
            div(class = "mb-3",
                tags$h6(class = "fw-bold text-dark", icon("info-circle", class = "me-1 text-primary"), tr("dashboard.scales_modal_def_heading", lang)),
                div(class = "p-3 rounded bg-white border text-secondary", style = "line-height: 1.5;", row$def_desc)
            )
          },
          tags$h6(class = "fw-bold text-dark mt-3 mb-2", icon("list-ol", class = "me-1 text-primary"), tr("dashboard.scales_modal_steps_heading", lang)),
          div(stufen_items)
        )
      ))
    })
  })
}
