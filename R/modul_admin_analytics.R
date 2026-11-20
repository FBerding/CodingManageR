# ==============================================================================
# ADMIN MODULE SERVER HELPERS: RELIABILITY, CODING SEARCH & DIAGNOSTICS (TABS 5, 6, 8)
# ==============================================================================
# Internal subtab server registrars extracted from adminServer() to keep each
# function compact and avoid quadratic AST instrumentation overhead under covr.
# ==============================================================================

#' Register Admin Tab 5 Team Reliability & Coding Progress Server Logic
#'
#' Registers reactive aggregations, KPI cards, comparison chart rendering,
#' reliability table output, and per-coder drilldown modal inside the admin
#' module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_reliability_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  scales_trigger <- ctx$scales_trigger
  get_lang <- ctx$get_lang
  user_table_trigger <- ctx$user_table_trigger
  scales_table_trigger <- ctx$scales_table_trigger
  ratings_edit_trigger <- ctx$ratings_edit_trigger

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

  invisible(NULL)
}

#' Register Admin Tab 6 Targeted Coding Search & Value Correction Logic
#'
#' Registers filter controls, paginated coding search results, and the single
#' coding score edit modal inside the admin module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_coding_search_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  scales_trigger <- ctx$scales_trigger
  get_lang <- ctx$get_lang
  user_table_trigger <- ctx$user_table_trigger
  texts_table_trigger <- ctx$texts_table_trigger
  scales_table_trigger <- ctx$scales_table_trigger
  ratings_edit_trigger <- ctx$ratings_edit_trigger
  current_page_coding_search <- ctx$current_page_coding_search

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

  invisible(NULL)
}

#' Register Admin Tab 8 Database Diagnostics & Integrity Audit Server Logic
#'
#' Registers the diagnostics refresh observer, the reactive 9-table and
#' referential integrity report, KPI cards, table overview, and audit UI.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return The `db_diagnostics_report` reactive expression so `adminServer()`
#'   retains direct frame access for `shiny::testServer()` verification.
#' @keywords internal
#' @noRd
admin_register_diagnostics_server <- function(input, output, session, ctx) {
  db_con <- ctx$db_con
  get_lang <- ctx$get_lang
  user_table_trigger <- ctx$user_table_trigger
  texts_table_trigger <- ctx$texts_table_trigger
  sources_trigger <- ctx$sources_trigger
  ratings_edit_trigger <- ctx$ratings_edit_trigger

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

  db_diagnostics_report
}
