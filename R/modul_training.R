# Dependencies (shiny, DBI) are imported via NAMESPACE

# ==============================================================================
# MODUL-UI
# ==============================================================================
schulungUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("schulungs_bereich"))
}

# ==============================================================================
# MODUL-SERVER
# ==============================================================================
schulungServer <- function(id, db_con, user_id_reactive = NULL, username_reactive, kategorie_spalten, kategorien_liste, trigger_parent, scales_trigger = NULL, current_lang = reactiveVal("de")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    get_lang <- function() {
      if (is.reactive(current_lang) || is.function(current_lang)) current_lang() else as.character(current_lang)
    }

    # Reaktive Hilfsfunktionen für Skalen und Kategorien (rein reaktiv)
    get_kategorie_spalten <- reactive({
      if (!is.null(scales_trigger) && is.function(scales_trigger)) scales_trigger()
      kategorie_spalten()
    })

    get_kategorien_liste <- reactive({
      if (!is.null(scales_trigger) && is.function(scales_trigger)) scales_trigger()
      kategorien_liste()
    })

    # Dynamische Aktualisierung der Kategorieauswahl bei Änderungen an scale_definitions
    observe({
      choices <- get_kategorien_liste()
      current_sel <- isolate(input$gewaehlte_kategorie)
      new_sel <- if (!is.null(current_sel) && current_sel %in% choices) {
        current_sel
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        NULL
      }
      updateSelectInput(session, "gewaehlte_kategorie", choices = choices, selected = new_sel)
    })

    schulung_gestartet <- reactiveVal(FALSE)
    schulung_abgeschlossen <- reactiveVal(FALSE)

    fall_pool <- reactiveVal(NULL)
    aktueller_fall <- reactiveVal(NULL)

    get_user_info <- function() {
      get_user_info_db(db_con, username_reactive, user_id_reactive)
    }

    max_bewertung_wert <- reactiveVal(5)
    beschreibung_text <- reactiveVal("")

    zaehler <- reactiveVal(0)
    ziel_einheiten <- reactiveVal(0)

    get_rand_fn <- function() {
      get_db_random_function(db_con)
    }

    # Skalen-Metadaten bei Kategorie-Änderung synchronisieren
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

    output$schulungs_bereich <- renderUI({
      lang <- get_lang()
      k_liste <- get_kategorien_liste()
      sel_kat <- isolate(input$gewaehlte_kategorie)
      if (is.null(sel_kat) || !(sel_kat %in% k_liste)) {
        sel_kat <- if (length(k_liste) > 0) k_liste[1] else NULL
      }

      if (!schulung_gestartet()) {
        fluidRow(
          column(width = 6,
                 wellPanel(
                   h4(tr("training.config_title", lang)),
                   p(tags$small(class = "text-muted", tr("training.config_desc", lang))),
                   br(),
                   selectInput(ns("gewaehlte_kategorie"), tr("training.select_scale_label", lang), choices = k_liste, selected = sel_kat),
                   uiOutput(ns("dynamischer_einheiten_input")),
                   br(),
                   actionButton(ns("start_schulung_btn"), tr("training.btn_start", lang), class = "btn-success w-100"),
                   actionButton(ns("btn_beispiele_config"), paste0("💡 ", tr("training.btn_show_examples", lang)), class = "btn-outline-info w-100 mt-2")
                 )
          )
        )
      } else if (schulung_gestartet() && !schulung_abgeschlossen()) {
        fall <- aktueller_fall()
        slider_val <- isolate(input$bewertung_slider)
        if (is.null(slider_val) || is.na(slider_val)) slider_val <- 0

        fluidPage(
          fluidRow(
            column(width = 12,
                   div(class = "card text-white bg-primary mb-3",
                       div(class = "card-body d-flex justify-content-between align-items-center",
                           {
                             k_spalten <- get_kategorie_spalten()
                             kat_lbl <- if (length(k_spalten) > 0) names(k_spalten)[which(k_spalten == sel_kat)[1]] else NULL
                             if (is.na(kat_lbl) || is.null(kat_lbl) || kat_lbl == "") kat_lbl <- paste0("ID #", sel_kat)
                             h5(class = "card-title mb-0", tr("training.scale_badge", lang, kat_lbl))
                           },
                           div(class = "text-end", h4(class = "mb-0", paste0(zaehler() + 1, " / ", ziel_einheiten())))
                       )
                   )
            )
          ),

          if (is.null(fall) || nrow(fall) == 0) {
            wellPanel(p(tr("training.err_no_gold_cases", lang)))
          } else {
            fluidRow(
              column(width = 8,
                     wellPanel(
                       div(class = "d-flex justify-content-between align-items-center mb-2",
                           h5(class = "mb-0", tr("training.current_case", lang))
                       ),
                       hr(),
                       div(
                         style = "max-height: 250px; overflow-y: auto; background-color: #f8f9fa; padding: 15px; border: 1px solid #ced4da; border-radius: 4px; font-family: monospace; white-space: pre-wrap;",
                         fall$text
                       ),
                       hr(),
                       p(tags$small(paste(tr("coding.case_selection", lang), paste0("#", fall$text_id))),
                         if (!is.null(fall$source_id) && !is.na(fall$source_id[1])) {
                           tags$span(class = "badge bg-secondary ms-2", paste0(tr("training.source_ref", lang), " [#", fall$source_id[1], "] ", fall$text_kurzreferenz[1]))
                         }
                       )
                     )
              ),
              column(width = 4,
                     wellPanel(
                       div(class = "d-flex justify-content-between align-items-center mb-3",
                           h5(class = "mb-0 fw-bold", tr("training.your_rating", lang)),
                           actionButton(ns("btn_beispiele"), paste0("💡 ", tr("training.btn_show_examples", lang)), class = "btn btn-sm btn-outline-primary")
                       ),
                       hr(),
                       sliderInput(ns("bewertung_slider"), tr("training.slider_label", lang, max_bewertung_wert()),
                                   min = 0, max = max_bewertung_wert(), value = slider_val, step = 1),
                       br(),
                       div(class = "card bg-light mb-3",
                           div(class = "card-header", tags$strong(tr("examples.definition_label", lang))),
                           div(class = "card-body", p(class = "card-text", style = "font-size: 0.95em; color: #2c3e50; font-weight: bold;", textOutput(ns("wert_definition")))))
                     ),
                     div(class = "card bg-light mb-3",
                         div(class = "card-header", tags$strong(tr("examples.general_def", lang))),
                         div(class = "card-body", p(class = "card-text", style = "font-size: 0.9em;",
                                                    if (nzchar(beschreibung_text())) beschreibung_text() else tr("common.no_description", lang)))
                     )
              )
            )
          },

          br(),
          fluidRow(
            column(width = 3,
                   actionButton(ns("naechster_fall_btn"),
                                if (zaehler() + 1 == ziel_einheiten()) tr("training.btn_finish", lang) else tr("training.btn_next_case", lang),
                                class = "btn-info w-100")),
            column(width = 3, actionButton(ns("back_to_config_btn"), tr("training.btn_abort", lang), class = "btn-secondary w-100"))
          )
        )
      } else {
        k_spalten <- get_kategorie_spalten()
        kat_lbl <- if (length(k_spalten) > 0) names(k_spalten)[which(k_spalten == sel_kat)[1]] else NULL
        if (is.na(kat_lbl) || is.null(kat_lbl) || kat_lbl == "") kat_lbl <- paste0("ID #", sel_kat)

        fluidPage(
          div(class = "alert alert-success text-center",
              h2(paste0("🎉 ", tr("training.completed_title", lang))),
              p(tr("training.completed_subtitle", lang, kat_lbl, as.integer(ziel_einheiten())))
          ),
          uiOutput(ns("external_metrics_panel")),
          br(),
          wellPanel(
            div(class = "d-flex justify-content-between align-items-center mb-2",
                h4(class = "mb-0", paste0("📊 ", tr("training.comparison_table_title", lang)))
            ),
            br(),
            tableOutput(ns("session_summary_table")), br(),
            div(class = "d-flex gap-2",
                actionButton(ns("restart_schulung_btn"), tr("training.btn_new_training", lang), class = "btn-primary"),
                downloadButton(ns("download_session_csv"), tr("training.btn_download_csv", lang), class = "btn btn-success")
            )
          )
        )
      }
    })

    output$dynamischer_einheiten_input <- renderUI({
      lang <- get_lang()
      req(input$gewaehlte_kategorie)
      meta_res <- dbGetQuery(db_con, "SELECT max_value FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(input$gewaehlte_kategorie)))
      min_erforderlich <- if (nrow(meta_res) > 0 && !is.na(meta_res$max_value[1])) as.integer(meta_res$max_value[1]) + 1 else 2

      cur_val <- isolate(input$kodiereinheiten)
      init_val <- if (!is.null(cur_val) && !is.na(cur_val) && cur_val >= min_erforderlich) cur_val else max(3, min_erforderlich)

      numericInput(ns("kodiereinheiten"), tr("training.units_slider_label", lang), value = init_val, min = min_erforderlich, max = 100)
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

    hole_sitzungs_daten <- function() {
      u_info <- get_user_info()

      query <- "
        SELECT u.bewertung AS user_val, ref.bewertung AS true_val
        FROM user_ratings u
        JOIN (
          SELECT r.text_id, r.bewertung,
                 ROW_NUMBER() OVER (
                   PARTITION BY r.text_id
                   ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC
                 ) AS rn
          FROM user_ratings r
          WHERE r.kategorie = ? AND r.typ IN ('gold_standard', 'kodierung')
        ) ref ON u.text_id = ref.text_id AND ref.rn = 1
        WHERE u.user_id = ? AND u.kategorie = ? AND u.typ = 'schulung'
        ORDER BY u.rating_id DESC LIMIT ?
      "
      params_all <- list(input$gewaehlte_kategorie, u_info$id, input$gewaehlte_kategorie, ziel_einheiten())
      dbGetQuery(db_con, query, params = params_all)
    }

    output$external_metrics_panel <- renderUI({
      lang <- get_lang()
      req(input$gewaehlte_kategorie)
      res_data <- hole_sitzungs_daten()
      if (nrow(res_data) < 2) return(div(class = "alert alert-warning", tr("training.err_no_gold_cases", lang)))

      m <- berechne_aifeducation_metriken(user_vals = res_data$user_val, true_vals = res_data$true_val, max_val = max_bewertung_wert())

      wellPanel(
        style = "background-color: #f8f9fa; border-left: 5px solid #2c3e50;",
        h4(paste0("📈 ", tr("training.summary_all_metrics_title", lang))), hr(),
        fluidRow(
          column(width = 4,
                 h6(tags$strong(tr("metrics.group_classification", lang))),
                 p(paste0(tr("metrics.accuracy.label", lang), ": "), m$accuracy, " %"),
                 p(paste0(tr("metrics.balanced_accuracy.label", lang), ": "), m$balanced_accuracy, " %"),
                 p(paste0(tr("metrics.macro_precision.label", lang), ": "), m$macro_precision, " %"),
                 p(paste0(tr("metrics.macro_recall.label", lang), ": "), m$macro_recall, " %"),
                 p(paste0(tr("metrics.macro_f1.label", lang), ": "), m$macro_f1, " %")
          ),
          column(width = 8,
                 h6(tags$strong(paste0(tr("metrics.group_nominal", lang), " & ", tr("metrics.group_ordinal", lang)))),
                 fluidRow(
                   column(width = 6,
                          p(paste0(tr("metrics.iota_index.label", lang), ": "), tags$code(m$iota_index)),
                          p(paste0(tr("metrics.avg_iota2.label", lang), ": "), tags$code(m$avg_iota2)),
                          p(paste0(tr("metrics.cohens_kappa.label", lang), ": "), tags$code(m$cohens_kappa)),
                          p(paste0(tr("metrics.kappa_linear.label", lang), ": "), tags$code(m$kappa_linear)),
                          p(paste0(tr("metrics.kappa_squared.label", lang), ": "), tags$code(m$kappa_squared))
                   ),
                   column(width = 6,
                          p(paste0(tr("metrics.fleiss_kappa.label", lang), ": "), tags$code(m$fleiss_kappa)),
                          p(paste0(tr("metrics.kendall_w.label", lang), ": "), tags$code(m$kendall_w)),
                          p(paste0(tr("metrics.alpha_nominal.label", lang), ": "), tags$code(m$alpha_nominal)),
                          p(paste0(tr("metrics.alpha_ordinal.label", lang), ": "), tags$code(m$alpha_ordinal)),
                          p(paste0(tr("metrics.gwet_ac2_quadratic.label", lang), ": "), tags$code(m$gwet_ac2_quadratic))
                   )
                 )
          )
        )
      )
    })

    generiere_garantierten_fall_pool <- function(kategorie_name, n_gesamt, max_val) {
      erzwungene_faelle <- list()
      rand_fn <- get_rand_fn()

      for (level in 0:max_val) {
        query <- sprintf("
          SELECT t.text_id, t.text, t.source_id,
                 s.Kurzreferenz AS text_kurzreferenz,
                 s.Referenz AS text_referenz
          FROM texts t
          JOIN (
            SELECT r.text_id, r.bewertung,
                   ROW_NUMBER() OVER (
                     PARTITION BY r.text_id
                     ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC
                   ) AS rn
            FROM user_ratings r
            WHERE r.kategorie = ? AND r.typ IN ('gold_standard', 'kodierung')
          ) ref ON t.text_id = ref.text_id AND ref.rn = 1
          LEFT JOIN sources s ON t.source_id = s.source_id
          WHERE ref.bewertung = ?
          ORDER BY %s LIMIT 1
        ", rand_fn)
        res <- dbGetQuery(db_con, query, params = list(kategorie_name, level))
        if (nrow(res) > 0) {
          erzwungene_faelle[[as.character(level)]] <- res
        }
      }

      pool_basis <- if (length(erzwungene_faelle) > 0) do.call(rbind, erzwungene_faelle) else data.frame()
      n_erzwungen <- nrow(pool_basis)
      n_rest <- n_gesamt - n_erzwungen

      if (n_rest > 0) {
        bereits_gezogen_ids <- if (n_erzwungen > 0) paste(pool_basis$text_id, collapse = ",") else ""
        where_not_in <- if (n_erzwungen > 0) sprintf("AND t.text_id NOT IN (%s)", bereits_gezogen_ids) else ""

        rest_query <- sprintf("
          SELECT t.text_id, t.text, t.source_id,
                 s.Kurzreferenz AS text_kurzreferenz,
                 s.Referenz AS text_referenz
          FROM texts t
          JOIN (
            SELECT r.text_id, r.bewertung,
                   ROW_NUMBER() OVER (
                     PARTITION BY r.text_id
                     ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC
                   ) AS rn
            FROM user_ratings r
            WHERE r.kategorie = ? AND r.typ IN ('gold_standard', 'kodierung')
          ) ref ON t.text_id = ref.text_id AND ref.rn = 1
          LEFT JOIN sources s ON t.source_id = s.source_id
          WHERE 1=1 %s
          ORDER BY %s LIMIT %d
        ", where_not_in, rand_fn, n_rest)
        rest_faelle <- dbGetQuery(db_con, rest_query, params = list(kategorie_name))
        if (nrow(rest_faelle) > 0) {
          pool_basis <- rbind(pool_basis, rest_faelle)
        }
      }
      if (nrow(pool_basis) > 0) {
        pool_basis <- pool_basis[sample(nrow(pool_basis)), ]
      }
      return(pool_basis)
    }

    speichere_aktuelle_bewertung <- function() {
      fall <- aktueller_fall()
      if (!is.null(fall) && nrow(fall) > 0 && !is.null(input$bewertung_slider)) {
        u_info <- get_user_info()
        dbExecute(db_con, "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ) VALUES (?, ?, ?, ?, 'schulung');",
                  params = list(u_info$id, fall$text_id, input$gewaehlte_kategorie, input$bewertung_slider))
      }
    }

    speichere_schulungs_abschluss <- function(m) {
      u_info <- get_user_info()

      sql_ins <- "
        INSERT INTO coding_training (user_id, kategorie, anzahl_einheiten, accuracy, balanced_accuracy, iota_index, avg_iota2, cohens_kappa, kappa_linear, kappa_squared, fleiss_kappa, kendall_w, alpha_nominal, alpha_ordinal, gwet_ac1, gwet_ac2_quadratic, macro_precision, macro_recall, macro_f1)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
      "

      dbExecute(db_con, sql_ins,
                params = list(u_info$id, input$gewaehlte_kategorie, ziel_einheiten(), as.numeric(m$accuracy), as.numeric(m$balanced_accuracy), as.numeric(m$iota_index), as.numeric(m$avg_iota2), as.numeric(m$cohens_kappa), as.numeric(m$kappa_linear), as.numeric(m$kappa_squared), as.numeric(m$fleiss_kappa), as.numeric(m$kendall_w), as.numeric(m$alpha_nominal), as.numeric(m$alpha_ordinal), as.numeric(m$gwet_ac1), as.numeric(m$gwet_ac2_quadratic), as.numeric(m$macro_precision), as.numeric(m$macro_recall), as.numeric(m$macro_f1)))
      trigger_parent(trigger_parent() + 1)
    }

    observeEvent(input$start_schulung_btn, {
      lang <- get_lang()
      req(input$gewaehlte_kategorie, input$kodiereinheiten)
      zaehler(0); ziel_einheiten(input$kodiereinheiten); schulung_abgeschlossen(FALSE)
      meta_res <- dbGetQuery(db_con, "SELECT max_value, def_desc FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(input$gewaehlte_kategorie)))
      if (nrow(meta_res) > 0) {
        max_bewertung_wert(meta_res$max_value)
        desc <- if (is.na(meta_res$def_desc) || meta_res$def_desc == "") "" else as.character(meta_res$def_desc)
        beschreibung_text(desc)
      }
      pool <- generiere_garantierten_fall_pool(input$gewaehlte_kategorie, input$kodiereinheiten, max_bewertung_wert())
      fall_pool(pool)
      if (!is.null(pool) && nrow(pool) > 0) {
        aktueller_fall(pool[1, , drop = FALSE])
        schulung_gestartet(TRUE)
      } else {
        showNotification(tr("training.err_no_gold_cases", lang), type = "error")
      }
    })

    observeEvent(input$naechster_fall_btn, {
      lang <- get_lang()
      speichere_aktuelle_bewertung()
      neuer_wert <- zaehler() + 1
      if (neuer_wert >= ziel_einheiten()) {
        res_data <- hole_sitzungs_daten()
        m_stats <- berechne_aifeducation_metriken(user_vals = res_data$user_val, true_vals = res_data$true_val, max_val = max_bewertung_wert())
        speichere_schulungs_abschluss(m_stats)
        schulung_abgeschlossen(TRUE)
        showNotification(tr("training.toast_saved", lang), type = "message")
      } else {
        zaehler(neuer_wert)
        pool <- fall_pool()
        aktueller_fall(pool[neuer_wert + 1, , drop = FALSE])
        updateSliderInput(session, "bewertung_slider", value = 0)
      }
    })

    observeEvent(input$restart_schulung_btn, { schulung_gestartet(FALSE); schulung_abgeschlossen(FALSE); zaehler(0); fall_pool(NULL) })
    observeEvent(input$back_to_config_btn, { schulung_gestartet(FALSE); aktueller_fall(NULL); fall_pool(NULL) })

    output$session_summary_table <- renderTable({
      lang <- get_lang()
      req(input$gewaehlte_kategorie)
      u_info <- get_user_info()

      sql_sum <- "
        SELECT u.text_id AS c_id,
               COALESCE(s.Kurzreferenz, '-') AS c_src,
               u.bewertung AS c_user,
               ref.bewertung AS c_ref,
               (u.bewertung - ref.bewertung) AS c_diff
        FROM user_ratings u
        JOIN texts t ON u.text_id = t.text_id
        LEFT JOIN sources s ON t.source_id = s.source_id
        JOIN (
          SELECT r.text_id, r.bewertung,
                 ROW_NUMBER() OVER (
                   PARTITION BY r.text_id
                   ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC
                 ) AS rn
          FROM user_ratings r
          WHERE r.kategorie = ? AND r.typ IN ('gold_standard', 'kodierung')
        ) ref ON u.text_id = ref.text_id AND ref.rn = 1
        WHERE u.user_id = ? AND u.kategorie = ? AND u.typ = 'schulung'
        ORDER BY u.rating_id DESC LIMIT ?
      "
      params_all <- list(input$gewaehlte_kategorie, u_info$id, input$gewaehlte_kategorie, ziel_einheiten())
      df <- dbGetQuery(db_con, sql_sum, params = params_all)
      if (nrow(df) > 0 || ncol(df) == 5) {
        colnames(df) <- c(
          tr("training.col_text_id", lang),
          tr("coding.source_info", lang),
          tr("training.col_your_rating", lang),
          tr("training.col_gold", lang),
          tr("training.col_result", lang)
        )
      }
      df
    })

    # Download-Handler für die Ergebnistabelle der Schulung (CSV mit Semikolon & Text)
    output$download_session_csv <- downloadHandler(
      filename = function() {
        lang <- get_lang()
        paste0(tr("training.csv_filename_prefix", lang), "_", input$gewaehlte_kategorie, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        lang <- get_lang()
        u_info <- get_user_info()

        sql_dl <- "
          SELECT u.text_id AS c_id,
                 COALESCE(s.Kurzreferenz, '-') AS c_src,
                 t.text AS c_txt,
                 u.kategorie AS c_kat,
                 u.bewertung AS c_user,
                 ref.bewertung AS c_ref,
                 (u.bewertung - ref.bewertung) AS c_diff
          FROM user_ratings u
          JOIN texts t ON u.text_id = t.text_id
          LEFT JOIN sources s ON t.source_id = s.source_id
          JOIN (
            SELECT r.text_id, r.bewertung,
                   ROW_NUMBER() OVER (
                     PARTITION BY r.text_id
                     ORDER BY CASE WHEN r.typ = 'gold_standard' THEN 1 ELSE 2 END, r.rating_id DESC
                   ) AS rn
            FROM user_ratings r
            WHERE r.kategorie = ? AND r.typ IN ('gold_standard', 'kodierung')
          ) ref ON u.text_id = ref.text_id AND ref.rn = 1
          WHERE u.user_id = ? AND u.kategorie = ? AND u.typ = 'schulung'
          ORDER BY u.rating_id DESC LIMIT ?
        "
        params_all <- list(input$gewaehlte_kategorie, u_info$id, input$gewaehlte_kategorie, ziel_einheiten())
        df <- dbGetQuery(db_con, sql_dl, params = params_all)
        if (ncol(df) == 7) {
          colnames(df) <- c(
            tr("training.col_text_id", lang),
            tr("coding.source_info", lang),
            tr("coding.text_content", lang),
            tr("dashboard.col_scale", lang),
            tr("training.col_your_rating", lang),
            tr("training.col_gold", lang),
            tr("training.col_result", lang)
          )
        }
        write.table(df, file, sep = ";", dec = ",", row.names = FALSE, na = "", fileEncoding = "UTF-8")
      }
    )

    # --------------------------------------------------------------------------
    # BEISPIEL-MODAL LOGIK (typ = 'kodierung' oder 'gold_standard' über alle Nutzer)
    # --------------------------------------------------------------------------
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
        title = tags$div(tags$strong(paste0("💡 ", tr("examples.modal_title", lang))),
                         tags$br(),
                         tags$small(class = "text-muted", tr("examples.example_label", lang))),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("btn_anderes_beispiel"), tr("coding.btn_skip", lang), icon = icon("random"), class = "btn-primary"),
          modalButton(tr("examples.btn_close", lang))
        ),
        fluidPage(
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
      max_v <- 5
      df_m <- tryCatch({
        dbGetQuery(db_con, "SELECT max_value FROM scale_definitions WHERE scale_id = ?", params = list(as.integer(kat)))
      }, error = function(e) data.frame())
      if (nrow(df_m) > 0 && !is.na(df_m$max_value[1])) max_v <- as.integer(df_m$max_value[1])

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
        rand_fn <- get_rand_fn()
        bsp_query <- sprintf("
          SELECT DISTINCT t.text_id, t.text, t.source_id, r.typ AS bewertung_typ,
                 s.Kurzreferenz AS text_kurzreferenz,
                 s.Referenz AS text_referenz
          FROM texts t
          JOIN user_ratings r ON t.text_id = r.text_id
          LEFT JOIN sources s ON t.source_id = s.source_id
          WHERE r.typ IN ('kodierung', 'gold_standard') AND r.kategorie = ? AND r.bewertung = ?
          ORDER BY %s LIMIT 1
        ", rand_fn)
        bsp <- dbGetQuery(db_con, bsp_query, params = list(kat, st))

        is_gold <- !is.null(bsp$bewertung_typ) && bsp$bewertung_typ[1] == "gold_standard"
        typ_label <- if (is_gold) tr("admin.filter_type_gold", lang) else tr("admin.filter_type_kodierung", lang)
        typ_class <- if (is_gold) "badge bg-warning text-dark me-2" else "badge bg-primary me-2"

        tagList(
          div(class = "d-flex justify-content-between align-items-center mb-2",
              div(
                tags$span(class = "badge bg-success p-2 me-2", paste0(tr("common.level_n", lang, st), " · ", n_verfuegbar)),
                tags$span(class = paste(typ_class, "p-2"), typ_label)
              ),
              tags$span(class = "text-muted small", tr("common.case_number", lang, as.integer(bsp$text_id[1])))
          ),
          div(
            style = "max-height: 250px; overflow-y: auto; background-color: #f8f9fa; padding: 15px; border: 1px solid #ced4da; border-radius: 4px; font-family: monospace; white-space: pre-wrap;",
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
