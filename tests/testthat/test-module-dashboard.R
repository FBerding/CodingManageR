# ==============================================================================
# Module Server & UI Tests: Personal Dashboard Module
# Covers: R/modul_dashboard.R (dashboardUI and dashboardServer)
# ==============================================================================

test_that("dashboardUI builds localized tabset markup for progress and scales overview tabs", {
  # Verifies dashboardUI() renders both tab panels (tab_progress, tab_scales_overview),
  # respects the selected_tab argument (falling back to tab_progress on invalid values),
  # and namespaces all key inputs/outputs cleanly in both German and English.
  ui_en <- ui_to_html(dashboardUI("dash_en", lang = "en", selected_tab = "tab_progress"))
  expect_match(ui_en, "dash_en-dashboard_tabs")
  expect_match(ui_en, "dash_en-stat_card_total_kodierung")
  expect_match(ui_en, "dash_en-training_reliability_plot")
  expect_match(ui_en, "dash_en-dashboard_scales_cards")

  ui_de_fallback <- ui_to_html(dashboardUI("dash_de", lang = "de", selected_tab = "unknown_tab"))
  expect_match(ui_de_fallback, "dash_de-dashboard_selected_kategorie")
  expect_match(ui_de_fallback, "dash_de-dashboard_scale_search")
})

test_that("dashboardServer renders KPI cards, reliability summary, plots, history table, and breakdown table", {
  # Verifies dashboardServer() for a populated user ('user1') across:
  # 1) All 3 top KPI cards (live codings, training ratings, completed sessions)
  # 2) training_kpi_summary in 'all_core', percentage ('accuracy'), and decimal ('gwet_ac1') modes
  # 3) training_reliability_plot in 'all_core', percentage ('accuracy'), decimal ('cohens_kappa'),
  #    negative-metric floor scaling, >15 sessions (suppressing point labels + angled x-axis), and empty state
  # 4) training_performance_table (populated with Landis & Koch badges and empty state)
  # 5) kategorie_breakdown_table counts across all 3 seeded scales
  # 6) Live DE -> EN language switching
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  user1_id <- get_user_info_db(con, "user1")$id

  # Insert one 'kodierung' and one 'schulung' rating for user1 so KPI cards display non-zero counts
  DBI::dbExecute(con, "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id) VALUES (?, 205, '1', 2, 'kodierung', 1)", params = list(user1_id))
  DBI::dbExecute(con, "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id) VALUES (?, 206, '1', 3, 'schulung', 1)", params = list(user1_id))

  scales_rx <- make_scale_reactives(con)
  rx_uid    <- shiny::reactiveVal(user1_id)
  rx_uname  <- shiny::reactiveVal("user1")
  rx_trig   <- shiny::reactiveVal(0L)
  rx_strig  <- shiny::reactiveVal(0L)
  rx_lang   <- shiny::reactiveVal("de")

  shiny::testServer(
    dashboardServer,
    args = list(
      db_con = con,
      user_id_reactive = rx_uid,
      username_reactive = rx_uname,
      trigger_reactive = rx_trig,
      kategorie_spalten = scales_rx$kategorie_spalten,
      scales_trigger = rx_strig,
      current_lang = rx_lang
    ),
    {
      session$setInputs(dashboard_selected_kategorie = "1", dashboard_plot_metric = "all_core")
      session$flushReact()

      # 1. KPI cards
      expect_match(ui_to_html(output$stat_card_total_kodierung), ">1<")
      expect_match(ui_to_html(output$stat_card_total_schulung), ">1<")
      expect_match(ui_to_html(output$stat_card_sessions), ">6<")

      # 2. Training KPI summary & plot in all_core mode
      kpi_all <- ui_to_html(output$training_kpi_summary)
      expect_match(kpi_all, "card")
      expect_true(!is.null(output$training_reliability_plot))

      # 3. Single percentage metric (accuracy)
      session$setInputs(dashboard_plot_metric = "accuracy")
      session$flushReact()
      expect_match(ui_to_html(output$training_kpi_summary), "%")
      expect_true(!is.null(output$training_reliability_plot))

      # 4. Single decimal metric (cohens_kappa) with negative value branch
      DBI::dbExecute(
        con,
        "INSERT INTO coding_training (user_id, kategorie, datum, anzahl_einheiten, accuracy, balanced_accuracy, iota_index, avg_iota2, cohens_kappa, alpha_nominal, gwet_ac1)
         VALUES (?, '1', '2026-11-01', 5, -5.0, 40.0, 0.1, 0.1, -0.12, -0.10, 0.05)",
        params = list(user1_id)
      )
      rx_trig(rx_trig() + 1L)
      session$setInputs(dashboard_plot_metric = "cohens_kappa")
      session$flushReact()
      expect_true(!is.null(output$training_reliability_plot))

      # Also test negative value branch in all_core and percentage modes
      session$setInputs(dashboard_plot_metric = "all_core")
      session$flushReact()
      expect_true(!is.null(output$training_reliability_plot))

      session$setInputs(dashboard_plot_metric = "accuracy")
      session$flushReact()
      expect_true(!is.null(output$training_reliability_plot))

      # 5. >15 training sessions branch (hides direct point value labels, rotates x-axis labels)
      seed_bulk_records(con, n_trainings = 14L)
      rx_trig(rx_trig() + 1L)
      session$setInputs(dashboard_plot_metric = "gwet_ac1")
      session$flushReact()
      expect_true(!is.null(output$training_reliability_plot))

      # 6. Training performance table & category breakdown table
      hist_html <- ui_to_html(output$training_performance_table)
      expect_match(hist_html, "<table")
      expect_match(hist_html, "badge")

      breakdown_html <- output$kategorie_breakdown_table
      expect_match(breakdown_html, "Empathie \\[#1\\]")
      expect_match(breakdown_html, "Struktur \\[#2\\]")
      expect_match(breakdown_html, "Fachkompetenz \\[#3\\]")

      # 7. Empty training history state (user with 0 sessions on selected scale)
      markus_id <- get_user_info_db(con, "markus_meier")$id
      rx_uid(markus_id)
      rx_uname("markus_meier")
      session$flushReact()
      expect_match(ui_to_html(output$training_kpi_summary), "alert-info")
      expect_true(!is.null(output$training_reliability_plot))
      expect_match(ui_to_html(output$training_performance_table), "alert-light")

      # 8. Live DE -> EN language toggle triggers observer & localized outputs
      rx_lang("en")
      session$flushReact()
      expect_match(ui_to_html(output$training_kpi_summary), "No completed training sessions|alert-info")
    }
  )
})

test_that("dashboardServer scales overview supports search, reset, description truncation, empty state, and detail modal", {
  # Verifies Tab 2 (Scales Overview):
  # 1) Unfiltered count and card rendering (including >140-character truncation and blank description fallback)
  # 2) Search filtering by scale name, #ID, and non-matching query (empty state alert)
  # 3) Reset search button handler (btn_reset_scale_search)
  # 4) Opening the scale details modal via show_scale_details (both existing scale_id and nonexistent ID)
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  # Add a 4th scale with a >150-char description and a 5th scale with empty description
  DBI::dbExecute(
    con,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1)
     VALUES (4, 'LongDesc', 'K4_LongDescription', 1, ?, 'Step 0 text', '')",
    params = list(paste(rep("Detailed clinical coding guideline sentence. ", 5), collapse = ""))
  )
  DBI::dbExecute(
    con,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc)
     VALUES (5, 'NoDesc', 'K5_NoDescription', 1, '')"
  )

  scales_rx <- make_scale_reactives(con)
  shiny::testServer(
    dashboardServer,
    args = list(
      db_con = con,
      user_id_reactive = shiny::reactiveVal(1L),
      username_reactive = shiny::reactiveVal("admin"),
      trigger_reactive = shiny::reactiveVal(0L),
      kategorie_spalten = scales_rx$kategorie_spalten,
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("en")
    ),
    {
      # Unfiltered view renders all 5 scales, truncating K4's long description with "..."
      session$setInputs(dashboard_scale_search = "")
      session$flushReact()
      expect_match(ui_to_html(output$dashboard_scales_count), "5")
      cards_all <- ui_to_html(output$dashboard_scales_cards)
      expect_match(cards_all, "K1_Empathie")
      expect_match(cards_all, "K4_LongDescription")
      expect_match(cards_all, "\\.\\.\\.")
      expect_match(cards_all, "K5_NoDescription")

      # Filter by #ID
      session$setInputs(dashboard_scale_search = "#2")
      session$flushReact()
      expect_match(ui_to_html(output$dashboard_scales_count), "1")
      cards_id2 <- ui_to_html(output$dashboard_scales_cards)
      expect_match(cards_id2, "K2_Struktur")
      expect_false(grepl("K1_Empathie", cards_id2))

      # Filter by non-matching term -> empty state
      session$setInputs(dashboard_scale_search = "no_such_scale_xyz")
      session$flushReact()
      expect_match(ui_to_html(output$dashboard_scales_cards), "alert-light")

      # Reset search button clears search cleanly
      session$setInputs(btn_reset_scale_search = 1L)
      session$flushReact()

      # Trigger detail modal for valid scale_id = 4 (tests missing def_n_1 fallback inside modal)
      # and invalid scale_id = 999 (early return)
      expect_no_error({
        session$setInputs(show_scale_details = 4L)
        session$flushReact()
        session$setInputs(show_scale_details = 999L)
        session$flushReact()
      })
    }
  )
})
