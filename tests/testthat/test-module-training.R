# ==============================================================================
# Module Server & UI Tests: Coding Training & Calibration Module
# Covers: R/modul_training.R (schulungUI and schulungServer)
# ==============================================================================

test_that("schulungUI renders namespaced dynamic container", {
  # Verifies schulungUI() produces the expected namespaced uiOutput container.
  html <- ui_to_html(schulungUI("train_mod"))
  expect_match(html, "train_mod-schulungs_bereich")
})

test_that("schulungServer runs complete stratified training workflow, persists metrics, and exports CSV", {
  # End-to-end server test of schulungServer() covering:
  # 1) Initial configuration screen & dynamic minimum units calculation (max_value + 1)
  # 2) Error path when starting training on a scale with 0 gold-standard cases
  # 3) Starting a stratified training session on K2_Struktur (max_value = 3, 4 units)
  # 4) Inspecting active case text, stage definition (wert_definition), and abort flow (back_to_config_btn)
  # 5) Completing all 4 training items via naechster_fall_btn, verifying:
  #    - 4 'schulung' rows written to user_ratings
  #    - 1 reliability summary row written to coding_training
  #    - trigger_parent incremented
  #    - external_metrics_panel renders computed aifeducation metrics
  #    - session_summary_table renders 4 rows with localized columns
  #    - download_session_csv writes a valid semicolon-delimited UTF-8 CSV with 7 columns
  # 6) Restarting training resets reactive state cleanly
  testthat::skip_if_not_installed("aifeducation")

  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  seed_full_gold_standards(con, scale_id = 2L, max_value = 3L)

  # Insert an extra scale #4 with NO gold standards to test empty-pool error notification
  DBI::dbExecute(
    con,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc)
     VALUES (4, 'EmptyScale', 'K4_EmptyScale', 2, 'Scale without gold standards')"
  )

  user1_id <- get_user_info_db(con, "user1")$id
  scales_rx <- make_scale_reactives(con)
  rx_trig   <- shiny::reactiveVal(0L)
  rx_strig  <- shiny::reactiveVal(0L)
  rx_lang   <- shiny::reactiveVal("en")

  shiny::testServer(
    schulungServer,
    args = list(
      db_con = con,
      user_id_reactive = shiny::reactiveVal(user1_id),
      username_reactive = shiny::reactiveVal("user1"),
      kategorie_spalten = scales_rx$kategorie_spalten,
      kategorien_liste = scales_rx$kategorien_liste,
      trigger_parent = rx_trig,
      scales_trigger = rx_strig,
      current_lang = rx_lang
    ),
    {
      # 1. Config UI renders before start
      session$setInputs(gewaehlte_kategorie = "2")
      session$flushReact()
      expect_false(schulung_gestartet())
      expect_equal(max_bewertung_wert(), 3L)
      expect_match(ui_to_html(output$schulungs_bereich), "start_schulung_btn")
      expect_match(ui_to_html(output$dynamischer_einheiten_input), "min=\"4\"")

      # 2. Starting on scale #4 (no gold standards) keeps schulung_gestartet == FALSE
      session$setInputs(gewaehlte_kategorie = "4", kodiereinheiten = 3L)
      session$setInputs(start_schulung_btn = 1L)
      session$flushReact()
      expect_false(schulung_gestartet())

      # 3. Start valid training on scale #2 with 4 units (covering levels 0..3)
      session$setInputs(gewaehlte_kategorie = "2", kodiereinheiten = 4L)
      session$setInputs(start_schulung_btn = 2L)
      session$flushReact()

      expect_true(schulung_gestartet())
      expect_false(schulung_abgeschlossen())
      expect_equal(nrow(fall_pool()), 4L)
      expect_equal(nrow(aktueller_fall()), 1L)

      # Verify active case UI and stage definition output
      session$setInputs(bewertung_slider = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$schulungs_bereich), "naechster_fall_btn")
      expect_true(nzchar(output$wert_definition))

      # Verify abort button returns to config cleanly
      session$setInputs(back_to_config_btn = 1L)
      session$flushReact()
      expect_false(schulung_gestartet())
      expect_null(aktueller_fall())

      # 4. Restart and complete all 4 items
      before_ratings   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM user_ratings WHERE typ = 'schulung'")$n[1]
      before_trainings <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM coding_training")$n[1]
      before_trig      <- rx_trig()

      session$setInputs(gewaehlte_kategorie = "2", kodiereinheiten = 4L)
      session$setInputs(start_schulung_btn = 3L)
      session$flushReact()

      for (step in 1:4) {
        session$setInputs(bewertung_slider = (step - 1L) %% 4L)
        session$setInputs(naechster_fall_btn = step)
        session$flushReact()
      }

      expect_true(schulung_abgeschlossen())
      expect_equal(rx_trig(), before_trig + 1L)

      after_ratings   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM user_ratings WHERE typ = 'schulung'")$n[1]
      after_trainings <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM coding_training")$n[1]
      expect_equal(after_ratings, before_ratings + 4L)
      expect_equal(after_trainings, before_trainings + 1L)

      # 5. Completed summary screen, aifeducation metrics panel, summary table, and CSV downloadHandler
      expect_match(ui_to_html(output$schulungs_bereich), "restart_schulung_btn")
      metrics_html <- ui_to_html(output$external_metrics_panel)
      expect_match(metrics_html, "<code>")
      expect_match(metrics_html, "%")

      expect_match(output$session_summary_table, "<table")

      csv_file <- output$download_session_csv
      expect_true(file.exists(csv_file))
      dl_df <- utils::read.table(csv_file, sep = ";", dec = ",", header = TRUE, fileEncoding = "UTF-8")
      expect_equal(nrow(dl_df), 4L)
      expect_equal(ncol(dl_df), 7L)

      # 6. Restart button resets back to config screen
      session$setInputs(restart_schulung_btn = 1L)
      session$flushReact()
      expect_false(schulung_gestartet())
      expect_false(schulung_abgeschlossen())
      expect_equal(zaehler(), 0L)
    }
  )
})

test_that("schulungServer examples modal displays reference cases, handles empty levels, and cycles via skip", {
  # Verifies the reference Examples modal inside schulungServer():
  # 1) Opening from config screen (btn_beispiele_config) and active screen (btn_beispiele)
  # 2) modal_stufen_selector adapts choices to the selected scale's max_value
  # 3) modal_beispiel_content renders a gold_standard/kodierung case with source citation when available,
  #    and renders a warning alert when no example exists for a given scale level
  # 4) btn_anderes_beispiel increments beispiel_offset to cycle examples
  # 5) external_metrics_panel renders a warning alert when <2 training ratings exist
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  scales_rx <- make_scale_reactives(con)

  shiny::testServer(
    schulungServer,
    args = list(
      db_con = con,
      user_id_reactive = shiny::reactiveVal(1L),
      username_reactive = shiny::reactiveVal("admin"),
      kategorie_spalten = scales_rx$kategorie_spalten,
      kategorien_liste = scales_rx$kategorien_liste,
      trigger_parent = shiny::reactiveVal(0L),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("de")
    ),
    {
      # Fewer than 2 training ratings -> external_metrics_panel shows warning alert
      session$setInputs(gewaehlte_kategorie = "1")
      ziel_einheiten(4L)
      session$flushReact()
      expect_match(ui_to_html(output$external_metrics_panel), "alert-warning")

      # Open examples modal from config and active buttons
      expect_no_error({
        session$setInputs(btn_beispiele_config = 1L)
        session$setInputs(btn_beispiele = 1L)
        session$flushReact()
      })

      # Scale 1 has gold_standard ratings for levels 2, 3, 4; level 0 has none (only 'field' ratings)
      session$setInputs(modal_beispiel_kategorie = "1")
      session$flushReact()
      expect_match(ui_to_html(output$modal_stufen_selector), "modal_gewaehlte_stufe")

      # Populated gold_standard level (level 3 on scale 1 -> texts 101, 104)
      session$setInputs(modal_gewaehlte_stufe = "3")
      session$flushReact()
      ex_ok <- ui_to_html(output$modal_beispiel_content)
      expect_match(ex_ok, "bg-warning|bg-primary")
      expect_match(ex_ok, "Mueller2024|ICD-10-GM-2025")

      # Empty level (level 0 on scale 1 has no gold_standard/kodierung ratings)
      session$setInputs(modal_gewaehlte_stufe = "0")
      session$flushReact()
      expect_match(ui_to_html(output$modal_beispiel_content), "alert-warning")

      # Skip button increments offset
      prev_off <- beispiel_offset()
      session$setInputs(btn_anderes_beispiel = 1L)
      session$flushReact()
      expect_equal(beispiel_offset(), prev_off + 1L)
    }
  )
})

test_that("schulungServer handles rest-pool backfill, empty descriptions, scalar language, and non-gold source-free examples", {
  # Verifies remaining schulungServer() edge cases:
  # 1) Non-reactive scalar current_lang = "en"
  # 2) Starting training with kodiereinheiten > max_value + 1 (e.g., 6 units on max_value = 3)
  #    exercises the secondary rest_faelle backfill SQL query & rbind()
  # 3) Scale with empty def_desc and empty def_n_0 triggers no-description fallbacks
  # 4) Examples modal displaying a 'kodierung' (non-gold) rating linked to a text with source_id = NULL
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  seed_full_gold_standards(con, scale_id = 2L, max_value = 3L)

  # Scale #5 with empty def_desc / empty def_n_0 + one source-free text with a 'kodierung' rating
  DBI::dbExecute(
    con,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1)
     VALUES (5, 'Bare', 'K5_BareScale', 1, '', '', 'Present')"
  )
  DBI::dbExecute(con, "INSERT INTO texts (text_id, text, source_id) VALUES (950, 'Source-free training example case.', NULL)")
  DBI::dbExecute(
    con,
    "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id)
     VALUES (1, 950, '5', 1, 'kodierung', NULL)"
  )

  scales_rx <- make_scale_reactives(con)

  shiny::testServer(
    schulungServer,
    args = list(
      db_con = con,
      user_id_reactive = shiny::reactiveVal(1L),
      username_reactive = shiny::reactiveVal("admin"),
      kategorie_spalten = scales_rx$kategorie_spalten,
      kategorien_liste = scales_rx$kategorien_liste,
      trigger_parent = shiny::reactiveVal(0L),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = "en"
    ),
    {
      # 1. Start session with 6 units on scale #2 (max_value = 3 -> 4 forced + 2 rest_faelle)
      session$setInputs(gewaehlte_kategorie = "2", kodiereinheiten = 6L)
      session$setInputs(start_schulung_btn = 1L)
      session$flushReact()
      expect_true(schulung_gestartet())
      expect_equal(nrow(fall_pool()), 6L)

      # Abort back to config
      session$setInputs(back_to_config_btn = 1L)
      session$flushReact()

      # 2. Scale #5 has empty def_desc and empty def_n_0
      session$setInputs(gewaehlte_kategorie = "5", kodiereinheiten = 2L)
      session$setInputs(start_schulung_btn = 2L)
      session$flushReact()
      expect_true(schulung_gestartet())
      expect_equal(beschreibung_text(), "")
      session$setInputs(bewertung_slider = 0L)
      session$flushReact()
      expect_true(nzchar(output$wert_definition))

      # 3. Examples modal for scale #5 level 1: 'kodierung' badge (bg-primary) + no_source_assigned
      session$setInputs(modal_beispiel_kategorie = "5", modal_gewaehlte_stufe = "1")
      session$flushReact()
      ex_html <- ui_to_html(output$modal_beispiel_content)
      expect_match(ex_html, "bg-primary")
      expect_match(ex_html, "Source-free training example case")
      expect_match(ex_html, "text-muted")
    }
  )
})

