# ==============================================================================
# Module Server & UI Tests: Live Case Coding Module
# Covers: R/modul_codings.R (kodierungenUI and kodierungenServer)
# ==============================================================================

test_that("kodierungenUI builds namespaced coding workspace container", {
  # Verifies kodierungenUI() creates the expected namespaced uiOutput container.
  expect_match(ui_to_html(kodierungenUI("cod_mod")), "cod_mod-kodier_bereich")
})

test_that("kodierungenServer enforces RBAC permissions for admin, restricted user1, and zero-permission users", {
  # Verifies role-based access control in kodierungenServer():
  # 1) Administrator sees all 3 scales and all 4 text/coding sources with active start button
  # 2) Restricted coder ('user1') sees only scales 1 & 2 and sources 1 & 2
  # 3) Coder with zero scale permissions ('markus_meier') sees lockout alert and disabled start button
  # 4) Coder with scale permission but zero source permissions ('eva_lindner') sees source-lockout alerts
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  scales_rx <- make_scale_reactives(con)

  admin_id  <- get_user_info_db(con, "admin")$id
  user1_id  <- get_user_info_db(con, "user1")$id
  markus_id <- get_user_info_db(con, "markus_meier")$id
  eva_id    <- get_user_info_db(con, "eva_lindner")$id

  rx_uid   <- shiny::reactiveVal(admin_id)
  rx_uname <- shiny::reactiveVal("admin")

  shiny::testServer(
    kodierungenServer,
    args = list(
      db_con = con,
      user_id_reactive = rx_uid,
      username_reactive = rx_uname,
      kategorie_spalten = scales_rx$kategorie_spalten,
      kategorien_liste = scales_rx$kategorien_liste,
      trigger_parent = shiny::reactiveVal(0L),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("en")
    ),
    {
      # 1. Admin gets all 3 scales and unrestricted NULL source lists
      session$setInputs(gewaehlte_kategorie = "1", fall_filter_modus = "alle", gewaehlte_quelle = "1")
      session$flushReact()
      expect_length(get_permitted_kategorien_liste(), 3L)
      expect_null(ermittle_erlaubte_quellen(admin_id))
      expect_null(ermittle_erlaubte_kodier_quellen(admin_id))
      expect_match(ui_to_html(output$konfiguration_status_ui), "start_kodierung_btn")

      # 2. user1 gets scales 1 & 2 and sources 1 & 2
      rx_uid(user1_id)
      rx_uname("user1")
      session$flushReact()
      expect_equal(unname(get_permitted_kategorien_liste()), c("1", "2"))
      expect_equal(sort(ermittle_erlaubte_quellen(user1_id)), c(1L, 2L))
      expect_match(ui_to_html(output$konfiguration_status_ui), "start_kodierung_btn")

      # 3. markus_meier has 0 scale permissions -> lockout alert & disabled button
      rx_uid(markus_id)
      rx_uname("markus_meier")
      session$flushReact()
      expect_length(get_permitted_kategorien_liste(), 0L)
      expect_match(ui_to_html(output$konfiguration_status_ui), "start_kodierung_disabled_btn")

      # 4. eva_lindner has scale 1 permission but 0 text/coding source permissions -> danger alerts & disabled button
      rx_uid(eva_id)
      rx_uname("eva_lindner")
      session$setInputs(gewaehlte_kategorie = "1", gewaehlte_quelle = "")
      session$flushReact()
      expect_equal(unname(get_permitted_kategorien_liste()), "1")
      expect_equal(zaehle_offene_faelle("1", eva_id), 0L)
      expect_match(ui_to_html(output$kodier_bereich), "alert-danger")
      expect_match(ui_to_html(output$konfiguration_status_ui), "start_kodierung_disabled_btn")
    }
  )
})

test_that("kodierungenServer supports source-filtered coding, saves ratings to DB, exhausts pool, and opens examples", {
  # Verifies active live coding lifecycle in kodierungenServer():
  # 1) Starting coding in 'quelle' mode targeting a single source (source_id = 4 with 2 freshly added texts)
  # 2) Inspecting active case text, stage definition (wert_definition), and scale overview (beschreibung_text_ui)
  # 3) Saving both cases via save_next_btn, verifying DB inserts into user_ratings (typ = 'kodierung'),
  #    trigger_parent increments, and automatic transition to the "all done" state when pool reaches 0
  # 4) Returning to configuration screen via back_to_select_btn and starting in 'alle' mode
  # 5) Opening the Examples modal (btn_beispiele_config / btn_beispiele), viewing populated & empty levels, and skipping
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  admin_id <- get_user_info_db(con, "admin")$id

  # Insert exactly 2 texts linked to source_id = 4 so we can deterministically exhaust the source pool
  DBI::dbExecute(con, "INSERT INTO texts (text_id, text, source_id) VALUES (901, 'Source 4 Case A for live coding.', 4)")
  DBI::dbExecute(con, "INSERT INTO texts (text_id, text, source_id) VALUES (902, 'Source 4 Case B for live coding.', 4)")

  scales_rx <- make_scale_reactives(con)
  rx_trig   <- shiny::reactiveVal(0L)

  shiny::testServer(
    kodierungenServer,
    args = list(
      db_con = con,
      user_id_reactive = shiny::reactiveVal(admin_id),
      username_reactive = shiny::reactiveVal("admin"),
      kategorie_spalten = scales_rx$kategorie_spalten,
      kategorien_liste = scales_rx$kategorien_liste,
      trigger_parent = rx_trig,
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("de")
    ),
    {
      # Configure source-specific coding for scale 1, text source = 4, coding source = 1
      session$setInputs(
        gewaehlte_kategorie = "1",
        fall_filter_modus = "quelle",
        gewaehlte_text_quelle = "4",
        gewaehlte_quelle = "1"
      )
      session$flushReact()
      expect_equal(max_bewertung_wert(), 4L)
      expect_match(ui_to_html(output$konfiguration_status_ui), "2")

      # Start coding session
      session$setInputs(start_kodierung_btn = 1L)
      session$flushReact()
      expect_true(modus_aktiv())
      expect_equal(verbleibende_faelle(), 2L)
      expect_true(aktueller_fall()$text_id[1] %in% c(901L, 902L))

      # Inspect active UI, stage definition, and general scale description
      session$setInputs(bewertung_slider = 3L)
      session$flushReact()
      expect_match(ui_to_html(output$kodier_bereich), "save_next_btn")
      expect_true(nzchar(output$wert_definition))
      expect_true(nzchar(output$beschreibung_text_ui))

      # Save first case
      first_tid <- aktueller_fall()$text_id[1]
      session$setInputs(save_next_btn = 1L)
      session$flushReact()

      expect_equal(rx_trig(), 1L)
      expect_equal(verbleibende_faelle(), 1L)
      saved1 <- DBI::dbGetQuery(
        con,
        "SELECT bewertung, typ, source_id FROM user_ratings WHERE text_id = ? AND kategorie = '1' AND typ = 'kodierung'",
        params = list(first_tid)
      )
      expect_equal(nrow(saved1), 1L)
      expect_equal(saved1$bewertung[1], 3L)
      expect_equal(saved1$source_id[1], 1L)

      # Save second (final) case -> pool exhausted -> all-done panel displayed
      session$setInputs(bewertung_slider = 4L)
      session$setInputs(save_next_btn = 2L)
      session$flushReact()

      expect_equal(rx_trig(), 2L)
      expect_equal(verbleibende_faelle(), 0L)
      expect_null(aktueller_fall())
      expect_match(ui_to_html(output$kodier_bereich), "back_to_select_btn")

      # Return to config screen -> konfiguration_status_ui now warns 0 cases left for source 4
      session$setInputs(back_to_select_btn = 1L)
      session$flushReact()
      expect_false(modus_aktiv())
      expect_match(ui_to_html(output$konfiguration_status_ui), "alert-warning")
      expect_match(ui_to_html(output$konfiguration_status_ui), "start_kodierung_disabled_btn")

      # Start again in "alle" mode (random across all sources)
      session$setInputs(fall_filter_modus = "alle", gewaehlte_quelle = "2")
      session$setInputs(start_kodierung_btn = 2L)
      session$flushReact()
      expect_true(modus_aktiv())
      expect_gt(verbleibende_faelle(), 0L)
      expect_match(ui_to_html(output$kodier_bereich), "bg-secondary")

      # Examples modal: open, inspect populated level 3, empty level 0, and skip button
      expect_no_error({
        session$setInputs(btn_beispiele = 1L)
        session$setInputs(btn_beispiele_config = 1L)
        session$flushReact()
      })
      session$setInputs(modal_beispiel_kategorie = "1")
      session$flushReact()
      expect_match(ui_to_html(output$modal_stufen_selector), "modal_gewaehlte_stufe")

      session$setInputs(modal_gewaehlte_stufe = "3")
      session$flushReact()
      expect_match(ui_to_html(output$modal_beispiel_content), "bg-warning|bg-primary")

      session$setInputs(modal_gewaehlte_stufe = "0")
      session$flushReact()
      expect_match(ui_to_html(output$modal_beispiel_content), "alert-warning")

      prev_off <- beispiel_offset()
      session$setInputs(btn_anderes_beispiel = 1L)
      session$flushReact()
      expect_equal(beispiel_offset(), prev_off + 1L)
    }
  )
})
