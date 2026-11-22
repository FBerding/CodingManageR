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

test_that("kodierungenServer handles restricted-source sessions, zero coding sources, empty descriptions, and source-free examples", {
  # Verifies remaining kodierungenServer() edge cases:
  # 1) Non-reactive scalar current_lang = "en"
  # 2) Restricted coder ('user1', permitted text sources 1 & 2) starting a live coding session
  #    exercises the IN (...) source-filter SQL branch inside ziehe_unbewerteten_fall()
  # 3) Coder with text source permission but ZERO coding source permissions renders no_coding_sources_err
  # 4) Scale with empty def_desc and empty def_n_0 exercises no-description fallbacks
  # 5) Examples modal displaying a 'kodierung' (non-gold) rating on canonical DB (source_id = 1)
  #    and a source-free text (source_id = NULL) in an isolated legacy SQLite DB
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  user1_id <- get_user_info_db(con, "user1")$id
  eva_id   <- get_user_info_db(con, "eva_lindner")$id

  # Grant eva_lindner text source #1 ONLY (keep user_coding_source_permissions empty!)
  DBI::dbExecute(con, "INSERT INTO user_source_permissions (user_id, source_id) VALUES (?, 1)", params = list(eva_id))

  # Add scale #5 with empty def_desc & empty def_n_0, plus a canonical text (source_id = 1) with 'kodierung' rating
  DBI::dbExecute(
    con,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1)
     VALUES (5, 'EmptyDesc', 'K5_EmptyDesc', 1, '', '', 'Defined')"
  )
  DBI::dbExecute(con, "INSERT INTO user_scale_permissions (user_id, scale_id) VALUES (?, 5)", params = list(user1_id))
  DBI::dbExecute(con, "INSERT INTO texts (text_id, text, source_id) VALUES (960, 'Live coding example case.', 1)")
  DBI::dbExecute(
    con,
    "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id)
     VALUES (1, 960, '5', 1, 'kodierung', 1)"
  )

  scales_rx <- make_scale_reactives(con)
  rx_uid    <- shiny::reactiveVal(user1_id)
  rx_uname  <- shiny::reactiveVal("user1")

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
      current_lang = "en"
    ),
    {
      # 1. user1 starts live coding on scale #5 (restricted IN clause + empty def_desc/def_n_0)
      session$setInputs(
        gewaehlte_kategorie = "5",
        fall_filter_modus = "alle",
        gewaehlte_quelle = "1"
      )
      session$flushReact()
      session$setInputs(start_kodierung_btn = 1L)
      session$flushReact()
      expect_true(modus_aktiv())
      expect_true(aktueller_fall()$source_id[1] %in% c(1L, 2L))

      session$setInputs(bewertung_slider = 0L)
      session$flushReact()
      expect_true(nzchar(output$wert_definition))
      expect_true(nzchar(output$beschreibung_text_ui))

      # 2. Examples modal on scale #5 level 1: 'kodierung' badge (bg-primary) with assigned source
      session$setInputs(modal_beispiel_kategorie = "5", modal_gewaehlte_stufe = "1")
      session$flushReact()
      ex_html <- ui_to_html(output$modal_beispiel_content)
      expect_match(ex_html, "bg-primary")
      expect_match(ex_html, "Live coding example case")
      expect_match(ex_html, "text-primary")

      # Return to config screen and switch to eva_lindner (has text source #1, 0 coding sources)
      session$setInputs(back_to_select_btn = 1L)
      rx_uid(eva_id)
      rx_uname("eva_lindner")
      session$setInputs(gewaehlte_kategorie = "1", gewaehlte_quelle = "")
      session$flushReact()
      cfg_html <- ui_to_html(output$kodier_bereich)
      expect_match(cfg_html, "alert-danger")
      expect_match(cfg_html, "alert-warning")
      expect_match(ui_to_html(output$konfiguration_status_ui), "start_kodierung_disabled_btn")
    }
  )

  # 3. Isolated legacy SQLite DB (nullable texts.source_id) -> covers no_source_assigned fallback
  con_legacy <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_legacy))
  DBI::dbExecute(con_legacy, "CREATE TABLE user_tokens (user_id INT, username TEXT, token TEXT, permissions TEXT, is_active INT);")
  DBI::dbExecute(con_legacy, "CREATE TABLE sources (source_id INT, Kurzreferenz TEXT, Referenz TEXT, Lizenz TEXT);")
  DBI::dbExecute(con_legacy, "CREATE TABLE texts (text_id INT, text TEXT, source_id INT);")
  DBI::dbExecute(con_legacy, "CREATE TABLE scale_definitions (scale_id INT, short_label TEXT, name TEXT, max_value INT, def_desc TEXT, def_n_0 TEXT, def_n_1 TEXT);")
  DBI::dbExecute(con_legacy, "CREATE TABLE user_ratings (rating_id INT, user_id INT, text_id INT, kategorie TEXT, bewertung INT, typ TEXT, source_id INT);")
  DBI::dbExecute(con_legacy, "INSERT INTO user_tokens VALUES (1, 'admin', 'pw', 'admin', 1);")
  DBI::dbExecute(con_legacy, "INSERT INTO scale_definitions VALUES (1, 'Leg', 'K1_Legacy', 1, '', '', 'Defined');")
  DBI::dbExecute(con_legacy, "INSERT INTO texts VALUES (961, 'Source-free live coding example.', NULL);")
  DBI::dbExecute(con_legacy, "INSERT INTO user_ratings VALUES (1, 1, 961, '1', 1, 'kodierung', NULL);")

  leg_scales_rx <- make_scale_reactives(con_legacy)
  shiny::testServer(
    kodierungenServer,
    args = list(
      db_con = con_legacy,
      user_id_reactive = shiny::reactiveVal(1L),
      username_reactive = shiny::reactiveVal("admin"),
      kategorie_spalten = leg_scales_rx$kategorie_spalten,
      kategorien_liste = leg_scales_rx$kategorien_liste,
      trigger_parent = shiny::reactiveVal(0L),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = "en"
    ),
    {
      session$setInputs(modal_beispiel_kategorie = "1", modal_gewaehlte_stufe = "1")
      session$flushReact()
      leg_ex_html <- ui_to_html(output$modal_beispiel_content)
      expect_match(leg_ex_html, "bg-primary")
      expect_match(leg_ex_html, "Source-free live coding example")
      expect_match(leg_ex_html, "text-muted")
    }
  )
})


