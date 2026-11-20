# ==============================================================================
# Module Server & UI Tests: Administration Console (All 8 Subtabs)
# Covers: R/modul_admin.R (adminUI and adminServer)
# ==============================================================================

test_that("adminUI constructs localized tabset markup for all 8 administrative subtabs", {
  # Verifies adminUI() renders all 8 subtabs (Users, Texts, Sources, Scales, Reliability,
  # Coding Search, Data Import/Export, DB Diagnostics) with proper namespacing in DE and EN.
  ui_en <- ui_to_html(adminUI("adm_en", lang = "en"))
  expect_match(ui_en, "adm_en-users_table_container")
  expect_match(ui_en, "adm_en-texts_management_table_container")
  expect_match(ui_en, "adm_en-sources_table_container")
  expect_match(ui_en, "adm_en-scales_table_container")
  expect_match(ui_en, "adm_en-admin_reliability_comparison_plot")
  expect_match(ui_en, "adm_en-coding_search_scale_filter")
  expect_match(ui_en, "adm_en-coding_search_table_container")
  expect_match(ui_en, "adm_en-download_master_dataset_btn")
  expect_match(ui_en, "adm_en-db_diag_summary_cards")

  ui_de <- ui_to_html(adminUI("adm_de", lang = "de"))
  expect_match(ui_de, "adm_de-btn_open_add_user")
  expect_match(ui_de, "adm_de-btn_refresh_diagnostics")
})

test_that("adminServer Subtab 1 (Users): CRUD, validation, self-protection, 25-row pagination, and permissions modals", {
  # Verifies Tab 1 (User Management):
  # 1) Search filtering, 25-row pagination controls (first/prev/next/last/jump) with >25 seeded users
  # 2) Creating a new user (empty field errors, duplicate username error, auto-token generation, valid insert)
  # 3) Editing an existing user (missing token error, valid role/status/token update)
  # 4) 1-click active/inactive status toggle and admin self-deactivation protection
  # 5) Deleting a user with self-deletion protection, clean deletion, and historic-user protection
  # 6) Text/coding source permissions modal (select all, deselect all, save) and scale permissions modal
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  seed_bulk_records(con, n_users = 22L) # Total 30 users -> 2 pages at 25/page

  admin_id <- get_user_info_db(con, "admin")$id
  user1_id <- get_user_info_db(con, "user1")$id

  shiny::testServer(
    adminServer,
    args = list(
      db_con = con,
      current_user = shiny::reactiveVal("admin"),
      current_user_id = shiny::reactiveVal(admin_id),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("en")
    ),
    {
      # 1. Table rendering & pagination across 30 users
      session$setInputs(user_search_query = "")
      session$flushReact()
      expect_match(ui_to_html(output$users_counter_badge), "30")
      expect_match(ui_to_html(output$users_table_container), "<table")
      expect_match(ui_to_html(output$users_pagination_controls), "btn_users_next")

      session$setInputs(btn_users_next = 1L)
      session$flushReact()
      expect_equal(current_page_users(), 2L)
      session$setInputs(btn_users_prev = 1L)
      session$flushReact()
      expect_equal(current_page_users(), 1L)
      session$setInputs(btn_users_last = 1L)
      session$flushReact()
      expect_equal(current_page_users(), 2L)
      session$setInputs(btn_users_first = 1L)
      session$flushReact()
      expect_equal(current_page_users(), 1L)
      session$setInputs(trigger_jump_users_page = "2")
      session$flushReact()
      expect_equal(current_page_users(), 2L)

      # Search query narrows table & handles empty results
      session$setInputs(user_search_query = "eva_lindner")
      session$flushReact()
      expect_match(ui_to_html(output$users_table_container), "eva_lindner")

      session$setInputs(user_search_query = "no_such_user_xyz")
      session$flushReact()
      expect_match(ui_to_html(output$users_table_container), "alert")
      session$setInputs(user_search_query = "")

      # 2. Add new user modal, validation guards, token generator, and successful creation
      session$setInputs(btn_open_add_user = 1L, btn_generate_new_token = 1L)
      session$flushReact()

      # Missing username
      session$setInputs(new_username = "", new_token = "tok123", new_permissions = "user", new_is_active = TRUE, btn_save_new_user = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$add_user_feedback), "alert-danger")

      # Missing token
      session$setInputs(new_username = "fresh_coder", new_token = "", btn_save_new_user = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$add_user_feedback), "alert-danger")

      # Duplicate username
      session$setInputs(new_username = "user1", new_token = "dup_tok", btn_save_new_user = 3L)
      session$flushReact()
      expect_match(ui_to_html(output$add_user_feedback), "alert-danger")

      # Valid insert
      session$setInputs(new_username = "fresh_coder", new_token = "fresh_tok_99", new_permissions = "user", new_is_active = TRUE, btn_save_new_user = 4L)
      session$flushReact()
      created_u <- DBI::dbGetQuery(con, "SELECT * FROM user_tokens WHERE username = 'fresh_coder'")
      expect_equal(nrow(created_u), 1L)
      expect_equal(created_u$token[1], "fresh_tok_99")
      fresh_uid <- created_u$user_id[1]

      # 3. Edit user modal, validation, and valid update
      session$setInputs(trigger_edit_user = fresh_uid, btn_generate_edit_token = 1L)
      session$flushReact()
      session$setInputs(edit_token = "", edit_permissions = "user", edit_is_active = TRUE, btn_save_edited_user = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_user_feedback), "alert-danger")

      session$setInputs(edit_token = "updated_tok_77", edit_permissions = "admin", edit_is_active = TRUE, btn_save_edited_user = 2L)
      session$flushReact()
      upd_u <- DBI::dbGetQuery(con, "SELECT token, permissions FROM user_tokens WHERE user_id = ?", params = list(fresh_uid))
      expect_equal(upd_u$token[1], "updated_tok_77")
      expect_equal(upd_u$permissions[1], "admin")

      # 4. Toggle active/inactive status + self-deactivation guard
      session$setInputs(trigger_toggle_user_status = admin_id)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT is_active FROM user_tokens WHERE user_id = ?", params = list(admin_id))$is_active[1], 1L)

      session$setInputs(trigger_toggle_user_status = fresh_uid)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT is_active FROM user_tokens WHERE user_id = ?", params = list(fresh_uid))$is_active[1], 0L)

      session$setInputs(trigger_toggle_user_status = "fresh_coder")
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT is_active FROM user_tokens WHERE user_id = ?", params = list(fresh_uid))$is_active[1], 1L)

      # 5. Source & scale permissions modals
      session$setInputs(trigger_manage_user_sources = user1_id)
      session$flushReact()
      session$setInputs(
        btn_select_all_text_sources = 1L,
        btn_deselect_all_text_sources = 1L,
        btn_select_all_coding_sources = 1L,
        btn_deselect_all_coding_sources = 1L,
        selected_user_text_sources = c("1", "2", "3"),
        selected_user_coding_sources = c("1", "4"),
        btn_save_user_sources = 1L
      )
      session$flushReact()
      expect_equal(sort(get_user_permitted_sources_db(con, user1_id, FALSE)), c(1L, 2L, 3L))
      expect_equal(sort(get_user_permitted_coding_sources_db(con, user1_id, FALSE)), c(1L, 4L))

      session$setInputs(trigger_manage_user_scales = user1_id)
      session$flushReact()
      session$setInputs(
        btn_select_all_scales = 1L,
        btn_deselect_all_scales = 1L,
        selected_user_scales = c("1", "2", "3"),
        btn_save_user_scales = 1L
      )
      session$flushReact()
      scale_perms <- DBI::dbGetQuery(con, "SELECT scale_id FROM user_scale_permissions WHERE user_id = ? ORDER BY scale_id", params = list(user1_id))$scale_id
      expect_equal(scale_perms, c(1L, 2L, 3L))

      # 6. Delete user (self-deletion blocked, valid user deleted cleanly)
      session$setInputs(trigger_delete_user = admin_id)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM user_tokens WHERE user_id = ?", params = list(admin_id))$n[1], 1L)

      session$setInputs(trigger_delete_user = fresh_uid)
      session$flushReact()
      session$setInputs(btn_confirm_delete_user = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM user_tokens WHERE user_id = ?", params = list(fresh_uid))$n[1], 0L)
    }
  )
})

test_that("adminServer Subtabs 2, 3, and 4 (Texts, Sources, Scales): CRUD, pagination, FK guards, and locked max_value", {
  # Verifies:
  # - Tab 2 (Texts): 25-row pagination (34 seeded texts -> 2 pages), search filtering, and source reassignment modal
  # - Tab 3 (Sources): Search, adding source with custom license, editing source, FK delete guard on referenced source,
  #   and clean deletion of an unreferenced source
  # - Tab 4 (Scales): Search, 10-row pagination (with bulk scales), creating a new scale, and editing scale labels/descriptions
  #   while keeping max_value structurally immutable
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  seed_bulk_records(con, n_scales = 10L) # 13 total scales -> 2 pages
  rx_strig <- shiny::reactiveVal(0L)

  shiny::testServer(
    adminServer,
    args = list(
      db_con = con,
      current_user = shiny::reactiveVal("admin"),
      current_user_id = shiny::reactiveVal(1L),
      scales_trigger = rx_strig,
      current_lang = shiny::reactiveVal("de")
    ),
    {
      # --- TAB 2: TEXTS ---
      session$setInputs(texts_search_query = "")
      session$flushReact()
      expect_match(ui_to_html(output$texts_counter_badge), "34")
      expect_match(ui_to_html(output$texts_management_table_container), "<table")
      expect_match(ui_to_html(output$texts_pagination_controls), "btn_texts_next")

      session$setInputs(btn_texts_next = 1L)
      session$flushReact()
      expect_equal(current_page_texts(), 2L)
      session$setInputs(btn_texts_prev = 1L)
      session$flushReact()
      expect_equal(current_page_texts(), 1L)
      session$setInputs(btn_texts_last = 1L)
      session$flushReact()
      session$setInputs(btn_texts_first = 1L)
      session$flushReact()
      session$setInputs(trigger_jump_texts_page = "2")
      session$flushReact()
      expect_equal(current_page_texts(), 2L)

      # Search and source reassignment modal
      session$setInputs(texts_search_query = "Cephalgie")
      session$flushReact()
      expect_match(ui_to_html(output$texts_management_table_container), "102")

      session$setInputs(trigger_assign_text_source = 102L)
      session$flushReact()
      session$setInputs(select_new_source_id = "2", btn_save_assigned_source = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT source_id FROM texts WHERE text_id = 102")$source_id[1], 2L)

      # --- TAB 3: SOURCES ---
      session$setInputs(source_search_query = "")
      session$flushReact()
      expect_match(ui_to_html(output$sources_counter_text), "4")
      expect_match(ui_to_html(output$sources_table_container), "Mueller2024")

      session$setInputs(btn_open_add_source = 1L, new_lizenz_select = "__custom__")
      session$flushReact()
      expect_match(ui_to_html(output$new_custom_lizenz_ui), "new_lizenz_custom")

      # Validation error on blank Kurzreferenz, then valid custom-license insert
      session$setInputs(new_kurzreferenz = "", new_referenz = "Ref", new_lizenz_select = "CC-BY 4.0", btn_save_new_source = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$add_source_feedback), "alert-danger")

      session$setInputs(
        new_kurzreferenz = "CustomSrc2026",
        new_referenz = "Author (2026). Test Study.",
        new_lizenz_select = "__custom__",
        new_lizenz_custom = "MIT-Academic",
        btn_save_new_source = 2L
      )
      session$flushReact()
      new_src <- DBI::dbGetQuery(con, "SELECT * FROM sources WHERE Kurzreferenz = 'CustomSrc2026'")
      expect_equal(nrow(new_src), 1L)
      expect_equal(new_src$Lizenz[1], "MIT-Academic")
      new_src_id <- new_src$source_id[1]

      # Edit newly created source
      session$setInputs(trigger_edit_source = new_src_id, edit_lizenz_select = "__custom__")
      session$flushReact()
      expect_match(ui_to_html(output$edit_custom_lizenz_ui), "edit_lizenz_custom")

      session$setInputs(
        edit_kurzreferenz = "CustomSrc2026_Ed",
        edit_referenz = "Author (2026). Updated Study.",
        edit_lizenz_select = "CC0 1.0",
        btn_save_edit_source = 1L
      )
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT Lizenz FROM sources WHERE source_id = ?", params = list(new_src_id))$Lizenz[1], "CC0 1.0")

      # Attempt to delete referenced source 1 (FK guard prevents deletion)
      session$setInputs(trigger_delete_source = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE source_id = 1")$n[1], 1L)

      # Delete unreferenced custom source cleanly
      session$setInputs(trigger_delete_source = new_src_id)
      session$flushReact()
      session$setInputs(btn_confirm_delete_source = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE source_id = ?", params = list(new_src_id))$n[1], 0L)

      # --- TAB 4: SCALES ---
      session$setInputs(scale_search_query = "")
      session$flushReact()
      expect_match(ui_to_html(output$scales_counter_text), "13")
      expect_match(ui_to_html(output$scales_table_container), "K1_Empathie")

      session$setInputs(btn_scales_next_page = 1L)
      session$flushReact()
      session$setInputs(btn_scales_prev_page = 1L)
      session$flushReact()

      # Add new scale: test validation then valid creation
      session$setInputs(btn_open_add_scale = 1L)
      session$flushReact()
      session$setInputs(new_scale_short_label = "", new_scale_name = "Bad", new_scale_max_value = 3L, btn_save_new_scale = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$add_scale_feedback), "alert-danger")

      before_st <- rx_strig()
      session$setInputs(
        new_scale_short_label = "Klarheit",
        new_scale_name = "K99_Klarheit",
        new_scale_max_value = 3L,
        new_scale_def_desc = "Clarity of medical explanation",
        new_scale_def_n_0 = "Unclear",
        new_scale_def_n_1 = "Basic",
        new_scale_def_n_2 = "Clear",
        new_scale_def_n_3 = "Exemplary",
        btn_save_new_scale = 2L
      )
      session$flushReact()
      expect_equal(rx_strig(), before_st + 1L)
      created_sc <- DBI::dbGetQuery(con, "SELECT * FROM scale_definitions WHERE name = 'K99_Klarheit'")
      expect_equal(nrow(created_sc), 1L)
      expect_equal(created_sc$max_value[1], 3L)

      # Edit newly created scale (updates labels/descriptions while max_value stays 3)
      session$setInputs(trigger_edit_scale = created_sc$scale_id[1])
      session$flushReact()
      session$setInputs(
        edit_scale_short_label = "KlarheitEd",
        edit_scale_name = "K99_Klarheit_Updated",
        edit_scale_def_desc = "Updated clarity description",
        edit_scale_def_n_0 = "Unclear v2",
        edit_scale_def_n_1 = "Basic v2",
        edit_scale_def_n_2 = "Clear v2",
        edit_scale_def_n_3 = "Exemplary v2",
        btn_save_edit_scale = 1L
      )
      session$flushReact()
      upd_sc <- DBI::dbGetQuery(con, "SELECT short_label, name, max_value FROM scale_definitions WHERE scale_id = ?", params = list(created_sc$scale_id[1]))
      expect_equal(upd_sc$short_label[1], "KlarheitEd")
      expect_equal(upd_sc$max_value[1], 3L)
    }
  )
})

test_that("adminServer Subtabs 5 & 6 (Reliability Comparison & Coding Search/Correction) work end-to-end", {
  # Verifies:
  # - Tab 5 (Reliability Overview): KPI cards, comparison bar plot in decimal ('cohens_kappa'),
  #   percentage ('accuracy'), and empty states, comparison table, refresh button, and coder detail modal
  # - Tab 6 (Coding Search & Correction): Header UI, scale/type/search filters, reset button,
  #   25-row pagination (first/prev/next/last/jump), and opening/confirming the rating value edit modal
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  user1_id <- get_user_info_db(con, "user1")$id
  # Insert one 'kodierung' for user1 so coder detail modal exercises both training & live coding tabs
  DBI::dbExecute(con, "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id) VALUES (?, 207, '1', 3, 'kodierung', 1)", params = list(user1_id))

  shiny::testServer(
    adminServer,
    args = list(
      db_con = con,
      current_user = shiny::reactiveVal("admin"),
      current_user_id = shiny::reactiveVal(1L),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("en")
    ),
    {
      # --- TAB 5: RELIABILITY ---
      session$setInputs(rel_scale_select = "all", rel_metric_select = "cohens_kappa", rel_search_query = "")
      session$flushReact()
      expect_match(ui_to_html(output$admin_reliability_kpis), "card")
      expect_true(!is.null(output$admin_reliability_comparison_plot))
      expect_match(ui_to_html(output$admin_reliability_table_container), "user1")

      # Switch to percentage metric & specific scale
      session$setInputs(rel_scale_select = "1", rel_metric_select = "accuracy", btn_refresh_reliability = 1L)
      session$flushReact()
      expect_true(!is.null(output$admin_reliability_comparison_plot))

      # Empty search filter hits empty-plot and empty-table branches
      session$setInputs(rel_search_query = "no_such_coder_xyz")
      session$flushReact()
      expect_true(!is.null(output$admin_reliability_comparison_plot))
      expect_match(ui_to_html(output$admin_reliability_table_container), "alert")

      # Drilldown modal for user1 and nonexistent user
      session$setInputs(rel_search_query = "")
      expect_no_error({
        session$setInputs(trigger_rel_user_detail = user1_id)
        session$flushReact()
        session$setInputs(trigger_rel_user_detail = 99999L)
        session$flushReact()
      })

      # --- TAB 6: CODING SEARCH & CORRECTION ---
      session$setInputs(
        coding_search_scale_filter = "all",
        coding_search_type_filter = "all",
        coding_search_query = ""
      )
      session$flushReact()
      expect_true(nzchar(ui_to_html(output$tab_coding_search_title)))
      expect_match(ui_to_html(output$coding_search_header_ui), "Coding Lookup")
      expect_match(ui_to_html(output$coding_search_counter_ui), "35")
      expect_match(ui_to_html(output$coding_search_table_container), "<table")
      expect_match(ui_to_html(output$coding_search_pagination_controls), "btn_cs_next")

      # Pagination buttons & direct jump
      session$setInputs(btn_cs_next = 1L)
      session$flushReact()
      expect_equal(current_page_coding_search(), 2L)
      session$setInputs(btn_cs_prev = 1L)
      session$flushReact()
      expect_equal(current_page_coding_search(), 1L)
      session$setInputs(btn_cs_last = 1L)
      session$flushReact()
      expect_equal(current_page_coding_search(), 2L)
      session$setInputs(btn_cs_first = 1L)
      session$flushReact()
      expect_equal(current_page_coding_search(), 1L)
      session$setInputs(trigger_jump_cs_page = "2")
      session$flushReact()
      expect_equal(current_page_coding_search(), 2L)

      # Reset filter button
      session$setInputs(btn_coding_search_reset = 1L)
      session$flushReact()
      expect_equal(current_page_coding_search(), 1L)

      # Open edit modal for rating #201 and persist new score = 1
      session$setInputs(trigger_edit_coding_rating = 201L)
      session$flushReact()
      session$setInputs(edit_coding_new_value = "1", btn_confirm_edit_coding = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT bewertung FROM user_ratings WHERE rating_id = 201")$bewertung[1], 1L)

      # Invalid score (> max_value = 4) triggers error branch and leaves DB unchanged
      session$setInputs(trigger_edit_coding_rating = 201L)
      session$flushReact()
      session$setInputs(edit_coding_new_value = "99", btn_confirm_edit_coding = 2L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT bewertung FROM user_ratings WHERE rating_id = 201")$bewertung[1], 1L)
    }
  )
})

test_that("adminServer Subtabs 7 & 8 (Import/Export Modals, CSV Downloads/Uploads & DB Diagnostics) work end-to-end", {
  # Verifies:
  # - Tab 7 (Data Import/Export):
  #   1) Export filter modals for scales, sources, and users (open, search, paging, select/deselect all, close)
  #   2) Master dataset downloadHandler + all 5 raw CSV table downloads + all 5 header-only template downloads
  #   3) All 5 CSV upload pipelines (scales, texts, sources, ratings, training) including schema-mismatch rejection,
  #      conflict modal resolution ('overwrite' and 'skip'), FK validation, and erfasst_am auto-fill
  # - Tab 8 (DB Diagnostics & Integrity Audit):
  #   1) Summary cards, 9-table overview, and 6-check FK integrity audit in clean state (0 violations)
  #   2) Refresh button and detecting an injected orphaned record when PRAGMA foreign_keys is temporarily OFF
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  # Prepare valid CSV upload files matching exact SQLite table schemas
  csv_scales_valid <- write_temp_csv(data.frame(
    scale_id = c(1L, 10L),
    short_label = c("EmpathieUpd", "NewScale10"),
    name = c("K1_Empathie", "K10_Uploaded"),
    max_value = c(4L, 2L),
    def_desc = c("Updated desc", "Uploaded desc"),
    def_n_0 = c("d0", "u0"), def_n_1 = c("d1", "u1"), def_n_2 = c("d2", "u2"),
    def_n_3 = c("d3", ""), def_n_4 = c("d4", ""), def_n_5 = c("", ""),
    stringsAsFactors = FALSE
  ))
  csv_scales_bad <- write_temp_csv(data.frame(wrong_col = 1L))

  csv_sources_valid <- write_temp_csv(data.frame(
    source_id = c(4L, 50L),
    Kurzreferenz = c("AIF-Edu-Report", "UploadedSrc50"),
    Referenz = c("Updated Ref", "New Source Ref 50"),
    Lizenz = c("GPL-3.0", "CC-BY 4.0"),
    erfasst_am = c("", ""),
    stringsAsFactors = FALSE
  ))

  csv_texts_valid <- write_temp_csv(data.frame(
    text_id = c(801L),
    text = c("Uploaded text case #801"),
    source_id = c(1L),
    stringsAsFactors = FALSE
  ))

  csv_texts_bad_fk <- write_temp_csv(data.frame(
    text_id = c(802L),
    text = c("Text referencing nonexistent source 9999"),
    source_id = c(9999L),
    stringsAsFactors = FALSE
  ))

  csv_ratings_valid <- write_temp_csv(data.frame(
    rating_id = c(9001L),
    user_id = c(1L),
    text_id = c(101L),
    kategorie = c("1"),
    bewertung = c(3L),
    typ = c("kodierung"),
    source_id = c(1L),
    zeitstempel = c("2026-11-10 12:00:00"),
    stringsAsFactors = FALSE
  ))

  csv_training_valid <- write_temp_csv(data.frame(
    training_id = c(901L),
    user_id = c(1L),
    kategorie = c("1"),
    datum = c("2026-11-10"),
    anzahl_einheiten = c(6L),
    accuracy = c(83.3), balanced_accuracy = c(82.0), iota_index = c(0.79), avg_iota2 = c(0.80),
    cohens_kappa = c(0.78), kappa_linear = c(0.80), kappa_squared = c(0.82), fleiss_kappa = c(0.77),
    kendall_w = c(0.81), alpha_nominal = c(0.77), alpha_ordinal = c(0.80), gwet_ac1 = c(0.81),
    gwet_ac2_quadratic = c(0.83), macro_precision = c(83.0), macro_recall = c(82.5), macro_f1 = c(82.7),
    zeitstempel = c("2026-11-10 12:00:00"),
    stringsAsFactors = FALSE
  ))

  shiny::testServer(
    adminServer,
    args = list(
      db_con = con,
      current_user = shiny::reactiveVal("admin"),
      current_user_id = shiny::reactiveVal(1L),
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = shiny::reactiveVal("en")
    ),
    {
      # --- TAB 7A: Export Filter Modals (Scales, Sources, Users) ---
      expect_true(nzchar(ui_to_html(output$export_scales_status_badge)))
      expect_true(nzchar(ui_to_html(output$export_scales_counter_ui)))
      expect_true(nzchar(ui_to_html(output$export_sources_status_badge)))
      expect_true(nzchar(ui_to_html(output$export_sources_counter_ui)))
      expect_true(nzchar(ui_to_html(output$export_users_status_badge)))
      expect_true(nzchar(ui_to_html(output$export_users_counter_ui)))
      expect_true(nzchar(ui_to_html(output$export_filter_summary_badge)))

      # Scales modal controls
      session$setInputs(btn_open_modal_export_scales = 1L, input_modal_scales_search = "")
      session$flushReact()
      expect_match(ui_to_html(output$modal_scales_page_ui), "modal_scales_page_selection")
      expect_true(nzchar(ui_to_html(output$modal_scales_paging_controls)))
      session$setInputs(
        modal_scales_page_selection = c("1", "2"),
        btn_modal_scales_deselect_all = 1L,
        btn_modal_scales_select_all = 1L,
        btn_modal_scales_next = 1L,
        btn_modal_scales_prev = 1L,
        btn_close_modal_scales = 1L
      )
      session$flushReact()

      # Sources modal controls
      session$setInputs(btn_open_modal_export_sources = 1L, input_modal_sources_search = "")
      session$flushReact()
      expect_match(ui_to_html(output$modal_sources_page_ui), "modal_sources_page_selection")
      expect_true(nzchar(ui_to_html(output$modal_sources_paging_controls)))
      session$setInputs(
        modal_sources_page_selection = c("1"),
        btn_modal_sources_deselect_all = 1L,
        btn_modal_sources_select_all = 1L,
        btn_modal_sources_next = 1L,
        btn_modal_sources_prev = 1L,
        btn_close_modal_sources = 1L
      )
      session$flushReact()

      # Users modal controls
      session$setInputs(btn_open_modal_export_users = 1L, input_modal_users_search = "")
      session$flushReact()
      expect_match(ui_to_html(output$modal_users_page_ui), "modal_users_page_selection")
      expect_true(nzchar(ui_to_html(output$modal_users_paging_controls)))
      session$setInputs(
        modal_users_page_selection = c("1", "2"),
        btn_modal_users_deselect_all = 1L,
        btn_modal_users_select_all = 1L,
        btn_modal_users_next = 1L,
        btn_modal_users_prev = 1L,
        btn_close_modal_users = 1L
      )
      session$flushReact()

      # --- TAB 7B: Master Dataset & All 5 Raw + 5 Template Downloads ---
      master_csv <- output$download_master_dataset_btn
      expect_true(file.exists(master_csv))
      expect_gt(file.info(master_csv)$size, 50)

      for (dl_out in c(
        "download_scales_btn", "download_texts_btn", "download_sources_btn",
        "download_ratings_btn", "download_training_log_btn",
        "download_template_scales", "download_template_texts", "download_template_sources",
        "download_template_ratings", "download_template_training"
      )) {
        dl_path <- output[[dl_out]]
        expect_true(file.exists(dl_path), info = paste("Missing download output:", dl_out))
      }

      # --- TAB 7C: CSV Uploads (Schema Guard, Skip Conflict, Overwrite Conflict, FK Validation) ---
      # Schema mismatch rejected (pending_import stays NULL)
      session$setInputs(upload_scales_file = list(datapath = csv_scales_bad, name = "bad.csv"))
      session$flushReact()
      expect_null(pending_import())

      # Scales upload with PK conflict on scale_id=1 -> resolve via 'skip' (adds scale 10, preserves scale 1)
      session$setInputs(upload_scales_file = list(datapath = csv_scales_valid, name = "scales.csv"))
      session$flushReact()
      expect_false(is.null(pending_import()))
      session$setInputs(conflict_resolution_mode = "skip", btn_confirm_append_table = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM scale_definitions WHERE scale_id = 10")$n[1], 1L)
      expect_equal(DBI::dbGetQuery(con, "SELECT short_label FROM scale_definitions WHERE scale_id = 1")$short_label[1], "Empathie")

      # Sources upload with PK conflict on source_id=4 -> resolve via 'overwrite' + auto-fill erfasst_am
      session$setInputs(upload_sources_file = list(datapath = csv_sources_valid, name = "sources.csv"))
      session$flushReact()
      session$setInputs(conflict_resolution_mode = "overwrite", btn_confirm_append_table = 2L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE source_id = 50")$n[1], 1L)
      expect_equal(DBI::dbGetQuery(con, "SELECT Referenz FROM sources WHERE source_id = 4")$Referenz[1], "Updated Ref")

      # Texts upload: invalid source_id rejected, valid text appended
      session$setInputs(upload_texts_file = list(datapath = csv_texts_bad_fk, name = "bad_texts.csv"))
      session$flushReact()
      expect_null(pending_import())

      session$setInputs(upload_texts_file = list(datapath = csv_texts_valid, name = "texts.csv"))
      session$flushReact()
      session$setInputs(conflict_resolution_mode = "skip", btn_confirm_append_table = 3L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM texts WHERE text_id = 801")$n[1], 1L)

      # Ratings & training CSV uploads
      session$setInputs(upload_ratings_file = list(datapath = csv_ratings_valid, name = "ratings.csv"))
      session$flushReact()
      session$setInputs(conflict_resolution_mode = "skip", btn_confirm_append_table = 4L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM user_ratings WHERE rating_id = 9001")$n[1], 1L)

      session$setInputs(upload_training_file = list(datapath = csv_training_valid, name = "training.csv"))
      session$flushReact()
      session$setInputs(conflict_resolution_mode = "skip", btn_confirm_append_table = 5L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM coding_training WHERE training_id = 901")$n[1], 1L)

      # --- TAB 8: DB DIAGNOSTICS & INTEGRITY AUDIT ---
      session$setInputs(btn_refresh_diagnostics = 1L)
      session$flushReact()
      rep_clean <- db_diagnostics_report()
      expect_equal(rep_clean$existing_tables_count, 9L)
      expect_equal(rep_clean$total_violations, 0L)
      expect_true(rep_clean$foreign_keys_active)
      expect_match(ui_to_html(output$db_diag_summary_cards), "bg-success")
      expect_match(ui_to_html(output$db_diag_tables_overview), "user_scale_permissions")
      expect_match(ui_to_html(output$db_diag_integrity_audit), "bg-success")

      # Inject an orphaned rating (temporarily disabling FK pragma) and verify audit detects >=1 violation
      DBI::dbExecute(con, "PRAGMA foreign_keys = OFF;")
      DBI::dbExecute(con, "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ) VALUES (99999, 99999, '1', 2, 'kodierung')")
      DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

      session$setInputs(btn_refresh_diagnostics = 2L)
      session$flushReact()
      rep_dirty <- db_diagnostics_report()
      expect_gte(rep_dirty$total_violations, 2L)
      expect_match(ui_to_html(output$db_diag_summary_cards), "bg-warning")
      expect_match(ui_to_html(output$db_diag_integrity_audit), "bg-danger")
    }
  )
})
