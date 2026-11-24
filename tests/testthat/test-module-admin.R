# Note: This file was generated with the assistance of AI.

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
      users_tbl_html <- ui_to_html(output$users_table_container)
      expect_match(users_tbl_html, "<table")
      expect_match(users_tbl_html, "fa-lock")
      expect_match(users_tbl_html, "\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022")
      expect_false(grepl("geheim123", users_tbl_html, fixed = TRUE))
      expect_false(grepl("sys_gold_standard_internal", users_tbl_html, fixed = TRUE))
      expect_false(grepl("sys_expert_internal", users_tbl_html, fixed = TRUE))
      expect_match(users_tbl_html, "token2026")
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
      session$setInputs(texts_search_query = "photosynthesis")
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
      expect_match(ui_to_html(output$sources_table_container), "Klieme2024")

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
      expect_match(ui_to_html(output$scales_table_container), "S1_Cognitive_Activation")

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
        new_scale_def_desc = "Clarity of instructional explanation",
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
    short_label = c("CogActUpd", "NewScale10"),
    name = c("S1_Cognitive_Activation", "K10_Uploaded"),
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
      expect_equal(DBI::dbGetQuery(con, "SELECT short_label FROM scale_definitions WHERE scale_id = 1")$short_label[1], "CogAct")

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

test_that("adminServer handles coordinator fallbacks, user self-edit/permissions guards, and content validation edge cases", {
  # Covers remaining non-MySQL branches in R/modul_admin.R, R/modul_admin_users.R, and R/modul_admin_content.R:
  # 1) adminUI() with valid selected_subtab and invalid fallback
  # 2) adminServer() with scalar current_user/current_lang and current_user_id = NULL DB lookup
  # 3) Users: nonexistent target guards, admin scale-permissions notification, admin self-edit modal/save,
  #    deleting a user with existing ratings warning, and empty-scales/empty-sources permission notifications
  # 4) Content: empty texts/sources/scales search results, source add/edit validation errors & blocked delete
  #    confirmation, scale search filtering, page clamping, and scale add/edit validation errors
  ui_sel <- ui_to_html(adminUI("adm_sel", lang = "en", selected_subtab = "tab_scales"))
  expect_match(ui_sel, "adm_sel-scales_table_container")
  ui_inv <- ui_to_html(adminUI("adm_inv", lang = "de", selected_subtab = "nonexistent_tab"))
  expect_match(ui_inv, "adm_inv-users_table_container")

  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  admin_id <- get_user_info_db(con, "admin")$id
  user1_id <- get_user_info_db(con, "user1")$id
  # Give user1 a rating so delete modal displays the ratings-present info banner
  DBI::dbExecute(con, "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id) VALUES (?, 208, '1', 2, 'kodierung', 1)", params = list(user1_id))

  shiny::testServer(
    adminServer,
    args = list(
      db_con = con,
      current_user = "admin",
      current_user_id = NULL,
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = "en"
    ),
    {
      # Coordinator scalar/NULL resolution
      expect_equal(aktiver_admin(), "admin")
      expect_equal(aktiver_admin_id(), admin_id)

      # --- USERS EDGE CASES ---
      expect_no_error({
        session$setInputs(trigger_manage_user_sources = "nonexistent_user_xyz")
        session$setInputs(trigger_manage_user_scales = "nonexistent_user_xyz")
        session$setInputs(trigger_edit_user = "nonexistent_user_xyz")
        session$setInputs(trigger_delete_user = "nonexistent_user_xyz")
        session$flushReact()
      })

      # Admin scale permissions -> immediate message notification
      expect_no_error({
        session$setInputs(trigger_manage_user_scales = admin_id)
        session$flushReact()
      })

      # Admin self-edit modal & save:
      # 1) Blank password keeps existing password unchanged and keeps role = admin, is_active = 1
      session$setInputs(trigger_edit_user = admin_id)
      session$flushReact()
      session$setInputs(edit_token = "", edit_permissions = "user", edit_is_active = 0L, btn_save_edited_user = 1L)
      session$flushReact()
      adm_row <- DBI::dbGetQuery(con, "SELECT token, permissions, is_active FROM user_tokens WHERE user_id = ?", params = list(admin_id))
      expect_equal(adm_row$token[1], "geheim123")
      expect_equal(adm_row$permissions[1], "admin")
      expect_equal(adm_row$is_active[1], 1L)

      # 2) Non-empty too-short password (<4 chars) rejected with alert-danger and leaves token unchanged
      session$setInputs(trigger_edit_user = admin_id)
      session$flushReact()
      session$setInputs(edit_token = "ab", edit_permissions = "user", edit_is_active = 0L, btn_save_edited_user = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_user_feedback), "alert-danger")
      expect_equal(DBI::dbGetQuery(con, "SELECT token FROM user_tokens WHERE user_id = ?", params = list(admin_id))$token[1], "geheim123")

      # 3) Valid new password updates token while preserving admin role and active status
      session$setInputs(edit_token = "admin_new_pw_99", edit_permissions = "user", edit_is_active = 0L, btn_save_edited_user = 3L)
      session$flushReact()
      adm_upd <- DBI::dbGetQuery(con, "SELECT token, permissions, is_active FROM user_tokens WHERE user_id = ?", params = list(admin_id))
      expect_equal(adm_upd$token[1], "admin_new_pw_99")
      expect_equal(adm_upd$permissions[1], "admin")
      expect_equal(adm_upd$is_active[1], 1L)

      # Delete modal for user1 (has ratings -> info alert branch; do not confirm delete)
      expect_no_error({
        session$setInputs(trigger_delete_user = user1_id)
        session$flushReact()
      })

      # --- TEXTS EMPTY SEARCH ---
      session$setInputs(texts_search_query = "zzz_no_matching_text_999")
      session$flushReact()
      expect_match(ui_to_html(output$texts_management_table_container), "alert-warning")
      expect_equal(ui_to_html(output$texts_pagination_controls), "")
      session$setInputs(texts_search_query = "")
      session$flushReact()

      # --- SOURCES SEARCH, VALIDATION ERRORS & BLOCKED DELETE CONFIRMATION ---
      session$setInputs(source_search_query = "Klieme2024")
      session$flushReact()
      expect_match(ui_to_html(output$sources_table_container), "Klieme2024")

      session$setInputs(source_search_query = "zzz_no_source_999")
      session$flushReact()
      expect_match(ui_to_html(output$sources_table_container), "alert-light")
      session$setInputs(source_search_query = "")

      # Add source: missing full ref, missing custom license, duplicate short ref
      session$setInputs(new_kurzreferenz = "ValidRef", new_referenz = "x", new_lizenz_select = "CC-BY 4.0", btn_save_new_source = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$add_source_feedback), "alert-danger")

      session$setInputs(new_kurzreferenz = "ValidRef", new_referenz = "Full valid citation 2026", new_lizenz_select = "__custom__", new_lizenz_custom = "", btn_save_new_source = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$add_source_feedback), "alert-danger")

      session$setInputs(new_kurzreferenz = "Klieme2024", new_referenz = "Full valid citation 2026", new_lizenz_select = "CC-BY 4.0", btn_save_new_source = 3L)
      session$flushReact()
      expect_match(ui_to_html(output$add_source_feedback), "alert-danger")

      # Edit source #2: short ref too short & duplicate of #1 ('Klieme2024')
      session$setInputs(trigger_edit_source = 2L)
      session$flushReact()
      session$setInputs(edit_kurzreferenz = "x", edit_referenz = "Valid Citation", edit_lizenz_select = "CC-BY 4.0", btn_save_edit_source = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_source_feedback), "alert-danger")

      session$setInputs(edit_kurzreferenz = "Klieme2024", edit_referenz = "Valid Citation", edit_lizenz_select = "CC-BY 4.0", btn_save_edit_source = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_source_feedback), "alert-danger")

      # Blocked delete confirmation for referenced source #1
      session$setInputs(trigger_delete_source = 1L)
      session$flushReact()
      session$setInputs(btn_confirm_delete_source = 1L)
      session$flushReact()
      expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE source_id = 1")$n[1], 1L)

      # --- SCALES SEARCH, EMPTY RESULT, PAGE CLAMPING & ADD/EDIT VALIDATIONS ---
      session$setInputs(scale_search_query = "CogAct")
      session$flushReact()
      expect_match(ui_to_html(output$scales_counter_text), "1")
      expect_match(ui_to_html(output$scales_table_container), "S1_Cognitive_Activation")

      session$setInputs(scale_search_query = "zzz_no_scale_999")
      session$flushReact()
      expect_match(ui_to_html(output$scales_table_container), "alert-light")
      session$setInputs(scale_search_query = "")
      session$flushReact()

      # Add scale validation: short name, max_value out of range, duplicate name
      session$setInputs(new_scale_short_label = "Ok", new_scale_name = "a", new_scale_max_value = 3L, btn_save_new_scale = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$add_scale_feedback), "alert-danger")

      session$setInputs(new_scale_short_label = "Ok", new_scale_name = "K99_RangeBad", new_scale_max_value = 9L, btn_save_new_scale = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$add_scale_feedback), "alert-danger")

      session$setInputs(new_scale_short_label = "Ok", new_scale_name = "S1_Cognitive_Activation", new_scale_max_value = 3L, btn_save_new_scale = 3L)
      session$flushReact()
      expect_match(ui_to_html(output$add_scale_feedback), "alert-danger")

      # Edit scale #2 validation: empty short_label, short name, duplicate name 'S1_Cognitive_Activation'
      session$setInputs(trigger_edit_scale = 2L)
      session$flushReact()
      session$setInputs(edit_scale_short_label = "", edit_scale_name = "S2_Instructional_Structure", btn_save_edit_scale = 1L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_scale_feedback), "alert-danger")

      session$setInputs(edit_scale_short_label = "Struct", edit_scale_name = "x", btn_save_edit_scale = 2L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_scale_feedback), "alert-danger")

      session$setInputs(edit_scale_short_label = "Struct", edit_scale_name = "S1_Cognitive_Activation", btn_save_edit_scale = 3L)
      session$flushReact()
      expect_match(ui_to_html(output$edit_scale_feedback), "alert-danger")
    }
  )

  # Empty DB (0 sources, 0 scales) exercises empty permission modal notifications and default current_user = NULL
  db_empty <- local_test_db(seeded = TRUE, empty = TRUE)
  DBI::dbExecute(db_empty$con, "INSERT INTO user_tokens (username, token, permissions, is_active) VALUES ('coder_empty', 'tok1234', 'user', 1)")
  coder_empty_id <- get_user_info_db(db_empty$con, "coder_empty")$id

  shiny::testServer(
    adminServer,
    args = list(
      db_con = db_empty$con,
      current_user = NULL,
      current_user_id = NULL,
      scales_trigger = NULL,
      current_lang = "de"
    ),
    {
      expect_equal(aktiver_admin(), "admin")
      expect_no_error({
        session$setInputs(trigger_manage_user_sources = coder_empty_id)
        session$setInputs(trigger_manage_user_scales = coder_empty_id)
        session$flushReact()
      })
    }
  )
})

test_that("adminServer covers export filter modal searches, zero-selection warnings, upload errors, legacy exports, and template fallbacks", {
  # Covers remaining non-MySQL branches in R/modul_admin_data.R:
  # 1) Live search (matched + unmatched) inside Scales, Sources, and Users export filter modals
  # 2) Zero-selection warning badges (bg-warning) & blocked summary badge + export_err_missing_group
  # 3) Texts CSV upload missing source_id column & DB transaction abort (duplicate PK in same CSV)
  # 4) Legacy schema master export (texts without source_id, user_ratings without user_id)
  # 5) CSV template fallback switch across all 5 tables when tables do not yet exist
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  csv_texts_no_src <- write_temp_csv(data.frame(text_id = 910L, text = "No source_id column", stringsAsFactors = FALSE))
  csv_texts_dup_pk <- write_temp_csv(data.frame(
    text_id = c(920L, 920L),
    text = c("Duplicate PK row 1", "Duplicate PK row 2"),
    source_id = c(1L, 1L),
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
      # 1. Modal live search (matching + empty match) for Scales, Sources, Users
      session$setInputs(input_modal_scales_search = "CogAct")
      session$flushReact()
      expect_match(ui_to_html(output$modal_scales_page_ui), "S1_Cognitive_Activation")
      session$setInputs(input_modal_scales_search = "zzz_none")
      session$flushReact()
      expect_match(ui_to_html(output$modal_scales_page_ui), "text-muted")
      session$setInputs(input_modal_scales_search = "")
      session$flushReact()

      session$setInputs(input_modal_sources_search = "Klieme")
      session$flushReact()
      expect_match(ui_to_html(output$modal_sources_page_ui), "Klieme2024")
      session$setInputs(input_modal_sources_search = "zzz_none")
      session$flushReact()
      expect_match(ui_to_html(output$modal_sources_page_ui), "text-muted")
      session$setInputs(input_modal_sources_search = "")
      session$flushReact()

      session$setInputs(input_modal_users_search = "user1")
      session$flushReact()
      expect_match(ui_to_html(output$modal_users_page_ui), "user1")
      session$setInputs(input_modal_users_search = "zzz_none")
      session$flushReact()
      expect_match(ui_to_html(output$modal_users_page_ui), "text-muted")
      session$setInputs(input_modal_users_search = "")
      session$flushReact()

      # 2. Deselect all across all 3 dimensions -> warning badges + blocked summary + missing-group export
      # Render page UIs first so filtered_modal_*_ids() contain all IDs
      invisible(output$modal_scales_page_ui)
      invisible(output$modal_sources_page_ui)
      invisible(output$modal_users_page_ui)
      session$setInputs(
        btn_modal_scales_deselect_all = 1L,
        btn_modal_sources_deselect_all = 1L,
        btn_modal_users_deselect_all = 1L
      )
      session$flushReact()
      expect_match(ui_to_html(output$export_scales_status_badge), "bg-warning")
      expect_match(ui_to_html(output$export_sources_status_badge), "bg-warning")
      expect_match(ui_to_html(output$export_users_status_badge), "bg-warning")
      expect_match(ui_to_html(output$export_filter_summary_badge), "bg-warning")
      missing_csv <- output$download_master_dataset_btn
      expect_true(file.exists(missing_csv))
      expect_match(
        paste(readLines(missing_csv, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
        tr("admin.export_err_missing_group", "en"),
        fixed = TRUE
      )

      # 3. Upload error paths: missing source_id column & transaction failure on duplicate PK
      session$setInputs(upload_texts_file = list(datapath = csv_texts_no_src, name = "no_src.csv"))
      session$flushReact()
      expect_null(pending_import())

      session$setInputs(upload_texts_file = list(datapath = csv_texts_dup_pk, name = "dup_pk.csv"))
      session$flushReact()
      expect_false(is.null(pending_import()))
      expect_no_error({
        session$setInputs(conflict_resolution_mode = "skip", btn_confirm_append_table = 1L)
        session$flushReact()
      })
    }
  )

  # 4. Legacy schema master export (texts without source_id, user_ratings without user_id)
  con_leg <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_leg))
  DBI::dbExecute(con_leg, "CREATE TABLE user_tokens (user_id INT, username TEXT, token TEXT, permissions TEXT, is_active INT);")
  DBI::dbExecute(con_leg, "CREATE TABLE sources (source_id INT, Kurzreferenz TEXT, Referenz TEXT, Lizenz TEXT, erfasst_am TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE texts (text_id INT, text TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE scale_definitions (scale_id INT, short_label TEXT, name TEXT, max_value INT, def_desc TEXT, def_n_0 TEXT, def_n_1 TEXT, def_n_2 TEXT, def_n_3 TEXT, def_n_4 TEXT, def_n_5 TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE user_ratings (rating_id INT, username TEXT, text_id INT, kategorie TEXT, bewertung INT, typ TEXT, source_id INT, zeitstempel TEXT);")
  DBI::dbExecute(con_leg, "INSERT INTO user_tokens VALUES (1, 'admin', 'pw', 'admin', 1);")
  DBI::dbExecute(con_leg, "INSERT INTO sources VALUES (0, 'LegDef', 'Legacy Default', 'CC0', '2026-01-01');")
  DBI::dbExecute(con_leg, "INSERT INTO sources VALUES (1, 'Src1', 'Ref1', 'CC-BY 4.0', '2026-01-01');")
  DBI::dbExecute(con_leg, "INSERT INTO texts VALUES (1, 'Legacy text without source_id');")
  DBI::dbExecute(con_leg, "INSERT INTO scale_definitions VALUES (1, 'S1', 'Scale1', 2, 'd', '0', '1', '2', '', '', '');")
  DBI::dbExecute(con_leg, "INSERT INTO user_ratings VALUES (1, 'admin', 1, '1', 2, 'kodierung', 1, '2026-01-01');")

  shiny::testServer(
    adminServer,
    args = list(db_con = con_leg, current_user = "admin", current_user_id = 1L, current_lang = "en"),
    {
      session$flushReact()
      leg_master <- output$download_master_dataset_btn
      expect_true(file.exists(leg_master))
      expect_gt(file.info(leg_master)$size, 20)
      expect_match(paste(readLines(leg_master, warn = FALSE), collapse = "\n"), "Legacy text without source_id")
    }
  )

  # 5. Template CSV switch fallback when tables are absent from DB
  con_blank <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_blank))
  shiny::testServer(
    adminServer,
    args = list(db_con = con_blank, current_user = "admin", current_user_id = 1L, current_lang = "en"),
    {
      for (tpl in c(
        "download_template_scales", "download_template_texts", "download_template_sources",
        "download_template_ratings", "download_template_training"
      )) {
        tpl_path <- output[[tpl]]
        expect_true(file.exists(tpl_path), info = paste("Missing fallback template:", tpl))
      }
    }
  )
})

test_that("adminServer covers all 16 reliability metrics, zero-session fallbacks, nonexistent rating edit, and missing-table diagnostics", {
  # Covers remaining non-MySQL branches in R/modul_admin_analytics.R:
  # 1) All 16 reliability metric switch branches (+ unknown fallback) in KPI cards and comparison plot
  # 2) Zero-training / zero-coding DB fallback (both global and scale-filtered = 36+ lines)
  # 3) Coder drilldown modal with 0 trainings/0 codings and with NA balanced_accuracy/iota fields
  # 4) Nonexistent rating_id passed to trigger_edit_coding_rating -> error notification
  # 5) Diagnostics with a dropped table (coding_training missing -> danger schema card + check error badge)
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  markus_id <- get_user_info_db(con, "markus_meier")$id

  # Insert a training row with NA balanced_accuracy/iota & low kappa (<0.6) for user1
  user1_id <- get_user_info_db(con, "user1")$id
  DBI::dbExecute(
    con,
    "INSERT INTO coding_training (user_id, kategorie, datum, anzahl_einheiten, accuracy, balanced_accuracy, iota_index, avg_iota2, cohens_kappa, alpha_nominal, gwet_ac1)
     VALUES (?, '1', '2026-11-12', 5, 55.0, NULL, NULL, NULL, 0.45, 0.44, 0.50)",
    params = list(user1_id)
  )

  all_metrics <- c(
    "cohens_kappa", "alpha_nominal", "alpha_ordinal", "fleiss_kappa",
    "kappa_linear", "kappa_squared", "gwet_ac1", "gwet_ac2_quadratic",
    "kendall_w", "avg_iota2", "iota_index",
    "accuracy", "balanced_accuracy", "macro_f1", "macro_precision", "macro_recall",
    "unknown_metric_fallback"
  )

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
      session$setInputs(rel_scale_select = "all", rel_search_query = "")
      for (m_key in all_metrics) {
        session$setInputs(rel_metric_select = m_key)
        session$flushReact()
        expect_match(ui_to_html(output$admin_reliability_kpis), "card")
        expect_true(!is.null(output$admin_reliability_comparison_plot))
      }

      # Drilldown modal for user1 (includes NA balanced_accuracy row) & markus_meier (0 trainings, 0 codings)
      expect_no_error({
        session$setInputs(trigger_rel_user_detail = user1_id)
        session$flushReact()
        session$setInputs(trigger_rel_user_detail = markus_id)
        session$flushReact()
      })

      # Nonexistent rating_id edit trigger -> error notification and early return
      expect_no_error({
        session$setInputs(trigger_edit_coding_rating = 999999L)
        session$flushReact()
      })
    }
  )

  # Isolated empty DB (0 trainings, 0 codings) + dropped coding_training table for diagnostics
  db_zero <- local_test_db(seeded = TRUE, empty = TRUE)
  con_zero <- db_zero$con

  shiny::testServer(
    adminServer,
    args = list(
      db_con = con_zero,
      current_user = "admin",
      current_user_id = 1L,
      scales_trigger = shiny::reactiveVal(0L),
      current_lang = "en"
    ),
    {
      # Global zero-training/zero-coding fallback
      session$setInputs(rel_scale_select = "all", rel_metric_select = "cohens_kappa", rel_search_query = "")
      session$flushReact()
      expect_match(ui_to_html(output$admin_reliability_kpis), "card")
      expect_true(!is.null(output$admin_reliability_comparison_plot))
      expect_match(ui_to_html(output$admin_reliability_table_container), "admin")

      # Scale-specific zero-training/zero-coding fallback
      session$setInputs(rel_scale_select = "1")
      session$flushReact()
      expect_match(ui_to_html(output$admin_reliability_kpis), "card")
      expect_match(ui_to_html(output$admin_reliability_table_container), "admin")

      # Drop coding_training to trigger missing-table & check-error diagnostics branches
      DBI::dbExecute(con_zero, "DROP TABLE coding_training;")
      session$setInputs(btn_refresh_diagnostics = 1L)
      session$flushReact()
      rep_missing <- db_diagnostics_report()
      expect_equal(rep_missing$existing_tables_count, 8L)
      expect_match(ui_to_html(output$db_diag_summary_cards), "bg-danger")
      expect_match(ui_to_html(output$db_diag_tables_overview), "bg-danger")
      expect_match(ui_to_html(output$db_diag_integrity_audit), "bg-warning")
    }
  )
})

