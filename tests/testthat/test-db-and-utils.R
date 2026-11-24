# Note: This file was generated with the assistance of AI.

# ==============================================================================
# Unit & Integration Tests: Database Helper Utilities
# Covers: R/utils_db.R
# ==============================================================================

test_that("generate_security_token creates alphanumeric tokens of requested length and validates inputs", {
  # Verifies token length, strictly alphanumeric character set, randomness across calls,
  # and input validation rejecting zero, negative, or non-numeric length arguments.
  tok_default <- generate_security_token()
  expect_type(tok_default, "character")
  expect_equal(nchar(tok_default), 12L)
  expect_match(tok_default, "^[A-Za-z0-9]{12}$")

  tok_24 <- generate_security_token(24)
  expect_equal(nchar(tok_24), 24L)
  expect_match(tok_24, "^[A-Za-z0-9]{24}$")
  expect_false(identical(generate_security_token(24), generate_security_token(24)))

  expect_error(generate_security_token(0), regexp = "positive integer")
  expect_error(generate_security_token(-5), regexp = "positive integer")
  expect_error(generate_security_token("12"), regexp = "positive integer")
})

test_that("get_db_random_function selects RANDOM() for NULL/SQLite and RAND() for MySQL/MariaDB", {
  # Verifies SQL dialect routing for random row sampling across NULL, SQLite, and MySQL stubs.
  db <- local_test_db(seeded = FALSE)
  expect_equal(get_db_random_function(NULL), "RANDOM()")
  expect_equal(get_db_random_function(db$con), "RANDOM()")
  expect_equal(get_db_random_function(structure(list(), class = "MariaDBConnection")), "RAND()")
  expect_equal(get_db_random_function(structure(list(), class = "MySQLConnection")), "RAND()")
})

test_that("get_user_info_db resolves metadata by username, user_id, reactive inputs, and unknown fallbacks", {
  # Verifies get_user_info_db() resolves admin and standard users by username or user_id,
  # unwraps Shiny reactive values inside reactiveConsole/isolate, fills blank username from DB
  # when user_id is supplied, and falls back gracefully on unknown users or invalid connections.
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  admin_info <- get_user_info_db(con, "admin")
  expect_type(admin_info, "list")
  expect_equal(admin_info$name, "admin")
  expect_equal(admin_info$permissions, "admin")
  expect_true(is.integer(admin_info$id) && admin_info$id >= 1L)

  user1_info <- get_user_info_db(con, "user1")
  expect_equal(user1_info$name, "user1")
  expect_equal(user1_info$permissions, "user")

  # Lookup by user_id with empty username fills username from database
  by_id <- get_user_info_db(con, username = "", user_id = user1_info$id)
  expect_equal(by_id$id, user1_info$id)
  expect_equal(by_id$name, "user1")
  expect_equal(by_id$permissions, "user")

  # Lookup via Shiny reactive expressions
  shiny::isolate({
    rx_name <- shiny::reactiveVal("eva_lindner")
    rx_id   <- shiny::reactiveVal(NULL)
    eva_info <- get_user_info_db(con, rx_name, rx_id)
    expect_equal(eva_info$name, "eva_lindner")
    expect_equal(eva_info$permissions, "user")

    rx_id_num <- shiny::reactiveVal(admin_info$id)
    rx_blank  <- shiny::reactiveVal("")
    adm_by_rx <- get_user_info_db(con, rx_blank, rx_id_num)
    expect_equal(adm_by_rx$name, "admin")
    expect_equal(adm_by_rx$permissions, "admin")
  })

  # Unknown user falls back to id = 1L and role = "user" (or "admin" if username == "admin")
  ghost_info <- get_user_info_db(con, "nonexistent_user")
  expect_equal(ghost_info$id, 1L)
  expect_equal(ghost_info$permissions, "user")

  # Invalid connection triggers tryCatch fallback cleanly
  fallback_admin <- get_user_info_db(NULL, "admin")
  expect_equal(fallback_admin$id, 1L)
  expect_equal(fallback_admin$permissions, "admin")
})

test_that("get_user_permitted_sources_db and get_user_permitted_coding_sources_db enforce role and RBAC permissions", {
  # Verifies that administrators unconditionally receive all 4 seeded source IDs, while
  # non-admin users receive only their explicitly authorized source IDs (or empty integer(0)
  # for unauthorized users or NULL/NA user_ids).
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  admin_id  <- get_user_info_db(con, "admin")$id
  user1_id  <- get_user_info_db(con, "user1")$id
  markus_id <- get_user_info_db(con, "markus_meier")$id
  eva_id    <- get_user_info_db(con, "eva_lindner")$id

  # Admin gets all 4 sources
  expect_equal(sort(get_user_permitted_sources_db(con, admin_id, is_admin = TRUE)), c(1L, 2L, 3L, 4L))
  expect_equal(sort(get_user_permitted_coding_sources_db(con, admin_id, is_admin = TRUE)), c(1L, 2L, 3L, 4L))

  # user1 -> sources 1, 2; markus_meier -> sources 1, 3; eva_lindner -> none
  expect_equal(sort(get_user_permitted_sources_db(con, user1_id, is_admin = FALSE)), c(1L, 2L))
  expect_equal(sort(get_user_permitted_coding_sources_db(con, user1_id, is_admin = FALSE)), c(1L, 2L))
  expect_equal(sort(get_user_permitted_sources_db(con, markus_id, is_admin = FALSE)), c(1L, 3L))
  expect_equal(sort(get_user_permitted_coding_sources_db(con, markus_id, is_admin = FALSE)), c(1L, 3L))
  expect_length(get_user_permitted_sources_db(con, eva_id, is_admin = FALSE), 0L)
  expect_length(get_user_permitted_coding_sources_db(con, eva_id, is_admin = FALSE), 0L)

  # NULL / NA user_id and error handling
  expect_length(get_user_permitted_sources_db(con, NULL, is_admin = FALSE), 0L)
  expect_length(get_user_permitted_sources_db(con, NA_integer_, is_admin = FALSE), 0L)
  expect_length(get_user_permitted_coding_sources_db(con, NULL, is_admin = FALSE), 0L)
  expect_length(get_user_permitted_sources_db(NULL, admin_id, is_admin = TRUE), 0L)
  expect_length(get_user_permitted_coding_sources_db(NULL, admin_id, is_admin = TRUE), 0L)
})

test_that("count_unrated_cases_db and draw_unrated_case_db exclude user ratings and gold standards across filter modes", {
  # Verifies unrated case counting and random case drawing:
  # 1) Excludes cases already coded by the active user OR marked as 'gold_standard' on that scale
  # 2) Respects mode = "quelle" with source_id filter and mode = "alle" with permitted_source_ids
  # 3) Handles NULL/empty arguments safely and supports legacy schemas containing a 'befund_text' column
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con
  user1_id <- get_user_info_db(con, "user1")$id

  # Invalid argument guards
  expect_equal(count_unrated_cases_db(NULL, "1", user1_id), 0L)
  expect_equal(count_unrated_cases_db(con, "", user1_id), 0L)
  expect_equal(nrow(draw_unrated_case_db(NULL, "1", user1_id)), 0L)
  expect_equal(nrow(draw_unrated_case_db(con, NA_character_, user1_id)), 0L)

  # All sources for user1 on scale "1": 34 texts minus 4 gold_standard (101..104) minus text 201 rated by user1 = 29
  cnt_all <- count_unrated_cases_db(con, "1", user1_id, mode = "alle")
  expect_equal(cnt_all, 29L)

  # Restricted to permitted sources c(1, 2)
  cnt_perm <- count_unrated_cases_db(con, "1", user1_id, mode = "alle", permitted_source_ids = c(1L, 2L))
  expect_gt(cnt_perm, 0L)
  expect_lt(cnt_perm, cnt_all)

  # Filtered by single source_id = 1
  cnt_src1 <- count_unrated_cases_db(con, "1", user1_id, mode = "quelle", source_id = 1L)
  expect_gt(cnt_src1, 0L)

  drawn_src1 <- draw_unrated_case_db(con, "1", user1_id, mode = "quelle", source_id = 1L)
  expect_equal(nrow(drawn_src1), 1L)
  expect_equal(drawn_src1$source_id[1], 1L)
  expect_true(all(c("text_id", "text", "source_id", "befund_text") %in% colnames(drawn_src1)))
  expect_false(drawn_src1$text_id[1] %in% c(101L, 102L, 103L, 104L, 201L))

  # Draw using permitted_source_ids in "alle" mode
  drawn_perm <- draw_unrated_case_db(con, "1", user1_id, mode = "alle", permitted_source_ids = c(2L))
  expect_equal(nrow(drawn_perm), 1L)
  expect_equal(drawn_perm$source_id[1], 2L)

  # Legacy schema with optional 'befund_text' column fallback when text is empty
  con_leg <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_leg))
  DBI::dbExecute(con_leg, "CREATE TABLE texts (text_id INTEGER PRIMARY KEY, text TEXT, befund_text TEXT, source_id INT);")
  DBI::dbExecute(con_leg, "CREATE TABLE user_ratings (rating_id INTEGER PRIMARY KEY, user_id INT, text_id INT, kategorie TEXT, typ TEXT);")
  DBI::dbExecute(con_leg, "INSERT INTO texts (text_id, text, befund_text, source_id) VALUES (501, '', 'Legacy Befund Text', 1);")

  drawn_leg <- draw_unrated_case_db(con_leg, "1", 1L, mode = "alle")
  expect_equal(nrow(drawn_leg), 1L)
  expect_equal(drawn_leg$befund_text[1], "Legacy Befund Text")
})

test_that("search_case_codings_db filters by scale, type, and multi-field search queries including legacy schema", {
  # Verifies search_case_codings_db() returns all 34 seeded ratings ordered by rating_id DESC,
  # filters accurately by scale_id/name/short_label, rating type, #rating_id/text_id/text/username,
  # handles NULL connections, and supports legacy user_ratings tables without user_id.
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  expect_equal(nrow(search_case_codings_db(NULL)), 0L)

  all_res <- search_case_codings_db(con)
  expect_equal(nrow(all_res), 34L)
  expect_true(all(diff(all_res$rating_id) < 0)) # Strictly descending order

  # Filter by scale_id, scale_name, and short_label
  by_scale_id <- search_case_codings_db(con, scale_filter = "2")
  expect_gt(nrow(by_scale_id), 0L)
  expect_true(all(by_scale_id$resolved_scale_id == 2L))

  by_scale_name <- search_case_codings_db(con, scale_filter = "S2_Instructional_Structure")
  expect_equal(nrow(by_scale_name), nrow(by_scale_id))

  # Filter by rating type
  gold_only <- search_case_codings_db(con, type_filter = "gold_standard")
  expect_equal(nrow(gold_only), 12L)
  expect_true(all(gold_only$typ == "gold_standard"))

  # Free-text search by #rating_id, username, and case narrative substring
  by_hash_id <- search_case_codings_db(con, search_query = "#201")
  expect_true(201L %in% by_hash_id$rating_id)

  by_user <- search_case_codings_db(con, search_query = "eva_lindner")
  expect_equal(nrow(by_user), 5L)
  expect_true(all(by_user$username == "eva_lindner"))

  by_text <- search_case_codings_db(con, search_query = "photosynthesis")
  expect_gt(nrow(by_text), 0L)
  expect_true(all(by_text$text_id == 102L))

  # Non-matching query returns 0 rows
  expect_equal(nrow(search_case_codings_db(con, search_query = "zzzz_no_match_999")), 0L)

  # Legacy user_ratings schema using username instead of user_id
  con_leg <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_leg))
  DBI::dbExecute(con_leg, "CREATE TABLE user_tokens (user_id INT, username TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE sources (source_id INT, Kurzreferenz TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE texts (text_id INT, text TEXT, source_id INT);")
  DBI::dbExecute(con_leg, "CREATE TABLE scale_definitions (scale_id INT, short_label TEXT, name TEXT, max_value INT, def_desc TEXT, def_n_0 TEXT, def_n_1 TEXT, def_n_2 TEXT, def_n_3 TEXT, def_n_4 TEXT, def_n_5 TEXT);")
  DBI::dbExecute(con_leg, "CREATE TABLE user_ratings (rating_id INT, username TEXT, text_id INT, kategorie TEXT, bewertung INT, typ TEXT, source_id INT, zeitstempel TEXT);")
  DBI::dbExecute(con_leg, "INSERT INTO texts VALUES (1, 'Legacy text', 1);")
  DBI::dbExecute(con_leg, "INSERT INTO scale_definitions VALUES (1, 'Emp', 'K1', 3, 'Desc', 'd0', 'd1', 'd2', 'd3', NULL, NULL);")
  DBI::dbExecute(con_leg, "INSERT INTO user_ratings VALUES (99, 'legacy_coder', 1, '1', 2, 'kodierung', 1, '2026-01-01');")

  leg_search <- search_case_codings_db(con_leg)
  expect_equal(nrow(leg_search), 1L)
  expect_equal(leg_search$username[1], "legacy_coder")

  leg_detail <- get_coding_detail_db(con_leg, 99L)
  expect_equal(nrow(leg_detail), 1L)
  expect_equal(leg_detail$username[1], "legacy_coder")
})

test_that("get_coding_detail_db and update_coding_rating_value_db fetch full scale stages and validate updates", {
  # Verifies get_coding_detail_db() joins rating, text, coder, and def_n_0..def_n_5 stages,
  # and update_coding_rating_value_db() enforces strict 0..max_value range checks while
  # updating only user_ratings.bewertung in place without altering metadata columns.
  db <- local_test_db(seeded = TRUE, empty = FALSE)
  con <- db$con

  expect_equal(nrow(get_coding_detail_db(NULL, 201L)), 0L)
  expect_equal(nrow(get_coding_detail_db(con, NA_integer_)), 0L)
  expect_equal(nrow(get_coding_detail_db(con, 999999L)), 0L)

  detail <- get_coding_detail_db(con, 201L)
  expect_equal(nrow(detail), 1L)
  expect_equal(detail$rating_id[1], 201L)
  expect_equal(detail$scale_max_value[1], 4L)
  expect_true(nzchar(detail$def_n_0[1]))
  expect_true(nzchar(detail$def_n_4[1]))

  # Invalid connection / missing rating_id / nonexistent rating_id
  expect_false(update_coding_rating_value_db(NULL, 201L, 2L)$success)
  expect_false(update_coding_rating_value_db(con, NA_integer_, 2L)$success)
  expect_false(update_coding_rating_value_db(con, 999999L, 2L)$success)

  # Out-of-range and non-numeric values rejected
  expect_false(update_coding_rating_value_db(con, 201L, -1L)$success)
  expect_false(update_coding_rating_value_db(con, 201L, 5L)$success)
  expect_false(update_coding_rating_value_db(con, 201L, "abc")$success)

  # Valid in-range update persists only bewertung
  before <- DBI::dbGetQuery(con, "SELECT * FROM user_ratings WHERE rating_id = 201")
  upd <- update_coding_rating_value_db(con, 201L, 4L)
  expect_true(upd$success)
  expect_equal(upd$old_value, before$bewertung[1])
  expect_equal(upd$new_value, 4L)
  expect_equal(upd$text_id, before$text_id[1])

  after <- DBI::dbGetQuery(con, "SELECT * FROM user_ratings WHERE rating_id = 201")
  expect_equal(after$bewertung[1], 4L)
  expect_equal(after$user_id[1], before$user_id[1])
  expect_equal(after$typ[1], before$typ[1])
  expect_equal(after$text_id[1], before$text_id[1])

  # Empty ratings table & NULL search_query edge cases
  db_empty <- local_test_db(seeded = TRUE, empty = TRUE)
  expect_equal(nrow(search_case_codings_db(db_empty$con)), 0L)
  expect_equal(nrow(search_case_codings_db(con, search_query = NULL)), 34L)

  # Database write error fallback in update_coding_rating_value_db() via SQLite trigger
  DBI::dbExecute(
    con,
    "CREATE TEMP TRIGGER fail_rating_upd BEFORE UPDATE ON user_ratings
     BEGIN SELECT RAISE(ABORT, 'simulated read-only database'); END;"
  )
  fail_res <- update_coding_rating_value_db(con, 201L, 3L)
  expect_false(fail_res$success)
  expect_match(fail_res$message, "Database update failed")
  DBI::dbExecute(con, "DROP TRIGGER fail_rating_upd;")
})

