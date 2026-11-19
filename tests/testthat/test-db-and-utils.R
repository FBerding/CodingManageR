test_that("format_int_locale formats integers without prettyNum warnings", {
  expect_silent(de_val <- format_int_locale(1234567, lang = "de"))
  expect_silent(en_val <- format_int_locale(1234567, lang = "en"))
  expect_equal(de_val, "1.234.567")
  expect_equal(en_val, "1,234,567")
  expect_equal(format_int_locale(NA, lang = "de"), "0")
})

test_that("reliability classification and schema validation work cleanly", {
  res_good <- classify_reliability_magnitude(0.85, lang = "en")
  expect_true(is.list(res_good))
  expect_true(nzchar(res_good$label))

  valid_df <- data.frame(username = "coder1", token = "abc", permissions = "user", stringsAsFactors = FALSE)
  expect_true(validate_import_schema("user_tokens", valid_df, lang = "en")$is_valid)

  bad_df <- data.frame(username = "coder1", stringsAsFactors = FALSE)
  expect_false(validate_import_schema("user_tokens", bad_df, lang = "en")$is_valid)
})

test_that("SQLite schema creation, seeding, and rating helpers work end-to-end", {
  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db), add = TRUE)

  con <- get_db_pool(config = list(sqlite_path = tmp_db), force_pool = FALSE)
  on.exit(close_db_pool(con), add = TRUE)

  expect_true(create_all_tables(con, verbose = FALSE))
  expect_true(seed_initial_data(con, empty = FALSE))

  admin_info <- get_user_info_db(con, "admin")
  expect_equal(nrow(admin_info), 1L)
  expect_equal(as.character(admin_info$permissions[1]), "admin")

  codings <- search_case_codings_db(con)
  expect_true(nrow(codings) > 0L)

  first_id <- as.integer(codings$rating_id[1])
  upd <- update_coding_rating_value_db(con, first_id, 1L)
  expect_true(isTRUE(upd$success))

  invalid_upd <- update_coding_rating_value_db(con, first_id, 999L)
  expect_false(isTRUE(invalid_upd$success))
})

test_that("coding_manager_ui renders CodingManageR root page cleanly", {
  ui_obj <- coding_manager_ui()
  html_str <- as.character(ui_obj)
  expect_match(html_str, "CodingManageR")
  expect_match(html_str, "page_content")
})
