# ==============================================================================
# Shared Test Fixtures and Helpers for CodingManageR Test Suite
# ==============================================================================
# Automatically sourced by testthat prior to executing test-*.R files.
# Provides isolated SQLite database factories (direct DBI and pool::dbPool),
# bulk data seeders for pagination/plot threshold testing, temporary CSV
# builders, scale reactive helpers, and headless browser E2E skip guards.
# ==============================================================================

#' Create an isolated SQLite test database (direct DBI or pool::dbPool)
#'
#' @param seeded Logical; if TRUE, invokes seed_initial_data().
#' @param empty Logical; passed to seed_initial_data(empty = empty).
#' @param use_pool Logical; if TRUE, opens a pool::dbPool connection instead of direct RSQLite.
#' @param sqlite_path Optional file path; defaults to a temporary file when use_pool = TRUE
#'   or when file-backed persistence is needed, or ":memory:" for fast direct DBI tests.
#' @param env Calling environment for automatic teardown via withr::defer().
#' @return A list with elements \code{con}, \code{path}, and \code{cleanup}.
local_test_db <- function(seeded = TRUE,
                          empty = FALSE,
                          use_pool = FALSE,
                          sqlite_path = NULL,
                          env = parent.frame()) {
  owns_file <- FALSE
  db_path <- sqlite_path

  if (is.null(db_path)) {
    if (isTRUE(use_pool)) {
      db_path <- tempfile(pattern = "codingmanager_test_", fileext = ".sqlite")
      owns_file <- TRUE
    } else {
      db_path <- ":memory:"
    }
  }

  con <- if (isTRUE(use_pool)) {
    suppressMessages(
      get_db_pool(
        config = list(sqlite_path = db_path),
        fallback_to_sqlite = TRUE,
        force_pool = TRUE
      )
    )
  } else {
    c_raw <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    DBI::dbExecute(c_raw, "PRAGMA foreign_keys = ON;")
    c_raw
  }

  create_all_tables(con, verbose = FALSE)
  if (isTRUE(seeded)) {
    suppressMessages(seed_initial_data(con, empty = empty))
  }

  cleanup <- function() {
    suppressMessages(suppressWarnings(close_db_pool(con)))
    if (isTRUE(owns_file) && !is.null(db_path) && file.exists(db_path)) {
      unlink(db_path, force = TRUE)
      unlink(paste0(db_path, "-wal"), force = TRUE)
      unlink(paste0(db_path, "-shm"), force = TRUE)
    }
  }

  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(cleanup(), envir = env)
  }

  list(con = con, path = db_path, cleanup = cleanup)
}

#' Seed additional bulk records to exercise pagination (>25 rows) and multi-session plots (>15)
#'
#' @param con Active DBI or Pool connection.
#' @param n_users Number of extra user_tokens rows to insert.
#' @param n_texts Number of extra texts rows to insert (linked to source_id = 1).
#' @param n_scales Number of extra scale_definitions rows to insert.
#' @param n_ratings Number of extra user_ratings rows to insert.
#' @param n_trainings Number of extra coding_training rows to insert for user1 on scale "1".
seed_bulk_records <- function(con,
                              n_users = 0L,
                              n_texts = 0L,
                              n_scales = 0L,
                              n_ratings = 0L,
                              n_trainings = 0L) {
  if (n_users > 0L) {
    for (i in seq_len(n_users)) {
      DBI::dbExecute(
        con,
        "INSERT INTO user_tokens (username, token, permissions, is_active) VALUES (?, ?, 'user', 1)",
        params = list(sprintf("bulk_user_%02d", i), sprintf("bulk_tok_%02d", i))
      )
    }
  }

  if (n_scales > 0L) {
    max_sid <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(scale_id), 0) AS m FROM scale_definitions")$m[1]
    for (i in seq_len(n_scales)) {
      sid <- as.integer(max_sid) + i
      DBI::dbExecute(
        con,
        "INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1, def_n_2)
         VALUES (?, ?, ?, 2, ?, 'Level 0', 'Level 1', 'Level 2')",
        params = list(sid, sprintf("BulkS%02d", i), sprintf("K%d_BulkScale_%02d", sid, i), sprintf("Bulk scale description %02d", i))
      )
    }
  }

  if (n_texts > 0L) {
    max_tid <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(text_id), 0) AS m FROM texts")$m[1]
    for (i in seq_len(n_texts)) {
      tid <- as.integer(max_tid) + i
      DBI::dbExecute(
        con,
        "INSERT INTO texts (text_id, text, source_id) VALUES (?, ?, 1)",
        params = list(tid, sprintf("Bulk clinical narrative case #%03d for pagination verification.", tid))
      )
    }
  }

  if (n_ratings > 0L) {
    uid <- DBI::dbGetQuery(con, "SELECT user_id FROM user_tokens WHERE username = 'user1' LIMIT 1")$user_id[1]
    if (is.null(uid) || is.na(uid)) uid <- 1L
    tid <- DBI::dbGetQuery(con, "SELECT text_id FROM texts ORDER BY text_id ASC LIMIT 1")$text_id[1]
    for (i in seq_len(n_ratings)) {
      DBI::dbExecute(
        con,
        "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id, zeitstempel)
         VALUES (?, ?, '1', ?, 'kodierung', 1, ?)",
        params = list(as.integer(uid), as.integer(tid), as.integer(i %% 5L), sprintf("2026-11-%02d 10:%02d:00", ((i - 1L) %% 28L) + 1L, i %% 60L))
      )
    }
  }

  if (n_trainings > 0L) {
    uid <- DBI::dbGetQuery(con, "SELECT user_id FROM user_tokens WHERE username = 'user1' LIMIT 1")$user_id[1]
    if (is.null(uid) || is.na(uid)) uid <- 1L
    for (i in seq_len(n_trainings)) {
      DBI::dbExecute(
        con,
        "INSERT INTO coding_training (
           user_id, kategorie, datum, anzahl_einheiten,
           accuracy, balanced_accuracy, iota_index, avg_iota2,
           cohens_kappa, kappa_linear, kappa_squared, fleiss_kappa, kendall_w,
           alpha_nominal, alpha_ordinal, gwet_ac1, gwet_ac2_quadratic,
           macro_precision, macro_recall, macro_f1
         ) VALUES (?, '1', ?, 10, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          as.integer(uid),
          sprintf("2026-11-%02d", ((i - 1L) %% 28L) + 1L),
          round(60 + (i %% 35), 1),
          round(58 + (i %% 35), 1),
          round(0.45 + (i %% 5) * 0.1, 3),
          round(0.47 + (i %% 5) * 0.1, 3),
          round(0.40 + (i %% 5) * 0.11, 3),
          round(0.42 + (i %% 5) * 0.11, 3),
          round(0.44 + (i %% 5) * 0.11, 3),
          round(0.39 + (i %% 5) * 0.11, 3),
          round(0.43 + (i %% 5) * 0.11, 3),
          round(0.38 + (i %% 5) * 0.11, 3),
          round(0.41 + (i %% 5) * 0.11, 3),
          round(0.46 + (i %% 5) * 0.10, 3),
          round(0.48 + (i %% 5) * 0.10, 3),
          round(62 + (i %% 30), 1),
          round(61 + (i %% 30), 1),
          round(61.5 + (i %% 30), 1)
        )
      )
    }
  }

  invisible(TRUE)
}

#' Seed full gold-standard coverage (0..max_value) for a given scale so stratified training pools work
#'
#' @param con Active DBI or Pool connection.
#' @param scale_id Integer scale ID (e.g. 1L for K1_Empathie, max_value = 4).
#' @param max_value Integer maximum level of the scale.
seed_full_gold_standards <- function(con, scale_id = 1L, max_value = 4L) {
  gs_uid <- DBI::dbGetQuery(con, "SELECT user_id FROM user_tokens WHERE username = 'gold_standard' LIMIT 1")$user_id[1]
  if (is.null(gs_uid) || is.na(gs_uid)) gs_uid <- 1L

  # Texts 201..210 correspond to levels 0,0,1,1,2,2,3,3,4,4 for scale 1 in the seeded dataset
  base_tid <- if (scale_id == 1L) 201L else if (scale_id == 2L) 211L else 219L
  offset <- 0L
  for (lvl in 0:max_value) {
    for (rep_idx in 1:2) {
      tid <- base_tid + offset
      offset <- offset + 1L
      exists_txt <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM texts WHERE text_id = ?", params = list(tid))$n[1]
      if (exists_txt == 0L) {
        DBI::dbExecute(
          con,
          "INSERT INTO texts (text_id, text, source_id) VALUES (?, ?, 1)",
          params = list(tid, sprintf("Gold-standard calibration case for scale %d level %d (#%d).", scale_id, lvl, rep_idx))
        )
      }
      already <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM user_ratings WHERE text_id = ? AND kategorie = ? AND typ = 'gold_standard'",
        params = list(tid, as.character(scale_id))
      )$n[1]
      if (already == 0L) {
        DBI::dbExecute(
          con,
          "INSERT INTO user_ratings (user_id, text_id, kategorie, bewertung, typ, source_id, zeitstempel)
           VALUES (?, ?, ?, ?, 'gold_standard', 1, '2026-10-01 12:00:00')",
          params = list(as.integer(gs_uid), as.integer(tid), as.character(scale_id), as.integer(lvl))
        )
      }
    }
  }
  invisible(TRUE)
}

#' Write a temporary CSV file with optional UTF-8 BOM and register automatic deletion
#'
#' @param df Data frame to serialize.
#' @param sep Field delimiter (default ";").
#' @param dec Decimal separator (default ",").
#' @param bom Logical; if TRUE, prepends a UTF-8 Byte Order Mark.
#' @param env Calling environment for withr::defer cleanup.
#' @return Character path to the temporary CSV file.
write_temp_csv <- function(df, sep = ";", dec = ",", bom = FALSE, env = parent.frame()) {
  path <- tempfile(pattern = "codingmanager_csv_", fileext = ".csv")
  utils::write.table(
    df,
    file = path,
    sep = sep,
    dec = dec,
    row.names = FALSE,
    na = "",
    quote = TRUE,
    fileEncoding = "UTF-8"
  )
  if (isTRUE(bom)) {
    raw_bytes <- readBin(path, what = "raw", n = file.info(path)$size)
    bom_bytes <- as.raw(c(0xEF, 0xBB, 0xBF))
    writeBin(c(bom_bytes, raw_bytes), path)
  }
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(unlink(path, force = TRUE), envir = env)
  }
  path
}

#' Build reactive scale lookup vectors matching R/app.R conventions for shiny::testServer()
#'
#' @param con Active DBI or Pool connection.
#' @return A list with \code{kategorie_spalten} and \code{kategorien_liste} shiny::reactive() objects.
make_scale_reactives <- function(con) {
  kategorie_spalten <- shiny::reactive({
    df <- DBI::dbGetQuery(con, "SELECT scale_id, short_label, name FROM scale_definitions ORDER BY scale_id ASC")
    if (nrow(df) == 0) return(character(0))
    stats::setNames(as.character(df$scale_id), as.character(df$short_label))
  })
  kategorien_liste <- shiny::reactive({
    df <- DBI::dbGetQuery(con, "SELECT scale_id, short_label, name FROM scale_definitions ORDER BY scale_id ASC")
    if (nrow(df) == 0) return(character(0))
    labels <- paste0(df$name, " (", df$short_label, " #", df$scale_id, ")")
    stats::setNames(as.character(df$scale_id), labels)
  })
  list(kategorie_spalten = kategorie_spalten, kategorien_liste = kategorien_liste)
}

#' Render any Shiny UI tag or renderUI payload cleanly to a single HTML string for assertions
#'
#' @param ui_obj A shiny.tag, shiny.tag.list, or renderUI list(html = ...).
#' @return Single character scalar containing rendered HTML.
ui_to_html <- function(ui_obj) {
  if (is.null(ui_obj)) return("")
  if (is.list(ui_obj) && !inherits(ui_obj, "shiny.tag") && !inherits(ui_obj, "shiny.tag.list") && !is.null(ui_obj$html)) {
    return(as.character(ui_obj$html))
  }
  if (requireNamespace("htmltools", quietly = TRUE)) {
    rendered <- htmltools::renderTags(ui_obj)
    head_str <- as.character(rendered$head)
    body_str <- as.character(rendered$html)
    if (length(head_str) > 0 && nzchar(head_str[1])) {
      return(paste(head_str, body_str, sep = "\n"))
    }
    return(body_str)
  }
  paste(as.character(ui_obj), collapse = "\n")
}

#' Skip guard for headless browser shinytest2 E2E tests
skip_if_no_shinytest2 <- function() {
  testthat::skip_on_cran()
  if (tolower(Sys.getenv("CODINGMANAGER_SKIP_E2E", unset = "false")) %in% c("true", "1", "yes")) {
    testthat::skip("E2E shinytest2 tests skipped via CODINGMANAGER_SKIP_E2E environment variable.")
  }
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(chrome_path) || !nzchar(chrome_path)) {
    testthat::skip("No Chrome/Chromium binary detected for shinytest2/chromote E2E execution.")
  }
}
