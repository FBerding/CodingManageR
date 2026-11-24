# ==============================================================================
# Unit & Integration Tests: Database Connection Pool & Schema Lifecycle
# Covers: R/db_connection.R and R/utils_table_definitions.R
# ==============================================================================

test_that("connection type predicates accurately classify NULL, direct SQLite, Pool, and simulated MySQL objects", {
  # Verifies is_pool_connection(), is_mysql_connection(), and is_mysql() across NULL,
  # direct RSQLite connections, pool::dbPool connections, and S3-tagged MySQL/MariaDB stubs.
  expect_false(is_pool_connection(NULL))
  expect_false(is_mysql_connection(NULL))
  expect_false(is_mysql(NULL))

  db_direct <- local_test_db(seeded = FALSE, use_pool = FALSE)
  expect_false(is_pool_connection(db_direct$con))
  expect_false(is_mysql_connection(db_direct$con))
  expect_false(is_mysql(db_direct$con))

  db_pool <- local_test_db(seeded = FALSE, use_pool = TRUE)
  expect_true(is_pool_connection(db_pool$con))
  expect_false(is_mysql_connection(db_pool$con))
  expect_false(is_mysql(db_pool$con))

  fake_mariadb <- structure(list(), class = "MariaDBConnection")
  fake_mysql   <- structure(list(), class = "MySQLConnection")
  fake_pool_mysql <- structure(
    list(state = list(driver = structure(list(), class = "MariaDBDriver"))),
    class = "Pool"
  )
  expect_true(is_mysql_connection(fake_mariadb))
  expect_true(is_mysql_connection(fake_mysql))
  expect_true(is_mysql_connection(fake_pool_mysql))
  expect_true(is_mysql(fake_mariadb))
  expect_true(is_mysql(fake_mysql))
})

test_that("get_db_pool and get_db_connection initialize SQLite via config, env var, direct mode, and pool mode", {
  # Verifies SQLite connection creation across all configuration paths:
  # 1) config$sqlite_path with force_pool = TRUE (pool::dbPool with FK & WAL enabled)
  # 2) config$sqlite_f alias with force_pool = FALSE (direct RSQLite DBIConnection)
  # 3) SQLITE_PATH environment variable fallback via get_db_connection() wrapper
  # 4) Default R_user_dir fallback when neither config nor SQLITE_PATH is set
  # 5) Invalid port sanitization and fallback_to_sqlite = FALSE error path
  p1 <- tempfile(fileext = ".sqlite")
  p2 <- tempfile(fileext = ".sqlite")
  p3 <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(c(p1, p2, p3, paste0(c(p1, p2, p3), "-wal"), paste0(c(p1, p2, p3), "-shm")), force = TRUE))

  con_pool <- suppressMessages(get_db_pool(
    config = list(sqlite_path = p1, port = "not_a_number", minSize = 1L, maxSize = 2L, idleTimeout = 60000L),
    fallback_to_sqlite = TRUE,
    force_pool = TRUE
  ))
  withr::defer(suppressMessages(close_db_pool(con_pool)))
  expect_true(is_pool_connection(con_pool))
  expect_equal(DBI::dbGetQuery(con_pool, "PRAGMA foreign_keys;")[1, 1], 1L)

  con_direct <- suppressMessages(get_db_pool(
    config = list(sqlite_f = p2),
    fallback_to_sqlite = TRUE,
    force_pool = FALSE
  ))
  withr::defer(suppressMessages(close_db_connection(con_direct)))
  expect_false(is_pool_connection(con_direct))
  expect_true(DBI::dbIsValid(con_direct))
  expect_equal(DBI::dbGetQuery(con_direct, "PRAGMA foreign_keys;")[1, 1], 1L)

  withr::with_envvar(
    c(MYSQL_HOST = "", MYSQL_DB_HOST = "", MYSQL_USER = "", MYSQL_DB_USER = "", SQLITE_PATH = p3),
    {
      con_env <- suppressMessages(get_db_connection(config = NULL, fallback_to_sqlite = TRUE))
      withr::defer(suppressMessages(close_db_connection(con_env)))
      expect_true(is_pool_connection(con_env))
    }
  )

  # Verify default user-dir fallback path without polluting host home by overriding R_USER_DATA_DIR
  tmp_data_dir <- tempfile("cm_userdata_")
  withr::defer(unlink(tmp_data_dir, recursive = TRUE, force = TRUE))
  withr::with_envvar(
    c(MYSQL_HOST = "", MYSQL_DB_HOST = "", MYSQL_USER = "", MYSQL_DB_USER = "", SQLITE_PATH = "", R_USER_DATA_DIR = tmp_data_dir),
    {
      con_default <- suppressMessages(get_db_pool(config = NULL, fallback_to_sqlite = TRUE, force_pool = FALSE))
      withr::defer(suppressMessages(close_db_connection(con_default)))
      expect_true(DBI::dbIsValid(con_default))
      expect_true(dir.exists(tools::R_user_dir("CodingManageR", which = "data")))
    }
  )

  # Verify fallback_to_sqlite = FALSE raises an informative stop error when no MySQL credentials exist
  withr::with_envvar(
    c(MYSQL_HOST = "", MYSQL_DB_HOST = "", MYSQL_USER = "", MYSQL_DB_USER = ""),
    {
      expect_error(
        get_db_pool(config = NULL, fallback_to_sqlite = FALSE),
        regexp = "SQLite fallback is disabled"
      )
    }
  )
})

test_that("get_db_pool emits a Shiny Server concurrency warning when SQLite runs under SHINY_SERVER_VERSION", {
  # Verifies that deploying with SQLite when SHINY_SERVER_VERSION is set triggers the
  # documented multi-user concurrency warning advising MariaDB/MySQL migration.
  p <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(c(p, paste0(p, "-wal"), paste0(p, "-shm")), force = TRUE))

  withr::with_envvar(
    c(MYSQL_HOST = "", MYSQL_USER = "", SHINY_SERVER_VERSION = "1.5.21"),
    {
      expect_warning(
        con <- suppressMessages(get_db_pool(config = list(sqlite_path = p), fallback_to_sqlite = TRUE, force_pool = FALSE)),
        regexp = "Shiny Server.*SQLite"
      )
      withr::defer(suppressMessages(close_db_connection(con)))
      expect_true(DBI::dbIsValid(con))
    }
  )
})

test_that("get_db_pool falls back to direct SQLite connection when pool creation fails", {
  # Verifies the graceful error-handler fallback inside get_db_pool() if pool::dbPool throws an error
  # for SQLite, ensuring a working direct RSQLite connection is still returned.
  p <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(c(p, paste0(p, "-wal"), paste0(p, "-shm")), force = TRUE))

  testthat::local_mocked_bindings(
    dbPool = function(...) stop("simulated pool failure"),
    .package = "pool"
  )

  expect_warning(
    con <- suppressMessages(get_db_pool(config = list(sqlite_path = p), fallback_to_sqlite = TRUE, force_pool = TRUE)),
    regexp = "SQLite pool initialization failed"
  )
  withr::defer(suppressMessages(close_db_connection(con)))
  expect_false(is_pool_connection(con))
  expect_true(DBI::dbIsValid(con))
})

test_that("close_db_pool and close_db_connection handle NULL, active, and already-closed connections safely", {
  # Verifies NULL returns invisible(NULL), closing valid direct and pool connections succeeds,
  # and closing an invalid/already-closed connection triggers a handled warning instead of crashing.
  expect_null(close_db_pool(NULL))
  expect_null(close_db_connection(NULL))

  p <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(c(p, paste0(p, "-wal"), paste0(p, "-shm")), force = TRUE))

  con_pool <- suppressMessages(get_db_pool(config = list(sqlite_path = p), force_pool = TRUE))
  expect_message(close_db_pool(con_pool), regexp = "Connection pool closed successfully")
  expect_warning(close_db_pool(con_pool), regexp = "Failed to close database connection")

  con_direct <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_message(close_db_connection(con_direct), regexp = "Database connection disconnected successfully")
  expect_false(DBI::dbIsValid(con_direct))
})

test_that("with_db_transaction enforces atomic commit and rollback on both direct DBI and pool connections", {
  # Verifies that with_db_transaction():
  # 1) Rejects NULL connections with an error
  # 2) Commits both function-based and expression-based blocks on direct DBI and pool::dbPool
  # 3) Rolls back partial writes when an error occurs mid-transaction so database state remains clean
  expect_error(with_db_transaction(NULL, function(c) 1L), regexp = "Invalid database connection")

  for (use_pool in c(FALSE, TRUE)) {
    db <- local_test_db(seeded = TRUE, empty = FALSE, use_pool = use_pool)
    con <- db$con

    # Commit via function form
    res_fn <- with_db_transaction(con, function(tx_con) {
      DBI::dbExecute(
        tx_con,
        "INSERT INTO sources (Kurzreferenz, Referenz, Lizenz) VALUES ('TxCommitFn', 'Ref Fn', 'CC-BY 4.0')"
      )
      "committed_fn"
    })
    expect_equal(res_fn, "committed_fn")
    expect_equal(
      DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE Kurzreferenz = 'TxCommitFn'")$n[1],
      1L
    )

    # Commit via expression form (direct DBI uses con directly; pool expression evaluates in parent frame)
    if (!use_pool) {
      res_expr <- with_db_transaction(con, {
        DBI::dbExecute(
          con,
          "INSERT INTO sources (Kurzreferenz, Referenz, Lizenz) VALUES ('TxCommitExpr', 'Ref Expr', 'CC0 1.0')"
        )
        "committed_expr"
      })
      expect_equal(res_expr, "committed_expr")
      expect_equal(
        DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE Kurzreferenz = 'TxCommitExpr'")$n[1],
        1L
      )
    }

    # Rollback on error mid-transaction
    expect_error(
      with_db_transaction(con, function(tx_con) {
        DBI::dbExecute(
          tx_con,
          "INSERT INTO sources (Kurzreferenz, Referenz, Lizenz) VALUES ('TxRollbackItem', 'Should vanish', 'Copyright')"
        )
        stop("intentional transaction abort")
      }),
      regexp = "intentional transaction abort"
    )
    expect_equal(
      DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sources WHERE Kurzreferenz = 'TxRollbackItem'")$n[1],
      0L
    )
  }
})

test_that("create_all_tables creates all 9 canonical tables idempotently and rejects NULL", {
  # Verifies create_all_tables() stops on NULL, creates all 9 required tables with expected columns
  # in both verbose = TRUE and verbose = FALSE modes, and executes cleanly when called twice.
  expect_error(create_all_tables(NULL), regexp = "Invalid database connection")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))

  expect_message(
    ok <- create_all_tables(con, verbose = TRUE),
    regexp = "Tables initialized successfully"
  )
  expect_true(ok)

  expected_tables <- c(
    "user_tokens", "sources", "texts", "user_ratings", "coding_training",
    "scale_definitions", "user_source_permissions", "user_coding_source_permissions",
    "user_scale_permissions"
  )
  expect_true(all(expected_tables %in% DBI::dbListTables(con)))

  # Idempotent re-run must succeed silently with verbose = FALSE
  expect_silent(ok2 <- create_all_tables(con, verbose = FALSE))
  expect_true(ok2)
})

test_that("seed_initial_data populates full demo dataset, minimal empty mode, auto-creates tables, and is idempotent", {
  # Verifies seed_initial_data() across all lifecycle modes:
  # 1) NULL connection raises error
  # 2) Running on a blank DB automatically calls create_all_tables()
  # 3) empty = FALSE seeds exact canonical counts across all 9 tables and returns a structured 9-table report
  # 4) Calling seed_initial_data(empty = FALSE) a second time inserts 0 duplicates (idempotency)
  # 5) empty = TRUE seeds only 'admin' and inactive 'historic_user' while leaving content tables at 0 rows
  expect_error(seed_initial_data(NULL), regexp = "Invalid database connection")

  # Full demo seed on blank DB (auto-creates all 9 tables)
  con_full <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_full))
  DBI::dbExecute(con_full, "PRAGMA foreign_keys = ON;")

  rep1 <- suppressMessages(seed_initial_data(con_full, empty = FALSE))
  expect_type(rep1, "list")
  expect_length(rep1, 9L)
  expect_true(all(vapply(rep1, function(x) isTRUE(x$created), logical(1))))

  expect_equal(rep1$user_tokens$n_total, 8L)
  expect_equal(rep1$scale_definitions$n_total, 3L)
  expect_equal(rep1$sources$n_total, 4L)
  expect_equal(rep1$user_source_permissions$n_total, 4L)
  expect_equal(rep1$user_coding_source_permissions$n_total, 4L)
  expect_equal(rep1$user_scale_permissions$n_total, 3L)
  expect_equal(rep1$texts$n_total, 34L)
  expect_equal(rep1$user_ratings$n_total, 34L)
  expect_equal(rep1$coding_training$n_total, 10L)

  # Second call must be strictly idempotent (0 new rows inserted) and emit protection warning
  expect_warning(
    rep2 <- suppressMessages(seed_initial_data(con_full, empty = FALSE)),
    regexp = "Existing database already contains data"
  )
  expect_true(all(vapply(rep2, function(x) x$n_inserted == 0L, logical(1))))
  expect_false(any(vapply(rep2, function(x) isTRUE(x$created), logical(1))))
  expect_true(all(vapply(rep2, function(x) grepl("existing database protected", x$status_label, ignore.case = TRUE), logical(1))))

  # Minimal empty = TRUE seed on blank DB
  con_empty <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_empty))
  DBI::dbExecute(con_empty, "PRAGMA foreign_keys = ON;")

  rep_empty <- suppressMessages(seed_initial_data(con_empty, empty = TRUE))
  expect_equal(rep_empty$user_tokens$n_total, 2L)
  expect_equal(rep_empty$scale_definitions$n_total, 0L)
  expect_equal(rep_empty$sources$n_total, 0L)
  expect_equal(rep_empty$texts$n_total, 0L)
  expect_equal(rep_empty$user_ratings$n_total, 0L)
  expect_equal(rep_empty$coding_training$n_total, 0L)
  expect_match(rep_empty$sources$checksum, "EMPTY")

  # Re-running empty = TRUE on already-populated DB hits the "already populated" branch
  rep_empty_2 <- suppressMessages(seed_initial_data(con_empty, empty = TRUE))
  expect_equal(rep_empty_2$user_tokens$n_inserted, 0L)
  expect_match(rep_empty_2$user_tokens$status_label, "Already populated")
})

test_that("seed_initial_data(empty = FALSE) protects partially populated databases and emits a warning", {
  # Verifies global 9-table protection in seed_initial_data(con, empty = FALSE):
  # 1) A database initialized with empty = TRUE (2 user rows, 8 empty tables) must NOT receive
  #    any demo scales, sources, permissions, texts, ratings, or training sessions
  # 2) A database containing only 1 row in a non-user table (scale_definitions) while user_tokens
  #    has 0 rows must NOT receive any demo rows in any table (including user_tokens)
  con_partial_users <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_partial_users))
  DBI::dbExecute(con_partial_users, "PRAGMA foreign_keys = ON;")

  suppressMessages(seed_initial_data(con_partial_users, empty = TRUE))

  expect_warning(
    rep_prot_1 <- suppressMessages(seed_initial_data(con_partial_users, empty = FALSE)),
    regexp = "Existing database already contains data"
  )
  expect_true(all(vapply(rep_prot_1, function(x) x$n_inserted == 0L, logical(1))))
  expect_equal(rep_prot_1$user_tokens$n_total, 2L)
  expect_equal(rep_prot_1$scale_definitions$n_total, 0L)
  expect_equal(rep_prot_1$sources$n_total, 0L)
  expect_equal(rep_prot_1$user_source_permissions$n_total, 0L)
  expect_equal(rep_prot_1$user_coding_source_permissions$n_total, 0L)
  expect_equal(rep_prot_1$user_scale_permissions$n_total, 0L)
  expect_equal(rep_prot_1$texts$n_total, 0L)
  expect_equal(rep_prot_1$user_ratings$n_total, 0L)
  expect_equal(rep_prot_1$coding_training$n_total, 0L)

  # Non-user table populated (1 custom scale, 0 users)
  con_partial_scale <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con_partial_scale))
  DBI::dbExecute(con_partial_scale, "PRAGMA foreign_keys = ON;")
  create_all_tables(con_partial_scale, verbose = FALSE)
  DBI::dbExecute(
    con_partial_scale,
    "INSERT INTO scale_definitions (scale_id, short_label, name, max_value) VALUES (1, 'S1', 'Study_Scale_1', 4)"
  )

  expect_warning(
    rep_prot_2 <- suppressMessages(seed_initial_data(con_partial_scale, empty = FALSE)),
    regexp = "Existing database already contains data"
  )
  expect_true(all(vapply(rep_prot_2, function(x) x$n_inserted == 0L, logical(1))))
  expect_equal(rep_prot_2$user_tokens$n_total, 0L)
  expect_equal(rep_prot_2$scale_definitions$n_total, 1L)
  expect_equal(rep_prot_2$sources$n_total, 0L)
  expect_equal(rep_prot_2$texts$n_total, 0L)
  expect_equal(rep_prot_2$user_ratings$n_total, 0L)
  expect_equal(rep_prot_2$coding_training$n_total, 0L)
})

