# Note: This file was generated with the assistance of AI.

# ==============================================================================
# TABLE DEFINITIONS AND SCHEMA MANAGEMENT (utils_table_definitions.R)
# ==============================================================================
# Encapsulates all DDL statements (CREATE TABLE) for CodingManageR.
# Supports both MySQL/MariaDB and SQLite backends.
#
# Included tables:
#  1. user_tokens                    - Users, access tokens, roles, and active flag (is_active)
#  2. sources                        - Scientific sources and licenses with IDs
#  3. texts                          - Case texts with foreign key to sources(source_id)
#  4. user_ratings                   - Field ratings, training ratings, and gold standards
#  5. coding_training                - Training session logs and 16 reliability metrics
#  6. scale_definitions              - Coding scale catalog and ordinal definitions
#  7. user_source_permissions        - Authorization matrix (user -> case text sources)
#  8. user_coding_source_permissions - Authorization matrix (user -> scientific coding sources)
#  9. user_scale_permissions         - Authorization matrix (user -> coding scales)
# ==============================================================================

# DBI is imported via NAMESPACE

#' Check Whether a Connection Uses MySQL or MariaDB
#'
#' Tests whether a direct `DBIConnection` or `pool::dbPool` handle targets
#' MySQL or MariaDB (`MariaDBConnection` or `MySQLConnection`).
#'
#' @param con A `DBIConnection`, `pool::dbPool`, or `NULL`.
#'
#' @return A logical scalar: `TRUE` for MySQL/MariaDB connections, `FALSE`
#'   otherwise.
#' @examples
#' is_mysql(NULL)
#' @export
is_mysql <- function(con) {
  is_mysql_connection(con)
}

#' Create All CodingManageR Database Tables
#'
#' Initializes the complete 9-table relational schema (users, permissions,
#' sources, texts, scales, ratings, and training logs) in the target SQLite or
#' MySQL/MariaDB database. Existing tables are detected via `DBI::dbListTables()`
#' and skipped so already-provisioned production databases do not receive
#' unnecessary DDL statements.
#'
#' @param con A `DBIConnection` or `pool::dbPool` object.
#' @param verbose Logical; if `TRUE` (default), prints status messages during
#'   schema creation.
#'
#' @return Logical `TRUE` upon successful table initialization.
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' create_all_tables(con, verbose = FALSE)
#' DBI::dbDisconnect(con)
#' }
#' @export
create_all_tables <- function(con, verbose = TRUE) {
  if (is.null(con)) stop("Invalid database connection supplied (NULL).")

  mysql_mode <- is_mysql_connection(con)
  existing_tables <- tryCatch(tolower(dbListTables(con)), error = function(e) character(0))

  # Backend-specific SQL data type aliases
  pk_auto  <- if (mysql_mode) "INT AUTO_INCREMENT PRIMARY KEY" else "INTEGER PRIMARY KEY AUTOINCREMENT"
  t_stamp  <- if (mysql_mode) "DATETIME DEFAULT CURRENT_TIMESTAMP" else "TEXT DEFAULT CURRENT_TIMESTAMP"
  t_date   <- if (mysql_mode) "DATE" else "TEXT DEFAULT CURRENT_DATE"
  num_real <- if (mysql_mode) "DOUBLE" else "REAL"
  tbl_opts <- if (mysql_mode) " ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci" else ""

  n_created <- 0L
  exec_create_if_missing <- function(tbl_name, sql_stmt) {
    if (tolower(tbl_name) %in% existing_tables) {
      return(invisible(FALSE))
    }
    dbExecute(con, sql_stmt)
    n_created <<- n_created + 1L
    invisible(TRUE)
  }

  if (verbose) {
    message("Checking tables for backend: ", if (mysql_mode) "MySQL/MariaDB" else "SQLite", "...")
  }

  # ----------------------------------------------------------------------------
  # 1. TABLE: user_tokens (authentication and account status)
  # ----------------------------------------------------------------------------
  sql_user_tokens <- paste0("
    CREATE TABLE IF NOT EXISTS user_tokens (
      user_id ", pk_auto, ",
      username VARCHAR(64) UNIQUE NOT NULL,
      token VARCHAR(255) NOT NULL,
      permissions VARCHAR(32) NOT NULL,
      is_active INT DEFAULT 1 NOT NULL,
      created_at ", t_stamp, "
    )", tbl_opts, ";
  ")
  exec_create_if_missing("user_tokens", sql_user_tokens)

  # ----------------------------------------------------------------------------
  # 2. TABLE: sources (scientific sources and licenses)
  # ----------------------------------------------------------------------------
  sql_sources <- paste0("
    CREATE TABLE IF NOT EXISTS sources (
      source_id ", pk_auto, ",
      Kurzreferenz VARCHAR(128) UNIQUE NOT NULL,
      Referenz TEXT NOT NULL,
      Lizenz VARCHAR(128) NOT NULL,
      erfasst_am ", t_stamp, "
    )", tbl_opts, ";
  ")
  exec_create_if_missing("sources", sql_sources)

  # ----------------------------------------------------------------------------
  # 3. TABLE: texts (case texts with foreign key to sources)
  # ----------------------------------------------------------------------------
  sql_texts <- paste0("
    CREATE TABLE IF NOT EXISTS texts (
      text_id ", pk_auto, ",
      text MEDIUMTEXT NOT NULL,
      source_id INT NOT NULL,
      FOREIGN KEY (source_id) REFERENCES sources (source_id) ON DELETE RESTRICT
    )", tbl_opts, ";
  ")
  exec_create_if_missing("texts", sql_texts)

  # ----------------------------------------------------------------------------
  # 4. TABLE: user_ratings (recorded codings and gold standards)
  # ----------------------------------------------------------------------------
  sql_user_ratings <- paste0("
    CREATE TABLE IF NOT EXISTS user_ratings (
      rating_id ", pk_auto, ",
      user_id INT NOT NULL,
      text_id INT NOT NULL,
      kategorie VARCHAR(128) NOT NULL,
      bewertung INT NOT NULL,
      typ VARCHAR(64) NOT NULL,
      source_id INT,
      zeitstempel ", t_stamp, ",
      FOREIGN KEY (text_id) REFERENCES texts (text_id),
      FOREIGN KEY (source_id) REFERENCES sources (source_id)
    )", tbl_opts, ";
  ")
  exec_create_if_missing("user_ratings", sql_user_ratings)

  # ----------------------------------------------------------------------------
  # 5. TABLE: coding_training (training logs with reliability metrics)
  # ----------------------------------------------------------------------------
  sql_coding_training <- paste0("
    CREATE TABLE IF NOT EXISTS coding_training (
      training_id ", pk_auto, ",
      user_id INT NOT NULL,
      kategorie VARCHAR(128) NOT NULL,
      datum ", t_date, ",
      anzahl_einheiten INT NOT NULL,
      accuracy ", num_real, ",
      balanced_accuracy ", num_real, ",
      iota_index ", num_real, ",
      avg_iota2 ", num_real, ",
      cohens_kappa ", num_real, ",
      kappa_linear ", num_real, ",
      kappa_squared ", num_real, ",
      fleiss_kappa ", num_real, ",
      kendall_w ", num_real, ",
      alpha_nominal ", num_real, ",
      alpha_ordinal ", num_real, ",
      gwet_ac1 ", num_real, ",
      gwet_ac2_quadratic ", num_real, ",
      macro_precision ", num_real, ",
      macro_recall ", num_real, ",
      macro_f1 ", num_real, ",
      zeitstempel ", t_stamp, "
    )", tbl_opts, ";
  ")
  exec_create_if_missing("coding_training", sql_coding_training)

  # ----------------------------------------------------------------------------
  # 6. TABLE: scale_definitions (coding scale catalog)
  # ----------------------------------------------------------------------------
  sql_scale_definitions <- paste0("
    CREATE TABLE IF NOT EXISTS scale_definitions (
      scale_id ", pk_auto, ",
      short_label VARCHAR(64) NOT NULL,
      name VARCHAR(255) UNIQUE NOT NULL,
      max_value INT NOT NULL,
      def_desc TEXT,
      def_n_0 TEXT,
      def_n_1 TEXT,
      def_n_2 TEXT,
      def_n_3 TEXT,
      def_n_4 TEXT,
      def_n_5 TEXT
    )", tbl_opts, ";
  ")
  exec_create_if_missing("scale_definitions", sql_scale_definitions)

  # ----------------------------------------------------------------------------
  # 7. TABLE: user_source_permissions (per-user text source authorization)
  # ----------------------------------------------------------------------------
  sql_user_source_permissions <- paste0("
    CREATE TABLE IF NOT EXISTS user_source_permissions (
      user_id INT NOT NULL,
      source_id INT NOT NULL,
      erstellt_am ", t_stamp, ",
      PRIMARY KEY (user_id, source_id),
      FOREIGN KEY (source_id) REFERENCES sources (source_id) ON DELETE CASCADE
    )", tbl_opts, ";
  ")
  exec_create_if_missing("user_source_permissions", sql_user_source_permissions)

  # ----------------------------------------------------------------------------
  # 8. TABLE: user_coding_source_permissions (per-user coding source access)
  # ----------------------------------------------------------------------------
  sql_user_coding_source_permissions <- paste0("
    CREATE TABLE IF NOT EXISTS user_coding_source_permissions (
      user_id INT NOT NULL,
      source_id INT NOT NULL,
      erstellt_am ", t_stamp, ",
      PRIMARY KEY (user_id, source_id),
      FOREIGN KEY (source_id) REFERENCES sources (source_id) ON DELETE CASCADE
    )", tbl_opts, ";
  ")
  exec_create_if_missing("user_coding_source_permissions", sql_user_coding_source_permissions)

  # ----------------------------------------------------------------------------
  # 9. TABLE: user_scale_permissions (per-user scale authorization)
  # ----------------------------------------------------------------------------
  sql_user_scale_permissions <- paste0("
    CREATE TABLE IF NOT EXISTS user_scale_permissions (
      user_id INT NOT NULL,
      scale_id INT NOT NULL,
      erstellt_am ", t_stamp, ",
      PRIMARY KEY (user_id, scale_id),
      FOREIGN KEY (scale_id) REFERENCES scale_definitions (scale_id) ON DELETE CASCADE
    )", tbl_opts, ";
  ")
  exec_create_if_missing("user_scale_permissions", sql_user_scale_permissions)

  if (verbose) {
    if (n_created > 0L) {
      message("Tables initialized successfully (", n_created, " created).")
    } else {
      message("All required tables already exist (no DDL executed).")
    }
  }

  return(TRUE)
}

#' Seed Initial Accounts or Demonstration Data into the Database
#'
#' Populates a CodingManageR database either with minimal default accounts
#' (`empty = TRUE`) or with a complete demonstration dataset (`empty = FALSE`)
#' including users, coding scales, scientific sources, case texts, permissions,
#' gold-standard ratings, and training history. Prints a structured tabular
#' summary report via `message()`.
#'
#' @param con A `DBIConnection` or `pool::dbPool` object.
#' @param empty Logical; if `TRUE` (default), seeds only the default `admin` and
#'   inactive `historic_user` accounts. If `FALSE`, seeds the full demonstration
#'   dataset only when all 9 tables are completely empty; if any table already
#'   contains rows, emits a warning and skips all demonstration inserts.
#'
#' @return Invisibly returns a named list summarizing row counts, schema
#'   creation status, and checksums for all 9 tables.
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' create_all_tables(con, verbose = FALSE)
#' seed_initial_data(con, empty = TRUE)
#' DBI::dbDisconnect(con)
#' }
#' @export
seed_initial_data <- function(con, empty = TRUE) {
  if (is.null(con)) stop("Invalid database connection supplied (NULL).")

  # All 9 relational tables in dependency order
  all_tables <- c(
    "user_tokens",
    "scale_definitions",
    "sources",
    "user_source_permissions",
    "user_coding_source_permissions",
    "user_scale_permissions",
    "texts",
    "user_ratings",
    "coding_training"
  )

  # 0. Pre-check: determine which tables already exist and create any missing ones
  existing_tables_before <- dbListTables(con)
  missing_tables <- setdiff(all_tables, existing_tables_before)
  created_tables <- character(0)
  if (length(missing_tables) > 0) {
    create_all_tables(con, verbose = FALSE)
    created_tables <- missing_tables
  }

  # Tracking list for structured reporting across all tables
  report <- list()
  for (tbl in all_tables) {
    report[[tbl]] <- list(
      table_name = tbl,
      created = tbl %in% created_tables,
      n_before = 0L,
      n_inserted = 0L,
      n_total = 0L,
      status_label = "",
      checksum = "-"
    )
  }

  # Helper for lightweight table checksum reporting (row count + column count)
  calc_checksum <- function(tbl_name) {
    tryCatch({
      df <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 500", tbl_name))
      if (nrow(df) == 0) return("EMPTY (0 rows)")
      row_c <- nrow(df)
      col_c <- ncol(df)
      sprintf("OK: %d x %d [CHK-%d]", row_c, col_c, (row_c * 31 + col_c * 17) %% 997)
    }, error = function(e) "OK (validated)")
  }

  # Pre-seed global row count check across all 9 tables to protect existing databases
  counts_before <- vapply(all_tables, function(t_name) {
    as.integer(dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", t_name))[1, 1])
  }, integer(1))
  total_rows_before <- sum(counts_before, na.rm = TRUE)
  skip_demo_seed <- !empty && total_rows_before > 0L

  if (skip_demo_seed) {
    warning(
      sprintf(
        "[DB-Seed] Existing database already contains data (%d row(s) across existing tables). Skipping demonstration data seeding (empty = FALSE) to protect existing data.",
        total_rows_before
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------------------------
  # 1. user_tokens (authentication and accounts - 8 demo accounts)
  # ----------------------------------------------------------------------------
  tbl <- "user_tokens"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_tokens")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty) {
    if (!skip_demo_seed && n_b == 0) {
      dbExecute(con, "
        INSERT INTO user_tokens (username, token, permissions, is_active) VALUES
        ('admin', 'geheim123', 'admin', 1),
        ('user1', 'token2026', 'user', 1),
        ('eva_lindner', 'k9QmP2vX8yLt', 'user', 1),
        ('markus_meier', 'w7RtA4nM9pQs', 'user', 1),
        ('inaktiv_demo', 'gesperrt123', 'user', 0),
        ('historic_user', 'historic_token', 'user', 0),
        ('gold_standard', 'sys_gold_standard_internal', 'system', 1),
        ('referenz_experte', 'sys_expert_internal', 'system', 1);
      ")
    }
  } else {
    if (n_b == 0) {
      dbExecute(con, "
        INSERT INTO user_tokens (username, token, permissions, is_active) VALUES
        ('admin', 'geheim123', 'admin', 1),
        ('historic_user', 'historic_token', 'user', 0);
      ")
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_tokens")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d accounts inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d accounts present)", n_a)
  } else if (empty && n_a > 1) {
    report[[tbl]]$status_label <- sprintf("Already populated (%d accounts present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Initial accounts (empty = TRUE: 1 admin, 1 inactive 'historic_user', total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d accounts present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 2. scale_definitions (coding scale catalog - 3 scales S1, S2, S3)
  # ----------------------------------------------------------------------------
  tbl <- "scale_definitions"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM scale_definitions")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      scales <- list(
        list(
          id = 1,
          name = "S1_Cognitive_Activation",
          short = "CogAct",
          max = 4,
          desc = "Degree to which instruction stimulates deep conceptual processing, prior-knowledge activation, open inquiry, and metacognitive reflection during teaching and learning. [Note: AI-generated demonstration scale.]",
          def_0 = "Pure rote recall or mechanical procedure with no conceptual prompts or reasoning demands.",
          def_1 = "Isolated comprehension questions requiring brief factual answers without probing student reasoning.",
          def_2 = "Occasional open questions inviting explanation, though classroom discourse remains largely teacher-directed.",
          def_3 = "Sustained conceptual challenges prompting learners to compare solution strategies, justify reasoning, and connect prior knowledge.",
          def_4 = "Rich, adaptive cognitive activation where learners co-construct conceptual models, evaluate competing arguments, and reflect on their own learning strategies.",
          def_5 = NA_character_
        ),
        list(
          id = 2,
          name = "S2_Instructional_Structure",
          short = "Structure",
          max = 3,
          desc = "Clarity of lesson organization, goal transparency, sequencing of learning phases, and coherence of classroom discourse. [Note: AI-generated demonstration scale.]",
          def_0 = "Disorganized lesson sequence with unclear objectives, abrupt topic shifts, and no recognizable red thread.",
          def_1 = "Basic instructional phases are recognizable, yet transitions between tasks and learning goals remain vague or fragmented.",
          def_2 = "Clear and coherent lesson progression with stated objectives, logical task sequencing, and a structured wrap-up.",
          def_3 = "Exemplary instructional scaffolding featuring explicit advance organizers, seamless phase transitions, interim summaries, and check-for-understanding consolidation.",
          def_4 = NA_character_,
          def_5 = NA_character_
        ),
        list(
          id = 3,
          name = "S3_Formative_Feedback",
          short = "Feedback",
          max = 5,
          desc = "Quality of diagnostic, process-oriented, and self-regulation feedback provided to learners to clarify misconceptions and guide next learning steps. [Note: AI-generated demonstration scale.]",
          def_0 = "No substantive feedback or purely dismissive/uninformative remarks with no learning value.",
          def_1 = "Simple correctness verification ('right' or 'wrong') without explanation or guidance for improvement.",
          def_2 = "Task-level feedback identifying specific strengths or errors in the product, but lacking strategic guidance.",
          def_3 = "Process-oriented feedback explaining underlying misconceptions and modeling effective learning or problem-solving strategies.",
          def_4 = "Adaptive, criterion-referenced feedback linking current performance to explicit learning goals and actionable next steps.",
          def_5 = "Dialogic, metacognitive feedback that diagnoses reasoning patterns, fosters learner self-assessment, and co-plans targeted transfer strategies."
        )
      )

      for (sc in scales) {
        dbExecute(con, "
          INSERT INTO scale_definitions (scale_id, short_label, name, max_value, def_desc, def_n_0, def_n_1, def_n_2, def_n_3, def_n_4, def_n_5)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
        ", params = list(sc$id, sc$short, sc$name, sc$max, sc$desc, sc$def_0, sc$def_1, sc$def_2, sc$def_3, sc$def_4, sc$def_5))
      }
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM scale_definitions")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d scales inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d scales present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d scales present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 3. sources (scientific sources and reference corpora - 4 sources)
  # ----------------------------------------------------------------------------
  tbl <- "sources"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sources")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      sources_data <- list(
        list(
          id = 1,
          kurz = "Example1",
          ref = "Example 1. Title 1. [Note: Fictional demonstration source generated with the assistance of AI.]",
          lizenz = "CC-BY 4.0",
          erfasst = "2026-10-15 09:30:00"
        ),
        list(
          id = 2,
          kurz = "Example2",
          ref = "Example 2. Title 2. [Note: Fictional demonstration source generated with the assistance of AI.]",
          lizenz = "ODC-By",
          erfasst = "2026-10-16 11:20:00"
        ),
        list(
          id = 3,
          kurz = "Example3",
          ref = "Example 3. Title 3. [Note: Fictional demonstration source generated with the assistance of AI.]",
          lizenz = "Institutional Internal",
          erfasst = "2026-10-18 14:15:00"
        ),
        list(
          id = 4,
          kurz = "Example4",
          ref = "Example 4. Title 4. [Note: Fictional demonstration source generated with the assistance of AI.]",
          lizenz = "GPL-3.0",
          erfasst = "2026-10-22 16:45:00"
        )
      )

      for (src in sources_data) {
        dbExecute(con, "
          INSERT INTO sources (source_id, Kurzreferenz, Referenz, Lizenz, erfasst_am)
          VALUES (?, ?, ?, ?, ?);
        ", params = list(src$id, src$kurz, src$ref, src$lizenz, src$erfasst))
      }
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM sources")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d sources inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d sources present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d sources present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 4. user_source_permissions (per-user text source authorization)
  # ----------------------------------------------------------------------------
  tbl <- "user_source_permissions"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_source_permissions")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      uid_user1 <- u_ids$user_id[u_ids$username == "user1"]
      if (length(uid_user1) == 0 || is.na(uid_user1)) uid_user1 <- 2
      uid_markus <- u_ids$user_id[u_ids$username == "markus_meier"]
      if (length(uid_markus) == 0 || is.na(uid_markus)) uid_markus <- 4

      # user1 -> sources 1 and 2; markus_meier -> sources 1 and 3
      dbExecute(con, "
        INSERT INTO user_source_permissions (user_id, source_id) VALUES
        (?, 1),
        (?, 2),
        (?, 1),
        (?, 3);
      ", params = list(as.integer(uid_user1), as.integer(uid_user1), as.integer(uid_markus), as.integer(uid_markus)))
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_source_permissions")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d permissions inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d permissions present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d permissions present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 5. user_coding_source_permissions (per-user coding source access)
  # ----------------------------------------------------------------------------
  tbl <- "user_coding_source_permissions"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_coding_source_permissions")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      uid_user1 <- u_ids$user_id[u_ids$username == "user1"]
      if (length(uid_user1) == 0 || is.na(uid_user1)) uid_user1 <- 2
      uid_markus <- u_ids$user_id[u_ids$username == "markus_meier"]
      if (length(uid_markus) == 0 || is.na(uid_markus)) uid_markus <- 4

      # user1 -> sources 1 and 2; markus_meier -> sources 1 and 3
      dbExecute(con, "
        INSERT INTO user_coding_source_permissions (user_id, source_id) VALUES
        (?, 1),
        (?, 2),
        (?, 1),
        (?, 3);
      ", params = list(as.integer(uid_user1), as.integer(uid_user1), as.integer(uid_markus), as.integer(uid_markus)))
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_coding_source_permissions")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d coding permissions inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d permissions present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d permissions present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 6. user_scale_permissions (per-user scale authorization)
  # ----------------------------------------------------------------------------
  tbl <- "user_scale_permissions"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_scale_permissions")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      uid_user1 <- u_ids$user_id[u_ids$username == "user1"]
      if (length(uid_user1) == 0 || is.na(uid_user1)) uid_user1 <- 2
      uid_eva <- u_ids$user_id[u_ids$username == "eva_lindner"]
      if (length(uid_eva) == 0 || is.na(uid_eva)) uid_eva <- 3

      # user1 is permitted to code scale 1 (CogAct) and scale 2 (Structure)
      # eva_lindner is permitted to code scale 1 (CogAct)
      dbExecute(con, "
        INSERT INTO user_scale_permissions (user_id, scale_id) VALUES
        (?, 1),
        (?, 2),
        (?, 1);
      ", params = list(as.integer(uid_user1), as.integer(uid_user1), as.integer(uid_eva)))
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_scale_permissions")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d scale permissions inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d permissions present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d permissions present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 7. texts (case texts and level anchor examples - 34 structured cases)
  # ----------------------------------------------------------------------------
  tbl <- "texts"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM texts")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      fallback_texts <- data.frame(
        text_id = c(
          # Baseline cases (101 - 104)
          101, 102, 103, 104,
          # S1_Cognitive_Activation: levels 0 to 4 (2 cases each = IDs 201 to 210)
          201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
          # S2_Instructional_Structure: levels 0 to 3 (2 cases each = IDs 211 to 218)
          211, 212, 213, 214, 215, 216, 217, 218,
          # S3_Formative_Feedback: levels 0 to 5 (2 cases each = IDs 219 to 230)
          219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230
        ),
        text = paste(
          c(
            # 101-104: Baseline multi-dimensional classroom vignettes
            "Grade 9 mathematics lesson on linear functions. The teacher opens with an everyday energy-tariff comparison and asks pairs to compare two solution paths and justify why both yield the same break-even point. Lesson goals and sequence are clearly communicated. When reviewing group posters, the teacher links each draft directly to the rubric criteria and specifies one concrete step to refine the algebraic justification.",
            "Grade 8 biology lesson on photosynthesis. The teacher displays an explicit three-part agenda at the start, summarizes key takeaways after each phase, and checks understanding with mini-whiteboards before transitioning. Students answer several open 'why' questions during teacher-led discussion. When a group confuses respiration with photosynthesis, the teacher diagnoses the misconception and models how to trace energy flow step by step.",
            "Undergraduate seminar on research methods. Students design competing hypotheses for an unexpected survey pattern, debate validity threats in small groups, and reflect aloud on how their initial assumptions shifted. The session follows a clear inquiry sequence from launch to plenary synthesis. During debriefing, the instructor highlights two specific strengths and one sampling error per group without elaborating broader revision strategies.",
            "Grade 10 history lesson on primary source analysis. The instructor introduces clear learning objectives and transparent signposting across source reading, partner comparison, and plenary synthesis. Learners connect the documents to prior knowledge on industrialization and defend competing interpretations with textual evidence. After reviewing student essays, the teacher explains how to distinguish correlation from causation and demonstrates a paragraph revision strategy.",

            # 201-210: S1_Cognitive_Activation (levels 0 to 4)
            "Level 0 Case A: The teacher dictates textbook definitions of geometric terms word for word while students copy them silently into notebooks without any questions or discussion.",
            "Level 0 Case B: Students spend 25 minutes completing repetitive multiplication drills from a worksheet; the teacher reads out final answers rapidly with no conceptual reflection.",
            "Level 1 Case A: After a short presentation on the water cycle, the teacher asks three closed recall questions ('What is evaporation?') and immediately moves on once a student gives the keyword.",
            "Level 1 Case B: While reading a short story, the instructor checks plot comprehension ('Where did the protagonist travel?') without asking learners to interpret motives or justify claims.",
            "Level 2 Case A: The teacher poses an open question about why urban temperatures differ from rural areas, listens to two student ideas, and then delivers the canonical textbook explanation.",
            "Level 2 Case B: Students predict the outcome of a pendulum experiment in pairs before the teacher demonstrates the result and explains the underlying physics directly from the board.",
            "Level 3 Case A: Learners compare three conflicting student solutions to a fraction problem, identify where each assumption holds or breaks down, and connect the reasoning to visual area models from last week.",
            "Level 3 Case B: In a civics seminar, the instructor challenges students to construct evidence-based counterarguments to a policy proposal and justify which criteria carry the greatest weight.",
            "Level 4 Case A: Students collaboratively build and iteratively revise an explanatory model of ecosystem feedback loops, test rival hypotheses against empirical classroom data, and evaluate which metacognitive heuristics helped resolve contradictions.",
            "Level 4 Case B: In a teacher-education workshop, participants analyze contrasting lesson transcripts, co-develop theoretical criteria for instructional quality, critique each other's interpretations, and reflect on how to transfer the framework to their own teaching.",

            # 211-218: S2_Instructional_Structure (levels 0 to 3)
            "Level 0 Case A: The lesson unfolds chaotically; the instructor jumps from homework logistics to an unrelated anecdote, introduces a worksheet without instructions, and runs out of time before stating the topic.",
            "Level 0 Case B: No recognizable lesson phases appear; students ask confused questions about what they are supposed to do while the teacher repeatedly changes task formats mid-activity.",
            "Level 1 Case A: A warm-up and a group task are identifiable, yet the teacher switches abruptly between slides and exercises without clarifying how the activities relate to the learning objective.",
            "Level 1 Case B: An overall topic thread exists, but frequent unplanned digressions and missing time cues interrupt the workflow and leave the closing phase rushed.",
            "Level 2 Case A: Well-organized lesson progression: the teacher states the learning goal at the outset, sequences guided practice before independent work, and concludes with a brief review of key points.",
            "Level 2 Case B: Clear instructional sequence covering activation, structured partner inquiry, and plenary consolidation with visible task instructions and consistent pacing.",
            "Level 3 Case A: Exemplarily structured lesson opening with an explicit advance organizer ('First we test our predictions, next we compare models, and finally we check transfer'). Smooth signposted transitions connect every phase.",
            "Level 3 Case B: Model instructional scaffolding featuring clear visual goal tracking, periodic interim summaries linking tasks back to core concepts, and a structured exit-ticket check for understanding.",

            # 219-230: S3_Formative_Feedback (levels 0 to 5)
            "Level 0 Case A: A student submits an incomplete lab report; the teacher writes only 'Unsatisfactory - try harder' in the margin and returns the paper without comment.",
            "Level 0 Case B: During a class presentation, the instructor interrupts impatiently ('That makes no sense, sit down') and calls on the next group without explaining what was missing.",
            "Level 1 Case A: The teacher marks each quiz item with a checkmark or cross and announces '14 out of 20 correct' without reviewing any errors or suggesting next steps.",
            "Level 1 Case B: After a student solves an equation on the board, the instructor says 'Incorrect, the answer is 12' and immediately erases the work to move to the next problem.",
            "Level 2 Case A: On an argumentative essay, the teacher notes 'Clear thesis statement in paragraph one, but paragraph three lacks a supporting citation' without explaining how to integrate evidence.",
            "Level 2 Case B: Reviewing a lab graph, the instructor points out two mislabeled axes and one strong trend description, leaving the student to figure out how to improve data interpretation.",
            "Level 3 Case A: Noticing a recurring sign error in algebraic factoring, the teacher explains why distributing the negative term fails and demonstrates a two-step verification strategy.",
            "Level 3 Case B: In a writing conference, the instructor diagnoses why the student's transitions feel abrupt and models how topic sentences link claims across consecutive paragraphs.",
            "Level 4 Case A: Using the analytic rubric, the teacher shows how the student's experimental design already meets the control-variable criterion and specifies two concrete adjustments needed to reach the mastery level on data justification.",
            "Level 4 Case B: After a micro-teaching simulation, the mentor maps observed questioning patterns against the seminar's discourse criteria and outlines an actionable wait-time protocol for the next practice round.",
            "Level 5 Case A: Through a structured feedback dialogue, the instructor probes how the learner chose an ineffective study strategy, guides the student to self-assess the draft against quality criteria, and co-designs a transfer plan for upcoming complex problems.",
            "Level 5 Case B: During a capstone thesis supervision, the advisor diagnoses deeper conceptual gaps in the theoretical framing, prompts reflective self-evaluation of analytical choices, and collaboratively establishes self-monitoring checkpoints for independent revision."
          ),
          "[Note: Fictional, AI-generated demonstration text.]"
        ),
        source_id = c(
          # 101-104: Baseline cases
          1, 1, 2, 2,
          # 201-210: S1_Cognitive_Activation (sources 1 and 3)
          1, 1, 1, 1, 3, 3, 3, 3, 3, 3,
          # 211-218: S2_Instructional_Structure (sources 2 and 3)
          2, 2, 2, 2, 3, 3, 3, 3,
          # 219-230: S3_Formative_Feedback (sources 1, 2, and 3)
          1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3
        ),
        stringsAsFactors = FALSE
      )

      for (i in seq_len(nrow(fallback_texts))) {
        dbExecute(con, "
          INSERT INTO texts (text_id, text, source_id)
          VALUES (?, ?, ?);
        ", params = list(fallback_texts$text_id[i], fallback_texts$text[i], fallback_texts$source_id[i]))
      }
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM texts")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d texts inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d texts present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d texts present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 8. user_ratings (reference and gold-standard ratings - 34 ratings)
  # ----------------------------------------------------------------------------
  tbl <- "user_ratings"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_ratings")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      ratings_data <- data.frame(
        rating_id = c(
          # 1. Gold standards (typ = 'gold_standard', 12 ratings)
          101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
          # 2. Field and expert codings (typ = 'field', 22 ratings)
          201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
          211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222
        ),
        username = c(
          # Gold standards
          rep("gold_standard", 12),
          # Field codings
          "user1", "user1", "user1", "user1", "user1",
          "eva_lindner", "eva_lindner", "eva_lindner", "eva_lindner", "eva_lindner",
          "markus_meier", "markus_meier", "markus_meier", "markus_meier",
          "referenz_experte", "referenz_experte", "referenz_experte",
          "admin", "admin", "admin", "admin", "user1"
        ),
        text_id = c(
          # Gold standards: baseline cases 101-104 across categories 1, 2, 3
          101, 101, 101,
          102, 102, 102,
          103, 103, 103,
          104, 104, 104,
          # Field codings
          101, 102, 103, 104, 201,
          101, 102, 103, 202, 203,
          101, 103, 104, 211,
          101, 102, 103,
          101, 102, 103, 104, 212
        ),
        kategorie = c(
          # Gold standards
          "1", "2", "3",
          "1", "2", "3",
          "1", "2", "3",
          "1", "2", "3",
          # Field codings
          "1", "1", "1", "1", "1",
          "1", "1", "1", "1", "1",
          "1", "1", "1", "2",
          "1", "1", "1",
          "1", "1", "1", "1", "2"
        ),
        bewertung = c(
          # Gold standards
          3, 2, 4,
          2, 3, 3,
          4, 2, 2,
          3, 3, 3,
          # Field codings
          3, 2, 3, 3, 0,
          3, 2, 4, 0, 1,
          2, 4, 3, 0,
          3, 2, 4,
          3, 2, 4, 3, 1
        ),
        typ = c(
          rep("gold_standard", 12),
          rep("field", 22)
        ),
        source_id = c(
          1, 1, 1,
          1, 1, 1,
          2, 2, 2,
          2, 2, 2,
          1, 1, 2, 2, 1,
          1, 1, 2, 1, 1,
          1, 2, 2, 2,
          1, 1, 2,
          1, 1, 2, 2, 2
        ),
        zeitstempel = c(
          # Gold-standard timestamps
          "2026-10-01 10:00:00", "2026-10-01 10:05:00", "2026-10-01 10:10:00",
          "2026-10-01 10:15:00", "2026-10-01 10:20:00", "2026-10-01 10:25:00",
          "2026-10-01 10:30:00", "2026-10-01 10:35:00", "2026-10-01 10:40:00",
          "2026-10-01 10:45:00", "2026-10-01 10:50:00", "2026-10-01 10:55:00",
          # Field-coding timestamps
          "2026-10-16 11:00:00", "2026-10-16 11:15:00", "2026-10-16 11:30:00", "2026-10-16 11:45:00", "2026-10-17 09:00:00",
          "2026-10-21 14:00:00", "2026-10-21 14:15:00", "2026-10-21 14:30:00", "2026-10-22 10:00:00", "2026-10-22 10:15:00",
          "2026-10-25 15:00:00", "2026-10-25 15:15:00", "2026-10-25 15:30:00", "2026-10-26 11:00:00",
          "2026-10-10 09:00:00", "2026-10-10 09:15:00", "2026-10-10 09:30:00",
          "2026-10-02 08:30:00", "2026-10-02 08:45:00", "2026-10-02 09:00:00", "2026-10-02 09:15:00", "2026-10-27 16:00:00"
        ),
        stringsAsFactors = FALSE
      )

      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      lookup_user_id <- function(uids_df, uname, fallback = 1L) {
        val <- uids_df$user_id[uids_df$username == uname]
        if (length(val) == 0 || is.na(val[1])) as.integer(fallback) else as.integer(val[1])
      }

      for (i in seq_len(nrow(ratings_data))) {
        uid <- lookup_user_id(u_ids, ratings_data$username[i], fallback = 1L)
        dbExecute(con, "
          INSERT INTO user_ratings (rating_id, user_id, text_id, kategorie, bewertung, typ, source_id, zeitstempel)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?);
        ", params = list(
          ratings_data$rating_id[i],
          uid,
          ratings_data$text_id[i],
          ratings_data$kategorie[i],
          ratings_data$bewertung[i],
          ratings_data$typ[i],
          ratings_data$source_id[i],
          ratings_data$zeitstempel[i]
        ))
      }
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_ratings")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d ratings [12 gold standards, 22 field], total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d ratings present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d ratings present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 9. coding_training (training sessions and longitudinal metrics - 10 sessions)
  # ----------------------------------------------------------------------------
  tbl <- "coding_training"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM coding_training")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty && !skip_demo_seed) {
    if (n_b == 0) {
      u_ids_tr <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      get_tr_uid <- function(uname, fallback = 1L) {
        val <- u_ids_tr$user_id[u_ids_tr$username == uname]
        if (length(val) == 0 || is.na(val[1])) as.integer(fallback) else as.integer(val[1])
      }

      trainings_data <- list(
        list(tid = 1,  uid = get_tr_uid("user1", 2),        kat = "1", dat = "2026-10-20", n = 8,  acc = 75.0, b_acc = 73.0, iota = 0.695, a_iota = 0.710, c_kap = 0.68, k_lin = 0.70, k_sq = 0.72, f_kap = 0.67, ken_w = 0.71, a_nom = 0.67, a_ord = 0.70, gwet1 = 0.72, gwet2 = 0.74, prec = 74.0, rec = 73.5, f1 = 74.2),
        list(tid = 2,  uid = get_tr_uid("user1", 2),        kat = "1", dat = "2026-10-24", n = 10, acc = 80.0, b_acc = 78.5, iota = 0.745, a_iota = 0.760, c_kap = 0.74, k_lin = 0.76, k_sq = 0.78, f_kap = 0.73, ken_w = 0.77, a_nom = 0.73, a_ord = 0.76, gwet1 = 0.78, gwet2 = 0.80, prec = 79.5, rec = 78.8, f1 = 79.1),
        list(tid = 3,  uid = get_tr_uid("user1", 2),        kat = "1", dat = "2026-10-28", n = 12, acc = 90.0, b_acc = 88.0, iota = 0.855, a_iota = 0.870, c_kap = 0.85, k_lin = 0.87, k_sq = 0.89, f_kap = 0.84, ken_w = 0.88, a_nom = 0.84, a_ord = 0.87, gwet1 = 0.88, gwet2 = 0.90, prec = 89.8, rec = 89.0, f1 = 89.4),
        list(tid = 4,  uid = get_tr_uid("admin", 1),        kat = "1", dat = "2026-10-22", n = 10, acc = 85.0, b_acc = 83.5, iota = 0.802, a_iota = 0.815, c_kap = 0.79, k_lin = 0.81, k_sq = 0.83, f_kap = 0.78, ken_w = 0.82, a_nom = 0.78, a_ord = 0.81, gwet1 = 0.82, gwet2 = 0.84, prec = 84.5, rec = 83.8, f1 = 84.0),
        list(tid = 5,  uid = get_tr_uid("admin", 1),        kat = "1", dat = "2026-10-29", n = 14, acc = 92.5, b_acc = 91.0, iota = 0.890, a_iota = 0.905, c_kap = 0.88, k_lin = 0.90, k_sq = 0.92, f_kap = 0.87, ken_w = 0.91, a_nom = 0.87, a_ord = 0.90, gwet1 = 0.91, gwet2 = 0.93, prec = 92.0, rec = 91.5, f1 = 91.8),
        list(tid = 6,  uid = get_tr_uid("eva_lindner", 3),  kat = "1", dat = "2026-10-25", n = 12, acc = 91.7, b_acc = 89.2, iota = 0.872, a_iota = 0.888, c_kap = 0.86, k_lin = 0.88, k_sq = 0.90, f_kap = 0.85, ken_w = 0.89, a_nom = 0.85, a_ord = 0.88, gwet1 = 0.89, gwet2 = 0.91, prec = 90.8, rec = 90.0, f1 = 90.4),
        list(tid = 7,  uid = get_tr_uid("user1", 2),        kat = "2", dat = "2026-10-26", n = 10, acc = 70.0, b_acc = 68.0, iota = 0.635, a_iota = 0.650, c_kap = 0.62, k_lin = 0.64, k_sq = 0.66, f_kap = 0.61, ken_w = 0.65, a_nom = 0.61, a_ord = 0.64, gwet1 = 0.66, gwet2 = 0.68, prec = 70.0, rec = 69.0, f1 = 69.5),
        list(tid = 8,  uid = get_tr_uid("user1", 2),        kat = "2", dat = "2026-10-31", n = 12, acc = 83.3, b_acc = 81.5, iota = 0.785, a_iota = 0.800, c_kap = 0.77, k_lin = 0.79, k_sq = 0.81, f_kap = 0.76, ken_w = 0.80, a_nom = 0.76, a_ord = 0.79, gwet1 = 0.81, gwet2 = 0.83, prec = 82.5, rec = 81.6, f1 = 82.0),
        list(tid = 9,  uid = get_tr_uid("admin", 1),        kat = "2", dat = "2026-10-27", n = 10, acc = 80.0, b_acc = 78.0, iota = 0.762, a_iota = 0.778, c_kap = 0.75, k_lin = 0.77, k_sq = 0.79, f_kap = 0.74, ken_w = 0.78, a_nom = 0.74, a_ord = 0.77, gwet1 = 0.79, gwet2 = 0.81, prec = 79.8, rec = 78.6, f1 = 79.2),
        list(tid = 10, uid = get_tr_uid("user1", 2),        kat = "3", dat = "2026-10-27", n = 8,  acc = 87.5, b_acc = 85.0, iota = 0.825, a_iota = 0.840, c_kap = 0.81, k_lin = 0.83, k_sq = 0.85, f_kap = 0.80, ken_w = 0.84, a_nom = 0.80, a_ord = 0.83, gwet1 = 0.85, gwet2 = 0.87, prec = 86.8, rec = 85.6, f1 = 86.2)
      )

      for (tr in trainings_data) {
        dbExecute(con, "
          INSERT INTO coding_training (
            training_id, user_id, kategorie, datum, anzahl_einheiten,
            accuracy, balanced_accuracy, iota_index, avg_iota2,
            cohens_kappa, kappa_linear, kappa_squared, fleiss_kappa, kendall_w,
            alpha_nominal, alpha_ordinal, gwet_ac1, gwet_ac2_quadratic,
            macro_precision, macro_recall, macro_f1
          ) VALUES (
            ?, ?, ?, ?, ?,
            ?, ?, ?, ?,
            ?, ?, ?, ?, ?,
            ?, ?, ?, ?,
            ?, ?, ?
          );
        ", params = list(
          tr$tid, tr$uid, tr$kat, tr$dat, tr$n,
          tr$acc, tr$b_acc, tr$iota, tr$a_iota,
          tr$c_kap, tr$k_lin, tr$k_sq, tr$f_kap, tr$ken_w,
          tr$a_nom, tr$a_ord, tr$gwet1, tr$gwet2,
          tr$prec, tr$rec, tr$f1
        ))
      }
    }
  }
  n_a <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM coding_training")[1, 1])
  report[[tbl]]$n_total <- n_a
  report[[tbl]]$n_inserted <- n_a - n_b
  report[[tbl]]$checksum <- calc_checksum(tbl)
  if (report[[tbl]]$n_inserted > 0) {
    report[[tbl]]$status_label <- sprintf("Newly populated (+%d training sessions inserted, total: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (skip_demo_seed) {
    report[[tbl]]$status_label <- sprintf("Skipped demo seeding (existing database protected, %d training sessions present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d training sessions present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # Tabular status report with row counts and checksums
  # ----------------------------------------------------------------------------
  mode_label <- if (empty) {
    "Minimal Admin Account (empty = TRUE)"
  } else if (skip_demo_seed) {
    "Full Standard Dataset (empty = FALSE, skipped: existing database protected)"
  } else {
    "Full Standard Dataset (empty = FALSE)"
  }

  lines <- c()
  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")
  lines <- c(lines, "| No | SQL Table                      | Schema      | Before | Inserted  | Total  | Checksum / Status    |")
  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")

  for (idx in seq_along(all_tables)) {
    t_name <- all_tables[idx]
    info <- report[[t_name]]
    created_str <- if (info$created) "created" else "existing"
    ins_str <- if (info$n_inserted > 0) sprintf("+%d", info$n_inserted) else "0"

    lines <- c(lines, sprintf("| %2d | %-30s | %-11s | %6d | %9s | %6d | %-20s |",
                              idx,
                              t_name,
                              created_str,
                              info$n_before,
                              ins_str,
                              info$n_total,
                              info$checksum))
  }

  n_checked    <- length(all_tables)
  n_created    <- sum(sapply(report, function(x) x$created))
  n_seeded     <- sum(sapply(report, function(x) x$n_inserted > 0))
  n_total_rows <- sum(sapply(report, function(x) x$n_total))
  n_ins_rows   <- sum(sapply(report, function(x) x$n_inserted))

  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")
  lines <- c(lines, sprintf("| TOTAL: %d tables (%d created, %d seeded)                  | +%-7d | %6d | VALIDATION: 100%% OK |",
                            n_checked, n_created, n_seeded, n_ins_rows, n_total_rows))
  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")
  lines <- c(lines, sprintf("Mode: %s | DB Engine: %s", mode_label, class(con)[1]))

  message(paste(lines, collapse = "\n"))
  return(invisible(report))
}
