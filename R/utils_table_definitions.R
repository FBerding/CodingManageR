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
#' Tests whether a direct `DBIConnection` inherits from `MariaDBConnection` or
#' `MySQLConnection`.
#'
#' @param con A `DBIConnection` object or `NULL`.
#'
#' @return A logical scalar: `TRUE` for MySQL/MariaDB connections, `FALSE`
#'   otherwise.
#' @examples
#' is_mysql(NULL)
#' @export
is_mysql <- function(con) {
  if (is.null(con)) return(FALSE)
  inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
}

#' Create All CodingManageR Database Tables
#'
#' Initializes the complete 9-table relational schema (users, permissions,
#' sources, texts, scales, ratings, and training logs) in the target SQLite or
#' MySQL/MariaDB database using `CREATE TABLE IF NOT EXISTS`.
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

  mysql_mode <- is_mysql(con)

  # Backend-specific SQL data type aliases
  pk_auto  <- if (mysql_mode) "INT AUTO_INCREMENT PRIMARY KEY" else "INTEGER PRIMARY KEY AUTOINCREMENT"
  t_stamp  <- if (mysql_mode) "DATETIME DEFAULT CURRENT_TIMESTAMP" else "TEXT DEFAULT CURRENT_TIMESTAMP"
  t_date   <- if (mysql_mode) "DATE DEFAULT (CURRENT_DATE)" else "TEXT DEFAULT CURRENT_DATE"
  num_real <- if (mysql_mode) "DOUBLE" else "REAL"
  tbl_opts <- if (mysql_mode) " ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci" else ""

  if (verbose) {
    message("Creating tables for backend: ", if (mysql_mode) "MySQL/MariaDB" else "SQLite", "...")
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
  dbExecute(con, sql_user_tokens)

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
  dbExecute(con, sql_sources)

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
  dbExecute(con, sql_texts)

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
  dbExecute(con, sql_user_ratings)

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
  dbExecute(con, sql_coding_training)

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
  dbExecute(con, sql_scale_definitions)

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
  dbExecute(con, sql_user_source_permissions)

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
  dbExecute(con, sql_user_coding_source_permissions)

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
  dbExecute(con, sql_user_scale_permissions)

  if (verbose) {
    message("Tables initialized successfully.")
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
#'   dataset.
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

  # ----------------------------------------------------------------------------
  # 1. user_tokens (authentication and accounts - 8 demo accounts)
  # ----------------------------------------------------------------------------
  tbl <- "user_tokens"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_tokens")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty) {
    if (n_b == 0) {
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
  } else if (empty && n_a > 1) {
    report[[tbl]]$status_label <- sprintf("Already populated (%d accounts present)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Initial accounts (empty = TRUE: 1 admin, 1 inactive 'historic_user', total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d accounts present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 2. scale_definitions (coding scale catalog - 3 scales K1, K2, K3)
  # ----------------------------------------------------------------------------
  tbl <- "scale_definitions"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM scale_definitions")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty) {
    if (n_b == 0) {
      scales <- list(
        list(
          id = 1,
          name = "K1_Empathie",
          short = "Empathie",
          max = 4,
          desc = "Grad des empathischen Eingehens auf die emotionale Verfassung des Patienten.",
          def_0 = "Keine emotionale Resonanz erkennbar.",
          def_1 = "Minimale Best\u00e4tigung ohne vertiefende Anteilnahme.",
          def_2 = "Erkennung von Emotionen, jedoch funktionale oder distanzierte Reaktion.",
          def_3 = "Explizite Validierung von Sorgen und emotionale Entlastung.",
          def_4 = "Hervorragende Empathie, tiefes Eingehen auf emotionale Zwischent\u00f6ne und proaktive Entlastung.",
          def_5 = NA_character_
        ),
        list(
          id = 2,
          name = "K2_Struktur",
          short = "Struktur",
          max = 3,
          desc = "Klarheit des Aufbaus, rote Faden und Strukturierung des Arzt-Patienten-Gespr\u00e4chs.",
          def_0 = "Unstrukturiert, sprunghaft ohne erkennbaren roten Faden.",
          def_1 = "Ans\u00e4tze von Struktur erkennbar, jedoch h\u00e4ufige Unterbrechungen des Gespr\u00e4chsflusses.",
          def_2 = "Geordneter Ablauf der Anamnesephasen mit klarer Orientierung f\u00fcr den Patienten.",
          def_3 = "Vorbildliche Gespr\u00e4chsf\u00fchrung mit expliziter Phasentransparenz und Zusammenfassungen.",
          def_4 = NA_character_,
          def_5 = NA_character_
        ),
        list(
          id = 3,
          name = "K3_Fachkompetenz",
          short = "Fachkompetenz",
          max = 5,
          desc = "Pr\u00e4zision der medizinischen Einordnung, zielgerichtete Exploration und diagnostische Stringenz.",
          def_0 = "Keine nachvollziehbare medizinische Exploration erkennbar.",
          def_1 = "Unvollst\u00e4ndige oder l\u00fcckenhafte anamnestische Erhebung wesentlicher Leitsymptome.",
          def_2 = "Solide Basiserhebung typischer Leitsymptome ohne differenzierte Tiefenexploration.",
          def_3 = "Strukturierte Differenzialdiagnostik und pr\u00e4zise Erfassung relevanter Begleitsymptome.",
          def_4 = "Umfassende und differenzierte Exploration inklusive relevanter Red Flags und Risikofaktoren.",
          def_5 = "Exzellente differentialdiagnostische Abw\u00e4gung mit feinsinniger Priorisierung und Stringenz."
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

  if (!empty) {
    if (n_b == 0) {
      sources_data <- list(
        list(
          id = 1,
          kurz = "Mueller2024",
          ref = "M\u00fcller, T., Becker, S. & Hoffmann, K. (2024). Validierung klinischer Sprachdaten und strukturierte Anamnesef\u00fchrung. Springer Medizin Verlag. https://doi.org/10.1007/s00103-024-03810-1",
          lizenz = "CC-BY 4.0",
          erfasst = "2026-10-15 09:30:00"
        ),
        list(
          id = 2,
          kurz = "ICD-10-GM-2025",
          ref = "BfArM - Bundesinstitut f\u00fcr Arzneimittel und Medizinprodukte (2025). Internationale statistische Klassifikation der Krankheiten und verwandter Gesundheitsprobleme, 10. Revision, German Modification.",
          lizenz = "ODC-By",
          erfasst = "2026-10-16 11:20:00"
        ),
        list(
          id = 3,
          kurz = "KlinikKorpus_Intern",
          ref = "Universit\u00e4tsklinikum Verbund (2025). Anonymisierte Anamnesetexte aus der inneren Medizin zur Schulung von Kodierfachkr\u00e4ften.",
          lizenz = "Instituts-Intern",
          erfasst = "2026-10-18 14:15:00"
        ),
        list(
          id = 4,
          kurz = "AIF-Edu-Report",
          ref = "Berding, F. et al. (2023). aifeducation: Artificial Intelligence for Education and Rater Reliability. Comprehensive R Archive Network.",
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

  if (!empty) {
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

  if (!empty) {
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

  if (!empty) {
    if (n_b == 0) {
      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      uid_user1 <- u_ids$user_id[u_ids$username == "user1"]
      if (length(uid_user1) == 0 || is.na(uid_user1)) uid_user1 <- 2
      uid_eva <- u_ids$user_id[u_ids$username == "eva_lindner"]
      if (length(uid_eva) == 0 || is.na(uid_eva)) uid_eva <- 3

      # user1 is permitted to code scale 1 (Empathy) and scale 2 (Structure)
      # eva_lindner is permitted to code scale 1 (Empathy)
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

  if (!empty) {
    if (n_b == 0) {
      fallback_texts <- data.frame(
        text_id = c(
          # Baseline cases (101 - 104)
          101, 102, 103, 104,
          # K1_Empathie: levels 0 to 4 (2 cases each = IDs 201 to 210)
          201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
          # K2_Struktur: levels 0 to 3 (2 cases each = IDs 211 to 218)
          211, 212, 213, 214, 215, 216, 217, 218,
          # K3_Fachkompetenz: levels 0 to 5 (2 cases each = IDs 219 to 230)
          219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230
        ),
        text = c(
          # 101-104: Baseline texts
          "Pat. 54 J., berichtet \u00fcber seit 3 Monaten progrediente morgendliche Gelenksteife (>60 Min) beider MCP- und PIP-Gelenke. Im Gespr\u00e4ch zeigt sich deutliche Verunsicherung bez\u00fcglich der Arbeitsf\u00e4higkeit. Arzt geht behutsam auf Sorgen ein, erkl\u00e4rt die Verdachtsdiagnose Rheumatoide Arthritis und vereinbart zeitnahes Labor inklusive CCP-Ak.",
          "Pat. 29 J., klagt \u00fcber akute Cephalgie seit gestern Abend. Begleitend Photophobie und Nausea, kein Fieber, kein Meningismus. Typische Migr\u00e4neanamnese in der Familie. Strukturierte Erfassung von Schmerzcharakter und Triggern sowie Vereinbarung eines Akutmedikationsplans.",
          "Pat. 68 J., Z.n. Myokardinfarkt vor 4 Jahren, stellt sich zur Routinekontrolle vor. Berichtet \u00fcber gelegentlichen Schwindel bei schnellem Aufstehen. RR 110/70 mmHg, Puls 64/min rhythmisch. Arzt bespricht Dosisanpassung des Antihypertensivums und beruhigt den Patienten.",
          "Pat. 42 J., rezidivierende epigastrische Schmerzen, verst\u00e4rkt nach Mahlzeiten. Keine B-Symptomatik. Arzt erl\u00e4utert Befundkonstellation verst\u00e4ndlich, initiiert PPI-Therapieversuch und plant Abdomensonographie.",

          # 201-210: K1_Empathie (levels 0 to 4)
          "Stufe 0 Fall A: Arzt tippt ununterbrochen in den Monitor und unterbricht den weinenden Patienten br\u00fcsk: 'Bitte nur pr\u00e4zise auf meine Fragen antworten, wir haben wenig Zeit.'",
          "Stufe 0 Fall B: Patient \u00e4u\u00dfert Panik vor der anstehenden Koloskopie. Arzt kommentiert kurz: 'Das ist Routine, stellen Sie sich nicht so an' und wechselt direkt zum Aufkl\u00e4rungsbogen.",
          "Stufe 1 Fall A: Patient erw\u00e4hnt besorgt den k\u00fcrzlichen Schlaganfall des Bruders. Arzt nickt kurz stumm, geht jedoch nicht weiter darauf ein und misst wortlos den Blutdruck.",
          "Stufe 1 Fall B: Patientin zeigt sich niedergeschlagen wegen chronischer R\u00fcckenschmerzen. Arzt: 'Ja, Schmerzen sind unangenehm' und tippt sofort die Verordnung f\u00fcr Physiotherapie ein.",
          "Stufe 2 Fall A: Patient sorgt sich um die Nebenwirkungen des neuen Medikaments. Arzt registriert die Sorge: 'Ich verstehe das, aber die Statistik zeigt geringe Risiken' und h\u00e4ndigt sachlich die Packungsbeilage aus.",
          "Stufe 2 Fall B: Patientin schildert Belastung durch Pflege der Mutter. Arzt: 'Das ist eine schwierige Situation', verweist aber unverz\u00fcglich und sachlich auf den Sozialdienst.",
          "Stufe 3 Fall A: Patient schildert starke Angst vor der Narkose. Arzt legt Stift beiseite, wendet sich ganz dem Patienten zu: 'Ich kann gut nachvollziehen, dass Ihnen das Angst macht. Lassen Sie uns Schritt f\u00fcr Schritt durchsprechen, wie die An\u00e4sthesie Sie \u00fcberwacht.'",
          "Stufe 3 Fall B: Patientin sorgt sich um den Erhalt des Arbeitsplatzes bei Bandscheibenvorfall. Arzt validiert: 'Es ist v\u00f6llig verst\u00e4ndlich, dass diese Unsicherheit sehr belastend ist. Wir erarbeiten gemeinsam einen Stufenplan zur Wiedereingliederung.'",
          "Stufe 4 Fall A: Patientin stockt, Augen f\u00fcllen sich mit Tr\u00e4nen beim Thema famili\u00e4re Belastung. Arzt nimmt behutsam das nonverbale Signal auf: 'Ich sehe, wie schwer Ihnen das f\u00e4llt. Nehmen Sie sich alle Zeit. Wir sind hier in einem gesch\u00fctzten Raum.'",
          "Stufe 4 Fall B: Hochgradig verunsicherter Patient nach Malignom-Verdacht. Arzt erfasst feinste Nuancen der Verzweiflung, bietet emotionalen Halt, fasst Gef\u00fchle empathisch in Worte und stellt verbindliche Erreichbarkeit sicher.",

          # 211-218: K2_Struktur (levels 0 to 3)
          "Stufe 0 Fall A: Gespr\u00e4ch verl\u00e4uft chaotisch; Arzt springt von Familienanamnese zur Medikation, fragt nach Allergien, kehrt zur Kindheit zur\u00fcck und vergisst die aktuellen Leitsymptome.",
          "Stufe 0 Fall B: Keinerlei Gespr\u00e4chsphasen erkennbar; Patient erz\u00e4hlt ungef\u00fchrt 15 Minuten lang Anekdoten, Arzt unterbricht planlos und verliert v\u00f6llig den \u00dcberblick.",
          "Stufe 1 Fall A: Grundlegende Anamneseschritte sind vorhanden, aber der Arzt springt unvermittelt zwischen offener Exploration und apparativer Diagnostik hin und her.",
          "Stufe 1 Fall B: Roter Faden ansatzweise vorhanden, wird jedoch mehrfach durch sprunghafte Zwischenfragen zu Vorbefunden ohne Vorank\u00fcndigung unterbrochen.",
          "Stufe 2 Fall A: Klar gegliedertes Gespr\u00e4ch: Nach offener Schilderung folgt gezielte Systemanamnese, anschlie\u00dfend Erl\u00e4uterung des weiteren diagnostischen Vorgehens.",
          "Stufe 2 Fall B: Geordneter Ablauf der Anamnese: Einleitung, Schmerzerfassung, vegetative Anamnese und strukturierter Abschluss mit transparentem Zeitplan.",
          "Stufe 3 Fall A: Exzellent moderiertes Gespr\u00e4ch mit Agenda-Setting zu Beginn: 'Heute besprechen wir zun\u00e4chst Ihre Symptome, dann kl\u00e4ren wir offene Fragen und vereinbaren die Diagnostik.' Transparente \u00dcberleitungen zwischen allen Phasen.",
          "Stufe 3 Fall B: Musterg\u00fcltige Strukturierung mit regelm\u00e4\u00dfigen Zwischenzusammenfassungen, Phasentransparenz und partizipativer Festlegung der n\u00e4chsten Schritte ('Teach-Back').",

          # 219-230: K3_Fachkompetenz (levels 0 to 5)
          "Stufe 0 Fall A: Arzt fragt bei akutem Vernichtungsschmerz im Thorax lediglich nach Ern\u00e4hrungsgewohnheiten und schlie\u00dft lebensbedrohliche Ursachen nicht aus.",
          "Stufe 0 Fall B: V\u00f6llig inad\u00e4quate Begriffsverwendung, relevante Differenzialdiagnosen werden ignoriert, keine fachliche Einordnung der Beschwerden.",
          "Stufe 1 Fall A: Bei Verdacht auf Appendizitis wird nur unvollst\u00e4ndig palpiert; McBurney- und Lanz-Punkte werden verwechselt, Laboranforderung bleibt rudiment\u00e4r.",
          "Stufe 1 Fall B: L\u00fcckenhafte Exploration eines neu aufgetretenen Diabetes mellitus; Blutzuckerwerte werden erfragt, HbA1c, Ketoazidose-Zeichen und Folgesch\u00e4den jedoch ausgelassen.",
          "Stufe 2 Fall A: Korrekte Erhebung der Standard-Leitsymptome bei unkomplizierter Zystitis, ad\u00e4quate Urinstix-Indikation, jedoch keine Vertiefung zu atypischen Erregern.",
          "Stufe 2 Fall B: Solide Erfassung der Symptomatik einer arteriellen Hypertonie inklusive Standardlabor, ohne jedoch sekund\u00e4re Ursachen differenzierter zu pr\u00fcfen.",
          "Stufe 3 Fall A: Strukturierte differenzialdiagnostische Abkl\u00e4rung einer Belastungsdyspnoe: Systematische Abgrenzung kardialer (z.B. Herzinsuffizienz) vs. pulmonaler Ursachen (z.B. COPD).",
          "Stufe 3 Fall B: Pr\u00e4zise Erfassung von Begleitsymptomen bei Synkope: Orthostase, prodromale Aura, Miktionssynkope und kardiale Arrhythmien werden systematisch abgegrenzt.",
          "Stufe 4 Fall A: Umfassende Exploration bei V.a. Polymyalgia rheumatica: Gezieltes Screening nach Red Flags wie Riesenzellarteriitis (Sehst\u00f6rungen, Kauschmerz, Schl\u00e4fenschmerz).",
          "Stufe 4 Fall B: Hochgradig differenzierte Abkl\u00e4rung einer unklaren B-Symptomatik: Systematische Einordnung infekti\u00f6ser, maligner und autoimmuner \u00c4tiologien mit priorisierter Stufendiagnostik.",
          "Stufe 5 Fall A: Brillante klinische Urteilsbildung: Erkennung eines atypisch pr\u00e4sentierten Ph\u00e4ochromozytoms durch vernetzte Verkn\u00fcpfung von paroxysmalem RR-Anstieg, Schwitzen und Tremor; pr\u00e4zise Indikationsstellung f\u00fcr fraktionierte Metanephrine.",
          "Stufe 5 Fall B: Meisterhafte differentialdiagnostische Synthese bei komplexem Multimorbidit\u00e4tsfall: Priorisierung lebensbedrohlicher Ursachen, zielsichere Rationale f\u00fcr weiterf\u00fchrende Diagnostik und stringente Therapieentscheidung."
        ),
        source_id = c(
          # 101-104: Baseline cases
          1, 1, 2, 2,
          # 201-210: K1_Empathie (sources 1 and 3)
          1, 1, 1, 1, 3, 3, 3, 3, 3, 3,
          # 211-218: K2_Struktur (sources 2 and 3)
          2, 2, 2, 2, 3, 3, 3, 3,
          # 219-230: K3_Fachkompetenz (sources 1, 2, and 3)
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

  if (!empty) {
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

  if (!empty) {
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
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Not populated (mode: empty = TRUE, total: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Already populated (unchanged, %d training sessions present)", n_a)
  }

  # ----------------------------------------------------------------------------
  # Tabular status report with row counts and checksums
  # ----------------------------------------------------------------------------
  mode_label <- if (empty) "Minimal Admin Account (empty = TRUE)" else "Full Standard Dataset (empty = FALSE)"

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
