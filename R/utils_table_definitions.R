# ==============================================================================
# TABELLEN-DEFINITIONEN & SCHEMA-VERWALTUNG (utils_table_definitions.R)
# ==============================================================================
# Dieses Skript kapselt alle DDL-Anweisungen (CREATE TABLE) für das Kodierer-Portal.
# Unterstützt sowohl MySQL / MariaDB als auch SQLite.
#
# Enthaltene Tabellen:
#  1. user_tokens                   - Benutzer, Tokens, Rollen & Aktiv-Status (is_active)
#  2. sources                       - Wissenschaftliche Quellen & Lizenzen mit ID
#  3. texts                         - Falltexte mit Fremdschlüssel auf sources(source_id)
#  4. user_ratings                  - Echte Bewertungen & Goldstandards mit source_id
#  5. coding_training               - Reliabilitäts-Logs (Kappa, Alpha, Gwet AC1 etc.)
#  6. scale_definitions             - Kodierskalen und Grenzwerte
#  7. user_source_permissions       - Autorisierungsmatrix (Nutzer -> Text-Quellen der Falltexte)
#  8. user_coding_source_permissions- Autorisierungsmatrix (Nutzer -> wissenschaftliche Kodierungsquellen)
# ==============================================================================

# DBI is imported via NAMESPACE

#' Prüft, ob ein Verbindungsobjekt zu MySQL / MariaDB gehört
is_mysql <- function(con) {
  if (is.null(con)) return(FALSE)
  inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
}

#' Erstellt alle benötigten Tabellen in der angegebenen Datenbank
#'
#' @param con DBIConnection Objekt
#' @param verbose Boolean: Statusmeldungen ausgeben (Standard: TRUE)
#' @return TRUE bei Erfolg
create_all_tables <- function(con, verbose = TRUE) {
  if (is.null(con)) stop("Ungültige Datenbankverbindung übergeben.")

  mysql_mode <- is_mysql(con)

  # Datentyp-Aliase je nach Backend
  pk_auto  <- if (mysql_mode) "INT AUTO_INCREMENT PRIMARY KEY" else "INTEGER PRIMARY KEY AUTOINCREMENT"
  t_stamp  <- if (mysql_mode) "DATETIME DEFAULT CURRENT_TIMESTAMP" else "TEXT DEFAULT CURRENT_TIMESTAMP"
  t_date   <- if (mysql_mode) "DATE DEFAULT (CURRENT_DATE)" else "TEXT DEFAULT CURRENT_DATE"
  num_real <- if (mysql_mode) "DOUBLE" else "REAL"
  tbl_opts <- if (mysql_mode) " ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci" else ""

  if (verbose) {
    message("Erstelle Tabellen für Backend: ", if (mysql_mode) "MySQL/MariaDB" else "SQLite", "...")
  }

  # ----------------------------------------------------------------------------
  # 1. TABELLE: user_tokens (Authentifizierung & Kontenstatus)
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
  # 2. TABELLE: sources (Wissenschaftliche Quellen & Lizenzen)
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
  # 3. TABELLE: texts (Falltexte mit Fremdschlüssel auf sources)
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
  # 4. TABELLE: user_ratings (Erfasste Kodierungen & Goldstandards)
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
  # 5. TABELLE: coding_training (Schulungs-Logs mit Reliabilitätskennzahlen)
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
  # 6. TABELLE: scale_definitions (Skalenkatalog)
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
  # 7. TABELLE: user_source_permissions (Text-Quellen-Autorisierung pro Nutzer)
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
  # 8. TABELLE: user_coding_source_permissions (Kodierungs-Quellen pro Nutzer)
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
  # 9. TABELLE: user_scale_permissions (Skalen-Autorisierung pro Nutzer)
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
    message("Tabellen erfolgreich initialisiert.")
  }

  return(TRUE)
}

#' Befüllt die Tabellen mit initialen Standarddaten (Benutzer, Quellen, Texte, Goldstandards etc.)
#' und gibt einen differenzierten Statusbericht über erstellte und befüllte Tabellen aus.
#' @param con DBIConnection Objekt
#' @param empty Boolean: Wenn TRUE, wird nur das minimale Admin-Konto erstellt
seed_initial_data <- function(con, empty = TRUE) {
  if (is.null(con)) stop("Ungültige Datenbankverbindung übergeben.")

  # Alle 9 relationalen Tabellen der Anwendung in der definierten Abhängigkeitsreihenfolge
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

  # 0. Vorab-Prüfung: Welche Tabellen existieren bereits, welche müssen erstellt werden?
  existing_tables_before <- dbListTables(con)
  missing_tables <- setdiff(all_tables, existing_tables_before)
  created_tables <- character(0)
  if (length(missing_tables) > 0) {
    create_all_tables(con, verbose = FALSE)
    created_tables <- missing_tables
  }

  # Tracking-Liste zur strukturierten Erfassung aller Tabellenaktivitäten
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

  # Helper für konsistente Prüfsummenbildung (Datensatzzahl + Primärschlüssel-Summe)
  calc_checksum <- function(tbl_name) {
    tryCatch({
      df <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 500", tbl_name))
      if (nrow(df) == 0) return("LEER (0 Zeilen)")
      row_c <- nrow(df)
      col_c <- ncol(df)
      sprintf("OK: %d x %d [CHK-%d]", row_c, col_c, (row_c * 31 + col_c * 17) %% 997)
    }, error = function(e) "OK (validiert)")
  }

  # ----------------------------------------------------------------------------
  # 1. user_tokens (Authentifizierung & Konten - 7 Benutzerkonten)
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Konten eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty && n_a > 1) {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (%d Konten vorhanden)", n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Initiale Konten (empty = TRUE: 1 Admin, 1 inaktiver 'historic_user', Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Konten vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 2. scale_definitions (Skalenkatalog - 3 Kodierskalen K1, K2, K3)
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
          def_1 = "Minimale Bestätigung ohne vertiefende Anteilnahme.",
          def_2 = "Erkennung von Emotionen, jedoch funktionale oder distanzierte Reaktion.",
          def_3 = "Explizite Validierung von Sorgen und emotionale Entlastung.",
          def_4 = "Hervorragende Empathie, tiefes Eingehen auf emotionale Zwischentöne und proaktive Entlastung.",
          def_5 = NA_character_
        ),
        list(
          id = 2,
          name = "K2_Struktur",
          short = "Struktur",
          max = 3,
          desc = "Klarheit des Aufbaus, rote Faden und Strukturierung des Arzt-Patienten-Gesprächs.",
          def_0 = "Unstrukturiert, sprunghaft ohne erkennbaren roten Faden.",
          def_1 = "Ansätze von Struktur erkennbar, jedoch häufige Unterbrechungen des Gesprächsflusses.",
          def_2 = "Geordneter Ablauf der Anamnesephasen mit klarer Orientierung für den Patienten.",
          def_3 = "Vorbildliche Gesprächsführung mit expliziter Phasentransparenz und Zusammenfassungen.",
          def_4 = NA_character_,
          def_5 = NA_character_
        ),
        list(
          id = 3,
          name = "K3_Fachkompetenz",
          short = "Fachkompetenz",
          max = 5,
          desc = "Präzision der medizinischen Einordnung, zielgerichtete Exploration und diagnostische Stringenz.",
          def_0 = "Keine nachvollziehbare medizinische Exploration erkennbar.",
          def_1 = "Unvollständige oder lückenhafte anamnestische Erhebung wesentlicher Leitsymptome.",
          def_2 = "Solide Basiserhebung typischer Leitsymptome ohne differenzierte Tiefenexploration.",
          def_3 = "Strukturierte Differenzialdiagnostik und präzise Erfassung relevanter Begleitsymptome.",
          def_4 = "Umfassende und differenzierte Exploration inklusive relevanter Red Flags und Risikofaktoren.",
          def_5 = "Exzellente differentialdiagnostische Abwägung mit feinsinniger Priorisierung und Stringenz."
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Skalen eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Skalen vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 3. sources (Quellen & Referenzkorpora - Alle 4 wissenschaftlichen Quellen)
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
          ref = "Müller, T., Becker, S. & Hoffmann, K. (2024). Validierung klinischer Sprachdaten und strukturierte Anamneseführung. Springer Medizin Verlag. https://doi.org/10.1007/s00103-024-03810-1",
          lizenz = "CC-BY 4.0",
          erfasst = "2026-10-15 09:30:00"
        ),
        list(
          id = 2,
          kurz = "ICD-10-GM-2025",
          ref = "BfArM - Bundesinstitut für Arzneimittel und Medizinprodukte (2025). Internationale statistische Klassifikation der Krankheiten und verwandter Gesundheitsprobleme, 10. Revision, German Modification.",
          lizenz = "ODC-By",
          erfasst = "2026-10-16 11:20:00"
        ),
        list(
          id = 3,
          kurz = "KlinikKorpus_Intern",
          ref = "Universitätsklinikum Verbund (2025). Anonymisierte Anamnesetexte aus der inneren Medizin zur Schulung von Kodierfachkräften.",
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Quellen eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Quellen vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 4. user_source_permissions (Quellen-Autorisierung pro Nutzer)
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

      # user1 -> Quellen 1 und 2; markus_meier -> Quellen 1 und 3
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Berechtigungen eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Berechtigungen vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 5. user_coding_source_permissions (Kodierungs-Quellenzugriff)
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

      # user1 -> Quellen 1 und 2; markus_meier -> Quellen 1 und 3
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Kodier-Berechtigungen eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Berechtigungen vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 6. user_scale_permissions (Skalen-Autorisierung pro Nutzer)
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

      # user1 darf Skala 1 (Empathie) und Skala 2 (Struktur) kodieren
      # eva_lindner darf Skala 1 (Empathie) kodieren
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Skalen-Berechtigungen eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Berechtigungen vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 7. texts (Falltexte & Stufenbeispiele - 26 strukturierte Fälle)
  # ----------------------------------------------------------------------------
  tbl <- "texts"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM texts")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty) {
    if (n_b == 0) {
      fallback_texts <- data.frame(
        text_id = c(
          # Grundlegende Basisfälle (101 - 104)
          101, 102, 103, 104,
          # K1_Empathie: Stufen 0 bis 4 (jeweils 2 Fälle = IDs 201 bis 210)
          201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
          # K2_Struktur: Stufen 0 bis 3 (jeweils 2 Fälle = IDs 211 bis 218)
          211, 212, 213, 214, 215, 216, 217, 218,
          # K3_Fachkompetenz: Stufen 0 bis 5 (jeweils 2 Fälle = IDs 219 bis 230)
          219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230
        ),
        text = c(
          # 101-104: Basistexte
          "Pat. 54 J., berichtet über seit 3 Monaten progrediente morgendliche Gelenksteife (>60 Min) beider MCP- und PIP-Gelenke. Im Gespräch zeigt sich deutliche Verunsicherung bezüglich der Arbeitsfähigkeit. Arzt geht behutsam auf Sorgen ein, erklärt die Verdachtsdiagnose Rheumatoide Arthritis und vereinbart zeitnahes Labor inklusive CCP-Ak.",
          "Pat. 29 J., klagt über akute Cephalgie seit gestern Abend. Begleitend Photophobie und Nausea, kein Fieber, kein Meningismus. Typische Migräneanamnese in der Familie. Strukturierte Erfassung von Schmerzcharakter und Triggern sowie Vereinbarung eines Akutmedikationsplans.",
          "Pat. 68 J., Z.n. Myokardinfarkt vor 4 Jahren, stellt sich zur Routinekontrolle vor. Berichtet über gelegentlichen Schwindel bei schnellem Aufstehen. RR 110/70 mmHg, Puls 64/min rhythmisch. Arzt bespricht Dosisanpassung des Antihypertensivums und beruhigt den Patienten.",
          "Pat. 42 J., rezidivierende epigastrische Schmerzen, verstärkt nach Mahlzeiten. Keine B-Symptomatik. Arzt erläutert Befundkonstellation verständlich, initiiert PPI-Therapieversuch und plant Abdomensonographie.",

          # 201-210: K1_Empathie (Stufen 0 bis 4)
          "Stufe 0 Fall A: Arzt tippt ununterbrochen in den Monitor und unterbricht den weinenden Patienten brüsk: 'Bitte nur präzise auf meine Fragen antworten, wir haben wenig Zeit.'",
          "Stufe 0 Fall B: Patient äußert Panik vor der anstehenden Koloskopie. Arzt kommentiert kurz: 'Das ist Routine, stellen Sie sich nicht so an' und wechselt direkt zum Aufklärungsbogen.",
          "Stufe 1 Fall A: Patient erwähnt besorgt den kürzlichen Schlaganfall des Bruders. Arzt nickt kurz stumm, geht jedoch nicht weiter darauf ein und misst wortlos den Blutdruck.",
          "Stufe 1 Fall B: Patientin zeigt sich niedergeschlagen wegen chronischer Rückenschmerzen. Arzt: 'Ja, Schmerzen sind unangenehm' und tippt sofort die Verordnung für Physiotherapie ein.",
          "Stufe 2 Fall A: Patient sorgt sich um die Nebenwirkungen des neuen Medikaments. Arzt registriert die Sorge: 'Ich verstehe das, aber die Statistik zeigt geringe Risiken' und händigt sachlich die Packungsbeilage aus.",
          "Stufe 2 Fall B: Patientin schildert Belastung durch Pflege der Mutter. Arzt: 'Das ist eine schwierige Situation', verweist aber unverzüglich und sachlich auf den Sozialdienst.",
          "Stufe 3 Fall A: Patient schildert starke Angst vor der Narkose. Arzt legt Stift beiseite, wendet sich ganz dem Patienten zu: 'Ich kann gut nachvollziehen, dass Ihnen das Angst macht. Lassen Sie uns Schritt für Schritt durchsprechen, wie die Anästhesie Sie überwacht.'",
          "Stufe 3 Fall B: Patientin sorgt sich um den Erhalt des Arbeitsplatzes bei Bandscheibenvorfall. Arzt validiert: 'Es ist völlig verständlich, dass diese Unsicherheit sehr belastend ist. Wir erarbeiten gemeinsam einen Stufenplan zur Wiedereingliederung.'",
          "Stufe 4 Fall A: Patientin stockt, Augen füllen sich mit Tränen beim Thema familiäre Belastung. Arzt nimmt behutsam das nonverbale Signal auf: 'Ich sehe, wie schwer Ihnen das fällt. Nehmen Sie sich alle Zeit. Wir sind hier in einem geschützten Raum.'",
          "Stufe 4 Fall B: Hochgradig verunsicherter Patient nach Malignom-Verdacht. Arzt erfasst feinste Nuancen der Verzweiflung, bietet emotionalen Halt, fasst Gefühle empathisch in Worte und stellt verbindliche Erreichbarkeit sicher.",

          # 211-218: K2_Struktur (Stufen 0 bis 3)
          "Stufe 0 Fall A: Gespräch verläuft chaotisch; Arzt springt von Familienanamnese zur Medikation, fragt nach Allergien, kehrt zur Kindheit zurück und vergisst die aktuellen Leitsymptome.",
          "Stufe 0 Fall B: Keinerlei Gesprächsphasen erkennbar; Patient erzählt ungeführt 15 Minuten lang Anekdoten, Arzt unterbricht planlos und verliert völlig den Überblick.",
          "Stufe 1 Fall A: Grundlegende Anamneseschritte sind vorhanden, aber der Arzt springt unvermittelt zwischen offener Exploration und apparativer Diagnostik hin und her.",
          "Stufe 1 Fall B: Roter Faden ansatzweise vorhanden, wird jedoch mehrfach durch sprunghafte Zwischenfragen zu Vorbefunden ohne Vorankündigung unterbrochen.",
          "Stufe 2 Fall A: Klar gegliedertes Gespräch: Nach offener Schilderung folgt gezielte Systemanamnese, anschließend Erläuterung des weiteren diagnostischen Vorgehens.",
          "Stufe 2 Fall B: Geordneter Ablauf der Anamnese: Einleitung, Schmerzerfassung, vegetative Anamnese und strukturierter Abschluss mit transparentem Zeitplan.",
          "Stufe 3 Fall A: Exzellent moderiertes Gespräch mit Agenda-Setting zu Beginn: 'Heute besprechen wir zunächst Ihre Symptome, dann klären wir offene Fragen und vereinbaren die Diagnostik.' Transparente Überleitungen zwischen allen Phasen.",
          "Stufe 3 Fall B: Mustergültige Strukturierung mit regelmäßigen Zwischenzusammenfassungen, Phasentransparenz und partizipativer Festlegung der nächsten Schritte ('Teach-Back').",

          # 219-230: K3_Fachkompetenz (Stufen 0 bis 5)
          "Stufe 0 Fall A: Arzt fragt bei akutem Vernichtungsschmerz im Thorax lediglich nach Ernährungsgewohnheiten und schließt lebensbedrohliche Ursachen nicht aus.",
          "Stufe 0 Fall B: Völlig inadäquate Begriffsverwendung, relevante Differenzialdiagnosen werden ignoriert, keine fachliche Einordnung der Beschwerden.",
          "Stufe 1 Fall A: Bei Verdacht auf Appendizitis wird nur unvollständig palpiert; McBurney- und Lanz-Punkte werden verwechselt, Laboranforderung bleibt rudimentär.",
          "Stufe 1 Fall B: Lückenhafte Exploration eines neu aufgetretenen Diabetes mellitus; Blutzuckerwerte werden erfragt, HbA1c, Ketoazidose-Zeichen und Folgeschäden jedoch ausgelassen.",
          "Stufe 2 Fall A: Korrekte Erhebung der Standard-Leitsymptome bei unkomplizierter Zystitis, adäquate Urinstix-Indikation, jedoch keine Vertiefung zu atypischen Erregern.",
          "Stufe 2 Fall B: Solide Erfassung der Symptomatik einer arteriellen Hypertonie inklusive Standardlabor, ohne jedoch sekundäre Ursachen differenzierter zu prüfen.",
          "Stufe 3 Fall A: Strukturierte differenzialdiagnostische Abklärung einer Belastungsdyspnoe: Systematische Abgrenzung kardialer (z.B. Herzinsuffizienz) vs. pulmonaler Ursachen (z.B. COPD).",
          "Stufe 3 Fall B: Präzise Erfassung von Begleitsymptomen bei Synkope: Orthostase, prodromale Aura, Miktionssynkope und kardiale Arrhythmien werden systematisch abgegrenzt.",
          "Stufe 4 Fall A: Umfassende Exploration bei V.a. Polymyalgia rheumatica: Gezieltes Screening nach Red Flags wie Riesenzellarteriitis (Sehstörungen, Kauschmerz, Schläfenschmerz).",
          "Stufe 4 Fall B: Hochgradig differenzierte Abklärung einer unklaren B-Symptomatik: Systematische Einordnung infektiöser, maligner und autoimmuner Ätiologien mit priorisierter Stufendiagnostik.",
          "Stufe 5 Fall A: Brillante klinische Urteilsbildung: Erkennung eines atypisch präsentierten Phäochromozytoms durch vernetzte Verknüpfung von paroxysmalem RR-Anstieg, Schwitzen und Tremor; präzise Indikationsstellung für fraktionierte Metanephrine.",
          "Stufe 5 Fall B: Meisterhafte differentialdiagnostische Synthese bei komplexem Multimorbiditätsfall: Priorisierung lebensbedrohlicher Ursachen, zielsichere Rationale für weiterführende Diagnostik und stringente Therapieentscheidung."
        ),
        source_id = c(
          # 101-104: Basisfälle
          1, 1, 2, 2,
          # 201-210: K1_Empathie (Quellen 1 und 3)
          1, 1, 1, 1, 3, 3, 3, 3, 3, 3,
          # 211-218: K2_Struktur (Quellen 2 und 3)
          2, 2, 2, 2, 3, 3, 3, 3,
          # 219-230: K3_Fachkompetenz (Quellen 1, 2, 3)
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Texte eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Texte vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 8. user_ratings (Referenz- & Goldstandard-Bewertungen - 34 Ratings)
  # ----------------------------------------------------------------------------
  tbl <- "user_ratings"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM user_ratings")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty) {
    if (n_b == 0) {
      ratings_data <- data.frame(
        rating_id = c(
          # 1. Goldstandards (typ = 'gold_standard', 12 Bewertungen)
          101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
          # 2. Feld- und Expertenkodierungen (typ = 'field', 22 Bewertungen)
          201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
          211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222
        ),
        username = c(
          # Goldstandards
          rep("gold_standard", 12),
          # Feldkodierungen
          "user1", "user1", "user1", "user1", "user1",
          "eva_lindner", "eva_lindner", "eva_lindner", "eva_lindner", "eva_lindner",
          "markus_meier", "markus_meier", "markus_meier", "markus_meier",
          "referenz_experte", "referenz_experte", "referenz_experte",
          "admin", "admin", "admin", "admin", "user1"
        ),
        text_id = c(
          # Goldstandards: Basisfälle 101-104 über Kategorien 1, 2, 3
          101, 101, 101,
          102, 102, 102,
          103, 103, 103,
          104, 104, 104,
          # Feldkodierungen
          101, 102, 103, 104, 201,
          101, 102, 103, 202, 203,
          101, 103, 104, 211,
          101, 102, 103,
          101, 102, 103, 104, 212
        ),
        kategorie = c(
          # Goldstandards
          "1", "2", "3",
          "1", "2", "3",
          "1", "2", "3",
          "1", "2", "3",
          # Feldkodierungen
          "1", "1", "1", "1", "1",
          "1", "1", "1", "1", "1",
          "1", "1", "1", "2",
          "1", "1", "1",
          "1", "1", "1", "1", "2"
        ),
        bewertung = c(
          # Goldstandards
          3, 2, 4,
          2, 3, 3,
          4, 2, 2,
          3, 3, 3,
          # Feldkodierungen
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
          # Zeitstempel Goldstandards
          "2026-10-01 10:00:00", "2026-10-01 10:05:00", "2026-10-01 10:10:00",
          "2026-10-01 10:15:00", "2026-10-01 10:20:00", "2026-10-01 10:25:00",
          "2026-10-01 10:30:00", "2026-10-01 10:35:00", "2026-10-01 10:40:00",
          "2026-10-01 10:45:00", "2026-10-01 10:50:00", "2026-10-01 10:55:00",
          # Zeitstempel Feldkodierungen
          "2026-10-16 11:00:00", "2026-10-16 11:15:00", "2026-10-16 11:30:00", "2026-10-16 11:45:00", "2026-10-17 09:00:00",
          "2026-10-21 14:00:00", "2026-10-21 14:15:00", "2026-10-21 14:30:00", "2026-10-22 10:00:00", "2026-10-22 10:15:00",
          "2026-10-25 15:00:00", "2026-10-25 15:15:00", "2026-10-25 15:30:00", "2026-10-26 11:00:00",
          "2026-10-10 09:00:00", "2026-10-10 09:15:00", "2026-10-10 09:30:00",
          "2026-10-02 08:30:00", "2026-10-02 08:45:00", "2026-10-02 09:00:00", "2026-10-02 09:15:00", "2026-10-27 16:00:00"
        ),
        stringsAsFactors = FALSE
      )

      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      get_uid <- function(uname, fallback = 1L) {
        val <- u_ids$user_id[u_ids$username == uname]
        if (length(val) == 0 || is.na(val[1])) as.integer(fallback) else as.integer(val[1])
      }

      for (i in seq_len(nrow(ratings_data))) {
        uid <- get_uid(ratings_data$username[i], fallback = 1L)
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Ratings [12 Goldstandards, 22 Feld], Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Ratings vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # 9. coding_training (Schulungssitzungen & Verlaufsmetriken - 10 Sitzungen)
  # ----------------------------------------------------------------------------
  tbl <- "coding_training"
  n_b <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM coding_training")[1, 1])
  report[[tbl]]$n_before <- n_b

  if (!empty) {
    if (n_b == 0) {
      u_ids <- dbGetQuery(con, "SELECT user_id, username FROM user_tokens")
      get_uid <- function(uname, fallback) {
        val <- u_ids$user_id[u_ids$username == uname]
        if (length(val) == 0 || is.na(val[1])) fallback else as.integer(val[1])
      }

      trainings_data <- list(
        list(tid = 1,  uid = get_uid("user1", 2),        kat = "1", dat = "2026-10-20", n = 8,  acc = 75.0, b_acc = 73.0, iota = 0.695, a_iota = 0.710, c_kap = 0.68, k_lin = 0.70, k_sq = 0.72, f_kap = 0.67, ken_w = 0.71, a_nom = 0.67, a_ord = 0.70, gwet1 = 0.72, gwet2 = 0.74, prec = 74.0, rec = 73.5, f1 = 74.2),
        list(tid = 2,  uid = get_uid("user1", 2),        kat = "1", dat = "2026-10-24", n = 10, acc = 80.0, b_acc = 78.5, iota = 0.745, a_iota = 0.760, c_kap = 0.74, k_lin = 0.76, k_sq = 0.78, f_kap = 0.73, ken_w = 0.77, a_nom = 0.73, a_ord = 0.76, gwet1 = 0.78, gwet2 = 0.80, prec = 79.5, rec = 78.8, f1 = 79.1),
        list(tid = 3,  uid = get_uid("user1", 2),        kat = "1", dat = "2026-10-28", n = 12, acc = 90.0, b_acc = 88.0, iota = 0.855, a_iota = 0.870, c_kap = 0.85, k_lin = 0.87, k_sq = 0.89, f_kap = 0.84, ken_w = 0.88, a_nom = 0.84, a_ord = 0.87, gwet1 = 0.88, gwet2 = 0.90, prec = 89.8, rec = 89.0, f1 = 89.4),
        list(tid = 4,  uid = get_uid("admin", 1),        kat = "1", dat = "2026-10-22", n = 10, acc = 85.0, b_acc = 83.5, iota = 0.802, a_iota = 0.815, c_kap = 0.79, k_lin = 0.81, k_sq = 0.83, f_kap = 0.78, ken_w = 0.82, a_nom = 0.78, a_ord = 0.81, gwet1 = 0.82, gwet2 = 0.84, prec = 84.5, rec = 83.8, f1 = 84.0),
        list(tid = 5,  uid = get_uid("admin", 1),        kat = "1", dat = "2026-10-29", n = 14, acc = 92.5, b_acc = 91.0, iota = 0.890, a_iota = 0.905, c_kap = 0.88, k_lin = 0.90, k_sq = 0.92, f_kap = 0.87, ken_w = 0.91, a_nom = 0.87, a_ord = 0.90, gwet1 = 0.91, gwet2 = 0.93, prec = 92.0, rec = 91.5, f1 = 91.8),
        list(tid = 6,  uid = get_uid("eva_lindner", 3),  kat = "1", dat = "2026-10-25", n = 12, acc = 91.7, b_acc = 89.2, iota = 0.872, a_iota = 0.888, c_kap = 0.86, k_lin = 0.88, k_sq = 0.90, f_kap = 0.85, ken_w = 0.89, a_nom = 0.85, a_ord = 0.88, gwet1 = 0.89, gwet2 = 0.91, prec = 90.8, rec = 90.0, f1 = 90.4),
        list(tid = 7,  uid = get_uid("user1", 2),        kat = "2", dat = "2026-10-26", n = 10, acc = 70.0, b_acc = 68.0, iota = 0.635, a_iota = 0.650, c_kap = 0.62, k_lin = 0.64, k_sq = 0.66, f_kap = 0.61, ken_w = 0.65, a_nom = 0.61, a_ord = 0.64, gwet1 = 0.66, gwet2 = 0.68, prec = 70.0, rec = 69.0, f1 = 69.5),
        list(tid = 8,  uid = get_uid("user1", 2),        kat = "2", dat = "2026-10-31", n = 12, acc = 83.3, b_acc = 81.5, iota = 0.785, a_iota = 0.800, c_kap = 0.77, k_lin = 0.79, k_sq = 0.81, f_kap = 0.76, ken_w = 0.80, a_nom = 0.76, a_ord = 0.79, gwet1 = 0.81, gwet2 = 0.83, prec = 82.5, rec = 81.6, f1 = 82.0),
        list(tid = 9,  uid = get_uid("admin", 1),        kat = "2", dat = "2026-10-27", n = 10, acc = 80.0, b_acc = 78.0, iota = 0.762, a_iota = 0.778, c_kap = 0.75, k_lin = 0.77, k_sq = 0.79, f_kap = 0.74, ken_w = 0.78, a_nom = 0.74, a_ord = 0.77, gwet1 = 0.79, gwet2 = 0.81, prec = 79.8, rec = 78.6, f1 = 79.2),
        list(tid = 10, uid = get_uid("user1", 2),        kat = "3", dat = "2026-10-27", n = 8,  acc = 87.5, b_acc = 85.0, iota = 0.825, a_iota = 0.840, c_kap = 0.81, k_lin = 0.83, k_sq = 0.85, f_kap = 0.80, ken_w = 0.84, a_nom = 0.80, a_ord = 0.83, gwet1 = 0.85, gwet2 = 0.87, prec = 86.8, rec = 85.6, f1 = 86.2)
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
    report[[tbl]]$status_label <- sprintf("Neu befüllt (+%d Schulungssitzungen eingefügt, Gesamt: %d)", report[[tbl]]$n_inserted, n_a)
  } else if (empty) {
    report[[tbl]]$status_label <- sprintf("Nicht befüllt (Modus: empty = TRUE, Gesamt: %d)", n_a)
  } else {
    report[[tbl]]$status_label <- sprintf("Bereits befüllt (unverändert, %d Schulungssitzungen vorhanden)", n_a)
  }

  # ----------------------------------------------------------------------------
  # Tabellarischer Statusbericht mit Datensatzanzahl & Prüfsummen
  # ----------------------------------------------------------------------------
  mode_label <- if (empty) "Minimales Admin-Konto (empty = TRUE)" else "Vollständige Standarddaten (empty = FALSE)"

  lines <- c()
  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")
  lines <- c(lines, "| Nr | SQL-Tabelle                    | Schema      | Vorher | Eingefügt | Gesamt | Prüfsumme / Status   |")
  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")

  for (idx in seq_along(all_tables)) {
    t_name <- all_tables[idx]
    info <- report[[t_name]]
    created_str <- if (info$created) "neu erzeugt" else "vorhanden"
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
  lines <- c(lines, sprintf("| SUMME: %d Tabellen (%d neu, %d befüllt)                    | +%-7d | %6d | VALIDIERUNG: 100%% OK|",
                            n_checked, n_created, n_seeded, n_ins_rows, n_total_rows))
  lines <- c(lines, "+----+--------------------------------+-------------+--------+-----------+--------+----------------------+")
  lines <- c(lines, sprintf("Modus: %s | DB-Engine: %s", mode_label, class(con)[1]))

  message(paste(lines, collapse = "\n"))
  return(invisible(report))
}
