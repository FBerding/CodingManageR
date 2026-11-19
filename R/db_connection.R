# ==============================================================================
# DATENBANK-VERBINDUNGS- & POOL-KONFIGURATION (db_connection.R)
# ==============================================================================
# Dieses Skript verwaltet den Verbindungsaufbau zu einer relationalen Datenbank
# (MySQL / MariaDB oder SQLite) mit nativer Unterstützung für:
#
#  1. MULTI-USER BETRIEB (Shiny Server Open Source / Server-Umgebung):
#     - Verwendet das R-Paket 'pool' (pool::dbPool)
#     - Verwaltet einen langlebigen Connection-Pool für parallele User-Sessions
#     - Automatische Ausleihe, Thread-Sicherheit und Reconnect bei Timeouts
#     - Volle DBI-Kompatibilität (dbGetQuery, dbExecute etc. laufen transparent)
#
#  2. SINGLE-USER BETRIEB (Lokaler PC / Einzelarbeitsplatz):
#     - Lokale SQLite-Datenbank (z. B. 'datenbank.sqlite' oder ':memory:')
#     - Ausgelegt für genau einen Benutzer (Single-User)
#     - Explizite Multi-User-Sperre / Warnung bei Server-Betrieb
#
#  3. Konfigurierbar über:
#     - Parameter-Liste (config)
#     - Umgebungsvariablen (Sys.getenv("MYSQL_HOST") etc.)
# ==============================================================================

# Dependencies are imported via NAMESPACE for CRAN compliance

#' Prüft, ob ein übergebenes Objekt ein pool::dbPool ist
#' @param con Zu prüfendes Verbindungsobjekt
#' @return TRUE falls Pool, sonst FALSE
is_pool_connection <- function(con) {
  if (is.null(con)) return(FALSE)
  inherits(con, "Pool")
}

#' Prüft, ob es sich um eine MySQL- oder MariaDB-Verbindung oder einen entsprechenden Pool handelt
#' @param con DBIConnection oder Pool Objekt
#' @return TRUE falls MySQL/MariaDB, sonst FALSE
is_mysql_connection <- function(con) {
  if (is.null(con)) return(FALSE)
  if (is_pool_connection(con)) {
    # Driver-Klasse aus Pool ermitteln
    drv_class <- tryCatch(class(con$state$driver)[1], error = function(e) "")
    return(grepl("MariaDB|MySQL", drv_class, ignore.case = TRUE))
  }
  inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
}

#' Erstellt einen thread-sicheren Connection-Pool (für Multi-User / Shiny Server)
#' oder eine Standard-Verbindung für lokale Einzelplatznutzung.
#'
#' @param config Optionales benanntes List-Objekt mit:
#'   host, port, user, password, dbname, sqlite_path, minSize, maxSize, idleTimeout
#' @param fallback_to_sqlite Boolean: Soll bei Verbindungsfehler auf lokales SQLite
#'   zurückgegriffen werden? (Standard: TRUE)
#' @param force_pool Boolean: Soll zwingend pool::dbPool verwendet werden? (Standard: TRUE)
#' @return Pool oder DBIConnection Objekt
get_db_pool <- function(config = NULL, fallback_to_sqlite = TRUE, force_pool = TRUE) {
  # 1. Konfigurationsparameter bestimmen (übergebene Config oder Env-Variablen)
  host     <- if (!is.null(config$host) && nzchar(as.character(config$host))) as.character(config$host) else Sys.getenv("MYSQL_HOST", unset = Sys.getenv("MYSQL_DB_HOST", unset = ""))
  raw_port <- if (!is.null(config$port) && !is.na(config$port)) config$port else Sys.getenv("MYSQL_PORT", unset = Sys.getenv("MYSQL_DB_PORT", unset = "3306"))
  port     <- suppressWarnings(as.integer(raw_port))
  if (is.na(port) || port <= 0) port <- 3306

  user     <- if (!is.null(config$user) && nzchar(as.character(config$user))) as.character(config$user) else Sys.getenv("MYSQL_USER", unset = Sys.getenv("MYSQL_DB_USER", unset = ""))
  password <- if (!is.null(config$password)) as.character(config$password) else Sys.getenv("MYSQL_PASSWORD", unset = Sys.getenv("MYSQL_DB_PASSWORD", unset = ""))
  dbname   <- if (!is.null(config$dbname) && nzchar(as.character(config$dbname))) as.character(config$dbname) else Sys.getenv("MYSQL_DBNAME", unset = Sys.getenv("MYSQL_DB_NAME", unset = "kodierer_portal"))

  sqlite_f <- if (!is.null(config$sqlite_path) && nzchar(as.character(config$sqlite_path))) {
    as.character(config$sqlite_path)
  } else if (!is.null(config$sqlite_f) && nzchar(as.character(config$sqlite_f))) {
    as.character(config$sqlite_f)
  } else if (nzchar(Sys.getenv("SQLITE_PATH", unset = ""))) {
    Sys.getenv("SQLITE_PATH")
  } else {
    data_dir <- tools::R_user_dir("CodingManageR", which = "data")
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(data_dir, "lokale_datenbank.sqlite")
  }

  min_size     <- if (!is.null(config$minSize)) as.integer(config$minSize) else 2L
  max_size     <- if (!is.null(config$maxSize)) as.integer(config$maxSize) else 15L
  idle_timeout <- if (!is.null(config$idleTimeout)) as.integer(config$idleTimeout) else 3600000L # 1 Stunde in ms

  use_mysql <- nzchar(host) && nzchar(user)

  # 2. Versuch 1: MySQL / MariaDB (Zentrale Server-Datenbank für Mehrbenutzerbetrieb)
  # nocov start
  if (use_mysql) {
    message("--------------------------------------------------------------------------------")
    message("[DB-Manager] Multi-User-Modus: Verbinde mit MySQL/MariaDB auf '", host, ":", port, "' (DB: '", dbname, "')...")

    # A: Pool-Modus via pool::dbPool (Empfohlen für Shiny Server Open Source)
    if (force_pool && requireNamespace("pool", quietly = TRUE)) {
      pool_obj <- tryCatch({
        if (requireNamespace("RMariaDB", quietly = TRUE)) {
          message("[DB-Manager] Initialisiere thread-sicheren Connection-Pool mit 'RMariaDB' (Pool-Größe: ", min_size, "-", max_size, ")...")
          p <- pool::dbPool(
            drv = RMariaDB::MariaDB(),
            host = host,
            port = port,
            user = user,
            password = password,
            dbname = dbname,
            minSize = min_size,
            maxSize = max_size,
            idleTimeout = idle_timeout,
            client.flag = RMariaDB::CLIENT_MULTI_STATEMENTS
          )
          # UTF-8 und Foreign Keys absichern
          try(pool::dbExecute(p, "SET NAMES utf8mb4;"), silent = TRUE)
          try(pool::dbExecute(p, "SET foreign_key_checks = 1;"), silent = TRUE)
          message("[DB-Manager] Connection-Pool erfolgreich aufgebaut und einsatzbereit.")
          p
        } else if (requireNamespace("RMySQL", quietly = TRUE)) {
          message("[DB-Manager] Initialisiere Connection-Pool mit 'RMySQL'...")
          p <- pool::dbPool(
            drv = RMySQL::MySQL(),
            host = host,
            port = port,
            user = user,
            password = password,
            dbname = dbname,
            minSize = min_size,
            maxSize = max_size,
            idleTimeout = idle_timeout
          )
          message("[DB-Manager] Connection-Pool erfolgreich aufgebaut.")
          p
        } else {
          warning("[DB-Manager] Weder 'RMariaDB' noch 'RMySQL' installiert.")
          NULL
        }
      }, error = function(e) {
        warning(paste("[DB-Manager] Fehler beim Erstellen des MariaDB-Pools:", e$message))
        NULL
      })

      if (!is.null(pool_obj)) return(pool_obj)
    }

    # B: Direkte DBI-Verbindung (Fallback falls pool nicht verfügbar)
    message("[DB-Manager] Erstelle direkte DBI-Verbindung zu MySQL/MariaDB...")
    con <- tryCatch({
      if (requireNamespace("RMariaDB", quietly = TRUE)) {
        dbConnect(
          RMariaDB::MariaDB(),
          host = host,
          port = port,
          user = user,
          password = password,
          dbname = dbname,
          client.flag = RMariaDB::CLIENT_MULTI_STATEMENTS
        )
      } else if (requireNamespace("RMySQL", quietly = TRUE)) {
        dbConnect(
          RMySQL::MySQL(),
          host = host,
          port = port,
          user = user,
          password = password,
          dbname = dbname
        )
      } else {
        NULL
      }
    }, error = function(e) {
      warning(paste("[DB-Manager] Direkte MySQL-Verbindung fehlgeschlagen:", e$message))
      NULL
    })

    if (!is.null(con)) {
      try(dbExecute(con, "SET NAMES utf8mb4;"), silent = TRUE)
      try(dbExecute(con, "SET foreign_key_checks = 1;"), silent = TRUE)
      message("[DB-Manager] Direkte Verbindung zu MySQL/MariaDB erfolgreich aufgebaut.")
      return(con)
    }
  }
  # nocov end

  # 3. Versuch 2: SQLite (Ausschließlich für lokalen Einzelplatzbetrieb auf einem PC)
  if (fallback_to_sqlite) {
    message("--------------------------------------------------------------------------------")
    message("[DB-Manager] ACHTUNG: SQLite-Modus aktiviert (Pfad: '", sqlite_f, "').")
    message("[DB-Manager] SQLite ist für genau EINEN Benutzer auf einem lokalen PC ausgelegt.")

    # Prüfung auf laufende Shiny Server Umgebung
    is_shiny_server <- nzchar(Sys.getenv("SHINY_SERVER_VERSION")) || file.exists("/etc/shiny-server")
    if (is_shiny_server) {
      warning(
        "\n================================================================================\n",
        "WARNUNG: Die App läuft auf einem Shiny Server, verwendet jedoch SQLite!\n",
        "SQLite unterstützt keine gleichzeitigen Schreibzugriffe mehrerer Web-Sessions.\n",
        "Es drohen 'database is locked'-Fehler und möglicher Datenverlust.\n",
        "Bitte richten Sie für den Mehrbenutzerbetrieb MariaDB/MySQL ein.\n",
        "Siehe Vignette: vignette(\"CodingManageR\", package = \"CodingManageR\")\n",
        "================================================================================"
      )
    }

    if (!requireNamespace("RSQLite", quietly = TRUE)) { # nocov start
      stop("Das Paket 'RSQLite' ist nicht installiert. Bitte 'install.packages(\"RSQLite\")' ausführen.")
    } # nocov end

    # Bei Pool-Anforderung auch für SQLite pool::dbPool nutzen
    if (force_pool && requireNamespace("pool", quietly = TRUE)) {
      p_sqlite <- tryCatch({
        p <- pool::dbPool(
          drv = RSQLite::SQLite(),
          dbname = sqlite_f,
          minSize = 1L,
          maxSize = 1L # Strikt 1 Verbindung bei SQLite, um Race-Conditions zu minimieren
        )
        pool::dbExecute(p, "PRAGMA foreign_keys = ON;")
        pool::dbExecute(p, "PRAGMA journal_mode = WAL;") # Write-Ahead Logging für bessere Lese-Parallelität
        message("[DB-Manager] SQLite-Pool erfolgreich initialisiert (PRAGMA foreign_keys = ON, WAL = ON).")
        p
      }, error = function(e) {
        warning(paste("[DB-Manager] SQLite-Pool fehlgeschlagen, versuche Standardverbindung:", e$message))
        NULL
      })
      if (!is.null(p_sqlite)) return(p_sqlite)
    }

    con <- dbConnect(RSQLite::SQLite(), sqlite_f)
    dbExecute(con, "PRAGMA foreign_keys = ON;")
    dbExecute(con, "PRAGMA journal_mode = WAL;")
    message("[DB-Manager] Lokale SQLite-Verbindung erfolgreich hergestellt (Single-User).")
    return(con)
  }

  stop("[DB-Manager] Keine Datenbankverbindung möglich und Fallback auf SQLite ist deaktiviert.")
}

#' Rückwärtskompatible Wrapper-Funktion für bestehenden Code
#' @param config Optionale Konfiguration
#' @param fallback_to_sqlite Fallback-Flag
#' @return Pool oder Verbindungsobjekt
get_db_connection <- function(config = NULL, fallback_to_sqlite = TRUE) {
  get_db_pool(config = config, fallback_to_sqlite = fallback_to_sqlite, force_pool = TRUE)
}

#' Schließt einen Connection-Pool oder eine DBI-Verbindung sauber
#' @param con Zu schließendes Pool- oder Verbindungsobjekt
close_db_pool <- function(con) {
  if (is.null(con)) return(invisible(NULL))

  tryCatch({
    if (is_pool_connection(con)) {
      if (requireNamespace("pool", quietly = TRUE)) {
        pool::poolClose(con)
        message("[DB-Manager] Connection-Pool erfolgreich geschlossen.")
      }
    } else {
      dbDisconnect(con)
      message("[DB-Manager] Datenbankverbindung erfolgreich getrennt.")
    }
  }, error = function(e) {
    warning(paste("[DB-Manager] Fehler beim Schließen der Datenbankverbindung:", e$message))
  })
}

#' Rückwärtskompatibler Alias für close_db_pool
#' @param con Verbindungsobjekt
close_db_connection <- function(con) {
  close_db_pool(con)
}

#' Führt einen Code-Block atomar innerhalb einer Datenbank-Transaktion aus.
#' Unterstützt sowohl direkte DBIConnection-Objekte als auch pool::dbPool.
#' Bei einem Fehler wird ein automatischer Rollback ausgeführt und der vorherige
#' Zustand wiederhergestellt (Datenintegrität garantiert).
#'
#' @param con DBIConnection oder Pool Objekt
#' @param expr R-Ausdruck oder Funktion, die die Transaktions-Operationen ausführt
#' @return Ergebnis des ausgeführten Ausdrucks
with_db_transaction <- function(con, expr) {
  if (is.null(con)) stop("[Transaktion] Ungültige Datenbankverbindung (NULL).")

  # Fall A: pool::dbPool Verbindung
  if (is_pool_connection(con)) {
    if (!requireNamespace("pool", quietly = TRUE)) { # nocov start
      stop("[Transaktion] Paket 'pool' erforderlich, aber nicht verfügbar.")
    } # nocov end

    # Dedizierte Verbindung für die Dauer der Transaktion aus dem Pool leihen
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)

    # Transaktion auf der ausgeliehenen Verbindung durchführen
    dbBegin(raw_con)
    res <- tryCatch({
      val <- if (is.function(expr)) expr(raw_con) else eval.parent(substitute(expr))
      dbCommit(raw_con)
      val
    }, error = function(e) {
      try(dbRollback(raw_con), silent = TRUE)
      stop(e)
    })
    return(res)
  }

  # Fall B: Direkte DBI-Verbindung
  dbBegin(con)
  res <- tryCatch({
    val <- if (is.function(expr)) expr(con) else eval.parent(substitute(expr))
    dbCommit(con)
    val
  }, error = function(e) {
    try(dbRollback(con), silent = TRUE)
    stop(e)
  })
  return(res)
}

