# Note: This file was generated with the assistance of AI.

# ==============================================================================
# DATABASE CONNECTION AND POOL CONFIGURATION (db_connection.R)
# ==============================================================================
# Manages connections to a relational database (MySQL/MariaDB or SQLite) with
# native support for:
#
#  1. MULTI-USER MODE (Shiny Server Open Source / server deployment):
#     - Uses the `pool` package (`pool::dbPool`)
#     - Maintains a long-lived connection pool for concurrent user sessions
#     - Automatic checkout, thread safety, and reconnection on idle timeouts
#     - Full DBI compatibility (`dbGetQuery()`, `dbExecute()`, etc.)
#
#  2. SINGLE-USER MODE (local workstation / desktop session):
#     - Local SQLite database (e.g., `lokale_datenbank.sqlite` or `:memory:`)
#     - Intended strictly for a single concurrent user
#     - Emits an explicit warning when SQLite is detected on Shiny Server
#
#  3. Configurable via:
#     - Named `config` list argument
#     - Environment variables (`MYSQL_HOST`, `MYSQL_PORT`, `SQLITE_PATH`, etc.)
# ==============================================================================

# Dependencies are imported via NAMESPACE for CRAN compliance

#' Check Whether an Object Is a `pool::dbPool` Connection
#'
#' Tests whether the supplied database handle inherits from the `Pool` class
#' provided by the `pool` package.
#'
#' @param con Database connection or pool object to inspect.
#'
#' @return A logical scalar: `TRUE` if `con` is a `Pool` object, `FALSE` otherwise.
#' @examples
#' is_pool_connection(NULL)
#' @export
is_pool_connection <- function(con) {
  if (is.null(con)) return(FALSE)
  inherits(con, "Pool")
}

#' Check Whether a Connection Targets MySQL or MariaDB
#'
#' Inspects a direct `DBIConnection` or `pool::dbPool` handle to determine
#' whether the underlying database backend is MySQL or MariaDB.
#'
#' @param con A `DBIConnection` or `pool::dbPool` object.
#'
#' @return A logical scalar: `TRUE` for MySQL/MariaDB connections, `FALSE` otherwise.
#' @examples
#' is_mysql_connection(NULL)
#' @export
is_mysql_connection <- function(con) {
  if (is.null(con)) return(FALSE)
  if (is_pool_connection(con)) {
    # Fast path: inspect driver stored on legacy pool state or mock stubs
    drv_class <- tryCatch({
      drv <- con$state$driver
      if (!is.null(drv)) class(drv)[1] else ""
    }, error = function(e) "")
    if (nzchar(drv_class)) {
      return(grepl("MariaDB|MySQL", drv_class, ignore.case = TRUE))
    }

    # Pool >= 1.0 fallback: safely inspect the underlying checked-out DBI connection
    if (requireNamespace("pool", quietly = TRUE)) {
      return(tryCatch({
        raw_con <- pool::poolCheckout(con)
        on.exit(try(pool::poolReturn(raw_con), silent = TRUE), add = TRUE)
        inherits(raw_con, "MariaDBConnection") || inherits(raw_con, "MySQLConnection")
      }, error = function(e) FALSE))
    }
    return(FALSE)
  }
  inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
}

#' Resolve Effective SQLite Database File Path
#'
#' Resolves the target SQLite file path using CodingManageR's standard
#' precedence: explicit `sqlite_path`, `config$sqlite_path`, `config$sqlite_f`,
#' optional `getOption("CodingManageR.sqlite_path")`, `Sys.getenv("SQLITE_PATH")`,
#' and finally the default file under `tools::R_user_dir("CodingManageR", "data")`.
#'
#' @param config Optional named database configuration list.
#' @param sqlite_path Optional explicit SQLite file path.
#' @param check_option Logical; if `TRUE`, checks `getOption("CodingManageR.sqlite_path")`
#'   before `Sys.getenv("SQLITE_PATH")`. Defaults to `FALSE`.
#' @param create_dir Logical; if `TRUE`, creates the default user data directory
#'   when falling back to the default file path. Defaults to `FALSE`.
#'
#' @return Character scalar containing the resolved SQLite path.
#' @keywords internal
#' @noRd
resolve_sqlite_path <- function(config = NULL,
                                sqlite_path = NULL,
                                check_option = FALSE,
                                create_dir = FALSE) {
  if (!is.null(sqlite_path) && length(sqlite_path) > 0L && nzchar(as.character(sqlite_path)[1])) {
    return(as.character(sqlite_path)[1])
  }
  if (!is.null(config$sqlite_path) && length(config$sqlite_path) > 0L && nzchar(as.character(config$sqlite_path)[1])) {
    return(as.character(config$sqlite_path)[1])
  }
  if (!is.null(config$sqlite_f) && length(config$sqlite_f) > 0L && nzchar(as.character(config$sqlite_f)[1])) {
    return(as.character(config$sqlite_f)[1])
  }
  if (isTRUE(check_option)) {
    opt_path <- getOption("CodingManageR.sqlite_path", default = "")
    if (!is.null(opt_path) && length(opt_path) > 0L && nzchar(as.character(opt_path)[1])) {
      return(as.character(opt_path)[1])
    }
  }
  env_path <- Sys.getenv("SQLITE_PATH", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  data_dir <- tools::R_user_dir("CodingManageR", which = "data")
  if (isTRUE(create_dir) && !dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(data_dir, "lokale_datenbank.sqlite")
}

#' Load Missing Environment Variables from an App-Local `.Renviron` File
#'
#' Reads a local `.Renviron` file (by default in the current working directory)
#' so server deployments launched from outside the app directory still pick up
#' missing database and runtime settings. Any environment variables that are
#' already set to a non-empty value in the current process are preserved.
#'
#' @param path Path to the `.Renviron` file. Defaults to `".Renviron"`.
#'
#' @return Logical scalar (`TRUE` if the file existed and was read, `FALSE`
#'   otherwise), invisibly.
#' @keywords internal
#' @noRd
load_local_renviron <- function(path = ".Renviron") {
  if (!is.character(path) || length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    return(invisible(FALSE))
  }

  prev <- Sys.getenv()
  prev_nonempty <- prev[nzchar(prev)]

  loaded <- tryCatch({
    readRenviron(path)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (length(prev_nonempty) > 0L) {
    after <- Sys.getenv(names(prev_nonempty), unset = NA_character_)
    changed <- is.na(after) | after != unname(prev_nonempty)
    if (any(changed)) {
      do.call(Sys.setenv, as.list(prev_nonempty[changed]))
    }
  }

  invisible(loaded)
}

#' Resolve First Non-Empty Environment Variable from an Ordered Candidate List
#'
#' Iterates through `var_names` in priority order and returns the first
#' non-empty value (skipping both unset and empty/whitespace-only entries).
#'
#' @param var_names Character vector of environment variable names in priority
#'   order.
#' @param default Character scalar fallback returned when none are set.
#' @param trim Logical; if `TRUE`, trims leading/trailing whitespace and
#'   carriage returns. Set to `FALSE` for passwords.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
resolve_env_setting <- function(var_names, default = "", trim = TRUE) {
  for (nm in var_names) {
    raw <- Sys.getenv(nm, unset = "")
    val <- if (isTRUE(trim)) trimws(raw) else raw
    if (nzchar(val)) {
      return(val)
    }
  }
  default
}

#' Resolve MySQL/MariaDB Connection Parameters from Config and Environment
#'
#' Resolves `host`, `port`, `user`, `password`, and `dbname` using explicit
#' `config` entries first and environment variables second (`MYSQL_HOST`,
#' `MYSQL_PORT`, `MYSQL_USER`, `MYSQL_PASSWORD`, and `MYSQL_DBNAME`, with
#' secondary alias fallbacks when the primary variable is unset or empty).
#' When `auto_load_renviron = TRUE` and any required parameter is missing, an
#' app-local `.Renviron` file is loaded automatically without overwriting
#' already configured non-empty environment variables.
#'
#' @param config Optional named database configuration list.
#' @param auto_load_renviron Logical; whether to attempt loading `renviron_path`
#'   when required MySQL parameters are missing. Defaults to `TRUE`.
#' @param renviron_path Path to the local `.Renviron` file. Defaults to
#'   `".Renviron"`.
#'
#' @return Named list with elements `host`, `port`, `user`, `password`, and
#'   `dbname`.
#' @keywords internal
#' @noRd
resolve_mysql_config <- function(config = NULL,
                                 auto_load_renviron = TRUE,
                                 renviron_path = ".Renviron") {
  read_cfg_chr <- function(val, trim = TRUE, allow_empty = FALSE) {
    if (is.null(val) || length(val) == 0L || is.na(val[1])) {
      return(NULL)
    }
    out <- as.character(val[1])
    if (isTRUE(trim)) out <- trimws(out)
    if (!isTRUE(allow_empty) && !nzchar(out)) {
      return(NULL)
    }
    out
  }

  resolve_once <- function() {
    cfg_host <- read_cfg_chr(config$host, trim = TRUE, allow_empty = FALSE)
    cfg_port <- read_cfg_chr(config$port, trim = TRUE, allow_empty = FALSE)
    cfg_user <- read_cfg_chr(config$user, trim = TRUE, allow_empty = FALSE)
    cfg_pass <- read_cfg_chr(config$password, trim = FALSE, allow_empty = TRUE)
    cfg_db   <- read_cfg_chr(config$dbname, trim = TRUE, allow_empty = FALSE)

    host <- if (!is.null(cfg_host)) cfg_host else resolve_env_setting(c("MYSQL_HOST", "MYSQL_DB_HOST"), default = "", trim = TRUE)
    raw_port <- if (!is.null(cfg_port)) cfg_port else resolve_env_setting(c("MYSQL_PORT", "MYSQL_DB_PORT"), default = "3306", trim = TRUE)
    user <- if (!is.null(cfg_user)) cfg_user else resolve_env_setting(c("MYSQL_USER", "MYSQL_DB_USER"), default = "", trim = TRUE)
    password <- if (!is.null(cfg_pass)) cfg_pass else resolve_env_setting(c("MYSQL_PASSWORD", "MYSQL_DB_PASSWORD"), default = "", trim = FALSE)
    dbname <- if (!is.null(cfg_db)) cfg_db else resolve_env_setting(c("MYSQL_DBNAME", "MYSQL_DB_NAME", "MYSQL_NAME", "MYSQL_DATABASE"), default = "", trim = TRUE)

    port <- suppressWarnings(as.integer(raw_port))
    if (is.na(port) || port <= 0L) port <- 3306L

    list(
      host = host,
      port = port,
      user = user,
      password = password,
      dbname = dbname
    )
  }

  res <- resolve_once()

  if (isTRUE(auto_load_renviron) &&
      !isTRUE(config$force_sqlite) &&
      (!nzchar(res$host) || !nzchar(res$user) || !nzchar(res$dbname)) &&
      file.exists(renviron_path)) {
    if (isTRUE(load_local_renviron(renviron_path))) {
      res <- resolve_once()
    }
  }

  res
}

#' Create a Database Connection Pool or Direct DBI Connection
#'
#' Creates a thread-safe `pool::dbPool` (recommended for multi-user Shiny Server
#' deployments) or a direct `DBIConnection` targeting MySQL/MariaDB, with
#' optional fallback to a local SQLite database for single-user desktop sessions.
#'
#' @param config Optional named list with connection settings: `host`, `port`,
#'   `user`, `password`, `dbname`, `sqlite_path`, `minSize`, `maxSize`, and
#'   `idleTimeout`. Missing entries fall back to environment variables
#'   (`MYSQL_HOST`, `MYSQL_PORT`, `MYSQL_USER`, `MYSQL_PASSWORD`,
#'   `MYSQL_DBNAME`), automatically loading an app-local `.Renviron` file if
#'   required variables are not yet set.
#' @param fallback_to_sqlite Logical; if `TRUE` (default), falls back to a local
#'   SQLite database when no MySQL/MariaDB connection is configured or available.
#' @param force_pool Logical; if `TRUE` (default), wraps the connection in a
#'   `pool::dbPool` when the `pool` package is available.
#'
#' @return A `pool::dbPool` or `DBIConnection` object.
#' @examples
#' \dontrun{
#' pool_con <- get_db_pool(config = list(sqlite_path = ":memory:"))
#' close_db_pool(pool_con)
#' }
#' @export
get_db_pool <- function(config = NULL, fallback_to_sqlite = TRUE, force_pool = TRUE) {
  # 1. Resolve connection parameters from explicit config or environment/.Renviron
  force_sqlite <- isTRUE(config$force_sqlite)
  mysql_cfg <- resolve_mysql_config(config = config, auto_load_renviron = !force_sqlite)

  host     <- mysql_cfg$host
  port     <- mysql_cfg$port
  user     <- mysql_cfg$user
  password <- mysql_cfg$password
  dbname   <- mysql_cfg$dbname

  min_size     <- if (!is.null(config$minSize)) as.integer(config$minSize) else 2L
  max_size     <- if (!is.null(config$maxSize)) as.integer(config$maxSize) else 15L
  idle_timeout <- if (!is.null(config$idleTimeout)) as.integer(config$idleTimeout) else 3600000L # 1 hour in ms

  use_mysql <- !force_sqlite && nzchar(host) && nzchar(user) && nzchar(dbname)

  if (!force_sqlite && nzchar(host) && nzchar(user) && !nzchar(dbname) && isTRUE(fallback_to_sqlite)) {
    warning(
      "[DB-Manager] MYSQL_HOST and MYSQL_USER are configured, but MYSQL_DBNAME is missing; falling back to local SQLite.",
      call. = FALSE
    )
  }

  # 2. Attempt 1: MySQL / MariaDB (central server database for multi-user deployments)
  # nocov start
  if (use_mysql) {
    message("--------------------------------------------------------------------------------")
    message("[DB-Manager] Multi-user mode: connecting to MySQL/MariaDB at '", host, ":", port, "' (DB: '", dbname, "')...")

    # Option A: Pooled mode via pool::dbPool (recommended for Shiny Server Open Source)
    if (force_pool && requireNamespace("pool", quietly = TRUE)) {
      pool_obj <- tryCatch({
        if (requireNamespace("RMariaDB", quietly = TRUE)) {
          message("[DB-Manager] Initializing thread-safe connection pool with 'RMariaDB' (pool size: ", min_size, "-", max_size, ")...")
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
          # Enforce UTF-8 encoding and foreign key constraints
          try(pool::dbExecute(p, "SET NAMES utf8mb4;"), silent = TRUE)
          try(pool::dbExecute(p, "SET foreign_key_checks = 1;"), silent = TRUE)
          message("[DB-Manager] Connection pool established and ready.")
          p
        } else if (requireNamespace("RMySQL", quietly = TRUE)) {
          message("[DB-Manager] Initializing connection pool with 'RMySQL'...")
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
          message("[DB-Manager] Connection pool established successfully.")
          p
        } else {
          warning("[DB-Manager] Neither 'RMariaDB' nor 'RMySQL' is installed.")
          NULL
        }
      }, error = function(e) {
        warning(paste("[DB-Manager] Failed to create MariaDB/MySQL connection pool:", e$message))
        NULL
      })

      if (!is.null(pool_obj)) return(pool_obj)
    }

    # Option B: Direct DBI connection (fallback when pool is unavailable)
    message("[DB-Manager] Creating direct DBI connection to MySQL/MariaDB...")
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
      warning(paste("[DB-Manager] Direct MySQL/MariaDB connection failed:", e$message))
      NULL
    })

    if (!is.null(con)) {
      try(dbExecute(con, "SET NAMES utf8mb4;"), silent = TRUE)
      try(dbExecute(con, "SET foreign_key_checks = 1;"), silent = TRUE)
      message("[DB-Manager] Direct MySQL/MariaDB connection established successfully.")
      return(con)
    }
  }
  # nocov end

  # 3. Attempt 2: SQLite (strictly intended for local single-user sessions)
  if (fallback_to_sqlite) {
    sqlite_f <- resolve_sqlite_path(config = config, check_option = FALSE, create_dir = TRUE)

    message("--------------------------------------------------------------------------------")
    message("[DB-Manager] NOTICE: SQLite mode active (path: '", sqlite_f, "').")
    message("[DB-Manager] SQLite is designed for a SINGLE local user on a workstation.")

    # Detect Shiny Server environment and warn against multi-user SQLite usage
    is_shiny_server <- nzchar(Sys.getenv("SHINY_SERVER_VERSION")) || file.exists("/etc/shiny-server")
    if (is_shiny_server) {
      warning(
        "\n================================================================================\n",
        "WARNING: The application is running on Shiny Server but is using SQLite!\n",
        "SQLite does not support concurrent writes across multiple web sessions.\n",
        "This may cause 'database is locked' errors and potential data loss.\n",
        "Please configure MariaDB/MySQL for multi-user deployments.\n",
        "See vignette: vignette(\"CodingManageR\", package = \"CodingManageR\")\n",
        "================================================================================"
      )
    }

    if (!requireNamespace("RSQLite", quietly = TRUE)) { # nocov start
      stop("Package 'RSQLite' is not installed. Please run 'install.packages(\"RSQLite\")'.")
    } # nocov end

    # Wrap SQLite in pool::dbPool when pooling is requested
    if (force_pool && requireNamespace("pool", quietly = TRUE)) {
      p_sqlite <- tryCatch({
        p <- pool::dbPool(
          drv = RSQLite::SQLite(),
          dbname = sqlite_f,
          minSize = 1L,
          maxSize = 1L # Strictly 1 connection for SQLite to minimize lock contention
        )
        pool::dbExecute(p, "PRAGMA foreign_keys = ON;")
        pool::dbExecute(p, "PRAGMA journal_mode = WAL;") # Write-Ahead Logging improves concurrent reads
        message("[DB-Manager] SQLite pool initialized successfully (PRAGMA foreign_keys = ON, WAL = ON).")
        p
      }, error = function(e) {
        warning(paste("[DB-Manager] SQLite pool initialization failed; falling back to direct connection:", e$message))
        NULL
      })
      if (!is.null(p_sqlite)) return(p_sqlite)
    }

    con <- dbConnect(RSQLite::SQLite(), sqlite_f)
    dbExecute(con, "PRAGMA foreign_keys = ON;")
    dbExecute(con, "PRAGMA journal_mode = WAL;")
    message("[DB-Manager] Local SQLite connection established successfully (single-user mode).")
    return(con)
  }

  if (!use_mysql) {
    missing_vars <- c(
      if (!nzchar(host)) "MYSQL_HOST",
      if (!nzchar(user)) "MYSQL_USER",
      if (!nzchar(dbname)) "MYSQL_DBNAME"
    )
    stop(
      "[DB-Manager] MySQL/MariaDB server configuration is incomplete (missing ",
      paste(missing_vars, collapse = ", "),
      " in .Renviron or config) and SQLite fallback is disabled. ",
      "Configure MYSQL_HOST, MYSQL_PORT, MYSQL_USER, MYSQL_PASSWORD, and MYSQL_DBNAME ",
      "(or use CodingManageR::run_app_local() for single-user local SQLite sessions).",
      call. = FALSE
    )
  }

  # nocov start
  has_mysql_drv <- requireNamespace("RMariaDB", quietly = TRUE) ||
    requireNamespace("RMySQL", quietly = TRUE)
  if (!has_mysql_drv) {
    stop(
      "[DB-Manager] Neither 'RMariaDB' nor 'RMySQL' is installed and SQLite fallback is disabled. ",
      "Install 'RMariaDB' system-wide (install.packages(\"RMariaDB\")) to connect to MySQL/MariaDB.",
      call. = FALSE
    )
  }

  stop(
    "[DB-Manager] Failed to connect to MySQL/MariaDB at '", host, ":", port,
    "' (database '", dbname, "') and SQLite fallback is disabled. ",
    "Check that the MySQL/MariaDB service is running and verify your credentials in .Renviron.",
    call. = FALSE
  )
  # nocov end
}

#' Open a Database Connection (Backward-Compatible Wrapper)
#'
#' Backward-compatible wrapper around [get_db_pool()] with `force_pool = TRUE`.
#'
#' @param config Optional named list of database configuration settings.
#' @param fallback_to_sqlite Logical; whether to fall back to SQLite if
#'   MySQL/MariaDB is not configured or unreachable. Defaults to `TRUE`.
#'
#' @return A `pool::dbPool` or `DBIConnection` object.
#' @examples
#' \dontrun{
#' con <- get_db_connection(config = list(sqlite_path = ":memory:"))
#' close_db_connection(con)
#' }
#' @export
get_db_connection <- function(config = NULL, fallback_to_sqlite = TRUE) {
  get_db_pool(config = config, fallback_to_sqlite = fallback_to_sqlite, force_pool = TRUE)
}

#' Safely Close a Connection Pool or DBI Connection
#'
#' Closes a `pool::dbPool` via `pool::poolClose()` or disconnects a direct
#' `DBIConnection` via `DBI::dbDisconnect()`, emitting a warning instead of
#' throwing if the handle is already closed.
#'
#' @param con A `pool::dbPool`, `DBIConnection`, or `NULL`.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' close_db_pool(pool_con)
#' }
#' @export
close_db_pool <- function(con) {
  if (is.null(con)) return(invisible(NULL))

  tryCatch({
    if (is_pool_connection(con)) {
      if (requireNamespace("pool", quietly = TRUE)) {
        pool::poolClose(con)
        message("[DB-Manager] Connection pool closed successfully.")
      }
    } else {
      dbDisconnect(con)
      message("[DB-Manager] Database connection disconnected successfully.")
    }
  }, error = function(e) {
    warning(paste("[DB-Manager] Failed to close database connection:", e$message))
  })
}

#' Safely Close a Database Connection (Backward-Compatible Alias)
#'
#' Backward-compatible alias for [close_db_pool()].
#'
#' @param con A `pool::dbPool`, `DBIConnection`, or `NULL`.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' close_db_connection(con)
#' }
#' @export
close_db_connection <- function(con) {
  close_db_pool(con)
}

#' Execute Database Operations Atomically Inside a Transaction
#'
#' Runs an R expression block or a `function(tx_con)` callback atomically within
#' a database transaction. Supports both direct `DBIConnection` objects and
#' `pool::dbPool` handles (checking out a dedicated connection for the duration
#' of the transaction). Any error triggers an automatic rollback and is
#' re-thrown to guarantee data integrity.
#'
#' @param con A `DBIConnection` or `pool::dbPool` object.
#' @param expr An R expression block or a function accepting the active
#'   transaction connection `tx_con`.
#'
#' @return The value returned by evaluating `expr`.
#' @examples
#' \dontrun{
#' with_db_transaction(con, function(tx_con) {
#'   DBI::dbExecute(tx_con, "SELECT 1")
#' })
#' }
#' @export
with_db_transaction <- function(con, expr) {
  if (is.null(con)) stop("[Transaction] Invalid database connection (NULL).")

  expr_sub <- substitute(expr)
  caller_env <- parent.frame()

  # Case A: pool::dbPool connection
  if (is_pool_connection(con)) {
    if (!requireNamespace("pool", quietly = TRUE)) { # nocov start
      stop("[Transaction] Package 'pool' is required but not available.")
    } # nocov end

    # Check out a dedicated connection from the pool for the transaction scope
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)

    # Execute transaction on the checked-out connection
    dbBegin(raw_con)
    res <- tryCatch({
      val <- eval(expr_sub, envir = caller_env)
      if (is.function(val)) {
        val <- val(raw_con)
      }
      dbCommit(raw_con)
      val
    }, error = function(e) {
      try(dbRollback(raw_con), silent = TRUE)
      stop(e)
    })
    return(res)
  }

  # Case B: Direct DBI connection
  dbBegin(con)
  res <- tryCatch({
    val <- eval(expr_sub, envir = caller_env)
    if (is.function(val)) {
      val <- val(con)
    }
    dbCommit(con)
    val
  }, error = function(e) {
    try(dbRollback(con), silent = TRUE)
    stop(e)
  })
  return(res)
}
