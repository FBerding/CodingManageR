#' @title Database Operations and Permission Utilities for Coding Portal
#' @description Provides modular helper functions for user lookup, permission checks,
#' case sampling, and database dialect compatibility (SQLite and MySQL/MariaDB).
#' Designed for robust Shiny multi-user server backends.
#' @author AI Studio Coding Engine
#' @keywords database rshiny permissions
NULL

# DBI is imported via NAMESPACE

#' Generate a Secure Random Alphanumeric Token
#'
#' @description Generates a random alphanumeric string suitable for user login tokens
#' or temporary security keys.
#'
#' @param length An integer specifying the number of characters in the generated token.
#' Default is 12. Must be greater than 0.
#'
#' @return A character string of the requested length containing random letters and digits.
#'
#' @examples
#' \dontrun{
#' token <- generate_security_token(16)
#' }
#' @export
generate_security_token <- function(length = 12) {
  if (!is.numeric(length) || length <= 0) {
    stop("Parameter 'length' must be a positive integer.")
  }
  chars <- c(letters, LETTERS, as.character(0:9))
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

#' Determine SQL Random Order Expression
#'
#' @description Returns the SQL function for random row ordering depending on whether
#' the connection uses MySQL/MariaDB (\code{RAND()}) or SQLite (\code{RANDOM()}).
#'
#' @param con A DBIConnection object representing the active database connection.
#'
#' @return A character string, either \code{"RAND()"} for MySQL/MariaDB or \code{"RANDOM()"} for SQLite.
#'
#' @examples
#' \dontrun{
#' rand_expr <- get_db_random_function(con)
#' query <- paste0("SELECT * FROM texts ORDER BY ", rand_expr, " LIMIT 1")
#' }
#' @export
get_db_random_function <- function(con) {
  if (is.null(con)) return("RANDOM()")
  is_mysql <- inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
  if (is_mysql) "RAND()" else "RANDOM()"
}

#' Retrieve User Metadata and Identifier
#'
#' @description Resolves user identification and permissions given either a username
#' or an existing user_id. Handles reactive values and falls back to database lookup.
#'
#' @param con A DBIConnection object.
#' @param username A character string or reactive expression containing the username.
#' @param user_id An optional integer or reactive expression containing the user identifier.
#'
#' @return A named list containing \code{id} (integer user_id), \code{name} (character username),
#' and \code{permissions} (character role: \code{"admin"} or \code{"user"}).
#'
#' @export
get_user_info_db <- function(con, username, user_id = NULL) {
  u_name <- if (is.reactive(username)) username() else as.character(username)
  u_id <- if (!is.null(user_id)) {
    val <- if (is.reactive(user_id)) user_id() else user_id
    if (!is.null(val) && !is.na(val) && val != "") as.integer(val) else NULL
  } else {
    NULL
  }

  u_perm <- "user"

  if (is.null(u_id) || is.na(u_id)) {
    res <- tryCatch({
      dbGetQuery(con, "SELECT user_id, permissions FROM user_tokens WHERE username = ?", params = list(u_name))
    }, error = function(e) data.frame(user_id = integer(0), permissions = character(0)))

    if (nrow(res) > 0) {
      u_id <- as.integer(res$user_id[1])
      u_perm <- as.character(res$permissions[1])
    } else {
      u_id <- 1L
      u_perm <- if (identical(u_name, "admin")) "admin" else "user"
    }
  } else {
    res <- tryCatch({
      dbGetQuery(con, "SELECT username, permissions FROM user_tokens WHERE user_id = ?", params = list(u_id))
    }, error = function(e) data.frame(username = character(0), permissions = character(0)))

    if (nrow(res) > 0) {
      if (is.null(u_name) || is.na(u_name) || u_name == "") {
        u_name <- as.character(res$username[1])
      }
      u_perm <- as.character(res$permissions[1])
    }
  }

  list(id = as.integer(u_id), name = as.character(u_name), permissions = as.character(u_perm))
}

#' Retrieve Authorized Text Sources for a User
#'
#' @description Fetches all source IDs that the specified user is authorized to read for text cases.
#' Administrators receive all available sources.
#'
#' @param con A DBIConnection object.
#' @param user_id An integer representing the user's primary key in \code{user_tokens}.
#' @param is_admin Logical; if TRUE, returns all source IDs unconditionally.
#'
#' @return An integer vector of permitted source IDs.
#'
#' @export
get_user_permitted_sources_db <- function(con, user_id, is_admin = FALSE) {
  if (isTRUE(is_admin)) {
    all_res <- tryCatch({
      dbGetQuery(con, "SELECT source_id FROM sources ORDER BY source_id ASC")$source_id
    }, error = function(e) integer(0))
    return(as.integer(all_res))
  }

  if (is.null(user_id) || is.na(user_id)) return(integer(0))

  perm_res <- tryCatch({
    dbGetQuery(con, "SELECT source_id FROM user_source_permissions WHERE user_id = ?", params = list(as.integer(user_id)))$source_id
  }, error = function(e) integer(0))

  as.integer(perm_res)
}

#' Retrieve Authorized Scientific Coding Sources for a User
#'
#' @description Fetches all source IDs that the specified user is authorized to assign
#' as scientific coding references. Administrators receive all sources unconditionally.
#'
#' @param con A DBIConnection object.
#' @param user_id An integer representing the user's primary key in \code{user_tokens}.
#' @param is_admin Logical; if TRUE, returns all source IDs unconditionally.
#'
#' @return An integer vector of permitted scientific coding source IDs.
#'
#' @export
get_user_permitted_coding_sources_db <- function(con, user_id, is_admin = FALSE) {
  if (isTRUE(is_admin)) {
    all_res <- tryCatch({
      dbGetQuery(con, "SELECT source_id FROM sources ORDER BY source_id ASC")$source_id
    }, error = function(e) integer(0))
    return(as.integer(all_res))
  }

  if (is.null(user_id) || is.na(user_id)) return(integer(0))

  perm_res <- tryCatch({
    dbGetQuery(con, "SELECT source_id FROM user_coding_source_permissions WHERE user_id = ?", params = list(as.integer(user_id)))$source_id
  }, error = function(e) integer(0))

  as.integer(perm_res)
}

#' Count Unrated Cases for a Specific Scale and User
#'
#' @description Counts how many cases in the database have not yet been evaluated by
#' the current user or marked as gold standard for the given scale/category.
#'
#' @param con A DBIConnection object.
#' @param scale_identifier Character or integer specifying the scale_id or scale name.
#' @param user_id Integer representing the active user.
#' @param mode Character string; either \code{"alle"} (all permitted sources) or \code{"quelle"} (filtered by source_id).
#' @param source_id Optional integer filter when \code{mode = "quelle"}.
#' @param permitted_source_ids Integer vector of sources the user is allowed to access.
#'
#' @return An integer representing the count of unrated cases remaining.
#'
#' @export
count_unrated_cases_db <- function(con, scale_identifier, user_id, mode = "alle", source_id = NULL, permitted_source_ids = NULL) {
  if (is.null(con) || is.null(scale_identifier) || is.na(scale_identifier) || scale_identifier == "") {
    return(0L)
  }

  uid <- as.integer(user_id)

  base_sql <- "
    SELECT COUNT(*) AS anzahl
    FROM texts t
    WHERE t.text_id NOT IN (
      SELECT ur.text_id
      FROM user_ratings ur
      WHERE ur.kategorie = ?
        AND (ur.user_id = ? OR ur.typ = 'gold_standard')
    )
  "

  params <- list(as.character(scale_identifier), uid)

  if (identical(mode, "quelle") && !is.null(source_id) && !is.na(source_id)) {
    base_sql <- paste0(base_sql, " AND t.source_id = ? ")
    params <- c(params, list(as.integer(source_id)))
  } else if (!is.null(permitted_source_ids) && length(permitted_source_ids) > 0) {
    placeholders <- paste(rep("?", length(permitted_source_ids)), collapse = ",")
    base_sql <- paste0(base_sql, " AND (t.source_id IN (", placeholders, ") OR t.source_id IS NULL) ")
    params <- c(params, as.list(as.integer(permitted_source_ids)))
  }

  res <- tryCatch({
    dbGetQuery(con, base_sql, params = params)$anzahl
  }, error = function(e) 0L)

  if (length(res) > 0 && !is.na(res[1])) as.integer(res[1]) else 0L
}

#' Draw an Unrated Case for Active Coding
#'
#' @description Fetches a random case from \code{texts} that has not yet been coded
#' by the user or locked as a gold standard on the given scale.
#'
#' @param con A DBIConnection object.
#' @param scale_identifier Character or integer specifying the scale_id or scale name.
#' @param user_id Integer representing the active user.
#' @param mode Character string; either \code{"alle"} or \code{"quelle"}.
#' @param source_id Optional integer filter when \code{mode = "quelle"}.
#' @param permitted_source_ids Integer vector of sources the user is allowed to access.
#'
#' @return A data.frame with 1 row containing case details (\code{text_id}, \code{text},
#' \code{source_id}, \code{befund_text}), or an empty data.frame if no cases remain.
#'
#' @export
draw_unrated_case_db <- function(con, scale_identifier, user_id, mode = "alle", source_id = NULL, permitted_source_ids = NULL) {
  if (is.null(con) || is.null(scale_identifier) || is.na(scale_identifier) || scale_identifier == "") {
    return(data.frame())
  }

  rand_fn <- get_db_random_function(con)
  uid <- as.integer(user_id)

  base_sql <- paste0("
    SELECT t.text_id, t.text, t.source_id,
           CASE WHEN t.text IS NOT NULL AND t.text != '' THEN t.text ELSE t.befund_text END AS befund_text
    FROM texts t
    WHERE t.text_id NOT IN (
      SELECT ur.text_id
      FROM user_ratings ur
      WHERE ur.kategorie = ?
        AND (ur.user_id = ? OR ur.typ = 'gold_standard')
    )
  ")

  params <- list(as.character(scale_identifier), uid)

  if (identical(mode, "quelle") && !is.null(source_id) && !is.na(source_id)) {
    base_sql <- paste0(base_sql, " AND t.source_id = ? ")
    params <- c(params, list(as.integer(source_id)))
  } else if (!is.null(permitted_source_ids) && length(permitted_source_ids) > 0) {
    placeholders <- paste(rep("?", length(permitted_source_ids)), collapse = ",")
    base_sql <- paste0(base_sql, " AND (t.source_id IN (", placeholders, ") OR t.source_id IS NULL) ")
    params <- c(params, as.list(as.integer(permitted_source_ids)))
  }

  base_sql <- paste0(base_sql, " ORDER BY ", rand_fn, " LIMIT 1")

  tryCatch({
    dbGetQuery(con, base_sql, params = params)
  }, error = function(e) data.frame())
}

#' Search and Filter Submitted Case Codings Across Scales
#'
#' @description Queries \code{user_ratings} joined with \code{texts}, \code{scale_definitions},
#' \code{user_tokens}, and \code{sources} to allow administrators to locate specific case ratings
#' by scale, rating type (\code{kodierung}, \code{gold_standard}, \code{schulung}), case ID,
#' text excerpt, or coder username.
#'
#' @param con A DBIConnection object representing the active database connection.
#' @param scale_filter Character string specifying either \code{"all"} or a target
#' \code{scale_id} (or scale name/short label). Default is \code{"all"}.
#' @param type_filter Character string specifying either \code{"all"} or one of
#' \code{"kodierung"}, \code{"gold_standard"}, \code{"schulung"}. Default is \code{"all"}.
#' @param search_query Optional character string used to filter results across rating ID,
#' text ID, case text content, coder username, and scale labels. Default is \code{""}.
#'
#' @return A \code{data.frame} containing matching rating records ordered by \code{rating_id DESC},
#' including resolved scale metadata (\code{resolved_scale_id}, \code{scale_name},
#' \code{scale_short_label}, \code{scale_max_value}) and case context.
#'
#' @examples
#' \dontrun{
#' results <- search_case_codings_db(con, scale_filter = "1", type_filter = "kodierung", search_query = "101")
#' }
#' @export
search_case_codings_db <- function(con, scale_filter = "all", type_filter = "all", search_query = "") {
  if (is.null(con)) return(data.frame())

  is_mysql <- inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
  cast_type <- if (is_mysql) "CHAR" else "TEXT"

  rating_fields <- tryCatch(dbListFields(con, "user_ratings"), error = function(e) character(0))
  has_uid <- "user_id" %in% rating_fields
  user_join <- if (has_uid) {
    "LEFT JOIN user_tokens ut ON ur.user_id = ut.user_id"
  } else {
    "LEFT JOIN user_tokens ut ON ur.username = ut.username"
  }
  user_select <- if (has_uid) {
    "COALESCE(ut.username, CASE WHEN ur.typ = 'gold_standard' THEN 'gold_standard' ELSE CAST(ur.user_id AS CHAR) END) AS username"
  } else {
    "COALESCE(ur.username, 'unbekannt') AS username"
  }
  if (!is_mysql && has_uid) {
    user_select <- "COALESCE(ut.username, CASE WHEN ur.typ = 'gold_standard' THEN 'gold_standard' ELSE CAST(ur.user_id AS TEXT) END) AS username"
  }

  sql <- paste0("
    SELECT
      ur.rating_id,
      ur.text_id,
      ur.kategorie,
      ur.bewertung,
      ur.typ,
      ur.source_id AS coding_source_id,
      ur.zeitstempel,
      ", user_select, ",
      COALESCE(t.text, '') AS case_text,
      sd.scale_id AS resolved_scale_id,
      COALESCE(sd.name, ur.kategorie) AS scale_name,
      COALESCE(sd.short_label, ur.kategorie) AS scale_short_label,
      COALESCE(sd.max_value, 5) AS scale_max_value,
      ts.Kurzreferenz AS text_source_ref,
      cs.Kurzreferenz AS coding_source_ref
    FROM user_ratings ur
    LEFT JOIN texts t ON ur.text_id = t.text_id
    ", user_join, "
    LEFT JOIN scale_definitions sd ON (
      CAST(sd.scale_id AS ", cast_type, ") = CAST(ur.kategorie AS ", cast_type, ") OR
      sd.name = ur.kategorie OR
      sd.short_label = ur.kategorie
    )
    LEFT JOIN sources ts ON t.source_id = ts.source_id
    LEFT JOIN sources cs ON ur.source_id = cs.source_id
    ORDER BY ur.rating_id DESC
  ")

  df <- tryCatch({
    dbGetQuery(con, sql)
  }, error = function(e) data.frame())

  if (nrow(df) == 0) return(df)

  # Filter by scale
  if (!is.null(scale_filter) && scale_filter != "" && scale_filter != "all") {
    sf <- as.character(scale_filter)
    match_scale <- (as.character(df$kategorie) == sf) |
      (!is.na(df$resolved_scale_id) & as.character(df$resolved_scale_id) == sf) |
      (!is.na(df$scale_name) & as.character(df$scale_name) == sf) |
      (!is.na(df$scale_short_label) & as.character(df$scale_short_label) == sf)
    df <- df[match_scale, , drop = FALSE]
  }

  # Filter by rating type
  if (!is.null(type_filter) && type_filter != "" && type_filter != "all" && nrow(df) > 0) {
    df <- df[as.character(df$typ) == as.character(type_filter), , drop = FALSE]
  }

  # Filter by search query (Rating ID, Text ID, Case Text, Username, Scale)
  q_raw <- if (!is.null(search_query)) trimws(as.character(search_query)) else ""
  if (q_raw != "" && nrow(df) > 0) {
    q_lower <- tolower(q_raw)
    q_no_hash <- tolower(gsub("^#", "", q_raw))

    r_id_str <- as.character(df$rating_id)
    t_id_str <- as.character(df$text_id)
    txt_l <- tolower(ifelse(is.na(df$case_text), "", df$case_text))
    usr_l <- tolower(ifelse(is.na(df$username), "", df$username))
    scl_l <- tolower(paste(ifelse(is.na(df$scale_name), "", df$scale_name), ifelse(is.na(df$scale_short_label), "", df$scale_short_label)))

    hits <- grepl(q_no_hash, r_id_str, fixed = TRUE) |
      grepl(q_no_hash, t_id_str, fixed = TRUE) |
      grepl(q_lower, txt_l, fixed = TRUE) |
      grepl(q_lower, usr_l, fixed = TRUE) |
      grepl(q_lower, scl_l, fixed = TRUE)

    df <- df[hits, , drop = FALSE]
  }

  df
}

#' Retrieve Detailed Context for a Single Rating Record
#'
#' @description Fetches a single rating from \code{user_ratings} together with its full
#' case text and complete \code{scale_definitions} stage descriptions (\code{def_n_0..def_n_5})
#' to populate the administrator value-correction dialog.
#'
#' @param con A DBIConnection object.
#' @param rating_id Integer primary key identifying the target record in \code{user_ratings}.
#'
#' @return A 1-row \code{data.frame} containing rating, case, coder, and scale stage fields,
#' or an empty \code{data.frame} if the record cannot be found.
#'
#' @export
get_coding_detail_db <- function(con, rating_id) {
  if (is.null(con) || is.null(rating_id) || is.na(rating_id)) return(data.frame())

  is_mysql <- inherits(con, "MariaDBConnection") || inherits(con, "MySQLConnection")
  cast_type <- if (is_mysql) "CHAR" else "TEXT"

  rating_fields <- tryCatch(dbListFields(con, "user_ratings"), error = function(e) character(0))
  has_uid <- "user_id" %in% rating_fields
  user_join <- if (has_uid) {
    "LEFT JOIN user_tokens ut ON ur.user_id = ut.user_id"
  } else {
    "LEFT JOIN user_tokens ut ON ur.username = ut.username"
  }
  user_select <- if (has_uid) {
    paste0("COALESCE(ut.username, CASE WHEN ur.typ = 'gold_standard' THEN 'gold_standard' ELSE CAST(ur.user_id AS ", cast_type, ") END) AS username")
  } else {
    "COALESCE(ur.username, 'unbekannt') AS username"
  }

  sql <- paste0("
    SELECT
      ur.rating_id,
      ur.text_id,
      ur.kategorie,
      ur.bewertung,
      ur.typ,
      ur.zeitstempel,
      ", user_select, ",
      COALESCE(t.text, '') AS case_text,
      sd.scale_id AS resolved_scale_id,
      COALESCE(sd.name, ur.kategorie) AS scale_name,
      COALESCE(sd.short_label, ur.kategorie) AS scale_short_label,
      COALESCE(sd.max_value, 5) AS scale_max_value,
      COALESCE(sd.def_desc, '') AS scale_def_desc,
      sd.def_n_0, sd.def_n_1, sd.def_n_2, sd.def_n_3, sd.def_n_4, sd.def_n_5
    FROM user_ratings ur
    LEFT JOIN texts t ON ur.text_id = t.text_id
    ", user_join, "
    LEFT JOIN scale_definitions sd ON (
      CAST(sd.scale_id AS ", cast_type, ") = CAST(ur.kategorie AS ", cast_type, ") OR
      sd.name = ur.kategorie OR
      sd.short_label = ur.kategorie
    )
    WHERE ur.rating_id = ?
    LIMIT 1
  ")

  tryCatch({
    dbGetQuery(con, sql, params = list(as.integer(rating_id)))
  }, error = function(e) data.frame())
}

#' Update Only the Numeric Scale Rating Value for a Stored Coding
#'
#' @description Validates that the specified \code{rating_id} exists and that \code{new_value}
#' falls strictly within \code{0..max_value} of the associated scale definition, then updates
#' \code{user_ratings.bewertung} in place without altering coder, type, case, or source metadata.
#'
#' @param con A DBIConnection object.
#' @param rating_id Integer primary key of the record in \code{user_ratings}.
#' @param new_value Numeric or integer representing the corrected score on the scale.
#'
#' @return A named list with \code{success} (logical), \code{old_value} (integer),
#' \code{new_value} (integer), \code{text_id} (integer), and \code{message} (character).
#'
#' @examples
#' \dontrun{
#' res <- update_coding_rating_value_db(con, rating_id = 201, new_value = 2)
#' }
#' @export
update_coding_rating_value_db <- function(con, rating_id, new_value) {
  if (is.null(con) || is.null(rating_id) || is.na(rating_id)) {
    return(list(success = FALSE, message = "Invalid database connection or rating_id."))
  }

  detail <- get_coding_detail_db(con, rating_id)
  if (nrow(detail) == 0) {
    return(list(success = FALSE, message = paste0("Rating #", rating_id, " not found.")))
  }

  val_int <- suppressWarnings(as.integer(new_value))
  max_v <- as.integer(detail$scale_max_value[1])
  if (is.na(val_int) || val_int < 0L || val_int > max_v) {
    return(list(
      success = FALSE,
      message = paste0("Value must be an integer between 0 and ", max_v, ".")
    ))
  }

  old_v <- as.integer(detail$bewertung[1])
  t_id  <- as.integer(detail$text_id[1])

  tryCatch({
    dbExecute(
      con,
      "UPDATE user_ratings SET bewertung = ? WHERE rating_id = ?",
      params = list(val_int, as.integer(rating_id))
    )
    list(
      success = TRUE,
      rating_id = as.integer(rating_id),
      text_id = t_id,
      old_value = old_v,
      new_value = val_int,
      message = paste0("Rating #", rating_id, " updated from ", old_v, " to ", val_int, ".")
    )
  }, error = function(e) {
    list(success = FALSE, message = paste0("Database update failed: ", e$message))
  })
}

