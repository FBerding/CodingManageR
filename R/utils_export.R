#' @title Data Export, Schema Validation, and Download Utilities
#' @description Encapsulates schema inspection, column sanitization, CSV export generation,
#' and consolidated master-data export queries for the coding portal.
#' @author AI Studio Coding Engine
#' @keywords export schema csv data-validation
NULL

# DBI is imported via NAMESPACE

#' Sanitize and Clean Data Frame Column Names
#'
#' @description Standardizes imported or uploaded column names by trimming whitespace,
#' removing byte-order marks (BOM), and stripping non-alphanumeric punctuation.
#'
#' @param df A \code{data.frame} whose column names should be cleaned.
#'
#' @return A \code{data.frame} with trimmed, standardized column names.
#'
#' @examples
#' \dontrun{
#' clean_df <- clean_table_columns(raw_uploaded_csv)
#' }
#' @export
clean_table_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  c_names <- colnames(df)
  c_names <- gsub("^\ufeff", "", c_names) # Strip UTF-8 BOM if present
  c_names <- trimws(c_names)
  colnames(df) <- c_names
  df
}

#' Retrieve Table Primary Key Column Name
#'
#' @description Returns the designated primary key column name for each of the core
#' relational tables within the coding platform.
#'
#' @param table_name A character string naming the database table.
#'
#' @return A character string with the primary key column name, or \code{NULL} if not recognized.
#'
#' @export
get_table_pk_definition <- function(table_name) {
  switch(as.character(table_name),
    "user_tokens"                   = "user_id",
    "sources"                       = "source_id",
    "texts"                         = "text_id",
    "user_ratings"                  = "rating_id",
    "coding_training"               = "training_id",
    "scale_definitions"             = "scale_id",
    "user_source_permissions"       = "source_id",
    "user_coding_source_permissions"= "source_id",
    "user_scale_permissions"        = "scale_id",
    NULL
  )
}

#' Validate Table Schema for Data Upload
#'
#' @description Validates whether an uploaded data frame possesses the mandatory columns
#' and conforming types required for insertion into a specified database table.
#'
#' @param target_table A character string naming the target database table.
#' @param df A \code{data.frame} containing the uploaded records.
#'
#' @return A named list with:
#' \describe{
#'   \item{is_valid}{Logical indicating if schema validation passed.}
#'   \item{missing_cols}{Character vector of required columns that are missing.}
#'   \item{error_msg}{Character message describing the validation failure, or \code{NULL}.}
#' }
#'
#' @export
validate_import_schema <- function(target_table, df, lang = "en") {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      is_valid = FALSE,
      missing_cols = character(0),
      error_msg = tr("admin.upload_err_empty_csv", lang)
    ))
  }

  required_cols <- switch(as.character(target_table),
    "user_tokens"       = c("username", "token", "permissions"),
    "sources"           = c("Kurzreferenz", "Vollstaendiges_Zitat"),
    "texts"             = c("text"),
    "scale_definitions" = c("short_label", "name", "max_value"),
    character(0)
  )

  df_clean <- clean_table_columns(df)
  actual_cols <- colnames(df_clean)
  missing <- setdiff(required_cols, actual_cols)

  if (length(missing) > 0) {
    return(list(
      is_valid = FALSE,
      missing_cols = missing,
      error_msg = tr("admin.upload_err_schema_mismatch", lang, target_table, paste(missing, collapse = ", "))
    ))
  }

  list(is_valid = TRUE, missing_cols = character(0), error_msg = NULL)
}

#' Generate CSV Content for a Database Table
#'
#' @description Queries all rows from the specified table in the active database
#' and serializes them into a UTF-8 CSV string with semicolon delimiters.
#'
#' @param con A DBIConnection object.
#' @param table_name A character string specifying the table name.
#'
#' @return A character string containing the serialized CSV content including headers.
#'
#' @export
export_table_to_csv <- function(con, table_name) {
  if (is.null(con) || is.null(table_name) || table_name == "") {
    return("")
  }

  safe_name <- gsub("[^a-zA-Z0-9_]", "", table_name)
  df <- tryCatch({
    dbGetQuery(con, paste0("SELECT * FROM ", safe_name))
  }, error = function(e) data.frame())

  if (nrow(df) == 0) {
    cols <- tryCatch({
      colnames(dbGetQuery(con, paste0("SELECT * FROM ", safe_name, " LIMIT 0")))
    }, error = function(e) character(0))
    return(paste(cols, collapse = ";"))
  }

  raw_conn <- rawConnection(raw(0), "w")
  on.exit(close(raw_conn), add = TRUE)
  write.table(df, file = raw_conn, sep = ";", dec = ",", row.names = FALSE, na = "", quote = TRUE)
  rawToChar(rawConnectionValue(raw_conn))
}

#' Generate Empty CSV Template for Table Upload
#'
#' @description Generates a header-only CSV template (with sample columns) to assist
#' administrators in preparing structured uploads.
#'
#' @param table_name A character string specifying the target table.
#'
#' @return A character string containing the semicolon-delimited header row.
#'
#' @export
export_table_template_csv <- function(table_name) {
  cols <- switch(as.character(table_name),
    "user_tokens"       = c("username", "token", "permissions", "is_active"),
    "sources"           = c("Kurzreferenz", "Vollstaendiges_Zitat", "Lizenz", "Lizenz_Details", "URL_DOI"),
    "texts"             = c("text", "source_id"),
    "scale_definitions" = c("short_label", "name", "def_desc", "max_value", "bsp_0", "bsp_1", "bsp_2", "bsp_3", "bsp_4"),
    character(0)
  )

  paste0(paste(cols, collapse = ";"), "\r\n")
}

#' Build and Execute Consolidated Master Data Export Query
#'
#' @description Queries and consolidates ratings, full-text contents, scale labels,
#' and scientific source references according to active administrator filters.
#'
#' @param con A DBIConnection object.
#' @param scale_ids An optional integer or character vector of selected scale IDs/names.
#' @param source_ids An optional integer vector of selected text source IDs.
#' @param user_ids An optional integer vector of selected user IDs.
#'
#' @return A data.frame containing consolidated research export columns.
#'
#' @export
fetch_consolidated_master_export <- function(con, scale_ids = NULL, source_ids = NULL, user_ids = NULL) {
  if (is.null(con)) return(data.frame())

  src_fields <- tryCatch(dbListFields(con, "sources"), error = function(e) character(0))
  cite_col <- if ("Vollstaendiges_Zitat" %in% src_fields) "Vollstaendiges_Zitat" else "Referenz"
  cast_type <- if (is_mysql_connection(con)) "CHAR" else "TEXT"

  base_query <- paste0("
    SELECT
      ur.rating_id,
      ur.zeitstempel,
      ur.typ,
      ur.bewertung,
      ut.username,
      ut.permissions AS user_role,
      t.text_id,
      t.text AS falltext,
      COALESCE(sd.name, ur.kategorie) AS skala_name,
      COALESCE(sd.short_label, ur.kategorie) AS skala_kuerzel,
      COALESCE(sd.max_value, 2) AS skala_max_wert,
      ts.Kurzreferenz AS text_quelle_kuerzel,
      ts.", cite_col, " AS text_quelle_zitat,
      cs.Kurzreferenz AS kodier_quelle_kuerzel,
      cs.", cite_col, " AS kodier_quelle_zitat
    FROM user_ratings ur
    LEFT JOIN user_tokens ut ON ur.user_id = ut.user_id
    LEFT JOIN texts t ON ur.text_id = t.text_id
    LEFT JOIN scale_definitions sd ON (
      CAST(sd.scale_id AS ", cast_type, ") = CAST(ur.kategorie AS ", cast_type, ") OR
      sd.name = ur.kategorie OR
      sd.short_label = ur.kategorie
    )
    LEFT JOIN sources ts ON t.source_id = ts.source_id
    LEFT JOIN sources cs ON ur.source_id = cs.source_id
    WHERE 1=1
  ")

  params <- list()

  if (!is.null(scale_ids) && length(scale_ids) > 0) {
    placeholders <- paste(rep("?", length(scale_ids)), collapse = ",")
    base_query <- paste0(base_query, " AND (ur.kategorie IN (", placeholders, ") OR sd.scale_id IN (", placeholders, "))")
    params <- c(params, as.list(as.character(scale_ids)), as.list(as.character(scale_ids)))
  }

  if (!is.null(source_ids) && length(source_ids) > 0) {
    placeholders <- paste(rep("?", length(source_ids)), collapse = ",")
    base_query <- paste0(base_query, " AND (t.source_id IN (", placeholders, ") OR ur.source_id IN (", placeholders, "))")
    params <- c(params, as.list(as.integer(source_ids)), as.list(as.integer(source_ids)))
  }

  if (!is.null(user_ids) && length(user_ids) > 0) {
    placeholders <- paste(rep("?", length(user_ids)), collapse = ",")
    base_query <- paste0(base_query, " AND ur.user_id IN (", placeholders, ")")
    params <- c(params, as.list(as.integer(user_ids)))
  }

  base_query <- paste0(base_query, " ORDER BY ur.rating_id ASC")

  tryCatch({
    dbGetQuery(con, base_query, params = params)
  }, error = function(e) {
    message("Error executing master export: ", e$message)
    data.frame()
  })
}
