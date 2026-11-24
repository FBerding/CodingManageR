# Note: This file was generated with the assistance of AI.

# Internationalization (i18n) and translation utilities

#' Internal Cache Environment for Parsed Translations
#'
#' Package-internal environment storing the parsed translation catalog (`translations`)
#' and last observed modification timestamp (`mtime`) to avoid repeated disk reads.
#'
#' @keywords internal
#' @noRd
.i18n_cache <- new.env(parent = emptyenv())
.i18n_cache$translations <- NULL
.i18n_cache$mtime <- NULL

#' Get Supported Language Codes
#'
#' Returns the character vector of supported ISO 639-1 language codes.
#'
#' @return A character vector containing supported language codes (`c("de", "en")`).
#'
#' @examples
#' get_supported_languages()
#' @export
get_supported_languages <- function() {
  c("de", "en")
}

#' Get Labeled Language Choices for Shiny Inputs
#'
#' Returns a named character vector suitable for `shiny::selectInput()` or
#' [language_selector_ui()].
#'
#' @return A named character vector mapping display labels to language codes.
#'
#' @examples
#' get_language_choices()
#' @export
get_language_choices <- function() {
  c("\U0001F310 DE" = "de", "\U0001F310 EN" = "en")
}

#' Locate the Canonical `translations.json` File
#'
#' Resolves the path to `inst/translations.json` across standard R project
#' roots, `shiny::runApp("R")` working directories, installed R package
#' locations (`system.file()`), and web preview fallbacks.
#'
#' @return Character scalar file path to `translations.json`, or `NULL` if not
#'   found.
#'
#' @examples
#' find_translations_file()
#' @export
find_translations_file <- function() {
  candidates <- c(
    file.path("inst", "translations.json"),
    file.path("..", "inst", "translations.json"),
    file.path("public", "translations.json"),
    "translations.json"
  )

  # Check installed package paths when bundled as an R package
  pkg_names <- unique(c(
    tryCatch(utils::packageName(), error = function(e) NULL),
    "CodingManageR",
    "CodingManager"
  ))
  for (pkg in pkg_names) {
    if (!is.null(pkg) && nzchar(pkg)) {
      sys_path <- tryCatch(system.file("translations.json", package = pkg), error = function(e) "")
      if (nzchar(sys_path) && file.exists(sys_path)) {
        return(sys_path)
      }
    }
  }

  for (path in candidates) {
    if (file.exists(path)) return(path)
  }
  return(NULL)
}

#' Load Translations Dictionary from JSON with Modification-Time Caching
#'
#' Reads and parses `inst/translations.json` into a nested R list, caching the
#' result in memory until the file's modification timestamp changes. Falls back
#' to a minimal built-in dictionary if the JSON file cannot be accessed.
#'
#' @param force_reload Logical; if `TRUE`, bypasses the cache and reloads from
#'   disk. Defaults to `FALSE`.
#'
#' @return A nested named `list` containing language sub-trees (e.g., `de`, `en`).
#'
#' @examples
#' dict <- load_translations()
#' names(dict)
#' @export
load_translations <- function(force_reload = FALSE) {
  path <- find_translations_file()
  if (is.null(path)) {
    return(list(
      de = list(
        common = list(refresh = "Ansicht aktualisieren", logout = "Abmelden", language = "Sprache"),
        auth = list(login_title = "Anmeldung", username = "Benutzername:", token = "Zugangs-Token:", login_btn = "Einloggen")
      ),
      en = list(
        common = list(refresh = "Refresh View", logout = "Log Out", language = "Language"),
        auth = list(login_title = "Sign In", username = "Username:", token = "Access Token:", login_btn = "Sign In")
      )
    ))
  }

  current_mtime <- file.info(path)$mtime
  if (!force_reload && !is.null(.i18n_cache$translations) && identical(.i18n_cache$mtime, current_mtime)) {
    return(.i18n_cache$translations)
  }

  parsed <- tryCatch({
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::fromJSON(path, simplifyVector = FALSE)
    } else {
      NULL # nocov
    }
  }, error = function(e) {
    warning("Failed to read translations.json: ", e$message)
    NULL
  })

  if (!is.null(parsed)) {
    .i18n_cache$translations <- parsed
    .i18n_cache$mtime <- current_mtime
    return(parsed)
  }

  if (!is.null(.i18n_cache$translations)) {
    return(.i18n_cache$translations)
  }

  list()
}

#' Normalize and Validate a Browser Locale String
#'
#' Extracts the two-letter ISO 639-1 language prefix from a raw locale header or
#' `navigator.language` value (e.g., `"de-DE"` -> `"de"`) and validates it
#' against `supported`.
#'
#' @param raw_lang Character scalar from `navigator.language` or
#'   `HTTP_ACCEPT_LANGUAGE`.
#' @param supported Character vector of allowed language codes. Defaults to
#'   [get_supported_languages()].
#' @param fallback Character scalar fallback language code returned if `raw_lang`
#'   is unsupported or empty. Defaults to `"en"`.
#'
#' @return A validated two-letter character language code (e.g., `"de"` or `"en"`).
#'
#' @examples
#' detect_browser_language("de-DE")
#' detect_browser_language("fr-FR", fallback = "en")
#' @export
detect_browser_language <- function(raw_lang = NULL, supported = get_supported_languages(), fallback = "en") {
  if (is.null(raw_lang) || length(raw_lang) == 0 || !nzchar(trimws(as.character(raw_lang[1])))) {
    return(fallback)
  }

  clean <- tolower(trimws(as.character(raw_lang[1])))
  clean <- gsub("[^a-z].*$", "", clean)

  if (nchar(clean) > 2) {
    clean <- substr(clean, 1, 2)
  }

  if (clean %in% supported) {
    return(clean)
  }

  return(fallback)
}

#' Lookup a Nested Translation Key Inside a Language Tree
#'
#' Internal helper that traverses dot-separated key segments inside a parsed
#' translations dictionary for a single language code.
#'
#' @param translations Nested translations list returned by [load_translations()].
#' @param lang_code Two-letter language code (e.g., `"de"` or `"en"`).
#' @param key_segments Character vector of path segments split on `"."`.
#'
#' @return A character scalar if found, or `NULL` otherwise.
#' @keywords internal
#' @noRd
resolve_translation_path <- function(translations, lang_code, key_segments) {
  if (is.null(translations[[lang_code]])) return(NULL)
  cur <- translations[[lang_code]]
  for (seg in key_segments) {
    if (is.list(cur) && !is.null(cur[[seg]])) {
      cur <- cur[[seg]]
    } else {
      return(NULL)
    }
  }
  if (is.character(cur) && length(cur) == 1) return(cur)
  return(NULL)
}

#' Translate a Dot-Separated UI Key with Fallback and Formatting
#'
#' Resolves a hierarchical translation key from `inst/translations.json` for the
#' requested language, falling back to `"en"`, then `"de"`, then `default` or
#' `[key]`. Supports optional `sprintf()` positional arguments.
#'
#' @param key Dot-separated translation key (e.g., `"auth.login_title"`).
#' @param lang Target language code (`"de"` or `"en"`). Defaults to `"en"`.
#' @param ... Optional positional arguments passed to `sprintf()`.
#' @param default Optional fallback string used when the key is missing across
#'   all languages.
#'
#' @return A localized character string.
#'
#' @examples
#' tr("auth.login_title", lang = "en")
#' @export
tr <- function(key, lang = "en", ..., default = NULL) {
  if (is.null(key) || !nzchar(key)) return("")

  lang <- if (is.null(lang) || !nzchar(lang)) "en" else tolower(as.character(lang))
  translations <- load_translations()
  key_segments <- unlist(strsplit(key, "\\."))

  res <- resolve_translation_path(translations, lang, key_segments)

  if (is.null(res) && lang != "en") {
    res <- resolve_translation_path(translations, "en", key_segments)
  }

  if (is.null(res) && lang != "de") {
    res <- resolve_translation_path(translations, "de", key_segments)
  }

  if (is.null(res)) {
    res <- if (!is.null(default)) default else paste0("[", key, "]")
  }

  args <- list(...)
  if (length(args) > 0) {
    res <- tryCatch(
      do.call(sprintf, c(list(res), args)),
      error = function(e) res
    )
  }

  return(res)
}

#' Render Header Language Selector Dropdown Widget
#'
#' Builds a compact language dropdown for switching between supported UI
#' languages in the login card and application header bar. Uses an explicit DOM
#' `onchange` event (`selected_ui_lang`) instead of bidirectional `selectInput()`
#' binding so re-renders inside `renderUI()` never trigger WebSocket echo loops
#' on Shiny Server.
#'
#' @param id Logical identifier attribute (`data-lang-selector`) for the widget.
#'   Defaults to `"app_language"`.
#' @param current_lang Active language code (`"de"` or `"en"`). Defaults to `"de"`.
#' @param class Optional extra CSS classes appended to the wrapper element.
#'
#' @return A Shiny `tags$div` UI element containing the styled language selector.
#'
#' @examples
#' \dontrun{
#' language_selector_ui("app_language", current_lang = "en")
#' }
#' @export
language_selector_ui <- function(id = "app_language", current_lang = "de", class = "") {
  choices <- get_language_choices()
  current_lang <- detect_browser_language(current_lang, fallback = "en")

  option_tags <- lapply(seq_along(choices), function(i) {
    val <- unname(choices[i])
    lbl <- names(choices)[i]
    tags$option(
      value = val,
      selected = if (identical(val, current_lang)) "selected" else NULL,
      lbl
    )
  })

  select_tag <- do.call(
    tags$select,
    c(
      list(
        class = "form-select form-select-sm",
        style = "width: 105px; height: 31px; padding: 2px 24px 2px 8px; font-size: 0.8rem; font-weight: 600; background-color: #ffffff; border: 1px solid #ced4da; border-radius: 4px; cursor: pointer;",
        `data-lang-selector` = id,
        `aria-label` = tr("common.language", current_lang),
        onchange = "if (window.Shiny) { Shiny.setInputValue('selected_ui_lang', this.value, {priority: 'event'}); }"
      ),
      option_tags
    )
  )

  tags$div(
    class = paste("language-selector-wrapper d-inline-flex align-items-center", class),
    style = "margin: 0; vertical-align: middle;",
    select_tag
  )
}
