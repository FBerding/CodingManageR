#' @title Internationalization (i18n) and Translation Utilities for Coding Portal
#' @description Provides JSON-based localization loaded from \code{inst/translations.json},
#' automatic browser locale detection with English fallback, formatted string lookup via
#' \code{tr()}, and a Shiny language selector widget.
#' @author AI Studio Coding Engine
#' @keywords i18n translation shiny localization
NULL

# Internal package/module cache for parsed translations
.i18n_cache <- new.env(parent = emptyenv())
.i18n_cache$translations <- NULL
.i18n_cache$mtime <- NULL

#' Get Supported Language Codes
#'
#' @description Returns the character vector of supported ISO 639-1 language codes.
#'
#' @return A character vector containing supported language codes (\code{c("de", "en")}).
#'
#' @examples
#' get_supported_languages()
#' @export
get_supported_languages <- function() {
  c("de", "en")
}

#' Get Labeled Language Choices for Shiny Inputs
#'
#' @description Returns a named character vector suitable for \code{shiny::selectInput}.
#'
#' @return A named character vector mapping display labels to language codes.
#'
#' @examples
#' get_language_choices()
#' @export
get_language_choices <- function() {
  c("\U0001F310 DE" = "de", "\U0001F310 EN" = "en")
}

#' Locate the Canonical \code{translations.json} File
#'
#' @description Resolves the path to \code{inst/translations.json} across standard R
#' project roots, \code{shiny::runApp("R")} working directories, installed R package
#' locations (\code{system.file}), and web preview fallbacks.
#'
#' @return Character scalar file path to \code{translations.json}, or \code{NULL} if not found.
#'
#' @examples
#' \dontrun{
#' path <- find_translations_file()
#' }
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
#' @description Reads and parses \code{inst/translations.json} into a nested R list,
#' caching the result in memory until the file's modification timestamp changes.
#' Falls back to a minimal built-in dictionary if the JSON file cannot be accessed.
#'
#' @param force_reload Logical; if \code{TRUE}, bypasses the cache and reloads from disk.
#'
#' @return A nested named \code{list} containing language sub-trees (e.g., \code{de}, \code{en}).
#'
#' @examples
#' \dontrun{
#' dict <- load_translations()
#' }
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
      NULL
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
#' @description Extracts the two-letter ISO 639-1 language prefix from a raw locale header
#' or \code{navigator.language} value (e.g., \code{"de-DE"} -> \code{"de"}) and validates it
#' against \code{supported}.
#'
#' @param raw_lang Character scalar from \code{navigator.language} or \code{HTTP_ACCEPT_LANGUAGE}.
#' @param supported Character vector of allowed language codes. Defaults to \code{get_supported_languages()}.
#' @param fallback Character scalar fallback language code returned if \code{raw_lang} is unsupported or empty. Defaults to \code{"en"}.
#'
#' @return A validated two-letter character language code (e.g., \code{"de"} or \code{"en"}).
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
#' @description Internal helper that traverses dot-separated key segments inside a parsed
#' translations dictionary for a single language code.
#'
#' @param translations Nested translations list returned by \code{\link{load_translations}}.
#' @param lang_code Two-letter language code (e.g., \code{"de"} or \code{"en"}).
#' @param key_segments Character vector of path segments split on \code{"."}.
#'
#' @return A character scalar if found, or \code{NULL} otherwise.
#' @keywords internal
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
#' @description Resolves a hierarchical translation key from \code{inst/translations.json}
#' for the requested language, falling back to \code{"en"}, then \code{"de"}, then \code{default}
#' or \code{[key]}. Supports optional \code{sprintf}-style positional arguments.
#'
#' @param key Dot-separated translation key (e.g., \code{"auth.login_title"}).
#' @param lang Target language code (\code{"de"} or \code{"en"}). Defaults to \code{"en"}.
#' @param ... Optional positional arguments passed to \code{sprintf()}.
#' @param default Optional fallback string used when the key is missing across all languages.
#'
#' @return Localized character string.
#'
#' @examples
#' \dontrun{
#' tr("auth.login_title", lang = "de")
#' tr("common.page_x_of_y", "en", 1, 5)
#' }
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
#' @description Builds a compact language dropdown for switching between supported UI
#' languages in the login card and application header bar. Uses an explicit DOM
#' \code{onchange} event (\code{selected_ui_lang}) instead of bidirectional
#' \code{selectInput} binding so re-renders inside \code{renderUI} never trigger
#' WebSocket echo loops on Shiny Server.
#'
#' @param id Logical identifier attribute (\code{data-lang-selector}) for the widget. Defaults to \code{"app_language"}.
#' @param current_lang Active language code (\code{"de"} or \code{"en"}). Defaults to \code{"de"}.
#' @param class Optional extra CSS classes appended to the wrapper element.
#'
#' @return A Shiny \code{tags$div} UI element containing the styled language selector.
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
