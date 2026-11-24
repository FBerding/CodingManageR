# Note: This file was generated with the assistance of AI.

# ==============================================================================
# ADMIN MODULE SERVER HELPERS: USER MANAGEMENT & PERMISSIONS (TAB 1)
# ==============================================================================
# Internal subtab server registrars extracted from adminServer() to keep each
# function compact and avoid quadratic AST instrumentation overhead under covr.
# ==============================================================================

#' Check Whether a User Role Has Protected (Hidden) Credentials
#'
#' @param perm Character role value from `user_tokens.permissions`.
#' @return Logical scalar; `TRUE` for `"admin"` and `"system"` roles.
#' @keywords internal
#' @noRd
is_protected_token_role <- function(perm) {
  !is.null(perm) && !is.na(perm) && tolower(trimws(as.character(perm))) %in% c("admin", "system")
}

#' Register Admin Tab 1 User Table, Search, Toggle, and Source Permissions
#'
#' Registers reactive outputs and observers for the paginated user table,
#' one-click active/inactive status toggle, and source permission modal inside
#' the admin module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_users_table_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  get_lang <- ctx$get_lang
  aktiver_admin <- ctx$aktiver_admin
  aktiver_admin_id <- ctx$aktiver_admin_id
  user_table_trigger <- ctx$user_table_trigger
  current_page_users <- ctx$current_page_users
  target_user_for_sources <- reactiveVal(NULL)

  page_size_users    <- 25

  # Reactive filtering of users by ID, username, role, or active status
  filtered_users_data <- reactive({
    user_table_trigger()

    users_df <- tryCatch({
      dbGetQuery(
        db_con,
        "SELECT user_id, username, CASE WHEN LOWER(COALESCE(permissions, '')) IN ('admin', 'system') THEN NULL ELSE token END AS token, permissions, is_active FROM user_tokens ORDER BY permissions ASC, user_id ASC"
      )
    }, error = function(e) {
      data.frame(user_id = integer(0), username = character(0), token = character(0), permissions = character(0), is_active = integer(0))
    })

    suchtext <- trimws(input$user_search_query)
    if (!is.null(suchtext) && suchtext != "" && nrow(users_df) > 0) {
      muster <- tolower(suchtext)
      u_name <- tolower(ifelse(is.na(users_df$username), "", users_df$username))
      u_id_str <- paste0("#", users_df$user_id, " ", users_df$user_id)
      u_perm <- tolower(ifelse(is.na(users_df$permissions), "", users_df$permissions))
      status_text <- ifelse(as.integer(users_df$is_active) == 1, "aktiv active enabled freigeschaltet", "inaktiv inactive disabled locked gesperrt")
      perm_display <- ifelse(users_df$permissions == "admin", "administrator admin", "kodierer coder user")

      treffer <- grepl(muster, u_name, fixed = TRUE) |
                 grepl(muster, u_id_str, fixed = TRUE) |
                 grepl(muster, u_perm, fixed = TRUE) |
                 grepl(muster, status_text, fixed = TRUE) |
                 grepl(muster, perm_display, fixed = TRUE)
      users_df <- users_df[treffer, , drop = FALSE]
    }

    users_df
  })

  # Reset pagination to page 1 when the search query changes
  observeEvent(input$user_search_query, {
    current_page_users(1)
  })

  # Compute total number of available pages
  total_pages_users <- reactive({
    df <- filtered_users_data()
    max(1, ceiling(nrow(df) / page_size_users))
  })

  # Counter badge (total vs. filtered)
  output$users_counter_badge <- renderUI({
    lang <- get_lang()
    df <- filtered_users_data()
    total_count <- tryCatch({
      dbGetQuery(db_con, "SELECT COUNT(*) AS total FROM user_tokens")$total
    }, error = function(e) nrow(df))

    if (nrow(df) == total_count) {
      tr("admin.users.counter_all", lang, as.integer(total_count))
    } else {
      tr("admin.users.counter_filtered", lang, nrow(df), as.integer(total_count))
    }
  })

  # Paging and pagination bar for user management
  output$users_pagination_controls <- renderUI({
    lang <- get_lang()
    df <- filtered_users_data()
    total_rows <- nrow(df)
    if (total_rows == 0) return(NULL)

    tot_pages <- total_pages_users()
    cur_page <- min(max(1, current_page_users()), tot_pages)

    start_idx <- (cur_page - 1) * page_size_users + 1
    end_idx <- min(total_rows, cur_page * page_size_users)

    div(
      class = "d-flex flex-wrap justify-content-between align-items-center gap-3 p-3 bg-light rounded border",
      div(
        class = "text-muted small",
        tags$strong(tr("admin.users.pagination_showing", lang, start_idx, end_idx, total_rows)),
        paste0(" (", tr("common.page_x_of_y", lang, cur_page, tot_pages), ")")
      ),
      div(
        class = "d-flex align-items-center gap-2",
        actionButton(ns("btn_users_first"), paste0("\u00ab ", tr("common.first", lang)), class = "btn-sm btn-outline-secondary", disabled = if (cur_page <= 1) "disabled" else NULL),
        actionButton(ns("btn_users_prev"), paste0("\u2039 ", tr("common.previous", lang)), class = "btn-sm btn-outline-primary", disabled = if (cur_page <= 1) "disabled" else NULL),
        div(
          class = "d-flex align-items-center gap-1",
          span(class = "small text-muted", tr("common.page_label", lang)),
          tags$input(
            id = ns("input_users_jump_page"),
            type = "number",
            class = "form-control form-control-sm text-center",
            style = "width: 65px; display: inline-block;",
            value = cur_page,
            min = 1,
            max = tot_pages,
            onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", ns("trigger_jump_users_page"))
          ),
          span(class = "small text-muted", paste0("/ ", tot_pages))
        ),
        actionButton(ns("btn_users_next"), paste0(tr("common.next", lang), " \u203a"), class = "btn-sm btn-outline-primary", disabled = if (cur_page >= tot_pages) "disabled" else NULL),
        actionButton(ns("btn_users_last"), paste0(tr("common.last", lang), " \u00bb"), class = "btn-sm btn-outline-secondary", disabled = if (cur_page >= tot_pages) "disabled" else NULL)
      )
    )
  })

  # Paging event handlers for user page navigation
  observeEvent(input$btn_users_first, {
    current_page_users(1)
  })

  observeEvent(input$btn_users_prev, {
    cur <- current_page_users()
    if (cur > 1) current_page_users(cur - 1)
  })

  observeEvent(input$btn_users_next, {
    cur <- current_page_users()
    tot <- total_pages_users()
    if (cur < tot) current_page_users(cur + 1)
  })

  observeEvent(input$btn_users_last, {
    tot <- total_pages_users()
    current_page_users(tot)
  })

  observeEvent(input$trigger_jump_users_page, {
    ziel <- as.integer(input$trigger_jump_users_page)
    tot <- total_pages_users()
    if (!is.na(ziel)) {
      sichere_seite <- max(1, min(tot, ziel))
      current_page_users(sichere_seite)
    }
  })

  output$users_table_container <- renderUI({
    lang <- get_lang()
    users_df <- filtered_users_data()

    if (nrow(users_df) == 0) {
      return(div(class = "alert alert-warning text-center py-4",
                 icon("info-circle", class = "me-2"),
                 tr("admin.users.empty", lang)))
    }

    tot_pages <- total_pages_users()
    cur_page <- min(max(1, current_page_users()), tot_pages)

    start_idx <- (cur_page - 1) * page_size_users + 1
    end_idx <- min(nrow(users_df), cur_page * page_size_users)

    page_df <- users_df[start_idx:end_idx, , drop = FALSE]

    logged_in <- aktiver_admin()
    logged_in_id <- aktiver_admin_id()
    all_sources_df <- tryCatch({ dbGetQuery(db_con, "SELECT source_id, Kurzreferenz FROM sources ORDER BY source_id ASC") }, error = function(e) data.frame())
    total_sources_count <- nrow(all_sources_df)
    all_scales_df <- tryCatch({ dbGetQuery(db_con, "SELECT scale_id, short_label FROM scale_definitions ORDER BY scale_id ASC") }, error = function(e) data.frame())
    total_scales_count <- nrow(all_scales_df)

    rows <- lapply(1:nrow(page_df), function(i) {
      u_id     <- as.integer(page_df$user_id[i])
      u_name   <- page_df$username[i]
      u_tok    <- page_df$token[i]
      u_perm   <- page_df$permissions[i]
      u_active <- as.integer(page_df$is_active[i])
      is_self  <- (!is.null(logged_in_id) && u_id == logged_in_id) || (u_name == logged_in)

      safe_id <- paste0("user_", u_id)

      role_badge <- if (u_perm == "admin") {
        tags$span(class = "badge bg-dark", style = "font-size: 0.85em; padding: 5px 9px;", paste0("\U0001f451 ", tr("common.role_admin", lang)))
      } else {
        tags$span(class = "badge bg-info text-dark", style = "font-size: 0.85em; padding: 5px 9px;", paste0("\U0001f464 ", tr("common.role_coder", lang)))
      }

      status_widget <- if (is_self) {
        tags$span(
          class = "badge bg-success",
          style = "font-size: 0.85em; padding: 5px 9px; cursor: default;",
          icon("shield-alt", class = "me-1"),
          tr("admin.users.badge_self_active", lang)
        )
      } else if (u_active == 1) {
        actionButton(
          inputId = ns(paste0("btn_toggle_active_", safe_id)),
          label = tr("admin.users.status_active_btn_short", lang),
          icon = icon("check-circle"),
          class = "btn-sm btn-outline-success",
          style = "font-weight: 500; min-width: 90px;",
          title = tr("admin.users.status_active_tip", lang),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_toggle_user_status"), u_id)
        )
      } else {
        actionButton(
          inputId = ns(paste0("btn_toggle_inactive_", safe_id)),
          label = tr("admin.users.status_inactive_btn_short", lang),
          icon = icon("ban"),
          class = "btn-sm btn-outline-danger",
          style = "font-weight: 500; min-width: 90px; background-color: #fff5f5;",
          title = tr("admin.users.status_inactive_tip", lang),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_toggle_user_status"), u_id)
        )
      }

      sources_widget <- if (u_perm == "admin") {
        tags$span(class = "badge bg-secondary", style = "font-size: 0.8em;", icon("globe", class = "me-1"), tr("admin.users.full_access_admin", lang))
      } else {
        user_text_sources <- tryCatch({
          dbGetQuery(db_con, "SELECT source_id FROM user_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
        }, error = function(e) integer(0))
        user_coding_sources <- tryCatch({
          dbGetQuery(db_con, "SELECT source_id FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
        }, error = function(e) integer(0))

        n_text <- length(user_text_sources)
        n_code <- length(user_coding_sources)

        btn_label <- tr("admin.users.btn_sources_label", lang, n_text, total_sources_count, n_code, total_sources_count)
        is_locked <- (n_text == 0 && n_code == 0)

        actionButton(
          inputId = ns(paste0("btn_sources_", safe_id)),
          label = btn_label,
          icon = icon("book"),
          class = if (is_locked) "btn-sm btn-outline-danger" else "btn-sm btn-outline-secondary",
          style = "font-size: 0.8em;",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_manage_user_sources"), u_id)
        )
      }

      scales_widget <- if (u_perm == "admin") {
        tags$span(class = "badge bg-secondary", style = "font-size: 0.8em;", icon("sliders-h", class = "me-1"), tr("admin.users.full_access_admin", lang))
      } else {
        user_scales <- tryCatch({
          dbGetQuery(db_con, "SELECT scale_id FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))$scale_id
        }, error = function(e) integer(0))

        n_scales <- length(user_scales)
        btn_label_scales <- tr("admin.users.btn_scales_label", lang, n_scales, total_scales_count)
        is_scales_locked <- (n_scales == 0)

        actionButton(
          inputId = ns(paste0("btn_scales_", safe_id)),
          label = btn_label_scales,
          icon = icon("sliders-h"),
          class = if (is_scales_locked) "btn-sm btn-outline-danger" else "btn-sm btn-outline-primary",
          style = "font-size: 0.8em;",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_manage_user_scales"), u_id)
        )
      }

      edit_btn <- actionButton(
        inputId = ns(paste0("btn_edit_", safe_id)),
        label = tr("common.edit", lang),
        icon = icon("edit"),
        class = "btn-sm btn-outline-primary me-1",
        onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_edit_user"), u_id)
      )

      delete_btn <- if (is_self) {
        tags$button(class = "btn btn-sm btn-outline-secondary", disabled = "disabled", title = tr("admin.users.self_protect_tip", lang), icon("trash"))
      } else {
        actionButton(
          inputId = ns(paste0("btn_del_", safe_id)),
          label = tr("common.delete", lang),
          icon = icon("trash"),
          class = "btn-sm btn-outline-danger",
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_delete_user"), u_id)
        )
      }

      token_cell <- if (is_protected_token_role(u_perm) || is.na(u_tok)) {
        tags$span(
          class = "text-muted small",
          title = tr("admin.users.token_hidden_tip", lang),
          icon("lock", class = "me-1"),
          "\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022"
        )
      } else {
        tags$code(u_tok)
      }

      tags$tr(
        class = if (u_active == 0) "table-light text-muted" else "",
        tags$td(
          style = "vertical-align: middle;",
          tags$span(class = "badge bg-light text-secondary border font-monospace", paste0("#", u_id))
        ),
        tags$td(
          style = "vertical-align: middle;",
          tags$strong(u_name),
          if (is_self) tags$span(class = "badge bg-secondary ms-1", tr("admin.users.badge_currently_logged_in", lang))
        ),
        tags$td(style = "vertical-align: middle; font-family: monospace;", token_cell),
        tags$td(style = "vertical-align: middle;", role_badge),
        tags$td(style = "vertical-align: middle;", status_widget),
        tags$td(style = "vertical-align: middle;", sources_widget),
        tags$td(style = "vertical-align: middle;", scales_widget),
        tags$td(style = "vertical-align: middle; text-align: right;", div(edit_btn, delete_btn))
      )
    })

    tags$div(
      class = "table-responsive",
      tags$table(
        class = "table table-hover align-middle mb-0",
        tags$thead(
          class = "table-light",
          tags$tr(
            tags$th(style = "width: 70px;", tr("admin.users.col_id", lang)),
            tags$th(tr("admin.users.col_username", lang)),
            tags$th(tr("admin.users.col_token_header", lang)),
            tags$th(tr("admin.users.col_role", lang)),
            tags$th(tr("admin.users.col_status_login", lang)),
            tags$th(tr("admin.users.col_sources", lang)),
            tags$th(tr("admin.users.col_scales", lang)),
            tags$th(style = "text-align: right;", tr("common.actions", lang))
          )
        ),
        tags$tbody(rows)
      )
    )
  })

  # 1-click active status toggle by user_id
  observeEvent(input$trigger_toggle_user_status, {
    lang <- get_lang()
    u_target <- input$trigger_toggle_user_status
    req(u_target)

    user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
      dbGetQuery(db_con, "SELECT user_id, username, is_active FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
    } else {
      dbGetQuery(db_con, "SELECT user_id, username, is_active FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
    }

    if (nrow(user_row) == 0) return()
    u_id <- as.integer(user_row$user_id[1])
    u_name <- as.character(user_row$username[1])

    if (u_id == aktiver_admin_id() || u_name == aktiver_admin()) {
      showNotification(tr("admin.users.notif_self_deactivate_err", lang), type = "error")
      return()
    }

    current_status <- user_row$is_active[1]
    new_status <- if (as.integer(current_status) == 1) 0 else 1

    tryCatch({
      dbExecute(db_con, "UPDATE user_tokens SET is_active = ? WHERE user_id = ?", params = list(new_status, u_id))
      user_table_trigger(user_table_trigger() + 1)
      if (new_status == 1) {
        showNotification(tr("admin.users.notif_status_active", lang, u_name, u_id), type = "message")
      } else {
        showNotification(tr("admin.users.notif_status_inactive", lang, u_name, u_id), type = "warning")
      }
    }, error = function(e) {
      showNotification(tr("admin.users.notif_status_err", lang, e$message), type = "error")
    })
  })

  # Manage source permissions by user_id
  observeEvent(input$trigger_manage_user_sources, {
    lang <- get_lang()
    u_target <- input$trigger_manage_user_sources
    req(u_target)

    user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
      dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
    } else {
      dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
    }

    if (nrow(user_row) == 0) return()
    u_id <- as.integer(user_row$user_id[1])
    u_name <- as.character(user_row$username[1])

    target_user_for_sources(list(user_id = u_id, username = u_name))

    all_sources <- dbGetQuery(db_con, "SELECT source_id, Kurzreferenz, Referenz FROM sources ORDER BY source_id ASC")
    if (nrow(all_sources) == 0) {
      showNotification(tr("admin.users.notif_no_sources", lang), type = "warning")
      return()
    }

    assigned_text <- tryCatch({
      dbGetQuery(db_con, "SELECT source_id FROM user_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
    }, error = function(e) integer(0))

    assigned_coding <- tryCatch({
      dbGetQuery(db_con, "SELECT source_id FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))$source_id
    }, error = function(e) integer(0))

    choices_list <- setNames(
      as.character(all_sources$source_id),
      paste0("[#", all_sources$source_id, "] ", all_sources$Kurzreferenz, " - ", substr(all_sources$Referenz, 1, 65), "...")
    )

    showModal(modalDialog(
      title = div(icon("shield-alt", class = "me-2 text-primary"), tr("admin.permissions.sources_modal_title", lang, paste0(u_name, " [#", u_id, "]"))),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton(tr("common.cancel", lang)),
        actionButton(ns("btn_save_user_sources"), tr("admin.permissions.btn_save_permissions", lang), class = "btn-primary")
      ),
      div(
        p(class = "text-muted small", tr("admin.permissions.sources_modal_subtitle", lang)),

        tabsetPanel(
          id = ns("tabs_user_sources"),
          type = "tabs",

          tabPanel(
            title = tagList(icon("file-lines", class = "me-1 text-primary"), tr("admin.permissions.tab_text_sources", lang)),
            br(),
            div(class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded border",
                div(
                  tags$strong("Status: "),
                  tags$span(class = "text-muted small", paste0("(", length(assigned_text), " / ", nrow(all_sources), ")"))
                ),
                div(class = "d-flex gap-2",
                    actionButton(ns("btn_select_all_text_sources"), tr("admin.permissions.btn_select_all", lang),
                                 icon = icon("check-square"), class = "btn-sm btn-outline-primary"),
                    actionButton(ns("btn_deselect_all_text_sources"), tr("admin.permissions.btn_deselect_all", lang),
                                 icon = icon("square"), class = "btn-sm btn-outline-secondary")
                )
            ),

            div(style = "max-height: 340px; overflow-y: auto; padding: 12px; border: 1px solid #ced4da; border-radius: 4px; background-color: #ffffff;",
                checkboxGroupInput(ns("selected_user_text_sources"), label = NULL, choices = choices_list, selected = as.character(assigned_text))
            )
          ),

          tabPanel(
            title = tagList(icon("tag", class = "me-1 text-success"), tr("admin.permissions.tab_coding_sources", lang)),
            br(),
            div(class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded border",
                div(
                  tags$strong("Status: "),
                  tags$span(class = "text-muted small", paste0("(", length(assigned_coding), " / ", nrow(all_sources), ")"))
                ),
                div(class = "d-flex gap-2",
                    actionButton(ns("btn_select_all_coding_sources"), tr("admin.permissions.btn_select_all", lang),
                                 icon = icon("check-square"), class = "btn-sm btn-outline-success"),
                    actionButton(ns("btn_deselect_all_coding_sources"), tr("admin.permissions.btn_deselect_all", lang),
                                 icon = icon("square"), class = "btn-sm btn-outline-secondary")
                )
            ),

            div(style = "max-height: 340px; overflow-y: auto; padding: 12px; border: 1px solid #ced4da; border-radius: 4px; background-color: #ffffff;",
                checkboxGroupInput(ns("selected_user_coding_sources"), label = NULL, choices = choices_list, selected = as.character(assigned_coding))
            )
          )
        ),
        tags$input(type = "hidden", id = ns("target_sources_user_id"), value = as.character(u_id)),
        tags$input(type = "hidden", id = ns("target_sources_username"), value = u_name)
      )
    ))
  })

  # Quick selection for text sources
  observeEvent(input$btn_select_all_text_sources, {
    all_ids <- dbGetQuery(db_con, "SELECT source_id FROM sources ORDER BY source_id ASC")$source_id
    updateCheckboxGroupInput(session, "selected_user_text_sources", selected = as.character(all_ids))
  })

  observeEvent(input$btn_deselect_all_text_sources, {
    updateCheckboxGroupInput(session, "selected_user_text_sources", selected = character(0))
  })

  # Quick selection for coding sources
  observeEvent(input$btn_select_all_coding_sources, {
    all_ids <- dbGetQuery(db_con, "SELECT source_id FROM sources ORDER BY source_id ASC")$source_id
    updateCheckboxGroupInput(session, "selected_user_coding_sources", selected = as.character(all_ids))
  })

  observeEvent(input$btn_deselect_all_coding_sources, {
    updateCheckboxGroupInput(session, "selected_user_coding_sources", selected = character(0))
  })

  # Save both source permission types
  observeEvent(input$btn_save_user_sources, {
    lang <- get_lang()
    u_info <- target_user_for_sources()
    req(u_info)
    u_id <- as.integer(u_info$user_id)
    u_name <- as.character(u_info$username)

    selected_text_ids <- as.integer(input$selected_user_text_sources)
    selected_coding_ids <- as.integer(input$selected_user_coding_sources)

    tryCatch({
      with_db_transaction(db_con, function(tx_con) {
        dbExecute(tx_con, "DELETE FROM user_source_permissions WHERE user_id = ?", params = list(u_id))
        if (length(selected_text_ids) > 0) {
          for (sid in selected_text_ids) {
            dbExecute(tx_con, "INSERT INTO user_source_permissions (user_id, source_id) VALUES (?, ?)", params = list(u_id, sid))
          }
        }

        dbExecute(tx_con, "DELETE FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))
        if (length(selected_coding_ids) > 0) {
          for (sid in selected_coding_ids) {
            dbExecute(tx_con, "INSERT INTO user_coding_source_permissions (user_id, source_id) VALUES (?, ?)", params = list(u_id, sid))
          }
        }
      })

      removeModal()
      user_table_trigger(user_table_trigger() + 1)
      showNotification(tr("admin.users.notif_sources_updated", lang, u_name, u_id), type = "message")
    }, error = function(e) {
      showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
    })
  })

  invisible(NULL)
}

#' Register Admin Tab 1 Scale Permissions and User CRUD Modals
#'
#' Registers observers and outputs for scale-level coding permissions and
#' creating, editing, and deleting user accounts inside the admin module server.
#'
#' @param input Shiny module `input` object.
#' @param output Shiny module `output` object.
#' @param session Shiny module `session` object.
#' @param ctx Shared reactive context list created by `adminServer()`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
admin_register_users_crud_server <- function(input, output, session, ctx) {
  ns <- ctx$ns
  db_con <- ctx$db_con
  get_lang <- ctx$get_lang
  aktiver_admin <- ctx$aktiver_admin
  aktiver_admin_id <- ctx$aktiver_admin_id
  user_table_trigger <- ctx$user_table_trigger
  user_to_edit   <- reactiveVal(NULL)
  user_to_delete <- reactiveVal(NULL)

  generiere_zufalls_token <- function(laenge = 12) {
    generate_security_token(laenge)
  }

  # ==========================================================================
  # MODAL: SCALE PERMISSIONS (CODING MODULE ONLY)
  # ==========================================================================
  target_user_for_scales <- reactiveVal(NULL)

  observeEvent(input$trigger_manage_user_scales, {
    lang <- get_lang()
    u_target <- input$trigger_manage_user_scales
    req(u_target)

    user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
      dbGetQuery(db_con, "SELECT user_id, username, permissions FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
    } else {
      dbGetQuery(db_con, "SELECT user_id, username, permissions FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
    }

    if (nrow(user_row) == 0) return()
    u_id <- as.integer(user_row$user_id[1])
    u_name <- as.character(user_row$username[1])
    u_perm <- as.character(user_row$permissions[1])

    if (u_perm == "admin") {
      showNotification(tr("admin.users.notif_admin_full_scales", lang), type = "message")
      return()
    }

    target_user_for_scales(list(user_id = u_id, username = u_name))

    all_scales <- dbGetQuery(db_con, "SELECT scale_id, short_label, name, max_value FROM scale_definitions ORDER BY scale_id ASC")
    if (nrow(all_scales) == 0) {
      showNotification(tr("admin.users.notif_no_scales", lang), type = "warning")
      return()
    }

    assigned_scales <- tryCatch({
      dbGetQuery(db_con, "SELECT scale_id FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))$scale_id
    }, error = function(e) integer(0))

    choices_list <- setNames(
      as.character(all_scales$scale_id),
      paste0("[#", all_scales$scale_id, "] ", all_scales$short_label, " - ", all_scales$name, " (0..", all_scales$max_value, ")")
    )

    showModal(modalDialog(
      title = div(icon("sliders-h", class = "me-2 text-primary"), tr("admin.permissions.scales_modal_title", lang, paste0(u_name, " [#", u_id, "]"))),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton(tr("common.cancel", lang)),
        actionButton(ns("btn_save_user_scales"), tr("admin.permissions.btn_save_permissions", lang), class = "btn-primary")
      ),
      div(
        div(class = "alert alert-info py-2 px-3 mb-3 small",
            icon("info-circle", class = "me-2"),
            tr("admin.permissions.scales_modal_subtitle", lang)
        ),
        div(class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded border",
            div(
              tags$strong("Status: "),
              tags$span(class = "text-muted small", tr("admin.permissions.scales_summary", lang, length(assigned_scales), nrow(all_scales)))
            ),
            div(class = "d-flex gap-2",
                actionButton(ns("btn_select_all_scales"), tr("admin.permissions.btn_select_all", lang),
                             icon = icon("check-square"), class = "btn-sm btn-outline-primary"),
                actionButton(ns("btn_deselect_all_scales"), tr("admin.permissions.btn_deselect_all", lang),
                             icon = icon("square"), class = "btn-sm btn-outline-secondary")
            )
        ),
        div(style = "max-height: 360px; overflow-y: auto; padding: 12px; border: 1px solid #ced4da; border-radius: 4px; background-color: #ffffff;",
            checkboxGroupInput(ns("selected_user_scales"), label = NULL, choices = choices_list, selected = as.character(assigned_scales))
        )
      )
    ))
  })

  # Quick selection for scales
  observeEvent(input$btn_select_all_scales, {
    all_ids <- dbGetQuery(db_con, "SELECT scale_id FROM scale_definitions ORDER BY scale_id ASC")$scale_id
    updateCheckboxGroupInput(session, "selected_user_scales", selected = as.character(all_ids))
  })

  observeEvent(input$btn_deselect_all_scales, {
    updateCheckboxGroupInput(session, "selected_user_scales", selected = character(0))
  })

  # Save scale permissions
  observeEvent(input$btn_save_user_scales, {
    lang <- get_lang()
    u_info <- target_user_for_scales()
    req(u_info)
    u_id <- as.integer(u_info$user_id)
    u_name <- as.character(u_info$username)

    selected_scale_ids <- as.integer(input$selected_user_scales)

    tryCatch({
      with_db_transaction(db_con, function(tx_con) {
        dbExecute(tx_con, "DELETE FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))
        if (length(selected_scale_ids) > 0) {
          for (sid in selected_scale_ids) {
            dbExecute(tx_con, "INSERT INTO user_scale_permissions (user_id, scale_id) VALUES (?, ?)", params = list(u_id, sid))
          }
        }
      })

      removeModal()
      user_table_trigger(user_table_trigger() + 1)
      showNotification(tr("admin.users.notif_scales_updated", lang, u_name, u_id), type = "message")
    }, error = function(e) {
      showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
    })
  })

  # Modal: Create new user
  observeEvent(input$btn_open_add_user, {
    lang <- get_lang()
    zufall <- generiere_zufalls_token(10)
    role_choices <- setNames(c("user", "admin"), c(tr("admin.users.opt_user_desc", lang), tr("admin.users.opt_admin_desc", lang)))
    status_choices <- setNames(c(1, 0), c(tr("admin.users.opt_active_desc", lang), tr("admin.users.opt_inactive_desc", lang)))

    showModal(modalDialog(
      title = div(icon("user-plus", class = "me-2 text-success"), tr("admin.users.modal_add_title", lang)),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton(tr("common.cancel", lang)),
        actionButton(ns("btn_save_new_user"), tr("admin.users.btn_submit_add", lang), class = "btn-success")
      ),
      div(
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_username_req", lang)),
            textInput(ns("new_username"), label = NULL, placeholder = tr("admin.users.placeholder_username", lang)),
            tags$small(class = "text-muted", tr("admin.users.hint_username", lang))
        ),
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_token_req", lang)),
            div(class = "input-group",
                textInput(ns("new_token"), label = NULL, value = zufall),
                actionButton(ns("btn_generate_new_token"), tr("admin.users.btn_generate", lang), icon = icon("sync"), class = "btn-outline-secondary")
            ),
            tags$small(class = "text-muted", tr("admin.users.placeholder_token", lang))
        ),
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_role_req", lang)),
            selectInput(ns("new_permissions"), label = NULL, choices = role_choices, selected = "user")
        ),
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_status_req", lang)),
            selectInput(ns("new_is_active"), label = NULL, choices = status_choices, selected = 1)
        ),
        uiOutput(ns("add_user_feedback"))
      )
    ))
  })

  observeEvent(input$btn_generate_new_token, {
    updateTextInput(session, "new_token", value = generiere_zufalls_token(10))
  })

  observeEvent(input$btn_save_new_user, {
    lang <- get_lang()
    u_name <- trimws(input$new_username)
    u_token <- trimws(input$new_token)
    u_perm <- input$new_permissions
    u_active <- as.integer(input$new_is_active)

    if (is.null(u_name) || nchar(u_name) < 3) {
      output$add_user_feedback <- renderUI({
        div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_username_short", lang))
      })
      return()
    }

    if (is.null(u_token) || nchar(u_token) < 4) {
      output$add_user_feedback <- renderUI({
        div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_token_short", lang))
      })
      return()
    }

    existiert <- dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM user_tokens WHERE username = ?", params = list(u_name))$n
    if (existiert > 0) {
      output$add_user_feedback <- renderUI({
        div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_username_exists", lang, u_name))
      })
      return()
    }

    tryCatch({
      dbExecute(db_con, "INSERT INTO user_tokens (username, token, permissions, is_active) VALUES (?, ?, ?, ?)",
                params = list(u_name, u_token, u_perm, u_active))

      new_user_res <- dbGetQuery(db_con, "SELECT user_id FROM user_tokens WHERE username = ?", params = list(u_name))
      new_u_id <- if (nrow(new_user_res) > 0) as.integer(new_user_res$user_id[1]) else 0L

      removeModal()
      user_table_trigger(user_table_trigger() + 1)
      showNotification(tr("admin.users.notif_created_id", lang, u_name, new_u_id), type = "message")
    }, error = function(e) {
      output$add_user_feedback <- renderUI({
        div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
      })
    })
  })

  # Modal: Edit user by user_id
  observeEvent(input$trigger_edit_user, {
    lang <- get_lang()
    u_target <- input$trigger_edit_user
    req(u_target)

    user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
      dbGetQuery(
        db_con,
        "SELECT user_id, username, CASE WHEN LOWER(COALESCE(permissions, '')) IN ('admin', 'system') THEN NULL ELSE token END AS token, permissions, is_active FROM user_tokens WHERE user_id = ?",
        params = list(as.integer(u_target))
      )
    } else {
      dbGetQuery(
        db_con,
        "SELECT user_id, username, CASE WHEN LOWER(COALESCE(permissions, '')) IN ('admin', 'system') THEN NULL ELSE token END AS token, permissions, is_active FROM user_tokens WHERE username = ?",
        params = list(as.character(u_target))
      )
    }

    if (nrow(user_row) == 0) return()

    user_to_edit(user_row)
    u_id <- as.integer(user_row$user_id[1])
    u_name <- as.character(user_row$username[1])
    is_self <- (u_id == aktiver_admin_id() || u_name == aktiver_admin())
    is_protected <- is_protected_token_role(user_row$permissions[1])

    role_choices <- setNames(c("user", "admin"), c(tr("admin.users.opt_user_desc", lang), tr("admin.users.opt_admin_desc", lang)))
    status_choices <- setNames(c(1, 0), c(tr("admin.users.opt_active_desc", lang), tr("admin.users.opt_inactive_desc", lang)))

    token_input_ui <- if (is_protected) {
      tagList(
        passwordInput(
          ns("edit_token"),
          label = NULL,
          value = "",
          placeholder = tr("admin.users.placeholder_token_unchanged", lang)
        ),
        tags$small(class = "text-muted", tr("admin.users.hint_token_unchanged", lang))
      )
    } else {
      div(class = "input-group",
          textInput(ns("edit_token"), label = NULL, value = user_row$token),
          actionButton(ns("btn_generate_edit_token"), tr("admin.users.btn_generate", lang), icon = icon("sync"), class = "btn-outline-secondary")
      )
    }

    showModal(modalDialog(
      title = div(icon("user-edit", class = "me-2 text-primary"), tr("admin.users.modal_edit_title", lang, paste0(u_name, " [#", u_id, "]"))),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton(tr("common.cancel", lang)),
        actionButton(ns("btn_save_edited_user"), tr("common.save_changes", lang), class = "btn-primary")
      ),
      div(
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_id_and_name", lang)),
            tags$div(
              class = "input-group",
              tags$span(class = "input-group-text font-monospace bg-light", paste0("#", u_id)),
              tags$input(type = "text", class = "form-control", value = u_name, disabled = "disabled")
            ),
            tags$small(class = "text-muted", tr("admin.users.hint_id_immutable", lang))
        ),
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_token_req", lang)),
            token_input_ui
        ),
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_role", lang)),
            if (is_self) {
              tagList(
                tags$input(type = "text", class = "form-control", value = tr("admin.users.self_admin_immutable", lang), disabled = "disabled"),
                tags$input(type = "hidden", id = ns("edit_permissions"), value = "admin")
              )
            } else {
              selectInput(ns("edit_permissions"), label = NULL, choices = role_choices, selected = user_row$permissions)
            }
        ),
        div(class = "mb-3",
            tags$label(class = "form-label fw-bold", tr("admin.users.label_account_status", lang)),
            if (is_self) {
              tagList(
                tags$span(class = "badge bg-success p-2", icon("shield-alt", class = "me-1"), tr("admin.users.self_active_protected", lang)),
                p(class = "text-muted small mt-1", tr("admin.users.err_self_deactivate", lang))
              )
            } else {
              selectInput(ns("edit_is_active"), label = NULL, choices = status_choices, selected = as.integer(user_row$is_active))
            }
        ),
        uiOutput(ns("edit_user_feedback"))
      )
    ))
  })

  observeEvent(input$btn_generate_edit_token, {
    updateTextInput(session, "edit_token", value = generiere_zufalls_token(10))
  })

  observeEvent(input$btn_save_edited_user, {
    lang <- get_lang()
    row <- user_to_edit()
    req(row)

    u_id <- as.integer(row$user_id[1])
    u_name <- as.character(row$username[1])
    is_self <- (u_id == aktiver_admin_id() || u_name == aktiver_admin())
    is_protected <- is_protected_token_role(row$permissions[1])
    new_token <- if (is.null(input$edit_token)) "" else trimws(as.character(input$edit_token))

    new_perm <- if (is_self) "admin" else input$edit_permissions
    new_active <- if (is_self) 1 else as.integer(input$edit_is_active)

    keep_existing_token <- is_protected && !nzchar(new_token)

    if (!keep_existing_token && nchar(new_token) < 4) {
      output$edit_user_feedback <- renderUI({
        div(class = "alert alert-danger py-2 mt-2", tr("admin.users.err_token_short", lang))
      })
      return()
    }

    tryCatch({
      if (keep_existing_token) {
        dbExecute(db_con, "UPDATE user_tokens SET permissions = ?, is_active = ? WHERE user_id = ?",
                  params = list(new_perm, new_active, u_id))
      } else {
        dbExecute(db_con, "UPDATE user_tokens SET token = ?, permissions = ?, is_active = ? WHERE user_id = ?",
                  params = list(new_token, new_perm, new_active, u_id))
      }

      removeModal()
      user_table_trigger(user_table_trigger() + 1)
      showNotification(tr("admin.users.notif_updated_id", lang, u_name, u_id), type = "message")
    }, error = function(e) {
      output$edit_user_feedback <- renderUI({
        div(class = "alert alert-danger py-2 mt-2", paste0(tr("common.error", lang), ": ", e$message))
      })
    })
  })

  # Modal: Delete user by user_id
  observeEvent(input$trigger_delete_user, {
    lang <- get_lang()
    u_target <- input$trigger_delete_user
    req(u_target)

    user_row <- if (grepl("^[0-9]+$", as.character(u_target))) {
      dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE user_id = ?", params = list(as.integer(u_target)))
    } else {
      dbGetQuery(db_con, "SELECT user_id, username FROM user_tokens WHERE username = ?", params = list(as.character(u_target)))
    }

    if (nrow(user_row) == 0) return()
    u_id <- as.integer(user_row$user_id[1])
    u_name <- as.character(user_row$username[1])

    if (u_id == aktiver_admin_id() || u_name == aktiver_admin()) {
      showNotification(tr("admin.users.notif_self_delete_err", lang), type = "error")
      return()
    }

    user_to_delete(list(user_id = u_id, username = u_name))

    bewertungs_count <- tryCatch({
      as.integer(dbGetQuery(db_con, "SELECT COUNT(*) AS n FROM user_ratings WHERE user_id = ?", params = list(u_id))$n[1])
    }, error = function(e) 0L)

    showModal(modalDialog(
      title = div(icon("trash", class = "me-2 text-danger"), tr("admin.users.modal_delete_title_id", lang, u_name, u_id)),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton(tr("common.cancel", lang)),
        actionButton(ns("btn_confirm_delete_user"), tr("admin.users.btn_confirm_delete", lang), class = "btn-danger")
      ),
      div(
        p(tags$strong(tr("admin.users.modal_delete_confirm_id", lang, u_name, u_id))),
        p(tr("admin.users.modal_delete_desc", lang), " ", tags$code("user_tokens"), "."),
        p(tags$small(class = "text-muted", tr("admin.users.modal_delete_hint", lang))),
        if (bewertungs_count > 0) {
          div(class = "alert alert-info py-2",
              icon("info-circle", class = "me-1"),
              tr("admin.users.modal_delete_ratings_info", lang, bewertungs_count, u_id)
          )
        } else {
          p(class = "text-muted small", tr("admin.users.modal_delete_no_ratings", lang))
        }
      )
    ))
  })

  observeEvent(input$btn_confirm_delete_user, {
    lang <- get_lang()
    u_info <- user_to_delete()
    req(u_info)
    u_id <- as.integer(u_info$user_id)
    u_name <- as.character(u_info$username)

    if (u_id == aktiver_admin_id() || u_name == aktiver_admin()) {
      removeModal()
      showNotification(tr("admin.users.notif_self_delete_denied", lang), type = "error")
      return()
    }

    tryCatch({
      with_db_transaction(db_con, function(tx_con) {
        dbExecute(tx_con, "DELETE FROM user_ratings WHERE user_id = ?", params = list(u_id))
        dbExecute(tx_con, "DELETE FROM coding_training WHERE user_id = ?", params = list(u_id))
        dbExecute(tx_con, "DELETE FROM user_source_permissions WHERE user_id = ?", params = list(u_id))
        dbExecute(tx_con, "DELETE FROM user_coding_source_permissions WHERE user_id = ?", params = list(u_id))
        dbExecute(tx_con, "DELETE FROM user_scale_permissions WHERE user_id = ?", params = list(u_id))
        dbExecute(tx_con, "DELETE FROM user_tokens WHERE user_id = ?", params = list(u_id))
      })

      removeModal()
      user_table_trigger(user_table_trigger() + 1)
      showNotification(tr("admin.users.notif_deleted_id", lang, u_name, u_id), type = "warning")
    }, error = function(e) {
      showNotification(paste0(tr("common.error", lang), ": ", e$message), type = "error")
    })
  })

  invisible(NULL)
}
