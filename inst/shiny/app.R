# Note: This file was generated with the assistance of AI.

# ==============================================================================
# CodingManageR - Shiny Server Open Source Entrypoint (Create Once)
# ==============================================================================
# Place this file once in your Shiny Server app directory (for example:
# /srv/shiny-server/codingmanager/app.R) together with a .Renviron file
# containing your MySQL/MariaDB credentials (MYSQL_HOST, MYSQL_PORT,
# MYSQL_USER, MYSQL_PASSWORD, MYSQL_DBNAME).
#
# Because all application logic and runtime options live inside the installed
# CodingManageR package and .Renviron, future package updates do not require
# editing or re-copying this file -- simply update CodingManageR in R and
# restart Shiny Server.
# ==============================================================================

if (!requireNamespace("CodingManageR", quietly = TRUE)) {
  stop(
    "Package 'CodingManageR' must be installed system-wide to run this Shiny Server application.",
    call. = FALSE
  )
}

CodingManageR::run_app_server()
