# Package index

## Launching the Application

Entrypoints for running CodingManageR locally (SQLite) or on Shiny
Server (MariaDB/MySQL).

- [`run_app_local()`](https://fberding.github.io/CodingManageR/reference/run_app_local.md)
  : Launch CodingManageR Locally (Single-User SQLite Mode)
- [`run_app_server()`](https://fberding.github.io/CodingManageR/reference/run_app_server.md)
  : Launch CodingManageR on a Multi-User Server (MySQL/MariaDB Mode)
- [`run_app()`](https://fberding.github.io/CodingManageR/reference/run_app.md)
  : Launch the CodingManageR Shiny Application (Backward-Compatible
  Wrapper)
- [`coding_manager_ui()`](https://fberding.github.io/CodingManageR/reference/coding_manager_ui.md)
  : Build the Root Shiny UI for CodingManageR
- [`coding_manager_server()`](https://fberding.github.io/CodingManageR/reference/coding_manager_server.md)
  : Shiny Server Function for CodingManageR

## Shiny Workspace Modules

Modular UI and server components for the Dashboard, Coding Training,
Live Coding, and Admin Console workspaces.

- [`dashboardUI()`](https://fberding.github.io/CodingManageR/reference/dashboardUI.md)
  : Dashboard Module UI
- [`dashboardServer()`](https://fberding.github.io/CodingManageR/reference/dashboardServer.md)
  : Dashboard Module Server
- [`schulungUI()`](https://fberding.github.io/CodingManageR/reference/schulungUI.md)
  : Training Module UI
- [`schulungServer()`](https://fberding.github.io/CodingManageR/reference/schulungServer.md)
  : Training Module Server
- [`kodierungenUI()`](https://fberding.github.io/CodingManageR/reference/kodierungenUI.md)
  : Coding Module UI
- [`kodierungenServer()`](https://fberding.github.io/CodingManageR/reference/kodierungenServer.md)
  : Coding Module Server
- [`adminUI()`](https://fberding.github.io/CodingManageR/reference/adminUI.md)
  : Admin Module UI
- [`adminServer()`](https://fberding.github.io/CodingManageR/reference/adminServer.md)
  : Admin Module Server

## Database Connections, Schema & Seeding

Connection pooling, SQLite/MariaDB dialect helpers, transaction
management, schema creation, and initial data seeding.

- [`get_db_pool()`](https://fberding.github.io/CodingManageR/reference/get_db_pool.md)
  : Create a Database Connection Pool or Direct DBI Connection

- [`get_db_connection()`](https://fberding.github.io/CodingManageR/reference/get_db_connection.md)
  : Open a Database Connection (Backward-Compatible Wrapper)

- [`close_db_pool()`](https://fberding.github.io/CodingManageR/reference/close_db_pool.md)
  : Safely Close a Connection Pool or DBI Connection

- [`close_db_connection()`](https://fberding.github.io/CodingManageR/reference/close_db_connection.md)
  : Safely Close a Database Connection (Backward-Compatible Alias)

- [`is_mysql()`](https://fberding.github.io/CodingManageR/reference/is_mysql.md)
  : Check Whether a Connection Uses MySQL or MariaDB

- [`is_mysql_connection()`](https://fberding.github.io/CodingManageR/reference/is_mysql_connection.md)
  : Check Whether a Connection Targets MySQL or MariaDB

- [`is_pool_connection()`](https://fberding.github.io/CodingManageR/reference/is_pool_connection.md)
  :

  Check Whether an Object Is a
  [`pool::dbPool`](http://rstudio.github.io/pool/reference/dbPool.md)
  Connection

- [`get_db_random_function()`](https://fberding.github.io/CodingManageR/reference/get_db_random_function.md)
  : Determine SQL Random Order Expression

- [`with_db_transaction()`](https://fberding.github.io/CodingManageR/reference/with_db_transaction.md)
  : Execute Database Operations Atomically Inside a Transaction

- [`create_all_tables()`](https://fberding.github.io/CodingManageR/reference/create_all_tables.md)
  : Create All CodingManageR Database Tables

- [`seed_initial_data()`](https://fberding.github.io/CodingManageR/reference/seed_initial_data.md)
  : Seed Initial Accounts or Demonstration Data into the Database

## Database Queries, Permissions & Rating Corrections

Per-user authorization lookups, unrated case sampling, paginated coding
search, and rating value updates.

- [`get_user_info_db()`](https://fberding.github.io/CodingManageR/reference/get_user_info_db.md)
  : Retrieve User Metadata and Identifier
- [`get_user_permitted_sources_db()`](https://fberding.github.io/CodingManageR/reference/get_user_permitted_sources_db.md)
  : Retrieve Authorized Text Sources for a User
- [`get_user_permitted_coding_sources_db()`](https://fberding.github.io/CodingManageR/reference/get_user_permitted_coding_sources_db.md)
  : Retrieve Authorized Scientific Coding Sources for a User
- [`count_unrated_cases_db()`](https://fberding.github.io/CodingManageR/reference/count_unrated_cases_db.md)
  : Count Unrated Cases for a Specific Scale and User
- [`draw_unrated_case_db()`](https://fberding.github.io/CodingManageR/reference/draw_unrated_case_db.md)
  : Draw an Unrated Case for Active Coding
- [`search_case_codings_db()`](https://fberding.github.io/CodingManageR/reference/search_case_codings_db.md)
  : Search and Filter Submitted Case Codings Across Scales
- [`get_coding_detail_db()`](https://fberding.github.io/CodingManageR/reference/get_coding_detail_db.md)
  : Retrieve Detailed Context for a Single Rating Record
- [`update_coding_rating_value_db()`](https://fberding.github.io/CodingManageR/reference/update_coding_rating_value_db.md)
  : Update Only the Numeric Scale Rating Value for a Stored Coding
- [`generate_security_token()`](https://fberding.github.io/CodingManageR/reference/generate_security_token.md)
  : Generate a Secure Random Alphanumeric Token

## Inter-Rater Reliability & Stratified Training

Computation of 16 reliability metrics via aifeducation, stratified
training pool sampling, and metric metadata helpers.

- [`berechne_aifeducation_metriken()`](https://fberding.github.io/CodingManageR/reference/berechne_aifeducation_metriken.md)
  :

  Compute Inter-Rater Reliability Metrics via `aifeducation`

- [`generate_stratified_training_pool()`](https://fberding.github.io/CodingManageR/reference/generate_stratified_training_pool.md)
  : Generate Stratified Training Pool Guaranteeing Full Scale Range

- [`get_reliability_metric_choices()`](https://fberding.github.io/CodingManageR/reference/get_reliability_metric_choices.md)
  : Retrieve Localized Optgroup Choices for Reliability Metric Selectors

- [`get_reliability_metric_meta()`](https://fberding.github.io/CodingManageR/reference/get_reliability_metric_meta.md)
  : Retrieve Localized Metadata for a Single Reliability Metric Key

- [`classify_reliability_magnitude()`](https://fberding.github.io/CodingManageR/reference/classify_reliability_magnitude.md)
  : Classify Inter-Rater Reliability Magnitude

- [`format_metric_display()`](https://fberding.github.io/CodingManageR/reference/format_metric_display.md)
  : Format Numeric Metric or Percentage for Display

- [`get_source_license_choices()`](https://fberding.github.io/CodingManageR/reference/get_source_license_choices.md)
  : Retrieve Localized Source License Options

## Data Export, Templates & Import Validation

Consolidated master dataset exports, raw table CSV snapshots, template
generators, and import schema validation.

- [`fetch_consolidated_master_export()`](https://fberding.github.io/CodingManageR/reference/fetch_consolidated_master_export.md)
  : Build and Execute Consolidated Master Data Export Query
- [`export_table_to_csv()`](https://fberding.github.io/CodingManageR/reference/export_table_to_csv.md)
  : Generate CSV Content for a Database Table
- [`export_table_template_csv()`](https://fberding.github.io/CodingManageR/reference/export_table_template_csv.md)
  : Generate Empty CSV Template for Table Upload
- [`clean_table_columns()`](https://fberding.github.io/CodingManageR/reference/clean_table_columns.md)
  : Sanitize and Clean Data Frame Column Names
- [`get_table_pk_definition()`](https://fberding.github.io/CodingManageR/reference/get_table_pk_definition.md)
  : Retrieve Table Primary Key Column Name
- [`validate_import_schema()`](https://fberding.github.io/CodingManageR/reference/validate_import_schema.md)
  : Validate Table Schema for Data Upload

## Localization & UI Builders

Bilingual German/English translation utilities and reusable Bootstrap 5
UI components.

- [`tr()`](https://fberding.github.io/CodingManageR/reference/tr.md) :
  Translate a Dot-Separated UI Key with Fallback and Formatting

- [`load_translations()`](https://fberding.github.io/CodingManageR/reference/load_translations.md)
  : Load Translations Dictionary from JSON with Modification-Time
  Caching

- [`find_translations_file()`](https://fberding.github.io/CodingManageR/reference/find_translations_file.md)
  :

  Locate the Canonical `translations.json` File

- [`get_supported_languages()`](https://fberding.github.io/CodingManageR/reference/get_supported_languages.md)
  : Get Supported Language Codes

- [`get_language_choices()`](https://fberding.github.io/CodingManageR/reference/get_language_choices.md)
  : Get Labeled Language Choices for Shiny Inputs

- [`detect_browser_language()`](https://fberding.github.io/CodingManageR/reference/detect_browser_language.md)
  : Normalize and Validate a Browser Locale String

- [`language_selector_ui()`](https://fberding.github.io/CodingManageR/reference/language_selector_ui.md)
  : Render Header Language Selector Dropdown Widget

- [`render_kpi_card_ui()`](https://fberding.github.io/CodingManageR/reference/render_kpi_card_ui.md)
  : Render Standardized KPI Stat Card

- [`render_metric_badge_ui()`](https://fberding.github.io/CodingManageR/reference/render_metric_badge_ui.md)
  : Render Metric Status Badge

- [`build_scale_examples_modal()`](https://fberding.github.io/CodingManageR/reference/build_scale_examples_modal.md)
  : Build Modal Dialog for Coding Scale Definitions and Stage Examples

- [`build_edit_coding_value_modal()`](https://fberding.github.io/CodingManageR/reference/build_edit_coding_value_modal.md)
  : Build Modal Dialog for Editing a Case Coding Rating Value

- [`build_coding_search_table_ui()`](https://fberding.github.io/CodingManageR/reference/build_coding_search_table_ui.md)
  : Build Paginated Table UI for Admin Coding Search and Correction

- [`build_coding_search_pagination_ui()`](https://fberding.github.io/CodingManageR/reference/build_coding_search_pagination_ui.md)
  : Build Pagination Bar for Admin Coding Search Table

- [`format_int_locale()`](https://fberding.github.io/CodingManageR/reference/format_int_locale.md)
  : Format Integer with Locale-Aware Separators
