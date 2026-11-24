# Package index

## All functions

- [`adminServer()`](adminServer.md) : Shiny Server Module for the Admin
  Console
- [`adminUI()`](adminUI.md) : Shiny UI Module for the Admin Console
- [`berechne_aifeducation_metriken()`](berechne_aifeducation_metriken.md)
  : Compute Inter-Rater Reliability Metrics via aifeducation
- [`build_coding_search_pagination_ui()`](build_coding_search_pagination_ui.md)
  : Build Pagination Controls for Coding Search Results
- [`build_coding_search_table_ui()`](build_coding_search_table_ui.md) :
  Build HTML Table for Coding Search Results
- [`build_edit_coding_value_modal()`](build_edit_coding_value_modal.md)
  : Build Modal Dialog for Editing a Coding Rating Value
- [`build_scale_examples_modal()`](build_scale_examples_modal.md) :
  Build Modal Dialog Displaying Scale Anchor Examples
- [`classify_reliability_magnitude()`](classify_reliability_magnitude.md)
  : Classify Inter-Rater Reliability Magnitude
- [`clean_table_columns()`](clean_table_columns.md) : Clean and
  Normalize Imported Table Column Names
- [`close_db_connection()`](close_db_connection.md) : Safely Close a
  Direct DBI Database Connection
- [`close_db_pool()`](close_db_pool.md) : Safely Close a Database
  Connection Pool or DBI Connection
- [`coding_manager_server()`](coding_manager_server.md) : Root Shiny
  Server Function for CodingManageR
- [`coding_manager_ui()`](coding_manager_ui.md) : Root Shiny UI Shell
  for CodingManageR
- [`count_unrated_cases_db()`](count_unrated_cases_db.md) : Count
  Unrated Cases for a Coder and Scale
- [`create_all_tables()`](create_all_tables.md) : Create All
  CodingManageR Database Tables
- [`dashboardServer()`](dashboardServer.md) : Shiny Server Module for
  the Progress Dashboard
- [`dashboardUI()`](dashboardUI.md) : Shiny UI Module for the Progress
  Dashboard
- [`detect_browser_language()`](detect_browser_language.md) : Detect
  Supported Application Language from Browser Locale
- [`draw_unrated_case_db()`](draw_unrated_case_db.md) : Draw a Random
  Unrated Case Text from the Database
- [`export_table_template_csv()`](export_table_template_csv.md) : Build
  an Empty Schema Template Data Frame for CSV Export
- [`export_table_to_csv()`](export_table_to_csv.md) : Fetch a Database
  Table for CSV Export
- [`fetch_consolidated_master_export()`](fetch_consolidated_master_export.md)
  : Fetch Consolidated Master Coding Export Dataset
- [`find_translations_file()`](find_translations_file.md) : Locate the
  Bundled Translations JSON File
- [`format_int_locale()`](format_int_locale.md) : Format an Integer with
  Locale-Specific Grouping
- [`format_metric_display()`](format_metric_display.md) : Format a
  Reliability Metric Value for Display
- [`generate_security_token()`](generate_security_token.md) : Generate a
  Random Alphanumeric Access Token
- [`generate_stratified_training_pool()`](generate_stratified_training_pool.md)
  : Generate a Stratified Gold-Standard Training Pool
- [`get_coding_detail_db()`](get_coding_detail_db.md) : Retrieve
  Detailed Metadata for a Single Rating Record
- [`get_db_connection()`](get_db_connection.md) : Open a Direct Database
  Connection (MySQL or SQLite)
- [`get_db_pool()`](get_db_pool.md) : Create a Pooled Database
  Connection (MySQL or SQLite)
- [`get_db_random_function()`](get_db_random_function.md) : Return the
  SQL Random Ordering Expression for a Connection
- [`get_language_choices()`](get_language_choices.md) : Get Named
  Language Choices for UI Selectors
- [`get_reliability_metric_choices()`](get_reliability_metric_choices.md)
  : Get Localized Choices of Inter-Rater Reliability Metrics
- [`get_reliability_metric_meta()`](get_reliability_metric_meta.md) :
  Get Metadata and Thresholds for a Reliability Metric
- [`get_source_license_choices()`](get_source_license_choices.md) : Get
  Localized Source License Choices
- [`get_supported_languages()`](get_supported_languages.md) : List
  Supported Interface Language Codes
- [`get_table_pk_definition()`](get_table_pk_definition.md) : Get
  Primary Key Column Name for a Database Table
- [`get_user_info_db()`](get_user_info_db.md) : Retrieve User Account
  Metadata from the Database
- [`get_user_permitted_coding_sources_db()`](get_user_permitted_coding_sources_db.md)
  : Get Sources Permitted for Live Field Coding by a User
- [`get_user_permitted_sources_db()`](get_user_permitted_sources_db.md)
  : Get All Sources Permitted for a User
- [`is_mysql()`](is_mysql.md) : Check Whether a Connection Uses MySQL or
  MariaDB
- [`is_mysql_connection()`](is_mysql_connection.md) : Check Whether a
  DBI or Pool Connection Targets MySQL/MariaDB
- [`is_pool_connection()`](is_pool_connection.md) : Check Whether an
  Object Is a pool::dbPool Connection
- [`kodierungenServer()`](kodierungenServer.md) : Shiny Server Module
  for Live Field Coding
- [`kodierungenUI()`](kodierungenUI.md) : Shiny UI Module for Live Field
  Coding
- [`language_selector_ui()`](language_selector_ui.md) : Build the
  Bilingual Language Selector Widget
- [`load_translations()`](load_translations.md) : Load and Cache
  Application Translation Dictionaries
- [`render_kpi_card_ui()`](render_kpi_card_ui.md) : Render a Bootstrap
  KPI Summary Card
- [`render_metric_badge_ui()`](render_metric_badge_ui.md) : Render a
  Color-Coded Reliability Metric Badge
- [`run_app()`](run_app.md) : Launch the CodingManageR Shiny Application
- [`schulungServer()`](schulungServer.md) : Shiny Server Module for
  Gold-Standard Coder Training
- [`schulungUI()`](schulungUI.md) : Shiny UI Module for Gold-Standard
  Coder Training
- [`search_case_codings_db()`](search_case_codings_db.md) : Search Case
  Coding Ratings in the Database
- [`seed_initial_data()`](seed_initial_data.md) : Seed Initial Accounts
  or Demo Data into the Database
- [`tr()`](tr.md) : Translate a Localization Key into the Selected
  Language
- [`update_coding_rating_value_db()`](update_coding_rating_value_db.md)
  : Update an Existing Coding Rating Value in the Database
- [`validate_import_schema()`](validate_import_schema.md) : Validate an
  Uploaded CSV Data Frame Against Table Schema Rules
- [`with_db_transaction()`](with_db_transaction.md) : Execute Database
  Operations Atomically Inside a Transaction
