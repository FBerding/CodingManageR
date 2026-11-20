# ==============================================================================
# DETERMINISTIC UNIVERSITY PLATFORM THEME & STYLESHEET HELPERS
# ==============================================================================
# Explicit Bootstrap 5 Sass theme and bundled CSS tokens guaranteeing identical
# appearance across local desktop runs, RStudio Viewer, and hosted Shiny Server
# web deployments without any external font or CDN dependencies.
# ==============================================================================

#' Build Explicit Bootstrap 5 University Platform Theme
#'
#' Constructs a deterministic `bslib::bs_theme()` pinned to Classic University
#' Navy (`#1B365D`) and Cool Slate (`#F6F8FA` / `#FFFFFF`) tokens using an
#' offline cross-platform system font stack and `bootswatch = NULL` so local
#' and web deployments render identically.
#'
#' @return A `bslib` theme object passed to `shiny::fluidPage(theme = ...)`.
#' @keywords internal
#' @noRd
coding_manager_theme <- function() {
  sans_stack <- '-apple-system, BlinkMacSystemFont, "Segoe UI", "Liberation Sans", "Helvetica Neue", Arial, sans-serif'
  mono_stack <- 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace'

  bslib::bs_theme(
    version = 5,
    bootswatch = NULL,
    bg = "#F6F8FA",
    fg = "#1F2937",
    primary = "#1B365D",
    secondary = "#526071",
    success = "#166534",
    info = "#1E40AF",
    warning = "#92400E",
    danger = "#991B1B",
    light = "#EEF2F6",
    dark = "#1B365D",
    base_font = sans_stack,
    heading_font = sans_stack,
    code_font = mono_stack,
    "font-size-base" = "0.9375rem",
    "line-height-base" = "1.55",
    "border-radius" = "0.375rem",
    "border-radius-sm" = "0.25rem",
    "border-radius-lg" = "0.5rem",
    "card-bg" = "#FFFFFF",
    "card-border-color" = "#D8E0EA",
    "input-bg" = "#FFFFFF",
    "input-border-color" = "#C2CDD9",
    "input-focus-border-color" = "#1B365D",
    "table-border-color" = "#E2E8F0",
    "link-color" = "#1B365D",
    "link-hover-color" = "#142947",
    "headings-font-weight" = "600",
    "headings-color" = "#16243A"
  )
}

#' Build Self-Contained University Platform CSS Stylesheet Tag
#'
#' Emits an inline `<style>` tag defining explicit CSS custom properties, fixed
#' centered `1280px` container geometry, institutional top bar, horizontal
#' underlined navigation tabs, single-elevation flat surfaces, tabular numeric
#' formatting, high-legibility case reading panes, and accessible focus states.
#'
#' @return A `shiny.tag` (`tags$style`) embedded in the document `<head>`.
#' @keywords internal
#' @noRd
coding_manager_css <- function() {
  shiny::tags$style(shiny::HTML("
    :root {
      --cm-bg-canvas: #F6F8FA;
      --cm-bg-surface: #FFFFFF;
      --cm-bg-subtle: #EEF2F6;
      --cm-bg-reading: #F8FAFC;
      --cm-border: #D8E0EA;
      --cm-border-strong: #C2CDD9;
      --cm-ink: #1F2937;
      --cm-ink-heading: #16243A;
      --cm-ink-muted: #526071;
      --cm-navy: #1B365D;
      --cm-navy-hover: #142947;
      --cm-navy-soft: #EBF0F7;
      --cm-success: #166534;
      --cm-success-bg: #ECFDF3;
      --cm-success-border: #BBF7D0;
      --cm-warning: #92400E;
      --cm-warning-bg: #FFFBEB;
      --cm-warning-border: #FDE68A;
      --cm-danger: #991B1B;
      --cm-danger-bg: #FEF2F2;
      --cm-danger-border: #FECACA;
      --cm-info: #1E40AF;
      --cm-info-bg: #EFF6FF;
      --cm-info-border: #BFDBFE;
      --cm-radius: 6px;
      --cm-radius-sm: 4px;
      --cm-shadow-xs: 0 1px 2px rgba(15, 23, 42, 0.04);
      --cm-max-width: 1280px;
      --cm-font-sans: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Liberation Sans', 'Helvetica Neue', Arial, sans-serif;
      --cm-font-mono: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, 'Liberation Mono', monospace;
    }

    *, *::before, *::after {
      box-sizing: border-box;
    }

    html {
      overflow-y: scroll;
      -webkit-text-size-adjust: 100%;
    }

    body {
      margin: 0;
      padding: 0;
      background-color: var(--cm-bg-canvas);
      color: var(--cm-ink);
      font-family: var(--cm-font-sans);
      font-size: 15px;
      line-height: 1.55;
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }

    .container-fluid.cm-root-container {
      padding: 0;
      margin: 0;
      width: 100%;
      max-width: none;
    }

    .cm-app {
      min-height: 100vh;
      background-color: var(--cm-bg-canvas);
      color: var(--cm-ink);
    }

    .cm-shell {
      max-width: var(--cm-max-width);
      width: 100%;
      margin: 0 auto;
      padding: 20px 24px 48px 24px;
    }

    @media (max-width: 767.98px) {
      .cm-shell {
        padding: 14px 16px 32px 16px;
      }
    }

    /* Typography & tabular numeric discipline */
    .cm-app h1, .cm-app h2, .cm-app h3, .cm-app h4, .cm-app h5, .cm-app h6 {
      color: var(--cm-ink-heading);
      font-weight: 600;
      letter-spacing: -0.01em;
      text-wrap: balance;
    }

    .cm-app h2 { font-size: 1.45rem; line-height: 1.3; margin-bottom: 0.35rem; }
    .cm-app h3 { font-size: 1.2rem;  line-height: 1.35; margin-bottom: 0.35rem; }
    .cm-app h4 { font-size: 1.05rem; line-height: 1.4; margin-bottom: 0.25rem; }
    .cm-app h5 { font-size: 0.95rem; line-height: 1.4; margin-bottom: 0.25rem; }

    .cm-app .font-monospace,
    .cm-app code,
    .cm-app .cm-kpi-value,
    .cm-app .table td,
    .cm-app .table th {
      font-variant-numeric: tabular-nums;
    }

    .cm-app code {
      font-family: var(--cm-font-mono);
      font-size: 0.85em;
      color: var(--cm-navy);
      background-color: var(--cm-bg-subtle);
      padding: 0.12rem 0.35rem;
      border-radius: var(--cm-radius-sm);
    }

    .cm-app hr {
      border-color: var(--cm-border);
      opacity: 1;
      margin: 1rem 0;
    }

    /* Authentication Screen */
    .cm-login-wrap {
      max-width: 440px;
      margin: 56px auto 32px auto;
    }

    .cm-login-card {
      background-color: var(--cm-bg-surface);
      border: 1px solid var(--cm-border);
      border-top: 3px solid var(--cm-navy);
      border-radius: var(--cm-radius);
      box-shadow: var(--cm-shadow-xs);
      overflow: hidden;
    }

    .cm-login-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 12px;
      padding: 16px 24px;
      background-color: var(--cm-bg-surface);
      border-bottom: 1px solid var(--cm-border);
    }

    .cm-login-body {
      padding: 24px;
    }

    /* Institutional Top Header */
    .cm-header {
      display: flex;
      flex-wrap: wrap;
      justify-content: space-between;
      align-items: center;
      gap: 16px;
      padding: 14px 20px;
      margin-bottom: 18px;
      background-color: var(--cm-bg-surface);
      border: 1px solid var(--cm-border);
      border-top: 3px solid var(--cm-navy);
      border-radius: var(--cm-radius);
      box-shadow: var(--cm-shadow-xs);
    }

    .cm-header-left {
      display: flex;
      flex-wrap: wrap;
      align-items: baseline;
      gap: 12px;
      min-width: 0;
    }

    .cm-brand {
      font-size: 1.125rem;
      font-weight: 700;
      color: var(--cm-navy);
      letter-spacing: -0.015em;
      white-space: nowrap;
      flex-shrink: 0;
    }

    .cm-header-meta {
      display: inline-flex;
      align-items: center;
      gap: 8px;
      font-size: 0.85rem;
      color: var(--cm-ink-muted);
      white-space: nowrap;
    }

    .cm-header-meta-sep {
      color: var(--cm-border-strong);
      user-select: none;
    }

    .cm-header-actions {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 8px;
      flex-shrink: 0;
    }

    .cm-header-actions .btn,
    .cm-header-actions select {
      white-space: nowrap;
      font-size: 0.85rem;
    }

    /* Language Selector Normalization */
    .cm-app select[data-lang-selector] {
      width: auto;
      min-width: 112px;
      height: 32px;
      padding: 0.2rem 1.75rem 0.2rem 0.6rem;
      font-size: 0.85rem;
      color: var(--cm-ink);
      background-color: var(--cm-bg-surface);
      border: 1px solid var(--cm-border-strong);
      border-radius: var(--cm-radius-sm);
    }

    /* Horizontal Underlined Primary & Secondary Navigation Tabs */
    .cm-main-nav > .tabbable > .nav-tabs,
    .cm-module-section .nav-tabs {
      border-bottom: 1px solid var(--cm-border);
      gap: 4px;
      margin-bottom: 20px;
      flex-wrap: wrap;
    }

    .cm-main-nav > .tabbable > .nav-tabs > .nav-item > .nav-link,
    .cm-module-section .nav-tabs > .nav-item > .nav-link {
      border: none;
      border-bottom: 2px solid transparent;
      border-radius: 0;
      background-color: transparent;
      color: var(--cm-ink-muted);
      font-size: 0.9rem;
      font-weight: 500;
      padding: 10px 16px;
      margin-bottom: -1px;
      white-space: nowrap;
      transition: color 150ms ease, border-color 150ms ease, background-color 150ms ease;
    }

    .cm-main-nav > .tabbable > .nav-tabs > .nav-item > .nav-link:hover,
    .cm-module-section .nav-tabs > .nav-item > .nav-link:hover {
      color: var(--cm-navy);
      border-bottom-color: var(--cm-border-strong);
      background-color: rgba(27, 54, 93, 0.03);
    }

    .cm-main-nav > .tabbable > .nav-tabs > .nav-item > .nav-link.active,
    .cm-module-section .nav-tabs > .nav-item > .nav-link.active {
      color: var(--cm-navy);
      font-weight: 600;
      background-color: transparent;
      border-bottom: 2px solid var(--cm-navy);
    }

    .cm-module-section .nav-tabs > .nav-item > .nav-link {
      font-size: 0.86rem;
      padding: 8px 13px;
    }

    .cm-module-section {
      width: 100%;
    }

    /* Unified Single-Elevation Academic Surfaces */
    .cm-surface,
    .cm-app .well {
      background-color: var(--cm-bg-surface);
      border: 1px solid var(--cm-border);
      border-radius: var(--cm-radius);
      padding: 20px 24px;
      margin-bottom: 20px;
      box-shadow: var(--cm-shadow-xs);
    }

    .cm-surface-subtle {
      background-color: var(--cm-bg-subtle);
      border: 1px solid var(--cm-border);
      border-radius: var(--cm-radius);
      padding: 16px 18px;
    }

    .cm-page-intro {
      margin-bottom: 18px;
    }

    .cm-page-intro p {
      color: var(--cm-ink-muted);
      margin-bottom: 0;
      font-size: 0.92rem;
    }

    /* Restrained Academic KPI Cards (preserving bg-* semantic classes) */
    .cm-kpi-card {
      background-color: var(--cm-bg-surface) !important;
      color: var(--cm-ink) !important;
      border: 1px solid var(--cm-border) !important;
      border-top: 3px solid var(--cm-navy) !important;
      border-radius: var(--cm-radius) !important;
      box-shadow: var(--cm-shadow-xs) !important;
    }

    .cm-kpi-card.bg-success { border-top-color: var(--cm-success) !important; }
    .cm-kpi-card.bg-info    { border-top-color: var(--cm-info) !important; }
    .cm-kpi-card.bg-warning { border-top-color: var(--cm-warning) !important; }
    .cm-kpi-card.bg-danger  { border-top-color: var(--cm-danger) !important; }
    .cm-kpi-card.bg-dark,
    .cm-kpi-card.bg-primary { border-top-color: var(--cm-navy) !important; }

    .cm-kpi-card .cm-kpi-value {
      font-family: var(--cm-font-mono);
      font-size: 1.65rem;
      font-weight: 700;
      color: var(--cm-ink-heading);
      line-height: 1.2;
      margin-bottom: 4px;
    }

    .cm-kpi-card .cm-kpi-title {
      font-size: 0.85rem;
      font-weight: 500;
      color: var(--cm-ink-muted);
      margin-bottom: 0;
    }

    .cm-kpi-card .cm-kpi-subtitle {
      font-size: 0.78rem;
      color: var(--cm-ink-muted);
      margin-top: 4px;
    }

    /* Active Session Context Banner (Training & Live Coding) */
    .cm-session-banner {
      background-color: var(--cm-bg-surface);
      border: 1px solid var(--cm-border);
      border-top: 3px solid var(--cm-navy);
      border-radius: var(--cm-radius);
      padding: 16px 20px;
      margin-bottom: 18px;
      box-shadow: var(--cm-shadow-xs);
    }

    .cm-session-meta {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 8px 14px;
      font-size: 0.84rem;
      color: var(--cm-ink-muted);
      margin-top: 6px;
    }

    .cm-session-counter {
      font-family: var(--cm-font-mono);
      font-variant-numeric: tabular-nums;
      font-size: 1.35rem;
      font-weight: 700;
      color: var(--cm-navy);
      margin: 0;
    }

    /* High-Legibility Qualitative Case Text Reading Pane */
    .cm-case-text {
      max-height: 320px;
      overflow-y: auto;
      background-color: var(--cm-bg-reading);
      border: 1px solid var(--cm-border);
      border-radius: var(--cm-radius-sm);
      padding: 18px 20px;
      font-family: var(--cm-font-sans);
      font-size: 0.95rem;
      line-height: 1.65;
      color: #111827;
      white-space: pre-wrap;
    }

    /* Stage Example Blocks inside Modals */
    .cm-example-item {
      padding: 12px 16px;
      margin-bottom: 10px;
      background-color: var(--cm-bg-reading);
      border: 1px solid var(--cm-border);
      border-radius: var(--cm-radius-sm);
    }

    .cm-example-item--active {
      background-color: var(--cm-success-bg);
      border-color: var(--cm-success-border);
    }

    /* High-Density Academic Tables */
    .cm-app .table {
      font-size: 0.88rem;
      color: var(--cm-ink);
      border-color: var(--cm-border);
      margin-bottom: 0;
    }

    .cm-app .table thead th {
      background-color: var(--cm-bg-subtle);
      color: var(--cm-ink-heading);
      font-size: 0.8rem;
      font-weight: 600;
      border-bottom: 1px solid var(--cm-border-strong);
      padding: 10px 12px;
      white-space: nowrap;
    }

    .cm-app .table tbody td {
      padding: 10px 12px;
      vertical-align: middle;
      border-color: var(--cm-border);
    }

    .cm-app .table-hover > tbody > tr:hover > * {
      background-color: #F8FAFC;
    }

    /* Restrained Badges & Semantic Alerts */
    .cm-app .badge {
      font-weight: 500;
      border-radius: var(--cm-radius-sm);
      padding: 0.28em 0.55em;
      letter-spacing: 0.01em;
    }

    .cm-app .badge.bg-primary,
    .cm-app .badge.bg-dark {
      background-color: var(--cm-navy-soft) !important;
      color: var(--cm-navy) !important;
      border: 1px solid #C8D6E8;
    }

    .cm-app .badge.bg-secondary {
      background-color: var(--cm-bg-subtle) !important;
      color: var(--cm-ink-muted) !important;
      border: 1px solid var(--cm-border);
    }

    .cm-app .badge.bg-success {
      background-color: var(--cm-success-bg) !important;
      color: var(--cm-success) !important;
      border: 1px solid var(--cm-success-border);
    }

    .cm-app .badge.bg-warning {
      background-color: var(--cm-warning-bg) !important;
      color: var(--cm-warning) !important;
      border: 1px solid var(--cm-warning-border);
    }

    .cm-app .badge.bg-danger {
      background-color: var(--cm-danger-bg) !important;
      color: var(--cm-danger) !important;
      border: 1px solid var(--cm-danger-border);
    }

    .cm-app .badge.bg-info {
      background-color: var(--cm-info-bg) !important;
      color: var(--cm-info) !important;
      border: 1px solid var(--cm-info-border);
    }

    .cm-app .alert {
      border-radius: var(--cm-radius);
      font-size: 0.9rem;
      padding: 12px 16px;
    }

    .cm-app .alert-info {
      background-color: var(--cm-info-bg);
      border-color: var(--cm-info-border);
      color: var(--cm-info);
    }

    .cm-app .alert-success {
      background-color: var(--cm-success-bg);
      border-color: var(--cm-success-border);
      color: var(--cm-success);
    }

    .cm-app .alert-warning {
      background-color: var(--cm-warning-bg);
      border-color: var(--cm-warning-border);
      color: var(--cm-warning);
    }

    .cm-app .alert-danger {
      background-color: var(--cm-danger-bg);
      border-color: var(--cm-danger-border);
      color: var(--cm-danger);
    }

    /* Buttons, Inputs & Focus Accessibility */
    .cm-app .btn {
      font-weight: 500;
      border-radius: var(--cm-radius-sm);
      transition: background-color 150ms ease, border-color 150ms ease, color 150ms ease;
    }

    .cm-app .btn-primary {
      background-color: var(--cm-navy);
      border-color: var(--cm-navy);
      color: #FFFFFF;
    }

    .cm-app .btn-primary:hover,
    .cm-app .btn-primary:focus {
      background-color: var(--cm-navy-hover);
      border-color: var(--cm-navy-hover);
      color: #FFFFFF;
    }

    .cm-app .btn-outline-primary {
      color: var(--cm-navy);
      border-color: var(--cm-navy);
    }

    .cm-app .btn-outline-primary:hover {
      background-color: var(--cm-navy);
      border-color: var(--cm-navy);
      color: #FFFFFF;
    }

    .cm-app .form-control,
    .cm-app .form-select,
    .cm-app .selectize-input {
      border-color: var(--cm-border-strong);
      border-radius: var(--cm-radius-sm);
      font-size: 0.9rem;
    }

    .cm-app .btn:focus-visible,
    .cm-app .nav-link:focus-visible,
    .cm-app .form-control:focus,
    .cm-app .form-select:focus,
    .cm-app select:focus-visible {
      outline: 2px solid var(--cm-navy);
      outline-offset: 1px;
      box-shadow: 0 0 0 3px rgba(27, 54, 93, 0.16);
    }

    .cm-pagination-bar {
      background-color: var(--cm-bg-subtle);
      border: 1px solid var(--cm-border);
      border-radius: var(--cm-radius-sm);
      padding: 10px 14px;
    }

    /* Clean Modal Dialogs */
    .modal-content {
      border: 1px solid var(--cm-border);
      border-radius: var(--cm-radius);
      box-shadow: 0 12px 32px rgba(15, 23, 42, 0.14);
    }

    .modal-header {
      background-color: var(--cm-bg-subtle);
      border-bottom: 1px solid var(--cm-border);
      padding: 14px 20px;
    }

    .modal-title {
      font-size: 1.05rem;
      font-weight: 600;
      color: var(--cm-ink-heading);
    }

    .modal-footer {
      background-color: var(--cm-bg-canvas);
      border-top: 1px solid var(--cm-border);
      padding: 12px 20px;
    }

    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after {
        transition: none !important;
        animation: none !important;
      }
    }
  "))
}
