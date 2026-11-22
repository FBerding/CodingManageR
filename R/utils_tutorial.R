# ==============================================================================
# IN-APP GETTING STARTED SPOTLIGHT TUTORIAL ENGINE (INTERNAL HELPERS)
# ==============================================================================
# Self-contained, offline, bilingual (en/de), role-aware spotlight walkthrough
# that guides users through the CodingManageR workflow directly inside the UI
# with automatic tab/subtab switching and viewport-clamped anchored tooltips.
# ==============================================================================

#' Build Role-Aware Localized Tutorial Step Payload
#'
#' Constructs the JSON-serializable payload sent via Shiny custom message
#' (`cm_start_tutorial`) to launch or live-update the in-app Getting Started
#' spotlight tutorial. Administrators receive the complete 3-part, 12-step
#' workflow track (Initial Setup, Coding Process, Further Admin Options);
#' standard coders receive a focused 4-step track covering Part 2.
#'
#' @param role Character scalar specifying user role (`"admin"` or `"user"`).
#' @param lang Character scalar language code (`"en"` or `"de"`).
#' @param preserve_step Logical; when `TRUE`, instructs the client engine to
#'   refresh localized copy in place without resetting the current step index.
#'
#' @return A named list suitable for `session$sendCustomMessage()`.
#' @keywords internal
#' @noRd
build_tutorial_payload <- function(role = "user", lang = "en", preserve_step = FALSE) {
  eff_lang <- if (identical(tolower(as.character(lang)), "de")) "de" else "en"
  is_admin <- identical(tolower(as.character(role)), "admin")

  mk_step <- function(id, part, part_title, main_tab, sub_tab_id, sub_tab, selector, key_prefix) {
    list(
      id = id,
      part = as.integer(part),
      part_title = part_title,
      main_tab = main_tab,
      sub_tab_id = sub_tab_id,
      sub_tab = sub_tab,
      selector = selector,
      title = tr(paste0("tutorial.", key_prefix, "_title"), eff_lang),
      body = tr(paste0("tutorial.", key_prefix, "_body"), eff_lang),
      tip = tr(paste0("tutorial.", key_prefix, "_tip"), eff_lang)
    )
  }

  p1_title <- tr("tutorial.part_1_title", eff_lang)
  p2_title <- tr("tutorial.part_2_title", eff_lang)
  p3_title <- tr("tutorial.part_3_title", eff_lang)

  if (is_admin) {
    parts <- list(
      list(part = 1L, label = tr("tutorial.part_1_short", eff_lang), start_index = 0L),
      list(part = 2L, label = tr("tutorial.part_2_short", eff_lang), start_index = 4L),
      list(part = 3L, label = tr("tutorial.part_3_short", eff_lang), start_index = 8L)
    )
    steps <- list(
      # Part 1: Initial Setup
      mk_step(
        "admin_setup_security", 1L, p1_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_users",
        "#mod_admin-users_table_container, #mod_admin-btn_open_add_user",
        "admin_step_1"
      ),
      mk_step(
        "admin_setup_sources", 1L, p1_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_sources",
        "#mod_admin-btn_open_add_source, #mod_admin-sources_table_container",
        "admin_step_2"
      ),
      mk_step(
        "admin_setup_scales", 1L, p1_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_scales",
        "#mod_admin-btn_open_add_scale, #mod_admin-scales_table_container",
        "admin_step_3"
      ),
      mk_step(
        "admin_setup_texts", 1L, p1_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_texts",
        "#mod_admin-texts_management_table_container, #mod_admin-texts_search_query",
        "admin_step_4"
      ),
      # Part 2: The Coding Process
      mk_step(
        "admin_coding_users", 2L, p2_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_users",
        "#mod_admin-btn_open_add_user, #mod_admin-users_table_container",
        "admin_step_5"
      ),
      mk_step(
        "admin_coding_training", 2L, p2_title,
        "tab_training", NULL, NULL,
        "#mod_training-schulungs_bereich",
        "admin_step_6"
      ),
      mk_step(
        "admin_coding_dashboard", 2L, p2_title,
        "tab_dashboard", "mod_dashboard-dashboard_tabs", "tab_progress",
        "#mod_dashboard-dashboard_tabs",
        "admin_step_7"
      ),
      mk_step(
        "admin_coding_live", 2L, p2_title,
        "tab_codings", NULL, NULL,
        "#mod_codings-kodier_bereich",
        "admin_step_8"
      ),
      # Part 3: Further Admin Options
      mk_step(
        "admin_extra_reliability", 3L, p3_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_reliability",
        "#mod_admin-btn_refresh_reliability, #mod_admin-admin_reliability_comparison_plot",
        "admin_step_9"
      ),
      mk_step(
        "admin_extra_edit_codings", 3L, p3_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_coding_search",
        "#mod_admin-coding_search_header_ui, #mod_admin-coding_search_table_container",
        "admin_step_10"
      ),
      mk_step(
        "admin_extra_data", 3L, p3_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_data",
        "#mod_admin-download_master_dataset_btn, #mod_admin-download_template_scales",
        "admin_step_11"
      ),
      mk_step(
        "admin_extra_diagnostics", 3L, p3_title,
        "tab_admin", "mod_admin-admin_subtabs", "tab_diagnose",
        "#mod_admin-db_diag_summary_cards, #mod_admin-btn_refresh_diagnostics",
        "admin_step_12"
      )
    )
  } else {
    parts <- list(
      list(part = 1L, label = tr("tutorial.part_2_short", eff_lang), start_index = 0L)
    )
    steps <- list(
      mk_step(
        "coder_dashboard_progress", 1L, p2_title,
        "tab_dashboard", "mod_dashboard-dashboard_tabs", "tab_progress",
        "#mod_dashboard-stat_card_total_kodierung, #mod_dashboard-dashboard_tabs",
        "coder_step_1"
      ),
      mk_step(
        "coder_dashboard_scales", 1L, p2_title,
        "tab_dashboard", "mod_dashboard-dashboard_tabs", "tab_scales_overview",
        "#mod_dashboard-dashboard_scale_search, #mod_dashboard-dashboard_scales_cards",
        "coder_step_2"
      ),
      mk_step(
        "coder_training", 1L, p2_title,
        "tab_training", NULL, NULL,
        "#mod_training-schulungs_bereich",
        "coder_step_3"
      ),
      mk_step(
        "coder_live", 1L, p2_title,
        "tab_codings", NULL, NULL,
        "#mod_codings-kodier_bereich",
        "coder_step_4"
      )
    )
  }

  list(
    role = if (is_admin) "admin" else "user",
    is_admin = is_admin,
    lang = eff_lang,
    preserve_step = isTRUE(preserve_step),
    total_parts = length(parts),
    parts = parts,
    ui = list(
      dialog_label = tr("tutorial.dialog_label", eff_lang),
      part_label = tr("tutorial.part_label", eff_lang),
      step_counter = tr("tutorial.step_counter", eff_lang),
      btn_back = tr("tutorial.btn_back", eff_lang),
      btn_next = tr("tutorial.btn_next", eff_lang),
      btn_finish = tr("tutorial.btn_finish", eff_lang),
      btn_close = tr("tutorial.btn_close", eff_lang),
      tip_prefix = tr("tutorial.tip_prefix", eff_lang),
      keyboard_hint = tr("tutorial.keyboard_hint", eff_lang)
    ),
    steps = steps
  )
}

#' Build Self-Contained Spotlight Tutorial CSS & JavaScript Assets
#'
#' Emits inline `<style>` and `<script>` tags implementing an accessible,
#' zero-dependency spotlight overlay and anchored tooltip walkthrough aligned
#' with the CodingManageR university design tokens.
#'
#' @return A `shiny.tag.list` embedded inside the document `<head>`.
#' @keywords internal
#' @noRd
coding_manager_tutorial_assets <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      /* =====================================================================
         Getting Started Spotlight Overlay & Anchored Tooltip
         ===================================================================== */
      .cm-tour-backdrop {
        position: fixed;
        inset: 0;
        z-index: 2040;
        background: transparent;
        pointer-events: auto;
      }

      .cm-tour-backdrop--centered {
        background: rgba(15, 23, 42, 0.48);
      }

      .cm-tour-spotlight {
        position: fixed;
        z-index: 2045;
        border-radius: 6px;
        border: 2px solid var(--cm-navy, #1B365D);
        box-shadow:
          0 0 0 4px rgba(27, 54, 93, 0.18),
          0 0 0 9999px rgba(15, 23, 42, 0.48);
        pointer-events: none;
        transition: opacity 160ms cubic-bezier(0.16, 1, 0.3, 1);
      }

      .cm-tour-tooltip {
        position: fixed;
        z-index: 2050;
        width: 388px;
        max-width: calc(100vw - 24px);
        background-color: var(--cm-bg-surface, #FFFFFF);
        color: var(--cm-ink, #1F2937);
        border: 1px solid var(--cm-border, #D8E0EA);
        border-top: 3px solid var(--cm-navy, #1B365D);
        border-radius: var(--cm-radius, 6px);
        box-shadow: 0 12px 32px rgba(15, 23, 42, 0.18);
        padding: 18px 20px 16px 20px;
        font-family: var(--cm-font-sans, sans-serif);
        opacity: 0;
        transform: translateY(4px);
        transition: opacity 160ms cubic-bezier(0.16, 1, 0.3, 1),
                    transform 160ms cubic-bezier(0.16, 1, 0.3, 1);
      }

      .cm-tour-tooltip--visible {
        opacity: 1;
        transform: translateY(0);
      }

      .cm-tour-arrow {
        position: absolute;
        width: 10px;
        height: 10px;
        background-color: var(--cm-bg-surface, #FFFFFF);
        border-left: 1px solid var(--cm-border, #D8E0EA);
        border-top: 1px solid var(--cm-border, #D8E0EA);
        transform: rotate(45deg);
        display: none;
      }

      .cm-tour-tooltip[data-placement='bottom'] .cm-tour-arrow {
        display: block;
        top: -6px;
        background-color: var(--cm-navy, #1B365D);
        border-color: var(--cm-navy, #1B365D);
      }

      .cm-tour-tooltip[data-placement='top'] .cm-tour-arrow {
        display: block;
        bottom: -6px;
        transform: rotate(225deg);
      }

      .cm-tour-tooltip[data-placement='right'] .cm-tour-arrow {
        display: block;
        left: -6px;
        transform: rotate(-45deg);
      }

      .cm-tour-tooltip[data-placement='left'] .cm-tour-arrow {
        display: block;
        right: -6px;
        transform: rotate(135deg);
      }

      .cm-tour-parts {
        display: flex;
        align-items: center;
        gap: 4px;
        padding: 3px;
        margin-bottom: 12px;
        background-color: var(--cm-bg-subtle, #EEF2F6);
        border: 1px solid var(--cm-border, #D8E0EA);
        border-radius: var(--cm-radius-sm, 4px);
      }

      .cm-tour-part-btn {
        flex: 1 1 0;
        appearance: none;
        border: 1px solid transparent;
        background: transparent;
        color: var(--cm-ink-muted, #526071);
        font-size: 0.74rem;
        font-weight: 500;
        line-height: 1.25;
        padding: 5px 6px;
        border-radius: 3px;
        cursor: pointer;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        transition: background-color 140ms ease, color 140ms ease;
      }

      .cm-tour-part-btn:hover {
        color: var(--cm-ink-heading, #16243A);
      }

      .cm-tour-part-btn--active {
        background-color: var(--cm-bg-surface, #FFFFFF);
        color: var(--cm-navy, #1B365D);
        border-color: var(--cm-border, #D8E0EA);
        font-weight: 600;
      }

      .cm-tour-part-btn:focus-visible,
      .cm-tour-close:focus-visible,
      .cm-tour-btn:focus-visible {
        outline: 2px solid var(--cm-navy, #1B365D);
        outline-offset: 2px;
      }

      .cm-tour-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 8px;
      }

      .cm-tour-meta {
        font-size: 0.78rem;
        color: var(--cm-ink-muted, #526071);
        font-variant-numeric: tabular-nums;
        display: flex;
        align-items: center;
        gap: 6px;
      }

      .cm-tour-close {
        appearance: none;
        border: 1px solid transparent;
        background: transparent;
        color: var(--cm-ink-muted, #526071);
        width: 28px;
        height: 28px;
        border-radius: var(--cm-radius-sm, 4px);
        font-size: 1.15rem;
        line-height: 1;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        flex-shrink: 0;
      }

      .cm-tour-close:hover {
        background-color: var(--cm-bg-subtle, #EEF2F6);
        color: var(--cm-ink-heading, #16243A);
      }

      .cm-tour-title {
        margin: 0 0 8px 0;
        font-size: 1.02rem;
        font-weight: 600;
        line-height: 1.35;
        color: var(--cm-ink-heading, #16243A);
      }

      .cm-tour-body {
        margin: 0;
        font-size: 0.89rem;
        line-height: 1.55;
        color: var(--cm-ink, #1F2937);
      }

      .cm-tour-tip {
        margin-top: 12px;
        padding-top: 10px;
        border-top: 1px solid var(--cm-border, #D8E0EA);
        font-size: 0.82rem;
        line-height: 1.45;
        color: var(--cm-ink-muted, #526071);
      }

      .cm-tour-tip strong {
        color: var(--cm-ink-heading, #16243A);
        font-weight: 600;
      }

      .cm-tour-footer {
        margin-top: 14px;
        padding-top: 12px;
        border-top: 1px solid var(--cm-border, #D8E0EA);
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
      }

      .cm-tour-kbd-hint {
        font-size: 0.74rem;
        color: var(--cm-ink-muted, #526071);
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .cm-tour-actions {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-shrink: 0;
      }

      .cm-tour-btn {
        appearance: none;
        font-size: 0.83rem;
        font-weight: 500;
        line-height: 1.25;
        padding: 7px 14px;
        border-radius: var(--cm-radius-sm, 4px);
        cursor: pointer;
        white-space: nowrap;
        transition: background-color 140ms ease, border-color 140ms ease, color 140ms ease;
      }

      .cm-tour-btn--secondary {
        background-color: var(--cm-bg-surface, #FFFFFF);
        color: var(--cm-ink, #1F2937);
        border: 1px solid var(--cm-border-strong, #C2CDD9);
      }

      .cm-tour-btn--secondary:hover:not(:disabled) {
        background-color: var(--cm-bg-subtle, #EEF2F6);
      }

      .cm-tour-btn--secondary:disabled {
        opacity: 0.45;
        cursor: not-allowed;
      }

      .cm-tour-btn--primary {
        background-color: var(--cm-navy, #1B365D);
        color: #FFFFFF;
        border: 1px solid var(--cm-navy, #1B365D);
      }

      .cm-tour-btn--primary:hover {
        background-color: var(--cm-navy-hover, #142947);
        border-color: var(--cm-navy-hover, #142947);
      }

      @media (max-width: 576px) {
        .cm-tour-kbd-hint {
          display: none;
        }
      }

      @media (prefers-reduced-motion: reduce) {
        .cm-tour-spotlight,
        .cm-tour-tooltip,
        .cm-tour-part-btn,
        .cm-tour-btn {
          transition: none !important;
        }
      }
    ")),
    shiny::tags$script(shiny::HTML("
      (function() {
        if (window.CodingManageRTutorial) return;

        var state = {
          active: false,
          payload: null,
          index: 0,
          backdropEl: null,
          spotlightEl: null,
          tooltipEl: null,
          rafId: null,
          pollTimer: null
        };

        function prefersReducedMotion() {
          return window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
        }

        function formatTwo(template, a, b) {
          var idx = 0;
          var vals = [a, b];
          return String(template || '').replace(/%d/g, function() {
            var v = vals[idx];
            idx += 1;
            return v !== undefined ? String(v) : '';
          });
        }

        function clearTimers() {
          if (state.pollTimer) {
            window.clearTimeout(state.pollTimer);
            state.pollTimer = null;
          }
          if (state.rafId) {
            window.cancelAnimationFrame(state.rafId);
            state.rafId = null;
          }
        }

        function removeDom() {
          if (state.backdropEl && state.backdropEl.parentNode) state.backdropEl.parentNode.removeChild(state.backdropEl);
          if (state.spotlightEl && state.spotlightEl.parentNode) state.spotlightEl.parentNode.removeChild(state.spotlightEl);
          if (state.tooltipEl && state.tooltipEl.parentNode) state.tooltipEl.parentNode.removeChild(state.tooltipEl);
          state.backdropEl = null;
          state.spotlightEl = null;
          state.tooltipEl = null;
        }

        function closeTour(notifyShiny) {
          if (!state.active && !state.tooltipEl) return;
          state.active = false;
          clearTimers();
          removeDom();
          document.removeEventListener('keydown', onKeyDown, true);
          window.removeEventListener('resize', schedulePosition);
          window.removeEventListener('scroll', schedulePosition, true);
          if (notifyShiny && window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('tutorial_closed', Date.now(), { priority: 'event' });
          }
          var triggerBtn = document.getElementById('btn_start_tutorial');
          if (triggerBtn && typeof triggerBtn.focus === 'function') {
            triggerBtn.focus();
          }
        }

        function activateTabDom(containerId, tabValue) {
          if (!containerId || !tabValue) return;
          var container = document.getElementById(containerId);
          if (!container) return;
          var link = container.querySelector('[data-value=\"' + tabValue + '\"]');
          if (!link) return;
          if (!link.classList.contains('active')) {
            if (window.bootstrap && window.bootstrap.Tab) {
              try {
                window.bootstrap.Tab.getOrCreateInstance(link).show();
              } catch (e) {
                link.click();
              }
            } else {
              link.click();
            }
          }
        }

        function syncTabsForStep(step) {
          if (!step) return;
          activateTabDom('main_nav_tabs', step.main_tab);
          if (step.sub_tab_id && step.sub_tab) {
            activateTabDom(step.sub_tab_id, step.sub_tab);
          }
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('tutorial_nav_request', {
              main_tab: step.main_tab || null,
              sub_tab_id: step.sub_tab_id || null,
              sub_tab: step.sub_tab || null,
              nonce: Date.now()
            }, { priority: 'event' });
          }
        }

        function findVisibleTarget(selectorStr) {
          if (!selectorStr) return null;
          var parts = String(selectorStr).split(',');
          for (var i = 0; i < parts.length; i++) {
            var sel = parts[i].trim();
            if (!sel) continue;
            var el = null;
            try {
              el = document.querySelector(sel);
            } catch (e) {
              el = null;
            }
            if (el) {
              var r = el.getBoundingClientRect();
              var style = window.getComputedStyle(el);
              if (style && style.display !== 'none' && style.visibility !== 'hidden' && r.width > 0 && r.height > 0) {
                return el;
              }
            }
          }
          return null;
        }

        function positionElements() {
          if (!state.active || !state.payload || !state.tooltipEl) return;
          var step = state.payload.steps[state.index];
          if (!step) return;

          var target = findVisibleTarget(step.selector);
          var vw = window.innerWidth;
          var vh = window.innerHeight;
          var pad = 8;
          var margin = 12;
          var tt = state.tooltipEl;
          var arrow = tt.querySelector('.cm-tour-arrow');

          if (!target) {
            state.spotlightEl.style.display = 'none';
            state.backdropEl.classList.add('cm-tour-backdrop--centered');
            tt.setAttribute('data-placement', 'center');
            var cw = tt.offsetWidth || 388;
            var ch = tt.offsetHeight || 240;
            tt.style.left = Math.max(margin, Math.round((vw - cw) / 2)) + 'px';
            tt.style.top = Math.max(margin, Math.round((vh - ch) / 2)) + 'px';
            tt.classList.add('cm-tour-tooltip--visible');
            return;
          }

          var rect = target.getBoundingClientRect();
          // Clamp tall container spotlights inside viewport so spotlight stays clean
          var spTop = Math.max(margin, rect.top - pad);
          var spLeft = Math.max(margin, rect.left - pad);
          var spRight = Math.min(vw - margin, rect.right + pad);
          var spBottom = Math.min(vh - margin, rect.bottom + pad);
          var spWidth = Math.max(24, spRight - spLeft);
          var spHeight = Math.max(24, Math.min(spBottom - spTop, vh * 0.56));

          state.backdropEl.classList.remove('cm-tour-backdrop--centered');
          state.spotlightEl.style.display = 'block';
          state.spotlightEl.style.top = Math.round(spTop) + 'px';
          state.spotlightEl.style.left = Math.round(spLeft) + 'px';
          state.spotlightEl.style.width = Math.round(spWidth) + 'px';
          state.spotlightEl.style.height = Math.round(spHeight) + 'px';

          var tw = tt.offsetWidth || 388;
          var th = tt.offsetHeight || 240;
          var gap = 14;
          var placement = 'bottom';
          var top = spTop + spHeight + gap;
          var left = spLeft + (spWidth - tw) / 2;

          if (top + th + margin <= vh) {
            placement = 'bottom';
          } else if (spTop - th - gap >= margin) {
            placement = 'top';
            top = spTop - th - gap;
          } else if (spLeft + spWidth + tw + gap + margin <= vw) {
            placement = 'right';
            left = spLeft + spWidth + gap;
            top = spTop + (spHeight - th) / 2;
          } else if (spLeft - tw - gap >= margin) {
            placement = 'left';
            left = spLeft - tw - gap;
            top = spTop + (spHeight - th) / 2;
          } else {
            placement = 'center';
            left = (vw - tw) / 2;
            top = Math.max(margin, vh - th - margin);
          }

          left = Math.max(margin, Math.min(left, vw - tw - margin));
          top = Math.max(margin, Math.min(top, vh - th - margin));

          tt.setAttribute('data-placement', placement);
          tt.style.left = Math.round(left) + 'px';
          tt.style.top = Math.round(top) + 'px';

          if (arrow) {
            if (placement === 'bottom' || placement === 'top') {
              var targetCenterX = spLeft + spWidth / 2;
              var arrowLeft = Math.max(18, Math.min(tw - 28, targetCenterX - left - 5));
              arrow.style.left = Math.round(arrowLeft) + 'px';
              arrow.style.top = '';
            } else if (placement === 'left' || placement === 'right') {
              var targetCenterY = spTop + spHeight / 2;
              var arrowTop = Math.max(18, Math.min(th - 28, targetCenterY - top - 5));
              arrow.style.top = Math.round(arrowTop) + 'px';
              arrow.style.left = '';
            }
          }

          tt.classList.add('cm-tour-tooltip--visible');
        }

        function schedulePosition() {
          if (!state.active) return;
          if (state.rafId) window.cancelAnimationFrame(state.rafId);
          state.rafId = window.requestAnimationFrame(positionElements);
        }

        function ensureDom() {
          if (!state.backdropEl) {
            state.backdropEl = document.createElement('div');
            state.backdropEl.className = 'cm-tour-backdrop';
            state.backdropEl.addEventListener('click', function() {
              closeTour(true);
            });
            document.body.appendChild(state.backdropEl);
          }
          if (!state.spotlightEl) {
            state.spotlightEl = document.createElement('div');
            state.spotlightEl.className = 'cm-tour-spotlight';
            document.body.appendChild(state.spotlightEl);
          }
          if (!state.tooltipEl) {
            state.tooltipEl = document.createElement('div');
            state.tooltipEl.className = 'cm-tour-tooltip';
            state.tooltipEl.setAttribute('role', 'dialog');
            state.tooltipEl.setAttribute('aria-modal', 'true');
            state.tooltipEl.setAttribute('aria-labelledby', 'cm-tour-step-title');
            state.tooltipEl.addEventListener('click', function(e) {
              e.stopPropagation();
            });
            document.body.appendChild(state.tooltipEl);
          }
        }

        function renderStepContent() {
          var p = state.payload;
          var steps = p.steps || [];
          var step = steps[state.index];
          if (!step || !state.tooltipEl) return;

          var ui = p.ui || {};
          var tt = state.tooltipEl;
          tt.setAttribute('aria-label', ui.dialog_label || 'Getting Started');
          tt.innerHTML = '';

          var arrow = document.createElement('div');
          arrow.className = 'cm-tour-arrow';
          arrow.setAttribute('aria-hidden', 'true');
          tt.appendChild(arrow);

          // Functional segmented part jump bar for administrators (3 parts)
          if (p.parts && p.parts.length > 1) {
            var partsBar = document.createElement('div');
            partsBar.className = 'cm-tour-parts';
            partsBar.setAttribute('role', 'tablist');
            for (var i = 0; i < p.parts.length; i++) {
              (function(partObj) {
                var btn = document.createElement('button');
                btn.type = 'button';
                btn.className = 'cm-tour-part-btn' + (partObj.part === step.part ? ' cm-tour-part-btn--active' : '');
                btn.textContent = partObj.label;
                btn.setAttribute('aria-selected', partObj.part === step.part ? 'true' : 'false');
                btn.addEventListener('click', function() {
                  goToStep(partObj.start_index);
                });
                partsBar.appendChild(btn);
              })(p.parts[i]);
            }
            tt.appendChild(partsBar);
          }

          var topRow = document.createElement('div');
          topRow.className = 'cm-tour-top';

          var meta = document.createElement('div');
          meta.className = 'cm-tour-meta';
          var metaText = (p.total_parts > 1)
            ? formatTwo(ui.part_label, step.part, p.total_parts) + ' \\u00b7 ' + formatTwo(ui.step_counter, state.index + 1, steps.length)
            : step.part_title + ' \\u00b7 ' + formatTwo(ui.step_counter, state.index + 1, steps.length);
          meta.textContent = metaText;
          topRow.appendChild(meta);

          var closeBtn = document.createElement('button');
          closeBtn.type = 'button';
          closeBtn.className = 'cm-tour-close';
          closeBtn.setAttribute('aria-label', ui.btn_close || 'Close');
          closeBtn.textContent = '\\u00d7';
          closeBtn.addEventListener('click', function() {
            closeTour(true);
          });
          topRow.appendChild(closeBtn);
          tt.appendChild(topRow);

          var h = document.createElement('h4');
          h.id = 'cm-tour-step-title';
          h.className = 'cm-tour-title';
          h.textContent = step.title || '';
          tt.appendChild(h);

          var body = document.createElement('p');
          body.className = 'cm-tour-body';
          body.textContent = step.body || '';
          tt.appendChild(body);

          if (step.tip) {
            var tip = document.createElement('div');
            tip.className = 'cm-tour-tip';
            var strong = document.createElement('strong');
            strong.textContent = (ui.tip_prefix || 'Tip:') + ' ';
            tip.appendChild(strong);
            tip.appendChild(document.createTextNode(step.tip));
            tt.appendChild(tip);
          }

          var footer = document.createElement('div');
          footer.className = 'cm-tour-footer';

          var hint = document.createElement('span');
          hint.className = 'cm-tour-kbd-hint';
          hint.textContent = ui.keyboard_hint || '';
          footer.appendChild(hint);

          var actions = document.createElement('div');
          actions.className = 'cm-tour-actions';

          var backBtn = document.createElement('button');
          backBtn.type = 'button';
          backBtn.className = 'cm-tour-btn cm-tour-btn--secondary';
          backBtn.textContent = ui.btn_back || 'Back';
          backBtn.disabled = state.index <= 0;
          backBtn.addEventListener('click', function() {
            if (state.index > 0) goToStep(state.index - 1);
          });
          actions.appendChild(backBtn);

          var isLast = state.index >= steps.length - 1;
          var nextBtn = document.createElement('button');
          nextBtn.type = 'button';
          nextBtn.className = 'cm-tour-btn cm-tour-btn--primary';
          nextBtn.textContent = isLast ? (ui.btn_finish || 'Finish') : (ui.btn_next || 'Next');
          nextBtn.addEventListener('click', function() {
            if (isLast) {
              closeTour(true);
            } else {
              goToStep(state.index + 1);
            }
          });
          actions.appendChild(nextBtn);

          footer.appendChild(actions);
          tt.appendChild(footer);
        }

        function waitForTargetAndPosition(step, attemptsLeft) {
          if (!state.active) return;
          var target = findVisibleTarget(step.selector);
          if (target) {
            try {
              target.scrollIntoView({
                behavior: prefersReducedMotion() ? 'auto' : 'smooth',
                block: 'center',
                inline: 'nearest'
              });
            } catch (e) {}
            positionElements();
            state.pollTimer = window.setTimeout(positionElements, prefersReducedMotion() ? 40 : 220);
            return;
          }
          if (attemptsLeft > 0) {
            // Retry tab activation once mid-way in case Shiny re-rendered DOM
            if (attemptsLeft === 6) {
              activateTabDom('main_nav_tabs', step.main_tab);
              if (step.sub_tab_id && step.sub_tab) activateTabDom(step.sub_tab_id, step.sub_tab);
            }
            state.pollTimer = window.setTimeout(function() {
              waitForTargetAndPosition(step, attemptsLeft - 1);
            }, 60);
          } else {
            positionElements();
          }
        }

        function goToStep(newIndex) {
          if (!state.active || !state.payload || !state.payload.steps) return;
          var total = state.payload.steps.length;
          if (total === 0) return;
          state.index = Math.max(0, Math.min(newIndex, total - 1));
          clearTimers();
          var step = state.payload.steps[state.index];
          renderStepContent();
          syncTabsForStep(step);
          waitForTargetAndPosition(step, 10);
        }

        function onKeyDown(e) {
          if (!state.active) return;
          if (e.key === 'Escape') {
            e.preventDefault();
            closeTour(true);
          } else if (e.key === 'ArrowRight') {
            e.preventDefault();
            if (state.index < state.payload.steps.length - 1) {
              goToStep(state.index + 1);
            } else {
              closeTour(true);
            }
          } else if (e.key === 'ArrowLeft') {
            e.preventDefault();
            if (state.index > 0) goToStep(state.index - 1);
          }
        }

        function startTour(payload) {
          if (!payload || !payload.steps || !payload.steps.length) return;
          var keepIndex = Boolean(payload.preserve_step && state.active);
          var prevIndex = state.index;
          state.payload = payload;
          state.active = true;
          ensureDom();
          document.removeEventListener('keydown', onKeyDown, true);
          window.removeEventListener('resize', schedulePosition);
          window.removeEventListener('scroll', schedulePosition, true);
          document.addEventListener('keydown', onKeyDown, true);
          window.addEventListener('resize', schedulePosition);
          window.addEventListener('scroll', schedulePosition, true);
          goToStep(keepIndex ? Math.min(prevIndex, payload.steps.length - 1) : 0);
        }

        window.CodingManageRTutorial = {
          start: startTour,
          close: function() { closeTour(false); }
        };

        function registerShinyHandlers() {
          if (window.Shiny && typeof Shiny.addCustomMessageHandler === 'function') {
            Shiny.addCustomMessageHandler('cm_start_tutorial', function(payload) {
              startTour(payload);
            });
            Shiny.addCustomMessageHandler('cm_close_tutorial', function() {
              closeTour(false);
            });
            return true;
          }
          return false;
        }

        if (!registerShinyHandlers()) {
          document.addEventListener('DOMContentLoaded', registerShinyHandlers);
          window.addEventListener('load', registerShinyHandlers);
        }
      })();
    "))
  )
}
