#' Berechnet die vollständige Metrik-Matrix für eine Sitzung via aifeducation
#'
#' @param user_vals Numerischer Vektor der Benutzerbewertungen
#' @param true_vals Numerischer Vektor der korrekten Vorgabewerte
#' @param max_val Der maximale mögliche Skalenwert (für die Faktordefinition)
#' @return Eine benannte Liste mit gerundeten Kennzahlen
#' @export
berechne_aifeducation_metriken <- function(user_vals, true_vals, max_val) {
  all_levels <- as.character(0:max_val)

  user_fact <- factor(as.character(user_vals), levels = all_levels)
  true_fact <- factor(as.character(true_vals), levels = all_levels)

  metrics <- aifeducation::get_coder_metrics(
    true_values = true_fact,
    predicted_values = user_fact,
    return_names_only = FALSE
  )

  return(list(
    accuracy            = as.character(round(metrics["percentage_agreement"] * 100, 1)),
    balanced_accuracy   = as.character(round(metrics["balanced_accuracy"] * 100, 1)),
    iota_index          = as.character(round(metrics["iota_index"], 3)),
    avg_iota2           = as.character(round(metrics["avg_iota2"], 3)),
    cohens_kappa        = as.character(round(metrics["c_kappa_unweighted"], 3)),
    kappa_linear        = as.character(round(metrics["c_kappa_linear"], 3)),
    kappa_squared       = as.character(round(metrics["c_kappa_squared"], 3)),
    fleiss_kappa        = as.character(round(metrics["kappa_fleiss"], 3)),
    kendall_w           = as.character(round(metrics["kendall"], 3)),
    alpha_nominal       = as.character(round(metrics["kalpha_nominal"], 3)),
    alpha_ordinal       = as.character(round(metrics["kalpha_ordinal"], 3)),
    gwet_ac1            = as.character(round(metrics["gwet_ac1_nominal"], 3)),
    gwet_ac2_quadratic  = as.character(round(metrics["gwet_ac2_quadratic"], 3)),
    macro_precision     = as.character(round(metrics["avg_precision"] * 100, 1)),
    macro_recall        = as.character(round(metrics["avg_recall"] * 100, 1)),
    macro_f1            = as.character(round(metrics["avg_f1"] * 100, 1))
  ))
}
