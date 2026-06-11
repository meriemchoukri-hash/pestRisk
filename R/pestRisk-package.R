#' @keywords internal
"_PACKAGE"

#' pestRisk: Modelling Agricultural Pest Invasion Risk
#'
#' @description
#' The `pestRisk` package provides a complete, reproducible workflow for
#' modelling the current distribution and invasion risk of agricultural pests
#' using Species Distribution Models (SDMs).
#'
#' ## Pipeline Overview
#'
#' ```
#' GBIF occurrences  ──►  download_gbif()
#'        │
#'        ▼
#' Spatial cleaning  ──►  clean_occurrences()
#'        │
#'        ▼
#' WorldClim data    ──►  download_worldclim()
#'        │
#'        ▼
#' Variable selection ──► prepare_predictors()   (VIF + Pearson)
#'        │
#'        ▼
#' Background points ──► generate_background_points()
#'        │
#'        ├──► train_rf_model()   (Random Forest)
#'        └──► train_maxent()     (MaxEnt via maxnet)
#'              │
#'              ▼
#'        evaluate_models()       (AUC, ROC, Accuracy …)
#'              │
#'              ▼
#'        predict_risk_map()      (suitability raster)
#'              │
#'        ┌─────┴──────────────────┐
#'        ▼                        ▼
#' summarize_country_risk()  plot_risk_map()
#'                           plot_variable_importance()
#'              │
#'              ▼
#'        generate_report()       (HTML / PDF)
#' ```
#'
#' ## Target species
#' * *Spodoptera frugiperda* – fall armyworm
#' * *Tuta absoluta* – tomato leafminer
#' * *Ceratitis capitata* – Mediterranean fruit fly
#'
#' ## References
#' * WorldClim: Fick & Hijmans (2017) <doi:10.1002/joc.5086>
#' * GBIF: GBIF Secretariat (2023) <https://www.gbif.org>
#' * MaxEnt: Phillips et al. (2017) <doi:10.1111/ecog.03049>
#'
#' @docType package
#' @name pestRisk-package
#' @aliases pestRisk
NULL
