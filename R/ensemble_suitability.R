# =============================================================================
# ensemble_suitability.R  – NOUVELLE FONCTIONNALITÉ #1
# =============================================================================

#' Combine RF and MaxEnt predictions into a weighted ensemble suitability map
#'
#' @description
#' Combines suitability rasters from a Random Forest and a MaxEnt model into
#' a single ensemble prediction using AUC-weighted averaging. The ensemble
#' approach reduces the uncertainty inherent in any single SDM algorithm and
#' generally outperforms individual models in transferability studies.
#'
#' This is a **new feature** extending the standard single-model workflow: in
#' the course, models were trained and evaluated individually. The ensemble
#' step adds scientific rigour by integrating complementary model assumptions
#' (RF: discriminative, MaxEnt: generative) into a consensus map.
#'
#' @param rf_risk `list`. Output of [predict_risk_map()] using a Random Forest
#'   model (i.e., `model_type == "rf"`).
#' @param mx_risk `list`. Output of [predict_risk_map()] using a MaxEnt model
#'   (i.e., `model_type == "maxent"`).
#' @param rf_auc `numeric`. AUC of the Random Forest model (from
#'   [evaluate_models()]`$metrics`). Used as weight. Default `0.5` = equal
#'   weight if AUC not provided.
#' @param mx_auc `numeric`. AUC of the MaxEnt model. Default `0.5`.
#' @param breaks `numeric vector` of length 2. Suitability thresholds for
#'   Low / Medium / High risk classification. Default `c(0.33, 0.66)`.
#' @param verbose `logical`. Print ensemble weights (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`ensemble_suitability`}{`SpatRaster`. AUC-weighted mean suitability
#'     in [0, 1].}
#'   \item{`ensemble_risk_class`}{`SpatRaster`. Classified risk
#'     (1=Low, 2=Medium, 3=High).}
#'   \item{`weights`}{Named numeric vector: RF and MaxEnt weights used.}
#'   \item{`breaks`}{Breakpoints used for classification.}
#' }
#'
#' @details
#' ## Weighting strategy
#' Weights are proportional to each model's AUC score:
#' \deqn{w_{RF} = \frac{AUC_{RF}}{AUC_{RF} + AUC_{MaxEnt}}}
#' \deqn{w_{MaxEnt} = 1 - w_{RF}}
#'
#' The ensemble suitability is then:
#' \deqn{S_{ensemble} = w_{RF} \cdot S_{RF} + w_{MaxEnt} \cdot S_{MaxEnt}}
#'
#' ## Why ensemble modelling?
#' Different SDM algorithms make different assumptions about species-environment
#' relationships. Their predictions can diverge substantially in novel climates
#' (e.g., under climate change scenarios). Ensemble models:
#' * Reduce single-model bias
#' * Provide implicit uncertainty quantification
#' * Are standard practice in state-of-the-art SDM workflows
#'   (Araújo & New 2007; IPBES biodiversity assessments use ensembles)
#'
#' @references
#' Araújo, M.B. & New, M. (2007). Ensemble forecasting of species distributions.
#' *Trends Ecol. Evol.* **22**, 42–47. <doi:10.1016/j.tree.2006.09.010>
#'
#' @examples
#' \dontrun{
#' # After training and predicting with both models:
#' rf_risk <- predict_risk_map(rf_out, clim)
#' mx_risk <- predict_risk_map(mx_out, clim)
#'
#' eval <- evaluate_models(rf_out, mx_out)
#' rf_auc <- eval$metrics$AUC[eval$metrics$Model == "RandomForest"]
#' mx_auc <- eval$metrics$AUC[eval$metrics$Model == "MaxEnt"]
#'
#' ens <- ensemble_suitability(rf_risk, mx_risk,
#'                              rf_auc = rf_auc, mx_auc = mx_auc)
#' terra::plot(ens$ensemble_suitability,
#'             main = "Ensemble suitability – S. frugiperda")
#' }
#'
#' @importFrom terra lapp classify
#' @export
ensemble_suitability <- function(rf_risk,
                                  mx_risk,
                                  rf_auc  = 0.5,
                                  mx_auc  = 0.5,
                                  breaks  = c(0.33, 0.66),
                                  verbose = TRUE) {

  .check_pkg("terra")

  # ---- Validate inputs -------------------------------------------------------
  if (!inherits(rf_risk$suitability, "SpatRaster"))
    stop("`rf_risk` must be output of predict_risk_map().", call. = FALSE)
  if (!inherits(mx_risk$suitability, "SpatRaster"))
    stop("`mx_risk` must be output of predict_risk_map().", call. = FALSE)

  # ---- Compute AUC-proportional weights -------------------------------------
  total_auc <- rf_auc + mx_auc
  w_rf  <- rf_auc  / total_auc
  w_mx  <- mx_auc  / total_auc

  if (verbose)
    message(
      sprintf("[pestRisk] Ensemble weights — RF: %.3f  |  MaxEnt: %.3f",
              w_rf, w_mx)
    )

  # ---- Weighted average suitability -----------------------------------------
  ensemble <- terra::lapp(
    c(rf_risk$suitability, mx_risk$suitability),
    fun = function(rf, mx) w_rf * rf + w_mx * mx
  )
  names(ensemble) <- "ensemble_suitability"

  # ---- Classify into risk categories ----------------------------------------
  rcl <- matrix(
    c(0,         breaks[1], 1,
      breaks[1], breaks[2], 2,
      breaks[2], 1,         3),
    nrow = 3, byrow = TRUE
  )
  risk_class        <- terra::classify(ensemble, rcl, include.lowest = TRUE)
  names(risk_class) <- "risk_class"
  levels(risk_class) <- data.frame(id   = 1:3,
                                    risk = c("Low", "Medium", "High"))

  if (verbose) {
    freq  <- terra::freq(risk_class)
    total <- sum(freq$count)
    message("[pestRisk] Ensemble risk distribution:")
    labels <- c("Low", "Medium", "High")
    for (i in seq_len(nrow(freq)))
      message(sprintf("  %s: %.1f%%",
                       labels[freq$value[i]],
                       100 * freq$count[i] / total))
  }

  list(
    ensemble_suitability = ensemble,
    ensemble_risk_class  = risk_class,
    weights              = c(RF = w_rf, MaxEnt = w_mx),
    breaks               = breaks
  )
}
