# =============================================================================
# predict_risk_map.R
# =============================================================================

#' Generate a spatial pest risk prediction map
#'
#' @description
#' Applies a fitted SDM (Random Forest or MaxEnt) to a WorldClim raster stack
#' to produce a continuous suitability surface across the study area. The
#' continuous suitability is optionally reclassified into three risk categories
#' (Low / Medium / High) using user-defined breakpoints.
#'
#' @param model_obj `list`. Fitted model, output of [train_rf_model()] or
#'   [train_maxent()].
#' @param clim_stack `SpatRaster`. WorldClim bioclimatic layers covering the
#'   prediction area. Must contain the same variables as used for training
#'   (names must match `model_obj$predictors`).
#' @param breaks `numeric vector` of length 3. Suitability thresholds for risk
#'   classification: `c(low_to_medium, medium_to_high)`. Default `c(0.33, 0.66)`.
#' @param verbose `logical`. Print progress (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`suitability`}{`SpatRaster`. Continuous suitability in [0, 1].}
#'   \item{`risk_class`}{`SpatRaster`. Categorical risk (1=Low, 2=Medium, 3=High).}
#'   \item{`breaks`}{The breakpoints used for classification.}
#'   \item{`model_type`}{Model type used for prediction.}
#' }
#'
#' @details
#' ## Prediction approach
#' The function iterates over all raster cells in blocks to avoid loading the
#' entire raster into memory at once, making it feasible for global predictions
#' at 2.5 or 5 arcmin resolution.
#'
#' ## Risk classification
#' The three-class scheme (Low / Medium / High) mirrors the classification
#' used in official EPPO (European and Mediterranean Plant Protection
#' Organization) pest risk analyses:
#' * **Low (< 0.33)**: Environmental conditions unlikely to sustain a viable
#'   population.
#' * **Medium (0.33–0.66)**: Conditions marginally favourable; population
#'   establishment possible in warm years.
#' * **High (> 0.66)**: Highly suitable conditions; pest likely to establish
#'   and spread rapidly.
#'
#' @examples
#' \dontrun{
#' data(spodoptera_model_data)
#' data(bio_sample)
#'
#' rf_out <- train_rf_model(spodoptera_model_data)
#' risk   <- predict_risk_map(rf_out, bio_sample)
#'
#' terra::plot(risk$suitability, main = "Suitability – S. frugiperda")
#' terra::plot(risk$risk_class,  main = "Risk Class",
#'             col = c("forestgreen", "orange", "red"))
#' }
#'
#' @importFrom terra rast nlyr predict classify
#' @export
predict_risk_map <- function(model_obj,
                              clim_stack,
                              breaks  = c(0.33, 0.66),
                              verbose = TRUE) {

  .check_pkg("terra")

  preds <- model_obj$predictors

  # ---- Check that required layers are present --------------------------------
  missing_layers <- setdiff(preds, names(clim_stack))
  if (length(missing_layers) > 0)
    stop("[pestRisk] The following predictor layers are missing from `clim_stack`:\n  ",
         paste(missing_layers, collapse = ", "), call. = FALSE)

  # Subset to the exact predictors in the right order
  clim_sub <- clim_stack[[preds]]

  if (verbose)
    message("[pestRisk] Predicting suitability over ",
            terra::nrow(clim_sub), " × ", terra::ncol(clim_sub),
            " raster (", terra::nlyr(clim_sub), " layers) …")

  # ---- Predict using terra::predict ------------------------------------------
  if (model_obj$type == "rf") {
    suit <- terra::predict(clim_sub, model_obj$model,
                           type = "prob", index = 2,   # probability of class "1"
                           na.rm = TRUE)
  } else if (model_obj$type == "maxent") {
    # terra::predict works with maxnet if we supply a custom fun
    pred_fun <- function(model, ...) {
      d <- list(...)[[1]]
      as.numeric(predict(model, as.matrix(d), type = "logistic"))
    }
    suit <- terra::predict(clim_sub, model_obj$model,
                           fun = pred_fun, na.rm = TRUE)
  } else {
    stop("[pestRisk] Unknown model type: ", model_obj$type, call. = FALSE)
  }

  names(suit) <- "suitability"

  # ---- Classify into risk categories -----------------------------------------
  if (verbose) message("[pestRisk] Classifying suitability into risk categories …")

  rcl_matrix <- matrix(
    c(0,          breaks[1], 1,
      breaks[1],  breaks[2], 2,
      breaks[2],  1,         3),
    nrow = 3, byrow = TRUE
  )
  risk_class        <- terra::classify(suit, rcl_matrix, include.lowest = TRUE)
  names(risk_class) <- "risk_class"
  levels(risk_class) <- data.frame(id = 1:3,
                                    risk = c("Low", "Medium", "High"))

  if (verbose) {
    freq <- terra::freq(risk_class)
    total <- sum(freq$count)
    message("[pestRisk] Risk distribution:")
    for (row in seq_len(nrow(freq))) {
      cat("  ", freq$value[row], " (", levels(risk_class)[[1]]$risk[freq$value[row]], "): ",
          round(100 * freq$count[row] / total, 1), "%\n", sep = "")
    }
  }

  list(
    suitability = suit,
    risk_class  = risk_class,
    breaks      = breaks,
    model_type  = model_obj$type
  )
}
