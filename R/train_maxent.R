# =============================================================================
# train_maxent.R
# =============================================================================

#' Train a MaxEnt SDM using maxnet
#'
#' @description
#' Trains a Maximum Entropy (MaxEnt) species distribution model using the
#' `maxnet` package (the modern, regularized R implementation of MaxEnt).
#' MaxEnt is a presence-only model that contrasts conditions at known presence
#' locations against those at background points to estimate the species'
#' environmental suitability across the study region.
#'
#' @param model_data `data.frame`. Output of [prepare_predictors()]`$model_data`.
#'   Must contain columns `presence` (1 / 0) and the predictor variables.
#' @param predictors `character vector`. Names of predictor columns.
#'   If `NULL`, all columns except `lon`, `lat`, `presence` are used.
#' @param reg_mult `numeric`. Regularization multiplier (λ). Higher values
#'   produce smoother, more generalised response curves (default `1`).
#'   Larger values (2–4) reduce overfitting; smaller values (0.5) allow more
#'   complex responses.
#' @param feature_classes `character`. MaxEnt feature types to enable.
#'   Concatenate any of: `"l"` (Linear), `"q"` (Quadratic), `"h"` (Hinge),
#'   `"p"` (Product), `"t"` (Threshold). Default `"lqh"` is a good starting
#'   point for most species.
#' @param test_frac `numeric`. Fraction of presence + background data for
#'   evaluation (default `0.25`).
#' @param seed `integer`. Random seed (default `42`).
#' @param verbose `logical`. Print progress (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`model`}{Fitted `maxnet` object.}
#'   \item{`response_curves`}{`data.frame` of response curves (one per variable).}
#'   \item{`train_data`}{Training subset.}
#'   \item{`test_data`}{Test subset.}
#'   \item{`predictors`}{Character vector of predictor names used.}
#'   \item{`reg_mult`}{Regularization multiplier used.}
#'   \item{`type`}{"maxent" (tag for downstream functions).}
#' }
#'
#' @details
#' ## MaxEnt vs Random Forest — when to use which
#' | Criterion | MaxEnt | Random Forest |
#' |-----------|--------|---------------|
#' | Data type | Presence-only | Presence + background/absence |
#' | Interpretability | Response curves | Variable importance |
#' | Small presence data | ✓ robust | Less reliable |
#' | Ensemble modelling | ✓ complement | ✓ |
#'
#' ## Regularization
#' The regularization multiplier (`reg_mult`) is the key hyperparameter in
#' MaxEnt. Using the default (1.0) is a common source of overfitting. A proper
#' workflow would tune this via ENMeval / AICc cross-validation. The function
#' accepts any value; `reg_mult = 2` is a safe starting point for species with
#' limited records (< 100 presences).
#'
#' ## Feature classes
#' * `l` – Linear: simple proportional relationships
#' * `q` – Quadratic: allows unimodal (hump-shaped) responses — typical for
#'   thermal tolerance of insects
#' * `h` – Hinge: piecewise-linear, most flexible for complex curves
#'
#' @references
#' Phillips, S.J. *et al.* (2017) Opening the black box: An open-source release
#' of MaxEnt. *Ecography* **40**, 887–893. <doi:10.1111/ecog.03049>
#'
#' @examples
#' data(spodoptera_model_data)
#'
#' mx_out <- train_maxent(spodoptera_model_data, reg_mult = 1.5)
#' head(mx_out$response_curves)
#'
#' @importFrom maxnet maxnet
#' @export
train_maxent <- function(model_data,
                          predictors     = NULL,
                          reg_mult       = 1,
                          feature_classes = "lqh",
                          test_frac      = 0.25,
                          seed           = 42L,
                          verbose        = TRUE) {

  .check_pkg("maxnet")

  set.seed(seed)

  # ---- Determine predictors --------------------------------------------------
  if (is.null(predictors))
    predictors <- setdiff(names(model_data), c("lon", "lat", "presence"))

  missing_cols <- setdiff(predictors, names(model_data))
  if (length(missing_cols) > 0)
    stop("Predictors not found in model_data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  # ---- Train / test split (stratified) --------------------------------------
  pres_idx <- which(model_data$presence == 1)
  back_idx <- which(model_data$presence == 0)

  test_idx  <- c(sample(pres_idx, max(1L, round(length(pres_idx) * test_frac))),
                 sample(back_idx, max(1L, round(length(back_idx) * test_frac))))
  train_idx <- setdiff(seq_len(nrow(model_data)), test_idx)

  train_df <- model_data[train_idx, ]
  test_df  <- model_data[test_idx, ]

  if (verbose)
    message("[pestRisk] MaxEnt training: ", sum(train_df$presence == 1),
            " presences + ", sum(train_df$presence == 0),
            " background | reg_mult=", reg_mult,
            " | features='", feature_classes, "'")

  # ---- Build feature class list for maxnet ----------------------------------
  fc_list <- strsplit(feature_classes, "")[[1]]
  fc_full <- c(
    l = "linear",  q = "quadratic",  h = "hinge",
    p = "product", t = "threshold"
  )
  fc_use  <- unname(fc_full[fc_list])

  # ---- Fit MaxEnt model ------------------------------------------------------
  p <- as.integer(train_df$presence)
  X <- as.data.frame(train_df[, predictors, drop = FALSE])

  mx_model <- tryCatch(
    maxnet::maxnet(p = p, data = X,
                   f = maxnet::maxnet.formula(p, X, classes = feature_classes),
                   regmult = reg_mult),
    error = function(e) stop("[pestRisk] maxnet error: ", conditionMessage(e),
                             call. = FALSE)
  )

  if (verbose) message("[pestRisk] MaxEnt model fitted.")

  # ---- Compute response curves (marginal responses) -------------------------
  response_list <- lapply(predictors, function(var) {
    var_range <- seq(min(X[[var]]), max(X[[var]]), length.out = 100)
    newdat    <- as.data.frame(matrix(
      rep(colMeans(X), 100), nrow = 100, byrow = TRUE
    ))
    colnames(newdat) <- predictors
    newdat[[var]]    <- var_range

    pred <- tryCatch(
      predict(mx_model, newdat, type = "logistic"),
      error = function(e) rep(NA_real_, 100)
    )
    data.frame(variable = var, value = var_range, suitability = as.numeric(pred))
  })
  response_df <- do.call(rbind, response_list)

  list(
    model           = mx_model,
    response_curves = response_df,
    train_data      = train_df,
    test_data       = test_df,
    predictors      = predictors,
    reg_mult        = reg_mult,
    type            = "maxent"
  )
}
