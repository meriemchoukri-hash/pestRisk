# =============================================================================
# train_rf_model.R
# =============================================================================

#' Train a Random Forest SDM for pest risk modelling
#'
#' @description
#' Trains a Random Forest classifier to predict the presence probability of an
#' agricultural pest from bioclimatic predictors. The function handles class
#' imbalance, performs a stratified train/test split, and returns the fitted
#' model alongside variable importance scores.
#'
#' @param model_data `data.frame`. Output of [prepare_predictors()]`$model_data`.
#'   Must contain a column `presence` (1 / 0) and predictor columns.
#' @param predictors `character vector`. Names of the predictor columns to use.
#'   Defaults to all columns except `lon`, `lat`, and `presence`.
#' @param n_trees `integer`. Number of trees in the forest (default `500`).
#'   Increasing this improves stability at the cost of computation time.
#' @param test_frac `numeric`. Fraction of data to hold out as test set
#'   (default `0.25`).
#' @param balance `logical`. If `TRUE` (default), downsample the majority class
#'   in the training set to produce a balanced 1:1 presence/background ratio.
#' @param seed `integer`. Random seed (default `42`).
#' @param verbose `logical`. Print training progress (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`model`}{Fitted `randomForest` object.}
#'   \item{`importance`}{`data.frame` of variable importance (MeanDecreaseGini).}
#'   \item{`train_data`}{Training subset.}
#'   \item{`test_data`}{Test subset (unused during training).}
#'   \item{`predictors`}{Names of predictor variables used.}
#'   \item{`type`}{"rf" (for use by [evaluate_models()] and [predict_risk_map()]).}
#' }
#'
#' @details
#' ## Why Random Forest?
#' RF is particularly well-suited for SDMs because:
#' * It handles non-linear relationships and feature interactions without
#'   explicit specification.
#' * It is robust to noisy or collinear predictors (though variable selection
#'   via [prepare_predictors()] is still recommended).
#' * Variable importance (MeanDecreaseGini) provides ecologically interpretable
#'   insights into which climate drivers explain the distribution.
#' * Unlike MaxEnt, it requires true presence/absence or background data, making
#'   it a complementary model in an ensemble strategy.
#'
#' ## Class imbalance
#' Background points typically outnumber presences by 10:1 or more, which biases
#' a classifier toward always predicting "background". Downsampling the majority
#' class (`balance = TRUE`) corrects this and generally improves AUC and
#' sensitivity for the presence class.
#'
#' @examples
#' data(spodoptera_model_data)
#'
#' rf_out <- train_rf_model(spodoptera_model_data)
#' rf_out$importance
#'
#' @importFrom randomForest randomForest importance
#' @export
train_rf_model <- function(model_data,
                            predictors = NULL,
                            n_trees    = 500L,
                            test_frac  = 0.25,
                            balance    = TRUE,
                            seed       = 42L,
                            verbose    = TRUE) {

  .check_pkg("randomForest")

  set.seed(seed)

  # ---- Determine predictors --------------------------------------------------
  if (is.null(predictors))
    predictors <- setdiff(names(model_data), c("lon", "lat", "presence"))

  missing_cols <- setdiff(predictors, names(model_data))
  if (length(missing_cols) > 0)
    stop("Predictors not found in model_data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  model_data$presence <- as.factor(model_data$presence)

  # ---- Train / test split (stratified) --------------------------------------
  pres_idx <- which(model_data$presence == 1)
  back_idx <- which(model_data$presence == 0)

  n_pres_test <- max(1L, round(length(pres_idx) * test_frac))
  n_back_test <- max(1L, round(length(back_idx) * test_frac))

  test_idx  <- c(sample(pres_idx, n_pres_test),
                 sample(back_idx, n_back_test))
  train_idx <- setdiff(seq_len(nrow(model_data)), test_idx)

  train_df  <- model_data[train_idx, ]
  test_df   <- model_data[test_idx, ]

  # ---- Balance training set --------------------------------------------------
  if (balance) {
    tr_pres  <- train_df[train_df$presence == 1, ]
    tr_back  <- train_df[train_df$presence == 0, ]
    n_pres   <- nrow(tr_pres)
    n_back   <- nrow(tr_back)

    if (n_back > n_pres) {
      tr_back <- tr_back[sample(n_back, n_pres), ]
    } else if (n_pres > n_back) {
      tr_pres <- tr_pres[sample(n_pres, n_back), ]
    }
    train_df <- rbind(tr_pres, tr_back)
    if (verbose)
      message("[pestRisk] Balanced training set: ", nrow(tr_pres),
              " presences + ", nrow(tr_back), " background")
  } else {
    if (verbose)
      message("[pestRisk] Training: ", sum(train_df$presence == 1),
              " presences + ", sum(train_df$presence == 0), " background")
  }

  # ---- Train Random Forest ---------------------------------------------------
  if (verbose) message("[pestRisk] Training Random Forest (", n_trees, " trees) …")

  form <- as.formula(paste("presence ~", paste(predictors, collapse = " + ")))

  rf_model <- randomForest::randomForest(
    form,
    data       = train_df,
    ntree      = n_trees,
    importance = TRUE
  )

  # ---- Variable importance ---------------------------------------------------
  imp_mat <- randomForest::importance(rf_model, type = 2)  # MeanDecreaseGini
  imp_df  <- data.frame(
    variable   = rownames(imp_mat),
    importance = imp_mat[, 1],
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  imp_df <- imp_df[order(imp_df$importance, decreasing = TRUE), ]

  if (verbose) {
    message("[pestRisk] Variable importance (MeanDecreaseGini):")
    print(imp_df, row.names = FALSE)
  }

  list(
    model      = rf_model,
    importance = imp_df,
    train_data = train_df,
    test_data  = test_df,
    predictors = predictors,
    type       = "rf"
  )
}
