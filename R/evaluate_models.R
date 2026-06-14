# =============================================================================
# evaluate_models.R
# =============================================================================

#' Evaluate SDM model performance
#'
#' @description
#' Evaluates one or more fitted SDM models (Random Forest or MaxEnt) on a
#' held-out test set. Computes AUC, Accuracy, Sensitivity (Recall), Specificity,
#' and F1-score. Returns a performance table and optionally plots ROC curves.
#'
#' @param ... One or more model objects returned by [train_rf_model()] or
#'   [train_maxent()]. Multiple models are evaluated and compared side-by-side.
#' @param model_names `character vector`. Labels for each model (e.g.
#'   `c("Random Forest", "MaxEnt")`). Defaults to `"RF"`, `"MaxEnt"`, etc.
#' @param threshold `numeric` in (0, 1). Decision threshold to convert
#'   continuous suitability predictions into binary presence/absence for
#'   Accuracy, Sensitivity, Specificity, and F1 (default `0.5`).
#' @param plot_roc `logical`. If `TRUE`, generates and returns a ROC curve
#'   ggplot (default `TRUE`).
#' @param verbose `logical`. Print performance table (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`metrics`}{`data.frame` with columns: `Model`, `AUC`, `Accuracy`,
#'     `Sensitivity`, `Specificity`, `F1`.}
#'   \item{`roc_plot`}{A `ggplot` object showing ROC curve(s), or `NULL` if
#'     `plot_roc = FALSE`.}
#'   \item{`confusion_matrices`}{Named list of confusion matrices per model.}
#' }
#'
#' @details
#' ## Metric definitions
#' Let TP, TN, FP, FN denote true positive, true negative, false positive,
#' and false negative counts respectively.
#'
#' | Metric | Formula | Interpretation in pest risk context |
#' |--------|---------|--------------------------------------|
#' | AUC | Area under ROC | Probability that a random presence is ranked above a random background; 0.5 = random, 1.0 = perfect |
#' | Accuracy | (TP+TN)/(TP+TN+FP+FN) | Overall correct classifications |
#' | Sensitivity | TP/(TP+FN) | Ability to detect true pest habitats (false negatives = missed invasions) |
#' | Specificity | TN/(TN+FP) | Ability to correctly identify non-habitat areas |
#' | F1 | 2·Prec·Recall/(Prec+Recall) | Harmonic mean; useful under imbalance |
#'
#' For pest surveillance, **Sensitivity is often more important than Specificity**:
#' a false negative (missed invasion risk) is more costly than a false positive
#' (unnecessary monitoring effort).
#'
#' ## AUC interpretation
#' * AUC > 0.9 — Excellent
#' * AUC 0.8–0.9 — Good
#' * AUC 0.7–0.8 — Fair
#' * AUC < 0.7 — Poor (model barely better than random)
#'
#' @examples
#' data(spodoptera_model_data)
#' rf_out <- train_rf_model(spodoptera_model_data)
#' mx_out <- train_maxent(spodoptera_model_data)
#'
#' eval <- evaluate_models(rf_out, mx_out,
#'                          model_names = c("Random Forest", "MaxEnt"))
#' print(eval$metrics)
#' print(eval$roc_plot)
#'
#' @importFrom pROC roc auc
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal theme
#'   element_text scale_colour_manual
#' @export
evaluate_models <- function(...,
                             model_names = NULL,
                             threshold   = 0.5,
                             plot_roc    = TRUE,
                             verbose     = TRUE) {

  .check_pkg("pROC")
  .check_pkg("ggplot2")

  models <- list(...)
  if (length(models) == 0)
    stop("At least one model must be provided.", call. = FALSE)

  # Default model names
  if (is.null(model_names)) {
    model_names <- vapply(models, function(m) {
      switch(m$type, rf = "RandomForest", maxent = "MaxEnt", "Model")
    }, character(1))
    # Make unique
    model_names <- make.unique(model_names, sep = "_")
  }

  metrics_list <- list()
  cm_list      <- list()
  roc_data     <- list()

  for (i in seq_along(models)) {
    mod  <- models[[i]]
    mname <- model_names[i]
    test_df <- mod$test_data

    # ---- Get predictions on test data ----------------------------------------
    probs <- .predict_model(mod, test_df)

    truth <- as.integer(as.character(test_df$presence))

    # ---- AUC -----------------------------------------------------------------
    roc_obj <- pROC::roc(truth, probs, direction = "<", quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))

    roc_data[[mname]] <- data.frame(
      model       = mname,
      specificity = 1 - roc_obj$specificities,   # FPR = 1 - specificity
      sensitivity = roc_obj$sensitivities
    )

    # ---- Binary classification metrics at threshold --------------------------
    pred_bin <- as.integer(probs >= threshold)

    TP <- sum(pred_bin == 1 & truth == 1)
    TN <- sum(pred_bin == 0 & truth == 0)
    FP <- sum(pred_bin == 1 & truth == 0)
    FN <- sum(pred_bin == 0 & truth == 1)

    accuracy    <- (TP + TN) / (TP + TN + FP + FN)
    sensitivity <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
    specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
    precision   <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
    f1          <- if (!is.na(precision) && !is.na(sensitivity) &&
                       (precision + sensitivity) > 0)
                     2 * precision * sensitivity / (precision + sensitivity)
                   else NA_real_

    metrics_list[[i]] <- data.frame(
      Model       = mname,
      AUC         = round(auc_val,    3),
      Accuracy    = round(accuracy,   3),
      Sensitivity = round(sensitivity, 3),
      Specificity = round(specificity, 3),
      F1          = round(f1,         3),
      stringsAsFactors = FALSE
    )

    cm_list[[mname]] <- matrix(
      c(TP, FP, FN, TN), nrow = 2,
      dimnames = list(Predicted = c("1", "0"), Actual = c("1", "0"))
    )
  }

  metrics_df <- do.call(rbind, metrics_list)

  if (verbose) {
    message("\n======= pestRisk | Model Evaluation =======")
    print(metrics_df, row.names = FALSE)
    message("===========================================\n")
  }

  # ---- ROC plot --------------------------------------------------------------
  roc_plot <- NULL
  if (plot_roc) {
    roc_df <- do.call(rbind, roc_data)

    roc_plot <- ggplot2::ggplot(roc_df,
                                 ggplot2::aes(x = specificity, y = sensitivity,
                                              colour = model)) +
      ggplot2::geom_line(size = 1.1) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                            linetype = "dashed", colour = "grey50") +
      ggplot2::labs(
        title    = "ROC Curves – SDM performance",
        subtitle = paste("Threshold =", threshold),
        x        = "1 – Specificity (False Positive Rate)",
        y        = "Sensitivity (True Positive Rate)",
        colour   = "Model"
      ) +
      ggplot2::annotate("text",
                         x = 0.75, y = 0.1,
                         label = paste(model_names, "AUC =",
                                       metrics_df$AUC,
                                       collapse = "\n"),
                         hjust = 0, size = 3.5) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom")
  }

  list(
    metrics            = metrics_df,
    roc_plot           = roc_plot,
    confusion_matrices = cm_list
  )
}

# ---- Internal: predict from any model type ----------------------------------
#' @noRd
.predict_model <- function(mod, newdata) {
  preds <- mod$predictors
  if (mod$type == "rf") {
    probs <- predict(mod$model, newdata = newdata, type = "prob")[, "1"]
  } else if (mod$type == "maxent") {
    X     <- as.data.frame(newdata[, preds, drop = FALSE])
    probs <- as.numeric(predict(mod$model, X, type = "logistic"))
  } else {
    stop("Unknown model type: ", mod$type, call. = FALSE)
  }
  as.numeric(probs)
}
