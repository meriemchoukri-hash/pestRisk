# =============================================================================
# prepare_predictors.R
# =============================================================================

#' Prepare and select environmental predictors for SDM
#'
#' @description
#' Extracts bioclimatic values at occurrence and background locations, then
#' applies a two-step variable selection to remove redundant predictors:
#'
#' 1. **Pearson correlation filter** — removes one variable from each pair
#'    whose |r| exceeds a threshold (keeps the one with lower mean correlation
#'    to all others).
#' 2. **VIF stepwise elimination** — iteratively removes the variable with the
#'    highest Variance Inflation Factor until all remaining VIFs fall below a
#'    user-defined threshold.
#'
#' @param occurrences_sf `sf` object. Cleaned presence points, output of
#'   [clean_occurrences()]`$data`.
#' @param background_sf `sf` object. Background / pseudo-absence points, output
#'   of [generate_background_points()].
#' @param clim_stack `SpatRaster`. WorldClim bioclimatic layers, output of
#'   [download_worldclim()].
#' @param cor_threshold `numeric` in (0, 1). Absolute Pearson |r| above which
#'   one variable of a pair is removed (default `0.75`).
#' @param vif_threshold `numeric`. Maximum accepted VIF for retained variables
#'   (default `5`; some authors use `10`).
#' @param verbose `logical`. Print selection steps (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`model_data`}{`data.frame` with columns: `lon`, `lat`, `presence`
#'     (1 = presence, 0 = background), plus one column per selected BIO variable.}
#'   \item{`selected_vars`}{`character` vector of kept variable names.}
#'   \item{`removed_cor`}{Variables removed by Pearson filter.}
#'   \item{`removed_vif`}{Variables removed by VIF filter, with their VIF values.}
#'   \item{`vif_final`}{VIF values of the final selected variables.}
#'   \item{`cor_matrix`}{Full Pearson correlation matrix (before filtering).}
#' }
#'
#' @details
#' ## Why remove multicollinear variables?
#' Highly correlated predictors inflate model uncertainty and make coefficient
#' interpretation unreliable. In an SDM context, multicollinearity can cause a
#' model to arbitrarily allocate importance between two nearly identical variables,
#' reducing transferability and ecological interpretability (Dormann *et al.* 2013).
#'
#' The stepwise VIF approach (Zuur *et al.* 2010) is preferred over selecting
#' variables *a priori* because it is data-driven and avoids introducing domain
#' assumptions that may not hold for every species.
#'
#' @references
#' Dormann *et al.* (2013) Collinearity: a review of methods to deal with it
#' and a simulation study evaluating their performance. *Ecography* **36**, 27–46.
#'
#' Zuur *et al.* (2010) A protocol for data exploration to avoid common
#' statistical problems. *Methods Ecol. Evol.* **1**, 3–14.
#'
#' @examples
#' data(spodoptera_occ)
#' data(pest_background)
#' data(bio_sample)
#'
#' prep <- prepare_predictors(
#'   occurrences_sf = spodoptera_occ,
#'   background_sf  = pest_background,
#'   clim_stack     = bio_sample
#' )
#' cat("Selected variables:", prep$selected_vars, "\n")
#' head(prep$model_data)
#'
#' @importFrom terra extract
#' @importFrom sf st_coordinates
#' @export
prepare_predictors <- function(occurrences_sf,
                                background_sf,
                                clim_stack,
                                cor_threshold = 0.75,
                                vif_threshold = 5,
                                verbose       = TRUE) {

  .check_pkg("terra")
  .check_pkg("sf")

  # ---- Extract climate values at all points ----------------------------------
  .extract_clim <- function(sf_pts, stack) {
    xy    <- sf::st_coordinates(sf_pts)
    vals  <- terra::extract(stack, xy)[, -1, drop = FALSE]  # drop ID col
    cbind(data.frame(lon = xy[, 1], lat = xy[, 2]), vals)
  }

  if (verbose) message("[pestRisk] Extracting climate values at occurrence points …")
  pres_df <- .extract_clim(occurrences_sf, clim_stack)
  pres_df$presence <- 1L

  if (verbose) message("[pestRisk] Extracting climate values at background points …")
  back_df <- .extract_clim(background_sf, clim_stack)
  back_df$presence <- 0L

  # Combined dataset
  all_df <- rbind(pres_df, back_df)

  # Remove rows with any NA in climate variables
  clim_cols <- names(clim_stack)
  n_before  <- nrow(all_df)
  all_df    <- all_df[complete.cases(all_df[, clim_cols]), ]
  if (verbose && nrow(all_df) < n_before)
    message("[pestRisk] Removed ", n_before - nrow(all_df),
            " rows with NA climate values (likely ocean pixels).")

  # ---- Climate matrix for selection -----------------------------------------
  clim_mat <- as.matrix(all_df[, clim_cols])

  # ---- Step 1: Pearson correlation filter ------------------------------------
  cor_mat      <- cor(clim_mat, use = "pairwise.complete.obs")
  removed_cor  <- character(0)
  active_vars  <- clim_cols

  if (verbose) message("[pestRisk] Step 1 – Pearson filter (|r| > ", cor_threshold, ")")

  repeat {
    sub_cor <- cor_mat[active_vars, active_vars, drop = FALSE]
    # Find the pair with highest absolute correlation (excluding diagonal)
    diag(sub_cor) <- NA
    max_cor <- max(abs(sub_cor), na.rm = TRUE)
    if (max_cor <= cor_threshold) break

    # Identify the two variables in that pair
    idx  <- which(abs(sub_cor) == max_cor, arr.ind = TRUE)[1, ]
    v1   <- rownames(sub_cor)[idx[1]]
    v2   <- colnames(sub_cor)[idx[2]]

    # Remove the one with higher mean absolute correlation to all others
    mean_cor_v1 <- mean(abs(cor_mat[v1, active_vars[active_vars != v1]]))
    mean_cor_v2 <- mean(abs(cor_mat[v2, active_vars[active_vars != v2]]))
    to_remove   <- if (mean_cor_v1 >= mean_cor_v2) v1 else v2

    removed_cor <- c(removed_cor, to_remove)
    active_vars <- setdiff(active_vars, to_remove)
    if (verbose)
      message("  Removed '", to_remove, "' (|r| = ", round(max_cor, 3),
              " with '", if (to_remove == v1) v2 else v1, "')")
  }

  if (verbose) message("[pestRisk]   Remaining after Pearson: ", active_vars)

  # ---- Step 2: VIF stepwise elimination --------------------------------------
  if (verbose) message("[pestRisk] Step 2 – VIF stepwise elimination (threshold = ",
                       vif_threshold, ")")

  removed_vif <- data.frame(variable = character(), VIF = numeric(),
                             stringsAsFactors = FALSE)

  repeat {
    sub_mat <- clim_mat[, active_vars, drop = FALSE]
    vif_now <- .compute_vif(sub_mat)
    if (all(vif_now <= vif_threshold, na.rm = TRUE)) break

    worst_var <- names(which.max(vif_now))
    removed_vif <- rbind(removed_vif,
                         data.frame(variable = worst_var,
                                    VIF = round(vif_now[worst_var], 2)))
    active_vars <- setdiff(active_vars, worst_var)

    if (verbose)
      message("  Removed '", worst_var, "' (VIF = ", round(vif_now[worst_var], 2), ")")

    if (length(active_vars) < 2) break
  }

  vif_final <- .compute_vif(clim_mat[, active_vars, drop = FALSE])

  if (verbose) {
    message("[pestRisk] Final selected variables (", length(active_vars), "):")
    for (v in active_vars)
      message("  ", v, "  VIF = ", round(vif_final[v], 2))
  }

  # ---- Build model dataset with selected variables only ----------------------
  model_data <- all_df[, c("lon", "lat", "presence", active_vars)]

  list(
    model_data    = model_data,
    selected_vars = active_vars,
    removed_cor   = removed_cor,
    removed_vif   = removed_vif,
    vif_final     = round(vif_final, 3),
    cor_matrix    = round(cor_mat, 3)
  )
}
