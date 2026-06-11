# =============================================================================
# generate_background_points.R
# =============================================================================

#' Generate background (pseudo-absence) points for SDM
#'
#' @description
#' Generates spatially controlled background points within a study area for
#' use in SDMs that require presence/absence data (Random Forest, GLM, XGBoost).
#' Points are sampled from non-`NA` cells of the climate raster and optionally
#' filtered to exclude a buffer zone around known presences, reducing the risk
#' of accidentally placing a "background" point where the species is likely
#' present.
#'
#' @param clim_stack `SpatRaster`. WorldClim raster defining the study area
#'   (only non-NA cells are eligible). Output of [download_worldclim()].
#' @param occurrences_sf `sf` object. Cleaned presence points. Used to define
#'   the exclusion buffer around presences.
#' @param n `integer`. Number of background points to generate (default `10000`,
#'   following Phillips *et al.* 2009 for MaxEnt; for RF use 1:1 or 1:10 ratio
#'   to presences).
#' @param min_dist_km `numeric`. Minimum distance in km from any presence point.
#'   Points closer than this are excluded (default `0` = no exclusion buffer).
#'   Setting `min_dist_km = 50` implements a 50 km sampling bias buffer as
#'   discussed in the SDM literature (Barbet-Massin *et al.* 2012).
#' @param seed `integer`. Random seed for reproducibility (default `42`).
#' @param verbose `logical`. Print progress (default `TRUE`).
#'
#' @return An `sf` object (CRS EPSG:4326) of background points.
#'
#' @details
#' ## Sampling strategy
#' Points are drawn uniformly at random from all land pixels (non-NA in the
#' first layer of `clim_stack`). This is the standard background sampling used
#' by MaxEnt (Phillips & Dudik 2008).
#'
#' ## Exclusion buffer
#' Setting `min_dist_km > 0` removes background points that are geographically
#' very close to presences. This is important when presence records are clustered
#' (e.g., monitoring stations, farms) because such nearby backgrounds could
#' represent environmental conditions very similar to actual presences, blurring
#' the model's discriminative ability.
#'
#' ## Number of points
#' * MaxEnt: 10 000 (default; Phillips *et al.* 2009)
#' * Random Forest: typically 1× to 10× the number of presences
#'   (Barbet-Massin *et al.* 2012)
#'
#' @references
#' Phillips, S.J. *et al.* (2009) Sample selection bias and presence-only
#' distribution models: implications for background and pseudo-absence data.
#' *Ecol. Appl.* **19**, 181–197.
#'
#' Barbet-Massin, M. *et al.* (2012) Selecting pseudo-absences for species
#' distribution models: how, where and how many? *Methods Ecol. Evol.* **3**, 327–338.
#'
#' @examples
#' data(spodoptera_occ)
#' data(bio_sample)
#'
#' bg <- generate_background_points(
#'   clim_stack     = bio_sample,
#'   occurrences_sf = spodoptera_occ,
#'   n              = 1000,
#'   min_dist_km    = 50,
#'   seed           = 42
#' )
#' plot(sf::st_geometry(bg), pch = ".", col = "grey50",
#'      main = "Background points (50 km buffer from presences)")
#' plot(sf::st_geometry(spodoptera_occ), pch = 20, col = "red", add = TRUE)
#'
#' @importFrom terra xyFromCell cells ncell
#' @importFrom sf st_as_sf st_coordinates
#' @export
generate_background_points <- function(clim_stack,
                                        occurrences_sf,
                                        n           = 10000L,
                                        min_dist_km = 0,
                                        seed        = 42L,
                                        verbose     = TRUE) {

  .check_pkg("terra")
  .check_pkg("sf")

  set.seed(seed)

  # ---- Get all non-NA land pixels from first layer of climate stack ----------
  ref_layer  <- clim_stack[[1]]
  land_cells <- terra::cells(ref_layer, terra::is.finite(ref_layer))[[1]]

  if (length(land_cells) == 0)
    stop("[pestRisk] No valid (non-NA) cells found in `clim_stack`.", call. = FALSE)

  if (verbose)
    message("[pestRisk] Available land pixels: ", length(land_cells),
            " | Requesting: ", n)

  # ---- Sample candidate points -----------------------------------------------
  n_sample   <- min(n * 5L, length(land_cells))   # oversample to allow buffer
  sampled_idx <- sample(land_cells, n_sample, replace = FALSE)
  xy_candidates <- terra::xyFromCell(ref_layer, sampled_idx)
  colnames(xy_candidates) <- c("lon", "lat")

  # ---- Apply exclusion buffer around presence points -------------------------
  if (min_dist_km > 0 && !is.null(occurrences_sf)) {
    pres_coords <- sf::st_coordinates(occurrences_sf)
    pres_lon    <- pres_coords[, 1]
    pres_lat    <- pres_coords[, 2]

    if (verbose)
      message("[pestRisk] Applying ", min_dist_km,
              " km exclusion buffer around presences …")

    # For each candidate, compute minimum distance to any presence
    keep <- vapply(seq_len(nrow(xy_candidates)), function(i) {
      dists <- .haversine_km(
        xy_candidates[i, 1], xy_candidates[i, 2],
        pres_lon, pres_lat
      )
      min(dists) >= min_dist_km
    }, logical(1))

    xy_candidates <- xy_candidates[keep, , drop = FALSE]
    if (verbose)
      message("[pestRisk]   Candidates after buffer: ", nrow(xy_candidates))
  }

  # ---- Trim to requested n ---------------------------------------------------
  if (nrow(xy_candidates) < n) {
    warning("[pestRisk] Only ", nrow(xy_candidates),
            " background points could be generated (requested ", n, ").",
            call. = FALSE)
    n <- nrow(xy_candidates)
  }

  xy_final <- xy_candidates[sample(nrow(xy_candidates), n), , drop = FALSE]

  # ---- Convert to sf ---------------------------------------------------------
  bg_sf <- sf::st_as_sf(as.data.frame(xy_final),
                         coords = c("lon", "lat"), crs = 4326)

  if (verbose)
    message("[pestRisk] Generated ", nrow(bg_sf), " background points.")

  bg_sf
}
