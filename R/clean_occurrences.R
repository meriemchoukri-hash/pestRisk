# =============================================================================
# clean_occurrences.R
# =============================================================================

#' Clean and spatially filter pest occurrence records
#'
#' @description
#' Applies a rigorous, multi-step spatial cleaning pipeline to raw occurrence
#' data. Steps include removal of null / impossible coordinates, deduplication,
#' optional removal of marine / ocean points, outlier detection based on
#' Mahalanobis distance in geographic space, and spatial thinning (rarefaction)
#' to reduce sampling bias.
#'
#' @param occ `data.frame`. Raw occurrence records, typically the output of
#'   [download_gbif()].
#' @param lon_col `character`. Name of the longitude column
#'   (default `"decimalLongitude"`).
#' @param lat_col `character`. Name of the latitude column
#'   (default `"decimalLatitude"`).
#' @param thin_dist `numeric`. Minimum distance in **kilometres** between any
#'   two retained points (spatial rarefaction). Set to `0` to skip (default
#'   `10`). This is a key step to reduce autocorrelation bias in SDMs.
#' @param remove_sea `logical`. Remove points falling in the ocean using world
#'   administrative boundaries from [geodata::world()] (default `TRUE`).
#' @param outlier_sd `numeric` or `NULL`. If provided, points whose geographic
#'   centroid distance exceeds `outlier_sd` standard deviations from the
#'   species' mean location are flagged and removed. Set to `NULL` to skip
#'   (default `3`).
#' @param verbose `logical`. Print the cleaning report table (default `TRUE`).
#'
#' @return A named `list` with:
#' \describe{
#'   \item{`data`}{An `sf` object (CRS EPSG:4326) of cleaned occurrences.}
#'   \item{`report`}{A `data.frame` summarising records removed at each step.}
#' }
#'
#' @details
#' ## Cleaning steps
#' | # | Step | Justification |
#' |---|------|---------------|
#' | 1 | Remove `NA` coordinates | Cannot be mapped |
#' | 2 | Remove impossible / Null-Island coordinates | Data entry errors |
#' | 3 | Remove spatial duplicates | Sampling bias |
#' | 4 | Remove marine / sea points | Terrestrial pest species |
#' | 5 | Detect geographic outliers (Mahalanobis) | Erroneous geo-referencing |
#' | 6 | Spatial thinning at `thin_dist` km | Reduce spatial autocorrelation |
#'
#' ## Why spatial thinning matters for SDMs
#' Clustered occurrences (e.g. many records from a single farm or monitoring
#' station) bias the model toward locally over-sampled conditions. Thinning
#' ensures each retained point represents a roughly independent environmental
#' observation (Aiello-Lammens *et al.* 2015).
#'
#' @references
#' Aiello-Lammens *et al.* (2015) spThin: an R package for spatial thinning of
#' species occurrence records for use in ecological niche models.
#' *Ecography* **38**, 541–545.
#'
#' @examples
#' data(spodoptera_occ)
#' result <- clean_occurrences(spodoptera_occ, thin_dist = 20)
#' print(result$report)
#' plot(sf::st_geometry(result$data), pch = 20, col = "firebrick",
#'      main = "Cleaned Spodoptera frugiperda occurrences")
#'
#' @importFrom sf st_as_sf st_intersects st_geometry
#' @importFrom geodata world
#' @export
clean_occurrences <- function(occ,
                               lon_col    = "decimalLongitude",
                               lat_col    = "decimalLatitude",
                               thin_dist  = 10,
                               remove_sea = TRUE,
                               outlier_sd = 3,
                               verbose    = TRUE) {

  # ---- Input validation -------------------------------------------------------
  if (!is.data.frame(occ))
    stop("`occ` must be a data.frame.", call. = FALSE)
  if (!lon_col %in% names(occ))
    stop("Column '", lon_col, "' not found in `occ`.", call. = FALSE)
  if (!lat_col %in% names(occ))
    stop("Column '", lat_col, "' not found in `occ`.", call. = FALSE)

  # ---- Initialise report table -----------------------------------------------
  .rec <- function(step, n_before, n_after) {
    data.frame(
      step        = step,
      n_removed   = n_before - n_after,
      n_remaining = n_after,
      stringsAsFactors = FALSE
    )
  }
  report <- data.frame(step = character(), n_removed = integer(),
                       n_remaining = integer(), stringsAsFactors = FALSE)

  n0 <- nrow(occ)

  # ---- Step 1: Remove NA coordinates ------------------------------------------
  occ <- occ[!is.na(occ[[lon_col]]) & !is.na(occ[[lat_col]]), ]
  report <- rbind(report, .rec("1 – Remove NA coordinates", n0, nrow(occ)))

  # ---- Step 2: Remove impossible / Null Island --------------------------------
  n1 <- nrow(occ)
  occ <- occ[
    abs(occ[[lat_col]])  <= 90 &
    abs(occ[[lon_col]]) <= 180 &
    !(round(occ[[lon_col]], 3) == 0 & round(occ[[lat_col]], 3) == 0),
  ]
  report <- rbind(report,
                  .rec("2 – Remove impossible / Null Island", n1, nrow(occ)))

  # ---- Step 3: Remove spatial duplicates (4 decimal places ≈ 11 m) -----------
  n2 <- nrow(occ)
  occ <- occ[!duplicated(round(occ[, c(lon_col, lat_col)], 4)), ]
  report <- rbind(report,
                  .rec("3 – Remove spatial duplicates (4 d.p.)", n2, nrow(occ)))

  # ---- Step 4: Remove marine / ocean points -----------------------------------
  if (remove_sea && nrow(occ) > 0) {
    .check_pkg("geodata")
    n3 <- nrow(occ)
    tryCatch({
      world_v  <- geodata::world(resolution = 3, path = tempdir())
      world_sf <- sf::st_as_sf(world_v)
      pts_sf   <- sf::st_as_sf(occ,
                                coords = c(lon_col, lat_col), crs = 4326)
      suppressWarnings({
        on_land <- lengths(sf::st_intersects(pts_sf, world_sf)) > 0
      })
      occ <- occ[on_land, ]
      report <- rbind(report,
                      .rec("4 – Remove marine / sea points", n3, nrow(occ)))
    }, error = function(e) {
      warning("[pestRisk] Sea filter skipped (", conditionMessage(e), ")",
              call. = FALSE)
    })
  }

  # ---- Step 5: Outlier detection (geographic Mahalanobis distance) -----------
  if (!is.null(outlier_sd) && nrow(occ) >= 10) {
    n4 <- nrow(occ)
    coords  <- as.matrix(occ[, c(lon_col, lat_col)])
    mu      <- colMeans(coords)
    S       <- cov(coords)
    if (det(S) > 1e-10) {                       # only if matrix is invertible
      mahal   <- mahalanobis(coords, mu, S)
      thr     <- qchisq(pnorm(outlier_sd)^2, df = 2)
      occ     <- occ[mahal <= thr, ]
      report  <- rbind(report,
                       .rec(paste0("5 – Geographic outliers (", outlier_sd, " SD)"),
                            n4, nrow(occ)))
    }
  }

  # ---- Step 6: Spatial thinning (grid-based rarefaction) ---------------------
  if (thin_dist > 0 && nrow(occ) > 1) {
    n5  <- nrow(occ)
    occ <- .spatial_thin(occ, lon_col, lat_col, thin_dist)
    report <- rbind(report,
                    .rec(paste0("6 – Spatial thinning (", thin_dist, " km)"),
                         n5, nrow(occ)))
  }

  # ---- Convert to sf EPSG:4326 -----------------------------------------------
  occ_sf <- sf::st_as_sf(occ, coords = c(lon_col, lat_col), crs = 4326)

  # ---- Print report ----------------------------------------------------------
  if (verbose) {
    message("\n========= pestRisk | Cleaning Report =========")
    message("Initial records : ", n0)
    print(report, row.names = FALSE)
    message("Final records   : ", nrow(occ_sf))
    message("==============================================\n")
  }

  list(data = occ_sf, report = report)
}
