# =============================================================================
# summarize_country_risk.R
# =============================================================================

#' Summarise pest invasion risk by country or administrative region
#'
#' @description
#' Extracts the mean, maximum, and proportion of high-risk pixels from a
#' suitability raster for each country or region polygon. Returns a ranked
#' table of the most vulnerable zones, which can be used to prioritise
#' phytosanitary surveillance and border inspection efforts.
#'
#' @param risk_map_obj `list`. Output of [predict_risk_map()].
#' @param regions `sf` object or `NULL`. Administrative boundaries to aggregate
#'   over. If `NULL`, world country boundaries are downloaded automatically
#'   from [geodata::world()].
#' @param name_col `character`. Column in `regions` containing region names
#'   (default `"NAME_0"` — the country name column from geodata world data).
#' @param top_n `integer`. Return only the `n` highest-risk regions (default
#'   `20`). Set to `Inf` for all.
#' @param verbose `logical`. Print ranked table (default `TRUE`).
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{`region`}{Region / country name.}
#'   \item{`mean_suitability`}{Mean predicted suitability in [0, 1].}
#'   \item{`max_suitability`}{Maximum suitability pixel.}
#'   \item{`pct_high_risk`}{Percentage of pixels classified as High risk (> 0.66).}
#'   \item{`n_pixels`}{Number of non-NA pixels in the region.}
#'   \item{`risk_rank`}{Rank from highest to lowest mean suitability.}
#' }
#'
#' @examples
#' \dontrun{
#' risk <- predict_risk_map(rf_out, bio_africa)
#' summary_tbl <- summarize_country_risk(risk, top_n = 15)
#' print(summary_tbl)
#' }
#'
#' @importFrom terra extract
#' @importFrom sf st_as_sf
#' @importFrom geodata world
#' @importFrom dplyr arrange desc mutate
#' @export
summarize_country_risk <- function(risk_map_obj,
                                    regions  = NULL,
                                    name_col = "NAME_0",
                                    top_n    = 20L,
                                    verbose  = TRUE) {

  .check_pkg("terra")
  .check_pkg("sf")

  suit <- risk_map_obj$suitability
  breaks <- risk_map_obj$breaks

  # ---- Load world boundaries if not provided ---------------------------------
  if (is.null(regions)) {
    .check_pkg("geodata")
    if (verbose) message("[pestRisk] Downloading world boundaries …")
    regions <- sf::st_as_sf(geodata::world(resolution = 3, path = tempdir()))
  }

  # ---- Extract suitability values per region --------------------------------
  if (verbose) message("[pestRisk] Aggregating suitability by region …")

  regions_v <- terra::vect(regions)
  extracted <- terra::extract(suit, regions_v, fun = NULL, na.rm = TRUE)

  # Compute per-region statistics
  stats_list <- lapply(
    split(extracted$suitability, extracted$ID),
    function(vals) {
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(NULL)
      data.frame(
        mean_suitability = round(mean(vals),                   4),
        max_suitability  = round(max(vals),                    4),
        pct_high_risk    = round(100 * mean(vals > breaks[2]), 2),
        n_pixels         = length(vals)
      )
    }
  )

  # Match back to region names
  valid_ids   <- as.integer(names(stats_list))
  region_names <- regions[[name_col]][valid_ids]

  stats_df <- do.call(rbind, stats_list[!sapply(stats_list, is.null)])
  stats_df$region    <- region_names[!sapply(stats_list, is.null)]
  stats_df$risk_rank <- rank(-stats_df$mean_suitability, ties.method = "min")
  stats_df <- stats_df[order(stats_df$risk_rank), ]

  # Trim to top_n
  if (is.finite(top_n)) stats_df <- stats_df[seq_len(min(top_n, nrow(stats_df))), ]
  rownames(stats_df) <- NULL

  if (verbose) {
    message("\n=== Top ", min(top_n, nrow(stats_df)),
            " highest-risk regions ===")
    print(stats_df[, c("risk_rank", "region", "mean_suitability",
                        "pct_high_risk")], row.names = FALSE)
  }

  stats_df
}
