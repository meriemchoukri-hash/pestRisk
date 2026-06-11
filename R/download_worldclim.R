# =============================================================================
# download_worldclim.R
# =============================================================================

#' Download WorldClim bioclimatic variables
#'
#' @description
#' Downloads the 19 bioclimatic variables (BIO1–BIO19) from WorldClim v2.1
#' at a user-specified resolution, optionally cropping and masking the raster
#' stack to a study area extent. Returns a `SpatRaster` (terra) object ready
#' for SDM variable extraction.
#'
#' @param path `character`. Local directory to store the downloaded files
#'   (default: `tempdir()`). Using a persistent directory avoids re-downloading
#'   on subsequent calls.
#' @param res `character`. Spatial resolution: `"10"` (≈ 20 km, default),
#'   `"5"` (≈ 10 km), `"2.5"` (≈ 5 km), or `"0.5"` (≈ 1 km).
#'   Higher resolutions produce larger files and longer download times.
#' @param extent `numeric vector` of length 4 or `NULL`. Bounding box
#'   `c(xmin, xmax, ymin, ymax)` in decimal degrees (WGS84) to crop the
#'   raster. `NULL` keeps the global extent (default).
#' @param vars `integer vector`. BIO variable indices to keep after download
#'   (1–19). Default: `c(1, 4, 5, 6, 12, 15)` — the six variables most
#'   predictive for agricultural insect pests (temperature mean/extremes/
#'   seasonality and precipitation annual/seasonality).
#' @param verbose `logical`. Print download progress (default `TRUE`).
#'
#' @return A named `SpatRaster` (package `terra`) where each layer is one BIO
#'   variable (e.g. `BIO1`, `BIO4`, …). Layer names follow the convention
#'   `wc2.1_<res>min_bio_<n>`.
#'
#' @details
#' ## Variable selection rationale for insect pests
#' | Variable | Ecological meaning |
#' |----------|--------------------|
#' | BIO1  | Annual Mean Temperature — drives metabolic rate |
#' | BIO4  | Temperature Seasonality — limits overwintering |
#' | BIO5  | Max Temperature Warmest Month — heat stress |
#' | BIO6  | Min Temperature Coldest Month — cold kill threshold |
#' | BIO12 | Annual Precipitation — habitat moisture |
#' | BIO15 | Precipitation Seasonality — drought stress |
#'
#' The complete BIO1–19 set is downloaded internally; only the selected subset
#' is returned, reducing memory footprint.
#'
#' @references
#' Fick, S.E. & Hijmans, R.J. (2017) WorldClim 2: new 1-km spatial resolution
#' climate surfaces for global land areas. *Int. J. Climatol.* **37**, 4302–4315.
#' <doi:10.1002/joc.5086>
#'
#' @examples
#' \dontrun{
#' # Download at 10 arcmin resolution, cropped to Africa + Mediterranean
#' africa_ext <- c(-20, 55, -40, 40)
#' clim <- download_worldclim(
#'   path   = "~/pestRisk_data",
#'   res    = "10",
#'   extent = africa_ext
#' )
#' terra::plot(clim[["BIO1"]], main = "Annual Mean Temperature (°C × 10)")
#' }
#'
#' @importFrom terra rast crop subset
#' @importFrom geodata worldclim
#' @export
download_worldclim <- function(path    = tempdir(),
                                res     = "10",
                                extent  = NULL,
                                vars    = .default_bio_vars(),
                                verbose = TRUE) {

  .check_pkg("geodata")
  .check_pkg("terra")

  res <- as.character(res)
  if (!res %in% c("0.5", "2.5", "5", "10"))
    stop("`res` must be one of '0.5', '2.5', '5', '10'.", call. = FALSE)

  if (!all(vars %in% 1:19))
    stop("`vars` must be integers between 1 and 19.", call. = FALSE)

  if (verbose)
    message("[pestRisk] Downloading WorldClim v2.1 BIO variables at ",
            res, " arcmin …")

  # ---- Download all 19 BIO variables (geodata caches them) ------------------
  clim <- tryCatch(
    geodata::worldclim_global(var = "bio", res = as.numeric(res), path = path),
    error = function(e) stop("[pestRisk] WorldClim download failed: ",
                             conditionMessage(e), call. = FALSE)
  )

  # ---- Subset to requested variables ----------------------------------------
  layer_idx <- vars
  clim      <- terra::subset(clim, layer_idx)

  # Rename layers for clarity (BIO1, BIO4, …)
  names(clim) <- paste0("BIO", vars)

  # ---- Crop to study area if extent provided --------------------------------
  if (!is.null(extent)) {
    if (length(extent) != 4)
      stop("`extent` must be a numeric vector of length 4: c(xmin, xmax, ymin, ymax).",
           call. = FALSE)
    ext_terra <- terra::ext(extent[1], extent[2], extent[3], extent[4])
    clim      <- terra::crop(clim, ext_terra)
    if (verbose)
      message("[pestRisk] Raster cropped to: xmin=", extent[1],
              " xmax=", extent[2], " ymin=", extent[3], " ymax=", extent[4])
  }

  if (verbose)
    message("[pestRisk] WorldClim stack ready: ", terra::nlyr(clim), " layers, ",
            terra::nrow(clim), " rows × ", terra::ncol(clim), " cols")

  clim
}
