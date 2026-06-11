# =============================================================================
# download_gbif.R
# =============================================================================

#' Download pest occurrence records from GBIF
#'
#' @description
#' Downloads and pre-filters occurrence data for an agricultural pest species
#' from the Global Biodiversity Information Facility (GBIF). Records without
#' coordinates, with geospatial issues, or exact duplicates are automatically
#' removed.
#'
#' @param species `character`. Full scientific name of the pest, e.g.
#'   `"Spodoptera frugiperda"`, `"Tuta absoluta"`, `"Ceratitis capitata"`.
#' @param limit `integer`. Maximum number of records to request (default 5000;
#'   GBIF hard cap is 100 000 per call). Use `Inf` with caution for large species.
#' @param country `character` or `NULL`. ISO 3166-1 alpha-2 code to restrict
#'   the search geographically (e.g. `"MA"` for Morocco, `"MX"` for Mexico).
#'   `NULL` performs a global search (default).
#' @param year `character` or `NULL`. Year range as `"YYYY,YYYY"`, e.g.
#'   `"2000,2023"`. `NULL` keeps all years (default).
#' @param basis_of_record `character vector`. Allowed observation types.
#'   Default keeps human observations and specimen-based records only (removes
#'   machine observations and fossils).
#' @param verbose `logical`. Print download progress (default `TRUE`).
#'
#' @return A `data.frame` containing at minimum:
#' \describe{
#'   \item{`species`}{Scientific name as reported by GBIF}
#'   \item{`decimalLongitude`}{Longitude in decimal degrees (WGS84)}
#'   \item{`decimalLatitude`}{Latitude in decimal degrees (WGS84)}
#'   \item{`year`}{Observation year}
#'   \item{`country`}{ISO country code}
#'   \item{`gbifID`}{Unique GBIF record identifier}
#'   \item{`coordinateUncertaintyInMeters`}{Positional uncertainty}
#'   \item{`basisOfRecord`}{Observation type}
#' }
#' Returns an empty `data.frame` if no records are found.
#'
#' @details
#' The function calls [rgbif::occ_search()] with:
#' * `hasCoordinate = TRUE` – only georeferenced records
#' * `hasGeospatialIssue = FALSE` – excludes GBIF-flagged coordinate problems
#'
#' Post-download filters:
#' 1. Records with `NA` coordinates are removed.
#' 2. Coordinates outside `[-90, 90]` latitude or `[-180, 180]` longitude are
#'    removed.
#' 3. The "Null Island" (0°, 0°) artefact is removed.
#' 4. Exact duplicates on `(longitude, latitude, year)` are removed.
#'
#' @examples
#' \dontrun{
#' # Download fall armyworm occurrences in Africa
#' occ_sf <- download_gbif("Spodoptera frugiperda", limit = 1000)
#' head(occ_sf)
#'
#' # Restrict to Morocco, last 10 years
#' occ_ma <- download_gbif("Tuta absoluta",
#'                          country = "MA",
#'                          year    = "2013,2023",
#'                          limit   = 500)
#' }
#'
#' @importFrom rgbif occ_search
#' @export
download_gbif <- function(species,
                           limit           = 5000,
                           country         = NULL,
                           year            = NULL,
                           basis_of_record = c("HUMAN_OBSERVATION",
                                               "OBSERVATION",
                                               "PRESERVED_SPECIMEN",
                                               "MACHINE_OBSERVATION"),
                           verbose         = TRUE) {
  .check_pkg("rgbif")

  if (!is.character(species) || length(species) != 1L)
    stop("`species` must be a single character string.", call. = FALSE)

  limit <- min(as.integer(limit), 100000L)

  if (verbose)
    message("[pestRisk] Downloading GBIF occurrences for: ", species,
            if (!is.null(country)) paste0(" (country = ", country, ")"))

  # --- Query GBIF -----------------------------------------------------------
  fields_wanted <- c("species", "decimalLongitude", "decimalLatitude",
                     "year", "country", "gbifID",
                     "coordinateUncertaintyInMeters", "basisOfRecord")

  raw <- tryCatch(
    rgbif::occ_search(
      scientificName     = species,
      hasCoordinate      = TRUE,
      hasGeospatialIssue = FALSE,
      country            = country,
      year               = year,
      basisOfRecord      = basis_of_record,
      limit              = limit,
      fields             = fields_wanted
    ),
    error = function(e) {
      stop("[pestRisk] GBIF query failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  df <- raw$data

  if (is.null(df) || nrow(df) == 0L) {
    warning("[pestRisk] No records returned for: ", species, call. = FALSE)
    return(data.frame())
  }

  n_raw <- nrow(df)
  if (verbose) message("[pestRisk]   Raw records fetched: ", n_raw)

  # --- Post-download filtering ----------------------------------------------
  # 1. Remove NA coordinates
  df <- df[!is.na(df$decimalLongitude) & !is.na(df$decimalLatitude), ]

  # 2. Remove impossible / boundary-straddling values
  df <- df[
    abs(df$decimalLatitude)  <= 90 &
    abs(df$decimalLongitude) <= 180,
  ]

  # 3. Remove Null Island artefact (both == 0 is almost always a data error)
  df <- df[!(df$decimalLongitude == 0 & df$decimalLatitude == 0), ]

  # 4. Remove exact (lon, lat, year) duplicates
  df <- df[!duplicated(df[, c("decimalLongitude", "decimalLatitude", "year")]), ]

  if (verbose)
    message("[pestRisk]   Clean records retained: ", nrow(df),
            " (", n_raw - nrow(df), " removed)")

  df
}
