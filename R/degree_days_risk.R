# =============================================================================
# degree_days_risk.R  – NOUVELLE FONCTIONNALITÉ #2
# =============================================================================

#' Compute a thermal degree-day risk index for pest development
#'
#' @description
#' Calculates the number of **growing degree-days (GDD)** accumulated above a
#' species-specific lower thermal threshold (T_base) and below an upper lethal
#' threshold (T_max), using WorldClim mean monthly temperature data. The GDD
#' index is a direct measure of the pest's capacity to complete one or more
#' generations per year — a key indicator of invasion intensity.
#'
#' This is a **new feature** connecting the SDM workflow to agronomic
#' phenology: whereas [predict_risk_map()] tells us *where* the pest can
#' survive, `degree_days_risk()` tells us *how quickly* it can reproduce.
#'
#' @param clim_stack `SpatRaster`. WorldClim monthly mean temperature raster
#'   stack (12 layers, one per month). Download with
#'   `geodata::worldclim_global(var = "tavg", ...)`.
#' @param t_base `numeric`. Lower developmental threshold in °C (default
#'   `12.5` for *Spodoptera frugiperda*; use `10.0` for *Tuta absoluta*;
#'   use `10.0` for *Ceratitis capitata*).
#' @param t_upper `numeric`. Upper lethal temperature threshold in °C above
#'   which development stops (default `40.0`). Values above this are capped.
#' @param n_gen_threshold `numeric`. Minimum GDD required to complete one
#'   generation. Used to classify the number of potential generations per year.
#'   Default `450` degree-days (typical for *S. frugiperda*).
#' @param verbose `logical`. Print summary statistics (default `TRUE`).
#'
#' @return A named `list`:
#' \describe{
#'   \item{`gdd_annual`}{`SpatRaster`. Total annual growing degree-days (GDD)
#'     above T_base and below T_upper. Higher values = more pest development.}
#'   \item{`n_generations`}{`SpatRaster`. Estimated number of complete pest
#'     generations per year (= GDD / `n_gen_threshold`, floored to integer).}
#'   \item{`risk_class`}{`SpatRaster`. Categorical risk:
#'     1 = 0 generations (Low), 2 = 1–2 gen. (Medium), 3 = ≥3 gen. (High).}
#'   \item{`t_base`}{The T_base value used.}
#'   \item{`n_gen_threshold`}{The threshold used.}
#' }
#'
#' @details
#' ## Degree-day calculation
#' For each month m:
#' \deqn{GDD_m = \max(0,\ \min(T_{mean,m},\ T_{upper}) - T_{base}) \times 30}
#' The annual GDD is the sum over all 12 months. This is the simplified
#' "single sine" method widely used in applied entomology.
#'
#' ## Species-specific parameters
#' | Species | T_base (°C) | GDD/generation | Source |
#' |---------|-------------|----------------|--------|
#' | *Spodoptera frugiperda* | 12.5 | 450 | Barfield et al. 1978 |
#' | *Tuta absoluta* | 8.0  | 340 | Urbaneja et al. 2012 |
#' | *Ceratitis capitata* | 10.0 | 300 | Tassan et al. 1983 |
#'
#' ## Why this matters for Morocco
#' In coastal regions (Souss-Massa, Gharb), warm temperatures may allow
#' *S. frugiperda* to complete 4–5 generations per year, making chemical
#' control extremely difficult. The inland High Atlas acts as a natural
#' barrier (< 0 generations) due to cold winters.
#'
#' @references
#' Barfield, C.S. *et al.* (1978). A temperature-dependent model for fall
#' armyworm development. *Ann. Entomol. Soc. Am.* **71**, 70–74.
#'
#' Berzitis, E.A. *et al.* (2013). Degree-day models for predicting insect
#' pest phenology in agricultural systems. *J. Pest Sci.* **88**, 1-12.
#'
#' @examples
#' \dontrun{
#' # Download monthly mean temperature for Africa
#' tavg <- geodata::worldclim_global(var = "tavg", res = 10,
#'                                    path = "~/pestRisk_data")
#' africa_ext <- c(-20, 55, -40, 40)
#' tavg_af    <- terra::crop(tavg, terra::ext(africa_ext))
#'
#' # Compute degree-days for Spodoptera frugiperda
#' dd <- degree_days_risk(tavg_af, t_base = 12.5, n_gen_threshold = 450)
#'
#' terra::plot(dd$n_generations,
#'             main = "Potential generations/year – S. frugiperda",
#'             col  = c("grey90","#F39C12","#C0392B"))
#'
#' # For Tuta absoluta (lower thermal threshold)
#' dd_tuta <- degree_days_risk(tavg_af, t_base = 8.0, n_gen_threshold = 340)
#' }
#'
#' @importFrom terra lapp app classify
#' @export
degree_days_risk <- function(clim_stack,
                              t_base         = 12.5,
                              t_upper        = 40.0,
                              n_gen_threshold = 450,
                              verbose        = TRUE) {

  .check_pkg("terra")

  if (terra::nlyr(clim_stack) != 12)
    stop("`clim_stack` must have exactly 12 layers (one per month).",
         call. = FALSE)

  if (verbose)
    message(sprintf(
      "[pestRisk] Computing GDD | T_base = %.1f°C | T_upper = %.1f°C | GDD/gen = %d",
      t_base, t_upper, n_gen_threshold
    ))

  # ---- Step 1: Compute monthly GDD ------------------------------------------
  # For each monthly mean temperature layer:
  #   GDD_month = max(0, min(T_mean, T_upper) - T_base) * 30 days
  gdd_monthly <- terra::lapp(
    clim_stack,
    fun = function(x) pmax(0, pmin(x, t_upper) - t_base) * 30
  )

  # ---- Step 2: Sum to annual GDD --------------------------------------------
  gdd_annual        <- terra::app(gdd_monthly, sum)
  names(gdd_annual) <- "annual_GDD"

  # ---- Step 3: Estimate number of generations per year ----------------------
  n_gen <- terra::lapp(
    gdd_annual,
    fun = function(x) floor(x / n_gen_threshold)
  )
  names(n_gen) <- "n_generations"

  # ---- Step 4: Classify into risk categories --------------------------------
  # 0 gen = Low | 1-2 gen = Medium | >=3 gen = High
  rcl <- matrix(
    c(-Inf, 0,   1,
      0,    2,   2,
      2,    Inf, 3),
    nrow = 3, byrow = TRUE
  )
  risk_class        <- terra::classify(n_gen, rcl, include.lowest = TRUE)
  names(risk_class) <- "gdd_risk_class"
  levels(risk_class) <- data.frame(
    id   = 1:3,
    risk = c("Low (0 gen.)", "Medium (1-2 gen.)", "High (≥3 gen.)")
  )

  # ---- Summary ---------------------------------------------------------------
  if (verbose) {
    gdd_vals <- terra::values(gdd_annual, na.rm = TRUE)
    message(sprintf(
      "[pestRisk] Annual GDD — mean: %.0f | max: %.0f | pixels with ≥1 gen: %.1f%%",
      mean(gdd_vals),
      max(gdd_vals),
      100 * mean(gdd_vals >= n_gen_threshold)
    ))
  }

  list(
    gdd_annual      = gdd_annual,
    n_generations   = n_gen,
    risk_class      = risk_class,
    t_base          = t_base,
    n_gen_threshold = n_gen_threshold
  )
}
