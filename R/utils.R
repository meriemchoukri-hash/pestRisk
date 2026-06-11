# =============================================================================
# utils.R – Internal helper functions (not exported)
# =============================================================================

# ---- Spatial thinning (grid-based rarefaction) ----------------------------
#' @noRd
.spatial_thin <- function(df, lon_col, lat_col, min_dist_km) {
  cell_size <- min_dist_km / 111.0   # 1 degree ≈ 111 km
  df$`.grid_lon` <- floor(df[[lon_col]] / cell_size)
  df$`.grid_lat` <- floor(df[[lat_col]] / cell_size)
  df_thin <- df[!duplicated(df[, c(".grid_lon", ".grid_lat")]), ]
  df_thin$`.grid_lon` <- NULL
  df_thin$`.grid_lat` <- NULL
  df_thin
}

# ---- VIF computation (manual, no extra dependency) ------------------------
#' Compute Variance Inflation Factor for each column of a numeric matrix
#' @noRd
.compute_vif <- function(mat) {
  stopifnot(is.matrix(mat) | is.data.frame(mat))
  mat <- as.matrix(mat)
  n   <- ncol(mat)
  vif <- setNames(numeric(n), colnames(mat))
  for (i in seq_len(n)) {
    y  <- mat[, i]
    Xm <- mat[, -i, drop = FALSE]
    if (ncol(Xm) == 0L) {
      vif[i] <- 1
    } else {
      r2    <- summary(lm(y ~ Xm))$r.squared
      vif[i] <- if (r2 >= 1) Inf else 1 / (1 - r2)
    }
  }
  vif
}

# ---- Safe package check ---------------------------------------------------
#' @noRd
.check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required. Install with:\n",
         "  install.packages('", pkg, "')", call. = FALSE)
}

# ---- Haversine distance (km) between one reference and many points --------
#' @noRd
.haversine_km <- function(lon_ref, lat_ref, lon_vec, lat_vec) {
  R   <- 6371
  phi1 <- lat_ref * pi / 180
  phi2 <- lat_vec * pi / 180
  dphi <- (lat_vec - lat_ref) * pi / 180
  dlam <- (lon_vec - lon_ref) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# ---- Pretty risk classification -------------------------------------------
#' @noRd
.classify_risk <- function(x, breaks = c(0, 0.33, 0.66, 1)) {
  cut(x,
      breaks = breaks,
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE)
}

# ---- Safely get coordinate columns from sf object -------------------------
#' @noRd
.sf_to_coords <- function(sf_obj) {
  crds <- sf::st_coordinates(sf_obj)
  data.frame(lon = crds[, 1], lat = crds[, 2])
}

# ---- Default bioclimatic variables (most relevant for insect pests) -------
#' @noRd
.default_bio_vars <- function() c(1, 4, 5, 6, 12, 15)
# BIO1  = Annual Mean Temperature
# BIO4  = Temperature Seasonality (coefficient of variation)
# BIO5  = Max Temperature of Warmest Month
# BIO6  = Min Temperature of Coldest Month
# BIO12 = Annual Precipitation
# BIO15 = Precipitation Seasonality (coefficient of variation)
