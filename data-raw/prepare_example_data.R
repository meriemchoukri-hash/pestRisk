## data-raw/prepare_example_data.R
## Run this script once to generate the example datasets bundled with pestRisk.
## Execute from the package root: source("data-raw/prepare_example_data.R")

set.seed(123)

# =============================================================================
# 1. spodoptera_occ  – simulated occurrences of Spodoptera frugiperda
#    Core range: sub-Saharan Africa + Central/South America
# =============================================================================
n_africa   <- 120
n_americas <- 80

# Africa cluster (tropical belt)
af_lon <- runif(n_africa, -18, 50)
af_lat <- rnorm(n_africa, mean = 5, sd = 12)
af_lat <- pmax(-30, pmin(18, af_lat))

# Americas cluster
am_lon <- runif(n_americas, -95, -35)
am_lat <- rnorm(n_americas, mean = 5, sd = 15)
am_lat <- pmax(-30, pmin(25, am_lat))

spodoptera_occ <- data.frame(
  species            = "Spodoptera frugiperda",
  decimalLongitude   = c(af_lon, am_lon),
  decimalLatitude    = c(af_lat, am_lat),
  year               = sample(2010:2023, n_africa + n_americas, replace = TRUE),
  country            = c(
    sample(c("NG","KE","ZA","ET","GH","TZ","CM","CD","CI","SN"),
           n_africa, replace = TRUE),
    sample(c("BR","MX","CO","VE","AR","PE","GT","HN","US","EC"),
           n_americas, replace = TRUE)
  ),
  gbifID             = sample(1e8:9e8, n_africa + n_americas),
  coordinateUncertaintyInMeters = sample(c(100, 500, 1000, 5000),
                                          n_africa + n_americas,
                                          replace = TRUE),
  basisOfRecord      = "HUMAN_OBSERVATION",
  stringsAsFactors   = FALSE
)

# =============================================================================
# 2. tuta_occ  – simulated occurrences of Tuta absoluta
#    Core range: Mediterranean basin + South America
# =============================================================================
n_med <- 90
n_sa  <- 60

med_lon <- runif(n_med, -10, 40)
med_lat <- rnorm(n_med, mean = 35, sd = 5)
med_lat <- pmax(25, pmin(45, med_lat))

sa_lon  <- runif(n_sa, -75, -35)
sa_lat  <- rnorm(n_sa, mean = -15, sd = 10)
sa_lat  <- pmax(-35, pmin(5, sa_lat))

tuta_occ <- data.frame(
  species            = "Tuta absoluta",
  decimalLongitude   = c(med_lon, sa_lon),
  decimalLatitude    = c(med_lat, sa_lat),
  year               = sample(2008:2023, n_med + n_sa, replace = TRUE),
  country            = c(
    sample(c("MA","ES","IT","TR","GR","TN","DZ","EG","FR","PT"),
           n_med, replace = TRUE),
    sample(c("BR","AR","CL","PE","BO","UY","CO","VE","EC","PY"),
           n_sa, replace = TRUE)
  ),
  gbifID             = sample(1e8:9e8, n_med + n_sa),
  coordinateUncertaintyInMeters = sample(c(100, 500, 1000), n_med + n_sa,
                                          replace = TRUE),
  basisOfRecord      = "HUMAN_OBSERVATION",
  stringsAsFactors   = FALSE
)

# =============================================================================
# 3. spodoptera_model_data – pre-built modelling dataset (already prepared)
#    Presence/background + 3 simulated BIO variables
# =============================================================================
n_pres <- 150
n_back <- 500

# Presences: warm/humid tropical conditions typical for S. frugiperda
pres_bio1  <- rnorm(n_pres, mean = 230, sd = 25)   # ~23°C × 10
pres_bio12 <- rnorm(n_pres, mean = 1200, sd = 300)  # ~1200 mm/yr
pres_bio15 <- rnorm(n_pres, mean = 60,  sd = 20)

# Background: random from a wider range
back_bio1  <- runif(n_back, 50, 290)
back_bio12 <- runif(n_back, 100, 2500)
back_bio15 <- runif(n_back, 10, 120)

spodoptera_model_data <- data.frame(
  lon      = c(runif(n_pres, -18, 50), runif(n_back, -20, 55)),
  lat      = c(runif(n_pres, -30, 18), runif(n_back, -40, 40)),
  presence = c(rep(1L, n_pres), rep(0L, n_back)),
  BIO1     = c(pres_bio1,  back_bio1),
  BIO12    = c(pres_bio12, back_bio12),
  BIO15    = c(pres_bio15, back_bio15)
)

# Ensure no negative values in climate cols
spodoptera_model_data$BIO1  <- pmax(0, spodoptera_model_data$BIO1)
spodoptera_model_data$BIO12 <- pmax(0, spodoptera_model_data$BIO12)
spodoptera_model_data$BIO15 <- pmax(0, spodoptera_model_data$BIO15)

# =============================================================================
# 4. Save to data/
# =============================================================================
usethis::use_data(spodoptera_occ,        overwrite = TRUE)
usethis::use_data(tuta_occ,              overwrite = TRUE)
usethis::use_data(spodoptera_model_data, overwrite = TRUE)

message("Example datasets created successfully:")
message("  data/spodoptera_occ.rda        (", nrow(spodoptera_occ), " rows)")
message("  data/tuta_occ.rda              (", nrow(tuta_occ), " rows)")
message("  data/spodoptera_model_data.rda (", nrow(spodoptera_model_data), " rows)")
