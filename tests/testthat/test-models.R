# tests/testthat/test-prepare_predictors.R

# Helper: build a tiny model_data-like object without a raster
.make_dummy_data <- function(n_pres = 30, n_back = 100, seed = 1) {
  set.seed(seed)
  data.frame(
    lon      = runif(n_pres + n_back, -10, 50),
    lat      = runif(n_pres + n_back, -20, 40),
    presence = c(rep(1L, n_pres), rep(0L, n_back)),
    BIO1     = c(rnorm(n_pres, 230, 20), runif(n_back, 50, 300)),
    BIO4     = c(rnorm(n_pres,  80, 10), runif(n_back, 20, 150)),
    BIO12    = c(rnorm(n_pres, 1200, 300), runif(n_back, 100, 2500)),
    BIO15    = c(rnorm(n_pres,  60, 15), runif(n_back, 10, 120))
  )
}

test_that(".compute_vif returns named numeric vector of correct length", {
  mat  <- as.matrix(.make_dummy_data()[, c("BIO1","BIO4","BIO12","BIO15")])
  vifs <- pestRisk:::.compute_vif(mat)
  expect_named(vifs)
  expect_length(vifs, 4L)
  expect_true(all(vifs >= 1))
})

test_that(".compute_vif gives VIF = 1 for single variable", {
  mat  <- matrix(rnorm(50), ncol = 1, dimnames = list(NULL, "X"))
  vifs <- pestRisk:::.compute_vif(mat)
  expect_equal(as.numeric(vifs), 1)
})

test_that(".spatial_thin reduces dense cluster", {
  df <- data.frame(
    decimalLongitude = rnorm(200, 0, 0.01),
    decimalLatitude  = rnorm(200, 0, 0.01)
  )
  thinned <- pestRisk:::.spatial_thin(df, "decimalLongitude",
                                       "decimalLatitude", 50)
  expect_true(nrow(thinned) < 200)
})


# =============================================================================
# tests/testthat/test-evaluate_models.R
# =============================================================================

test_that("evaluate_models produces correct metric columns", {
  # Minimal fake model object
  fake_rf <- list(
    type       = "rf",
    predictors = c("BIO1", "BIO12"),
    test_data  = data.frame(
      presence = c(1L, 1L, 0L, 0L, 1L, 0L),
      BIO1     = c(230, 220, 100, 80, 240, 90),
      BIO12    = c(1200, 1100, 300, 200, 1300, 250)
    )
  )
  # Attach a trivial model predict method via randomForest mock
  # (skip if randomForest not installed)
  skip_if_not_installed("randomForest")

  # Use spodoptera_model_data for a real (fast) test
  data("spodoptera_model_data", package = "pestRisk")
  rf_out <- train_rf_model(spodoptera_model_data, n_trees = 50,
                            verbose = FALSE)
  eval   <- evaluate_models(rf_out, verbose = FALSE, plot_roc = FALSE)

  expect_s3_class(eval$metrics, "data.frame")
  expect_true(all(c("Model","AUC","Accuracy","Sensitivity",
                    "Specificity","F1") %in% names(eval$metrics)))
  expect_true(eval$metrics$AUC >= 0 & eval$metrics$AUC <= 1)
})

test_that("generate_background_points returns sf with correct n", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Create a tiny 10×10 raster
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = -20, xmax = 55, ymin = -40, ymax = 40,
                   vals = 1)

  # Tiny presence sf
  pres <- sf::st_as_sf(
    data.frame(lon = c(10, 20, 30), lat = c(0, 5, 10)),
    coords = c("lon", "lat"), crs = 4326
  )

  bg <- generate_background_points(r, pres, n = 20, min_dist_km = 0,
                                    verbose = FALSE)
  expect_s3_class(bg, "sf")
  expect_lte(nrow(bg), 20)
})
