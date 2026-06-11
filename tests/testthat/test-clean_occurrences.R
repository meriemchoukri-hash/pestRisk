# tests/testthat/test-clean_occurrences.R

test_that("clean_occurrences removes NA coordinates", {
  df <- data.frame(
    decimalLongitude = c(10, NA, 20, 30),
    decimalLatitude  = c(5,  5, NA, 10)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 0,
                               outlier_sd = NULL, verbose = FALSE)
  expect_equal(nrow(result$data), 2L)
})

test_that("clean_occurrences removes Null Island (0,0)", {
  df <- data.frame(
    decimalLongitude = c(0,  10, 20),
    decimalLatitude  = c(0,  5,  10)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 0,
                               outlier_sd = NULL, verbose = FALSE)
  expect_true(nrow(result$data) < 3L)
})

test_that("clean_occurrences removes impossible coordinates", {
  df <- data.frame(
    decimalLongitude = c(200,  10, -200),
    decimalLatitude  = c(5,    5,    5)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 0,
                               outlier_sd = NULL, verbose = FALSE)
  expect_equal(nrow(result$data), 1L)
})

test_that("clean_occurrences removes spatial duplicates", {
  df <- data.frame(
    decimalLongitude = c(10.0000, 10.0001, 20),
    decimalLatitude  = c(5.0000, 5.0001,  10)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 0,
                               outlier_sd = NULL, verbose = FALSE)
  # 10.0000 and 10.0001 round to same 4-d.p. cell → 1 removed
  expect_equal(nrow(result$data), 2L)
})

test_that("clean_occurrences returns an sf object", {
  df <- data.frame(
    decimalLongitude = c(10, 20, 30),
    decimalLatitude  = c(5,  10, 15)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 0,
                               outlier_sd = NULL, verbose = FALSE)
  expect_s3_class(result$data, "sf")
})

test_that("clean_occurrences report has correct structure", {
  df <- data.frame(
    decimalLongitude = c(10, NA),
    decimalLatitude  = c(5,  5)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 0,
                               outlier_sd = NULL, verbose = FALSE)
  expect_true(all(c("step", "n_removed", "n_remaining") %in%
                    names(result$report)))
})

test_that("spatial thinning reduces point density", {
  # Create a dense cluster of 100 points all within 1 km of each other
  df <- data.frame(
    decimalLongitude = rnorm(100, mean = 10, sd = 0.001),
    decimalLatitude  = rnorm(100, mean = 5,  sd = 0.001)
  )
  result <- clean_occurrences(df, remove_sea = FALSE, thin_dist = 50,
                               outlier_sd = NULL, verbose = FALSE)
  expect_true(nrow(result$data) < 100)
})
