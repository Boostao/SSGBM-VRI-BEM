source(file.path("..", "..", "R", "terra_rrm_forest_age.R"))

test_that("terra_rrm_calc_forest_age_class assigns age classes from PROJ_AGE_1", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  ages <- c(-5, 0, 3, 4, 10, 11, 30, 31, 40, 41, 60, 61, 80, 81, 140, 141, 249, 250, NA)
  stack <- make_raster("PROJ_AGE_1", ages)

  result <- terra_rrm_calc_forest_age_class(stack, most_recent_harvest_year = 2020)

  expect_equal(
    as.vector(terra::values(result[["VRI_AGE_CL_STS"]])),
    c(-1, 2, 2, 7, 7, 20, 20, 35, 35, 50, 50, 70, 70, 125, 125, 195, 195, 301, -1)
  )
  expect_equal(
    as.vector(terra::values(result[["VRI_AGE_CL_STD"]])),
    c(-1, 15, 15, 15, 15, 15, 30, 50, 50, 50, 80, 80, 80, 9999, 9999, 9999, 9999, 9999, -1)
  )
})

test_that("terra_rrm_calc_forest_age_class overwrites PROJ_AGE_1 from disturbance-year layers", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stack <- c(
    make_raster("PROJ_AGE_1", c(50, 50, 50, 50)),
    make_raster("MRSRD_Y", c(2010, NA, NA, NA)),
    make_raster("HARVESTYR", c(NA, 2005, NA, NA)),
    make_raster("HRVSTDT", c(NA, NA, 20011231, NA))
  )

  result <- terra_rrm_calc_forest_age_class(stack, most_recent_harvest_year = 2020)

  expect_equal(as.vector(terra::values(result[["PROJ_AGE_1"]])), c(10, 15, 19, 50))
  expect_equal(as.vector(terra::values(result[["VRI_AGE_CL_STS"]])), c(7, 20, 20, 50))
  expect_equal(as.vector(terra::values(result[["VRI_AGE_CL_STD"]])), c(15, 15, 30, 50))
})

test_that("terra_rrm_calc_forest_age_class errors without PROJ_AGE_1", {
  skip_if_not_installed("terra")

  raster <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(raster) <- 2010
  names(raster) <- "HARVESTYR"

  expect_error(
    terra_rrm_calc_forest_age_class(raster, most_recent_harvest_year = 2020),
    "requires a 'PROJ_AGE_1' layer"
  )
})