source(file.path("..", "..", "R", "terra_rrm_corrections.R"))
source(file.path("..", "..", "R", "terra_rrm_crown.R"))

test_that("terra_rrm_find_crown_area_dominant_values copies crown only for forested high-structure components", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  add_levels <- function(raster, values, labels) {
    levels(raster) <- data.frame(value = values, label = labels)
    raster
  }

  stack <- c(
    add_levels(make_raster("CROWN_ALL", c(1L, 2L, 3L, 4L)), c(1L, 2L, 3L, 4L), c("VL-L", "M", "H", "VH")),
    add_levels(make_raster("FORESTED_1", c(1L, 1L, 2L, 1L)), c(1L, 2L), c("Y", "N")),
    add_levels(make_raster("FORESTED_2", c(1L, 1L, 1L, 1L)), c(1L, 2L), c("Y", "N")),
    add_levels(make_raster("FORESTED_3", c(1L, 1L, 1L, 1L)), c(1L, 2L), c("Y", "N")),
    add_levels(make_raster("STRCT_S1", c(1L, 2L, 2L, 3L)), c(1L, 2L, 3L, 4L, 5L), c("4", "5", "2", "7a", "3")),
    add_levels(make_raster("STRCT_S2", c(4L, 5L, 1L, 2L)), c(1L, 2L, 3L, 4L, 5L), c("4", "5", "2", "7a", "3")),
    add_levels(make_raster("STRCT_S3", c(5L, 1L, 2L, 4L)), c(1L, 2L, 3L, 4L, 5L), c("4", "5", "2", "7a", "3"))
  )
  names(stack) <- c("CROWN_ALL", "FORESTED_1", "FORESTED_2", "FORESTED_3", "STRCT_S1", "STRCT_S2", "STRCT_S3")

  result <- terra_rrm_find_crown_area_dominant_values(stack)

  expect_equal(as.vector(terra::values(result[["CROWN_ALL_1"]])), c(1, 2, NA, NA))
  expect_equal(as.vector(terra::values(result[["CROWN_ALL_2"]])), c(1, NA, 3, 4))
  expect_equal(as.vector(terra::values(result[["CROWN_ALL_3"]])), c(NA, 2, 3, 4))

  expect_equal(as.vector(terra::values(result[["CROWN_BEAR_1"]])), c(1, 2, NA, NA))
  expect_equal(as.vector(terra::values(result[["CROWN_MOOSE_3"]])), c(NA, 2, 3, 4))
})

test_that("terra_rrm_find_crown_area_dominant_values errors when required inputs are missing", {
  skip_if_not_installed("terra")

  raster <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(raster) <- 1
  names(raster) <- "CROWN_ALL"

  expect_error(
    terra_rrm_find_crown_area_dominant_values(raster),
    "Missing required layers"
  )
})