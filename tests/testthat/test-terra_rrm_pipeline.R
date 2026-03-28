source(file.path("..", "..", "R", "terra_rrm_pipeline.R"))

test_that("set_raster_levels_from_conv attaches categorical tables", {
  skip_if_not_installed("terra")

  raster <- terra::rast(nrows = 1, ncols = 2)
  terra::values(raster) <- c(1L, 2L)
  names(raster) <- "char_field"

  result <- set_raster_levels_from_conv(
    raster,
    list(char_field = data.table::data.table(value = c("A", "B"), factor = c(1L, 2L)))
  )

  level_table <- terra::cats(result[["char_field"]])[[1]]
  expect_equal(level_table[[1]], c(1L, 2L))
  expect_equal(level_table[[2]], c("A", "B"))
})

test_that(".terra_rrm_stack_inputs combines aligned rasters", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stacked <- .terra_rrm_stack_inputs(
    vri_raster = make_raster("BCLCS_LV_1", c(1L, 1L)),
    bem_raster = make_raster("BEUMC_S1", c(2L, 2L)),
    rivers_raster = make_raster("rivers", c(0L, 1L)),
    wetlands_raster = make_raster("wl_pct", c(0L, 1L)),
    ccb_raster = make_raster("HARVESTYR", c(NA, 2020L))
  )

  expect_identical(
    names(stacked),
    c("BCLCS_LV_1", "BEUMC_S1", "rivers", "wl_pct", "HARVESTYR")
  )
})

test_that(".terra_rrm_stack_inputs rejects duplicated layer names", {
  skip_if_not_installed("terra")

  make_raster <- function(name) {
    raster <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    terra::values(raster) <- 1L
    names(raster) <- name
    raster
  }

  expect_error(
    .terra_rrm_stack_inputs(
      vri_raster = make_raster("dup"),
      bem_raster = make_raster("dup"),
      rivers_raster = make_raster("rivers"),
      wetlands_raster = make_raster("wl_pct")
    ),
    "duplicated layer names"
  )
})