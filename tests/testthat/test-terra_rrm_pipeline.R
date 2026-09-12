source(file.path("..", "..", "R", "terra_rrm_pipeline.R"))
source(file.path("..", "..", "R", "terra_rrm_corrections.R"))

test_that("terra_rrm_compute_terrain_layers creates sf-equivalent terrain fields", {
  skip_if_not_installed("terra")

  raster_conv <- .terra_rrm_get_raster_conv()

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  beumc_s1_vals <- c(
    .terra_rrm_layer_codes(make_raster("BEUMC_S1", 0), "BEUMC_S1", "AT", raster_conv$bem, strict = FALSE)[[1]],
    .terra_rrm_layer_codes(make_raster("BEUMC_S1", 0), "BEUMC_S1", "AT", raster_conv$bem, strict = FALSE)[[1]],
    .terra_rrm_layer_codes(make_raster("BEUMC_S1", 0), "BEUMC_S1", "AT", raster_conv$bem, strict = FALSE)[[1]],
    .terra_rrm_layer_codes(make_raster("BEUMC_S1", 0), "BEUMC_S1", "OW", raster_conv$bem, strict = FALSE)[[1]]
  )
  beumc_s2_vals <- rep(.terra_rrm_layer_codes(make_raster("BEUMC_S2", 0), "BEUMC_S2", "AT", raster_conv$bem, strict = FALSE)[[1]], 4)
  beumc_s3_vals <- rep(.terra_rrm_layer_codes(make_raster("BEUMC_S3", 0), "BEUMC_S3", "AT", raster_conv$bem, strict = FALSE)[[1]], 4)
  bgc_vals <- c(
    .terra_rrm_layer_codes(make_raster("BGC_ZONE", 0), "BGC_ZONE", "SBS", raster_conv$bem, strict = FALSE)[[1]],
    .terra_rrm_layer_codes(make_raster("BGC_ZONE", 0), "BGC_ZONE", "SBS", raster_conv$bem, strict = FALSE)[[1]],
    .terra_rrm_layer_codes(make_raster("BGC_ZONE", 0), "BGC_ZONE", "CWH", raster_conv$bem, strict = FALSE)[[1]],
    .terra_rrm_layer_codes(make_raster("BGC_ZONE", 0), "BGC_ZONE", "SBS", raster_conv$bem, strict = FALSE)[[1]]
  )

  x <- c(
    make_raster("elevation", c(1600, 1200, 1800, 1700)),
    make_raster("BGC_ZONE", bgc_vals),
    make_raster("BEUMC_S1", beumc_s1_vals),
    make_raster("BEUMC_S2", beumc_s2_vals),
    make_raster("BEUMC_S3", beumc_s3_vals)
  )

  levels(x[["BGC_ZONE"]]) <- data.frame(value = unique(bgc_vals), label = c("SBS", "CWH"))
  levels(x[["BEUMC_S1"]]) <- data.frame(value = unique(beumc_s1_vals), label = c("AT", "OW"))
  levels(x[["BEUMC_S2"]]) <- data.frame(value = unique(beumc_s2_vals), label = c("AT"))
  levels(x[["BEUMC_S3"]]) <- data.frame(value = unique(beumc_s3_vals), label = c("AT"))

  terrain <- c(
    make_raster("slope", c(30, 15, 30, 30) * pi / 180),
    make_raster("aspect", c(90, 180, 90, 90) * pi / 180)
  )

  res <- terra_rrm_compute_terrain_layers(x, elevation_threshold = 1500, terrain_raster = terrain)

  expect_true(all(c("ELEV", "MEAN_SLOPE", "MEAN_ASP", "ABOVE_ELEV_THOLD", "SLOPE_MOD") %in% names(res)))
  expect_equal(as.vector(terra::values(res[["ABOVE_ELEV_THOLD"]])), c(2, 1, 2, 2))

  slope_mod_codes <- as.vector(terra::values(res[["SLOPE_MOD"]]))
  slope_mod_levels <- terra::cats(res[["SLOPE_MOD"]])[[1]]
  slope_mod_labels <- slope_mod_levels[[2]][match(slope_mod_codes, slope_mod_levels[[1]])]

  expect_equal(slope_mod_labels[1], "k")
  expect_equal(slope_mod_labels[2], "j")
  expect_equal(slope_mod_labels[3], "j")
  expect_true(is.na(slope_mod_labels[4]))
})

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