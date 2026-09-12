source(file.path("..", "..", "R", "terra_rrm_corrections.R"))
source(file.path("..", "..", "R", "update_beu_from_rules_dt.R"))

test_that(".terra_rrm_shift_component_fields shifts matching component layers", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stack <- c(
    make_raster("BEUMC_S1", c(10L, 10L)),
    make_raster("BEUMC_S2", c(20L, 20L)),
    make_raster("BEUMC_S3", c(30L, 30L)),
    make_raster("TREE_C1", c(1L, 1L)),
    make_raster("TREE_C2", c(2L, 2L)),
    make_raster("TREE_C3", c(3L, 3L))
  )

  mask <- make_raster("mask", c(1L, NA))

  shifted <- .terra_rrm_shift_component_fields(
    x = stack,
    mask = mask,
    shift_pattern = list(c(1, 2), c(2, 3), c(3, NA))
  )

  expect_equal(as.vector(terra::values(shifted[["BEUMC_S1"]])), c(20, 10))
  expect_equal(as.vector(terra::values(shifted[["BEUMC_S2"]])), c(30, 20))
  expect_true(is.na(as.vector(terra::values(shifted[["BEUMC_S3"]]))[1]))
  expect_equal(as.vector(terra::values(shifted[["BEUMC_S3"]]))[2], 30)

  expect_equal(as.vector(terra::values(shifted[["TREE_C1"]])), c(2, 1))
  expect_equal(as.vector(terra::values(shifted[["TREE_C2"]])), c(3, 2))
  expect_true(is.na(as.vector(terra::values(shifted[["TREE_C3"]]))[1]))
  expect_equal(as.vector(terra::values(shifted[["TREE_C3"]]))[2], 3)
})

test_that("terra_rrm_correct_small_lakes classifies rasterized lake patches", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values) * 1000, ymin = 0, ymax = 100)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  lake_values <- c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L)

  stack <- c(
    make_raster("lakes", lake_values),
    make_raster("BEUMC_S1", rep(1L, length(lake_values))),
    make_raster("BEUMC_S2", rep(9L, length(lake_values))),
    make_raster("BEUMC_S3", rep(8L, length(lake_values))),
    make_raster("SDEC_1", rep(4L, length(lake_values))),
    make_raster("SDEC_2", rep(3L, length(lake_values))),
    make_raster("SDEC_3", rep(3L, length(lake_values))),
    make_raster("BCLCS_LV_1", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_2", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_3", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_4", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_5", rep(9L, length(lake_values))),
    make_raster("SPEC_CD_1", rep(5L, length(lake_values))),
    make_raster("SPEC_PCT_1", rep(80L, length(lake_values)))
  )

  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 3L, 4L), label = c("XX", "OW", "LS", "LL"))
  levels(stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 8L, 9L), label = c("XX", "YY", "ZZ"))
  levels(stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 8L, 9L), label = c("XX", "YY", "ZZ"))
  levels(stack[["BCLCS_LV_1"]]) <- data.frame(value = c(1L, 9L), label = c("N", "X"))
  levels(stack[["BCLCS_LV_2"]]) <- data.frame(value = c(2L, 9L), label = c("W", "X"))
  levels(stack[["BCLCS_LV_5"]]) <- data.frame(value = c(2L, 9L), label = c("LA", "X"))

  result <- terra_rrm_correct_small_lakes(stack)
  raster_conv <- .terra_rrm_get_raster_conv()

  ow_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "OW", raster_conv$bem, strict = FALSE)[[1]]
  ls_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "LS", raster_conv$bem, strict = FALSE)[[1]]
  ll_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "LL", raster_conv$bem, strict = FALSE)[[1]]
  lv1_n <- .terra_rrm_layer_codes(result, "BCLCS_LV_1", "N", raster_conv$vri, strict = FALSE)[[1]]
  lv2_w <- .terra_rrm_layer_codes(result, "BCLCS_LV_2", "W", raster_conv$vri, strict = FALSE)[[1]]
  lv5_la <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", "LA", raster_conv$vri, strict = FALSE)[[1]]

  expect_equal(
    as.vector(terra::values(result[["BEUMC_S1"]])),
    c(ow_code, 1, ls_code, ls_code, 1, ll_code, ll_code, ll_code, ll_code, ll_code, ll_code, ll_code, 1)
  )
  expect_equal(as.vector(terra::values(result[["SDEC_1"]]))[lake_values == 1], rep(10, sum(lake_values == 1)))
  expect_equal(as.vector(terra::values(result[["SDEC_2"]]))[lake_values == 1], rep(0, sum(lake_values == 1)))
  expect_equal(as.vector(terra::values(result[["SDEC_3"]]))[lake_values == 1], rep(0, sum(lake_values == 1)))
  expect_true(all(is.na(as.vector(terra::values(result[["BEUMC_S2"]]))[lake_values == 1])))
  expect_true(all(is.na(as.vector(terra::values(result[["BEUMC_S3"]]))[lake_values == 1])))
  expect_equal(as.vector(terra::values(result[["BCLCS_LV_1"]]))[lake_values == 1], rep(lv1_n, sum(lake_values == 1)))
  expect_equal(as.vector(terra::values(result[["BCLCS_LV_2"]]))[lake_values == 1], rep(lv2_w, sum(lake_values == 1)))
  expect_equal(as.vector(terra::values(result[["BCLCS_LV_5"]]))[lake_values == 1], rep(lv5_la, sum(lake_values == 1)))
  expect_true(all(is.na(as.vector(terra::values(result[["SPEC_CD_1"]]))[lake_values == 1])))
  expect_true(all(is.na(as.vector(terra::values(result[["SPEC_PCT_1"]]))[lake_values == 1])))
})

test_that("terra_rrm_correct_small_lakes can expand corrections around lake pixels", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values) * 1000, ymin = 0, ymax = 100, crs = "EPSG:3005")
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  lake_values <- c(0L, 1L, 0L)

  stack <- c(
    make_raster("lakes", lake_values),
    make_raster("BEUMC_S1", rep(1L, length(lake_values))),
    make_raster("BEUMC_S2", rep(9L, length(lake_values))),
    make_raster("BEUMC_S3", rep(8L, length(lake_values))),
    make_raster("SDEC_1", rep(4L, length(lake_values))),
    make_raster("SDEC_2", rep(3L, length(lake_values))),
    make_raster("SDEC_3", rep(3L, length(lake_values))),
    make_raster("BCLCS_LV_1", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_2", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_3", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_4", rep(9L, length(lake_values))),
    make_raster("BCLCS_LV_5", rep(9L, length(lake_values))),
    make_raster("SPEC_CD_1", rep(5L, length(lake_values))),
    make_raster("SPEC_PCT_1", rep(80L, length(lake_values)))
  )

  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 3L, 4L), label = c("XX", "OW", "LS", "LL"))
  levels(stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 8L, 9L), label = c("XX", "YY", "ZZ"))
  levels(stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 8L, 9L), label = c("XX", "YY", "ZZ"))
  levels(stack[["BCLCS_LV_1"]]) <- data.frame(value = c(1L, 9L), label = c("N", "X"))
  levels(stack[["BCLCS_LV_2"]]) <- data.frame(value = c(2L, 9L), label = c("W", "X"))
  levels(stack[["BCLCS_LV_5"]]) <- data.frame(value = c(2L, 9L), label = c("LA", "X"))

  result <- terra_rrm_correct_small_lakes(stack, buffer_m = 1000)
  raster_conv <- .terra_rrm_get_raster_conv()
  ow_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "OW", raster_conv$bem, strict = FALSE)[[1]]

  expect_equal(as.vector(terra::values(result[["BEUMC_S1"]])), c(ow_code, ow_code, ow_code))
  expect_true(all(is.na(as.vector(terra::values(result[["SPEC_CD_1"]])))))
})

test_that("terra_rrm_correct_bem_from_wetlands applies primary WL add/remove transitions", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  buc <- data.frame(
    Code_Orig = c(10000, 9101, 8200, 8202),
    Code_WL0 = c(10000, 10000, 8200, 8202),
    Code_WL1 = c(9101, 9101, 8200, 8201),
    Code_WL2 = c(10000, 9101, 8200, 8202),
    Code_WL3 = c(10000, 9101, 8203, 8202),
    Code_WL4 = c(10000, 9101, 8200, 8202),
    Code_WL5 = c(10000, 9101, 8200, 8202),
    Code_WL6 = c(10000, 9101, 8200, 8202),
    Code_WL7 = c(10000, 9101, 8200, 8202),
    Code_WL8 = c(10000, 9101, 8200, 8202),
    Code_WL10 = c(10000, 9101, 8200, 8202)
  )

  stack <- c(
    make_raster("wl_pct", c(10L, 0L, 30L, 10L)),
    make_raster("SDEC_1", c(10L, 9L, 8L, 8L)),
    make_raster("SDEC_2", c(0L, 1L, 2L, 2L)),
    make_raster("SDEC_3", c(0L, 0L, 0L, 0L)),
    make_raster("BEUMC_S1", c(1L, 2L, 1L, 1L)),
    make_raster("BEUMC_S2", c(7L, 1L, 7L, 2L)),
    make_raster("BEUMC_S3", c(8L, NA, NA, NA)),
    make_raster("REALM_1", c(1L, 2L, 1L, 1L)),
    make_raster("GROUP_1", c(1L, 2L, 1L, 1L)),
    make_raster("KIND_1", c(1L, 2L, 1L, 1L)),
    make_raster("REALM_3", c(1L, 1L, 1L, 1L)),
    make_raster("GROUP_3", c(1L, 1L, 1L, 1L)),
    make_raster("KIND_3", c(1L, 1L, 1L, 1L))
  )

  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 7L, 8L), label = c("XX", "WL", "YY", "ZZ"))
  levels(stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 2L, 7L, 8L), label = c("XX", "WL", "YY", "ZZ"))
  levels(stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 2L, 7L, 8L), label = c("XX", "WL", "YY", "ZZ"))
  levels(stack[["REALM_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["GROUP_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["KIND_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "U"))
  levels(stack[["REALM_3"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["GROUP_3"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["KIND_3"]]) <- data.frame(value = c(1L, 2L), label = c("X", "U"))

  result <- terra_rrm_correct_bem_from_wetlands(stack, buc = buc)
  raster_conv <- .terra_rrm_get_raster_conv()

  expect_equal(as.vector(terra::values(result[["BEUMC_S1"]])), c(2, 2, 1, 2))
  expect_equal(as.vector(terra::values(result[["BEUMC_S2"]])), c(1, 1, 7, 1))
  expect_true(all(is.na(as.vector(terra::values(result[["BEUMC_S3"]])))))
  expect_equal(as.vector(terra::values(result[["SDEC_1"]])), c(9, 9, 8, 8))
  expect_equal(as.vector(terra::values(result[["SDEC_2"]])), c(1, 1, 2, 2))
  expect_equal(as.vector(terra::values(result[["SDEC_3"]])), c(0, 0, 0, 0))
  expect_equal(as.vector(terra::values(result[["REALM_1"]]))[1], 2)
  expect_equal(as.vector(terra::values(result[["GROUP_1"]]))[1], 2)
  expect_equal(as.vector(terra::values(result[["KIND_1"]]))[1], 2)
  expect_equal(as.vector(terra::values(result[["REALM_3"]]))[3], 2)
  expect_equal(as.vector(terra::values(result[["GROUP_3"]]))[3], 2)
  expect_equal(as.vector(terra::values(result[["KIND_3"]]))[3], 2)
})

test_that("terra_rrm_correct_bem_from_wetlands handles 0 to 2 insertion and specific wetland merges", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  buc <- data.frame(
    Code_Orig = c(8200, 8202, 8201, 10001),
    Code_WL0 = c(8200, 8202, 8201, 10001),
    Code_WL1 = c(8112, 8201, 8201, 10001),
    Code_WL2 = c(8112, 8202, 8201, 10001),
    Code_WL3 = c(8112, 8202, 8201, 10001),
    Code_WL4 = c(8112, 8202, 8201, 10001),
    Code_WL5 = c(8112, 8202, 8201, 10001),
    Code_WL6 = c(8112, 8202, 8201, 10001),
    Code_WL7 = c(8112, 8202, 8201, 10001),
    Code_WL8 = c(8112, 8202, 8201, 10001),
    Code_WL10 = c(8112, 8202, 8201, 10001)
  )

  stack <- c(
    make_raster("wl_pct", c(10L, 10L, 10L)),
    make_raster("SDEC_1", c(8L, 8L, 10L)),
    make_raster("SDEC_2", c(2L, 2L, 0L)),
    make_raster("SDEC_3", c(0L, 0L, 0L)),
    make_raster("BEUMC_S1", c(1L, 1L, 2L)),
    make_raster("BEUMC_S2", c(8L, 2L, 7L)),
    make_raster("BEUMC_S3", c(NA, NA, NA)),
    make_raster("REALM_1", c(1L, 1L, 2L)),
    make_raster("GROUP_1", c(1L, 1L, 2L)),
    make_raster("KIND_1", c(1L, 1L, 2L)),
    make_raster("REALM_2", c(1L, 2L, 2L)),
    make_raster("GROUP_2", c(1L, 2L, 2L)),
    make_raster("KIND_2", c(1L, 2L, 2L)),
    make_raster("REALM_3", c(1L, 1L, 1L)),
    make_raster("GROUP_3", c(1L, 1L, 1L)),
    make_raster("KIND_3", c(1L, 1L, 1L))
  )

  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 7L, 8L), label = c("XX", "WL", "BB", "ZZ"))
  levels(stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 2L, 7L, 8L), label = c("XX", "WL", "BB", "ZZ"))
  levels(stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 2L, 7L, 8L), label = c("XX", "WL", "BB", "ZZ"))
  levels(stack[["REALM_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["GROUP_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["KIND_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "U"))
  levels(stack[["REALM_2"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["GROUP_2"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["KIND_2"]]) <- data.frame(value = c(1L, 2L), label = c("X", "U"))
  levels(stack[["REALM_3"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["GROUP_3"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))
  levels(stack[["KIND_3"]]) <- data.frame(value = c(1L, 2L), label = c("X", "U"))

  result <- terra_rrm_correct_bem_from_wetlands(stack, buc = buc)

  expect_equal(as.vector(terra::values(result[["BEUMC_S1"]])), c(1, 2, 7))
  expect_equal(as.vector(terra::values(result[["BEUMC_S2"]])), c(2, 1, NA))
  expect_equal(as.vector(terra::values(result[["BEUMC_S3"]])), c(8, NA, NA))
  expect_equal(as.vector(terra::values(result[["SDEC_1"]])), c(8, 8, 10))
  expect_equal(as.vector(terra::values(result[["SDEC_2"]])), c(1, 2, 0))
  expect_equal(as.vector(terra::values(result[["SDEC_3"]])), c(1, 0, 0))
  expect_equal(as.vector(terra::values(result[["REALM_2"]]))[1], 2)
  expect_equal(as.vector(terra::values(result[["GROUP_2"]]))[1], 2)
  expect_equal(as.vector(terra::values(result[["KIND_2"]]))[1], 2)
})

test_that("terra_rrm_correct_bem_from_wetlands_riparian_stage assigns riparian mapcodes", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stack <- c(
    make_raster("SITE_M3A", c(1L, 1L, 2L, 1L)),
    make_raster("MEAN_SLOPE", c(5L, 6L, 5L, 12L)),
    make_raster("BGC_ZONE", c(1L, 2L, 1L, 1L)),
    make_raster("BEUMC_S1", c(1L, 1L, 4L, 1L)),
    make_raster("BEUMC_S2", c(9L, 9L, 9L, 9L)),
    make_raster("BEUMC_S3", c(8L, 8L, 8L, 8L)),
    make_raster("SDEC_1", c(6L, 6L, 6L, 6L)),
    make_raster("SDEC_2", c(2L, 2L, 2L, 2L)),
    make_raster("SDEC_3", c(2L, 2L, 2L, 2L))
  )

  levels(stack[["SITE_M3A"]]) <- data.frame(value = c(1L, 2L), label = c("a", "b"))
  levels(stack[["BGC_ZONE"]]) <- data.frame(value = c(1L, 2L), label = c("ICH", "CWH"))
  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 3L, 4L, 8L, 9L), label = c("XX", "RR", "SR", "OW", "YY", "ZZ"))
  levels(stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 2L, 3L, 4L, 8L, 9L), label = c("XX", "RR", "SR", "OW", "YY", "ZZ"))
  levels(stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 2L, 3L, 4L, 8L, 9L), label = c("XX", "RR", "SR", "OW", "YY", "ZZ"))

  result <- terra_rrm_correct_bem_from_wetlands_riparian_stage(stack)

  expect_equal(as.vector(terra::values(result[["BEUMC_S1"]])), c(2, 3, 4, 1))
  expect_true(all(is.na(as.vector(terra::values(result[["BEUMC_S2"]]))[1:2])))
  expect_true(all(is.na(as.vector(terra::values(result[["BEUMC_S3"]]))[1:2])))
  expect_equal(as.vector(terra::values(result[["SDEC_1"]])), c(10, 10, 6, 6))
  expect_equal(as.vector(terra::values(result[["SDEC_2"]])), c(0, 0, 2, 2))
  expect_equal(as.vector(terra::values(result[["SDEC_3"]])), c(0, 0, 2, 2))
})

test_that("terra_rrm_apply_rules applies ordered non-tree rules", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stack <- c(
    make_raster("BGC_ZONE", c(1L, 2L, 3L)),
    make_raster("MEAN_SLOPE", c(5L, 7L, 5L)),
    make_raster("BEUMC_S1", c(1L, 1L, 1L)),
    make_raster("BEUMC_S2", c(8L, 8L, 8L)),
    make_raster("BEUMC_S3", c(9L, 9L, 9L)),
    make_raster("SDEC_1", c(6L, 6L, 6L)),
    make_raster("SDEC_2", c(2L, 2L, 2L)),
    make_raster("SDEC_3", c(2L, 2L, 2L)),
    make_raster("REALM_1", c(1L, 1L, 1L))
  )

  levels(stack[["BGC_ZONE"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("ICH", "CWH", "SBS"))
  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("XX", "RR", "SR"))
  levels(stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 8L, 9L), label = c("XX", "YY", "ZZ"))
  levels(stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 8L, 9L), label = c("XX", "YY", "ZZ"))
  levels(stack[["REALM_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "W"))

  rules_dt <- data.frame(
    RULE = c("r1", "r2"),
    INPUTS = c(NA, NA),
    BGC_ZONE = c("ICH", "CWH,SBS"),
    MEAN_SLOPE = c("5", NA),
    OUTPUTS = c(NA, NA),
    BEUMC = c("RR", "SR"),
    REALM_1 = c("W", NA),
    stringsAsFactors = FALSE
  )

  result <- terra_rrm_apply_rules(stack, rules_dt)

  expect_equal(as.vector(terra::values(result[["BEUMC_S1"]])), c(2, 3, 3))
  expect_equal(as.vector(terra::values(result[["REALM_1"]])), c(2, 1, 1))
  expect_equal(as.vector(terra::values(result[["SDEC_1"]])), c(6, 6, 6))
})

test_that("terra_rrm_apply_rules matches legacy tree-rule outputs on a shared fixture", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  old_setDT <- if (exists("setDT", inherits = TRUE)) get("setDT", inherits = TRUE) else NULL
  old_fcase <- if (exists("fcase", inherits = TRUE)) get("fcase", inherits = TRUE) else NULL
  old_fifelse <- if (exists("fifelse", inherits = TRUE)) get("fifelse", inherits = TRUE) else NULL
  old_between <- if (exists("between", inherits = TRUE)) get("between", inherits = TRUE) else NULL
  old_replace_na <- if (exists("replace_na", inherits = TRUE)) get("replace_na", inherits = TRUE) else NULL
  old_copy <- if (exists("copy", inherits = TRUE)) get("copy", inherits = TRUE) else NULL
  old_set <- if (exists("set", inherits = TRUE)) get("set", inherits = TRUE) else NULL
  old_parse_expr <- if (exists("parse_expr", inherits = TRUE)) get("parse_expr", inherits = TRUE) else NULL
  assign("setDT", data.table::setDT, envir = .GlobalEnv)
  assign("fcase", data.table::fcase, envir = .GlobalEnv)
  assign("fifelse", data.table::fifelse, envir = .GlobalEnv)
  assign("between", data.table::between, envir = .GlobalEnv)
  assign("replace_na", tidyr::replace_na, envir = .GlobalEnv)
  assign("copy", data.table::copy, envir = .GlobalEnv)
  assign("set", data.table::set, envir = .GlobalEnv)
  assign("parse_expr", rlang::parse_expr, envir = .GlobalEnv)
  on.exit({
    if (is.null(old_setDT)) rm("setDT", envir = .GlobalEnv) else assign("setDT", old_setDT, envir = .GlobalEnv)
    if (is.null(old_fcase)) rm("fcase", envir = .GlobalEnv) else assign("fcase", old_fcase, envir = .GlobalEnv)
    if (is.null(old_fifelse)) rm("fifelse", envir = .GlobalEnv) else assign("fifelse", old_fifelse, envir = .GlobalEnv)
    if (is.null(old_between)) rm("between", envir = .GlobalEnv) else assign("between", old_between, envir = .GlobalEnv)
    if (is.null(old_replace_na)) rm("replace_na", envir = .GlobalEnv) else assign("replace_na", old_replace_na, envir = .GlobalEnv)
    if (is.null(old_copy)) rm("copy", envir = .GlobalEnv) else assign("copy", old_copy, envir = .GlobalEnv)
    if (is.null(old_set)) rm("set", envir = .GlobalEnv) else assign("set", old_set, envir = .GlobalEnv)
    if (is.null(old_parse_expr)) rm("parse_expr", envir = .GlobalEnv) else assign("parse_expr", old_parse_expr, envir = .GlobalEnv)
  }, add = TRUE)

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  tree_rules <- data.table::as.data.table(data.frame(
    RULE = c("r1", "r2"),
    INPUTS = c(NA, NA),
    SLOPE_MOD = c(NA, NA),
    TREE_RL_SP_CD_1 = c("AT,EP", "SB>PL"),
    TREE_RL_SP_PCT_1 = c("70-100", NA),
    OUTPUTS = c(NA, NA),
    BEUMC = c("RR", "SR"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))

  legacy_dt <- data.table::data.table(
    BGC_ZONE = c("ICH", "CWH", "SBS"),
    SLOPE_MOD = c("", "", ""),
    BEUMC_S1 = c("XX", "XX", "XX"),
    BEUMC_S2 = c(NA_character_, NA_character_, NA_character_),
    BEUMC_S3 = c(NA_character_, NA_character_, NA_character_),
    SDEC_1 = c(10, 10, 10),
    SDEC_2 = c(0, 0, 0),
    SDEC_3 = c(0, 0, 0),
    SPEC_CD_1 = c("AT", "SB", "PL"),
    SPEC_PCT_1 = c(80, 60, 60),
    SPEC_CD_2 = c(NA_character_, "PL", "SB"),
    SPEC_PCT_2 = c(0, 30, 20),
    SPEC_CD_3 = c(NA_character_, NA_character_, NA_character_),
    SPEC_PCT_3 = c(0, 0, 0),
    SPEC_CD_4 = c(NA_character_, NA_character_, NA_character_),
    SPEC_PCT_4 = c(0, 0, 0),
    SPEC_CD_5 = c(NA_character_, NA_character_, NA_character_),
    SPEC_PCT_5 = c(0, 0, 0),
    SPEC_CD_6 = c(NA_character_, NA_character_, NA_character_),
    SPEC_PCT_6 = c(0, 0, 0),
    x = c(1, 2, 3),
    y = c(1, 1, 1)
  )
  legacy_sf <- sf::st_as_sf(legacy_dt, coords = c("x", "y"), crs = 4326)
  legacy_result <- update_beu_from_rules_dt(legacy_sf, tree_rules)

  stack <- c(
    make_raster("BGC_ZONE", c(1L, 2L, 3L)),
    make_raster("SLOPE_MOD", c(1L, 1L, 1L)),
    make_raster("BEUMC_S1", c(1L, 1L, 1L)),
    make_raster("BEUMC_S2", c(NA, NA, NA)),
    make_raster("BEUMC_S3", c(NA, NA, NA)),
    make_raster("SDEC_1", c(10, 10, 10)),
    make_raster("SDEC_2", c(0, 0, 0)),
    make_raster("SDEC_3", c(0, 0, 0)),
    make_raster("SPEC_CD_1", c(1L, 2L, 3L)),
    make_raster("SPEC_PCT_1", c(80, 60, 60)),
    make_raster("SPEC_CD_2", c(NA, 3L, 2L)),
    make_raster("SPEC_PCT_2", c(0, 30, 20)),
    make_raster("SPEC_CD_3", c(NA, NA, NA)),
    make_raster("SPEC_PCT_3", c(0, 0, 0)),
    make_raster("SPEC_CD_4", c(NA, NA, NA)),
    make_raster("SPEC_PCT_4", c(0, 0, 0)),
    make_raster("SPEC_CD_5", c(NA, NA, NA)),
    make_raster("SPEC_PCT_5", c(0, 0, 0)),
    make_raster("SPEC_CD_6", c(NA, NA, NA)),
    make_raster("SPEC_PCT_6", c(0, 0, 0))
  )

  levels(stack[["BGC_ZONE"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("ICH", "CWH", "SBS"))
  levels(stack[["SLOPE_MOD"]]) <- data.frame(value = c(1L), label = c(""))
  levels(stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("XX", "RR", "SR"))
  levels(stack[["SPEC_CD_1"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("AT", "SB", "PL"))
  levels(stack[["SPEC_CD_2"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("AT", "SB", "PL"))
  levels(stack[["SPEC_CD_3"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("AT", "SB", "PL"))
  levels(stack[["SPEC_CD_4"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("AT", "SB", "PL"))
  levels(stack[["SPEC_CD_5"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("AT", "SB", "PL"))
  levels(stack[["SPEC_CD_6"]]) <- data.frame(value = c(1L, 2L, 3L), label = c("AT", "SB", "PL"))

  terra_result <- terra_rrm_apply_rules(stack, tree_rules)
  terra_labels <- .terra_rrm_lookup_factor_label(terra_result[["BEUMC_S1"]], terra::values(terra_result[["BEUMC_S1"]])[, 1])

  expect_equal(terra_labels, legacy_result$BEUMC_S1)
})

test_that(".terra_rrm_apply_river_adjacency_stage updates SITE_M3A from the rivers raster", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stack <- c(
    make_raster("SITE_M3A", c(2L, 2L, NA)),
    make_raster("rivers", c(0, 1, 1))
  )
  levels(stack[["SITE_M3A"]]) <- data.frame(value = c(1L, 2L), label = c("a", "b"))

  result <- .terra_rrm_apply_river_adjacency_stage(stack)
  expect_equal(as.vector(terra::values(result[["SITE_M3A"]])), c(2, 1, 1))
})

test_that(".terra_rrm_apply_river_adjacency_stage can buffer river adjacency", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values) * 1000, ymin = 0, ymax = 1000, crs = "EPSG:3005")
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  stack <- c(
    make_raster("SITE_M3A", rep(2L, 5)),
    make_raster("rivers", c(0, 0, 1, 0, 0))
  )
  levels(stack[["SITE_M3A"]]) <- data.frame(value = c(1L, 2L), label = c("a", "b"))

  result <- .terra_rrm_apply_river_adjacency_stage(stack, buffer_m = 1000)
  expect_equal(as.vector(terra::values(result[["SITE_M3A"]])), c(2, 1, 1, 1, 2))
})

test_that("terra_rrm_correct_bem_from_vri consolidates duplicates and applies primary rules", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values), ymin = 0, ymax = 1)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  code_vri <- function(field, labels) .terra_rrm_layer_codes(vri_stack, field, labels, raster_conv$vri)
  code_bem <- function(field, labels) .terra_rrm_layer_codes(bem_stack, field, labels, raster_conv$bem)

  vri_stack <- c(
    make_raster("BCLCS_LV_1", c(1L, 1L, 2L)),
    make_raster("BCLCS_LV_2", c(NA, NA, 1L)),
    make_raster("BCLCS_LV_3", c(NA, NA, 3L)),
    make_raster("BCLCS_LV_4", c(1L, 1L, 1L)),
    make_raster("BCLCS_LV_5", c(1L, 1L, 2L)),
    make_raster("SPEC_CD_1", c(1L, 1L, 1L)),
    make_raster("LAND_CD_1", c(1L, 1L, 1L)),
    make_raster("LBL_VEGCOV", c(1L, 1L, 1L))
  )

  levels(vri_stack[["BCLCS_LV_1"]]) <- data.frame(value = c(1L, 2L), label = c("N", "V"))
  levels(vri_stack[["BCLCS_LV_2"]]) <- data.frame(value = c(1L), label = c("N"))
  levels(vri_stack[["BCLCS_LV_3"]]) <- data.frame(value = c(3L), label = c("W"))
  levels(vri_stack[["BCLCS_LV_4"]]) <- data.frame(value = c(1L), label = c("TB"))
  levels(vri_stack[["BCLCS_LV_5"]]) <- data.frame(value = c(1L, 2L), label = c("LA", "RE"))
  levels(vri_stack[["SPEC_CD_1"]]) <- data.frame(value = c(1L), label = c("SB"))
  levels(vri_stack[["LAND_CD_1"]]) <- data.frame(value = c(1L), label = c("XX"))
  levels(vri_stack[["LBL_VEGCOV"]]) <- data.frame(value = c(1L), label = c("none"))

  bem_stack <- c(
    make_raster("BEUMC_S1", c(1L, 1L, 1L)),
    make_raster("BEUMC_S2", c(1L, 2L, 2L)),
    make_raster("BEUMC_S3", c(3L, 3L, 3L)),
    make_raster("SDEC_1", c(6L, 4L, 4L)),
    make_raster("SDEC_2", c(4L, 3L, 3L)),
    make_raster("SDEC_3", c(0L, 3L, 3L)),
    make_raster("AGE_CL_STS", c(0L, 0L, -1L)),
    make_raster("Area_Ha", c(1L, 50L, 5L)),
    make_raster("SPEC_PCT_1", c(0L, 0L, 0L)),
    make_raster("COV_PCT_1", c(0L, 0L, 0L)),
    make_raster("SLOPE_MOD", c(1L, 1L, 1L)),
    make_raster("DISTCLS_1", c(1L, 1L, 1L)),
    make_raster("REALM_1", c(1L, 1L, 1L)),
    make_raster("REALM_2", c(1L, 1L, 1L)),
    make_raster("REALM_3", c(1L, 1L, 1L))
  )

  levels(bem_stack[["BEUMC_S1"]]) <- data.frame(value = c(1L, 2L, 3L, 4L, 5L), label = c("XX", "WL", "YY", "OW", "RE"))
  levels(bem_stack[["BEUMC_S2"]]) <- data.frame(value = c(1L, 2L, 3L, 4L, 5L), label = c("XX", "WL", "YY", "OW", "RE"))
  levels(bem_stack[["BEUMC_S3"]]) <- data.frame(value = c(1L, 2L, 3L, 4L, 5L), label = c("XX", "WL", "YY", "OW", "RE"))
  levels(bem_stack[["SLOPE_MOD"]]) <- data.frame(value = c(1L), label = c(""))
  levels(bem_stack[["DISTCLS_1"]]) <- data.frame(value = c(1L, 2L), label = c("X", "F"))
  levels(bem_stack[["REALM_1"]]) <- data.frame(value = c(1L), label = c("R"))
  levels(bem_stack[["REALM_2"]]) <- data.frame(value = c(1L), label = c("R"))
  levels(bem_stack[["REALM_3"]]) <- data.frame(value = c(1L), label = c("R"))

  stack <- c(vri_stack, bem_stack)

  result <- terra_rrm_correct_bem_from_vri(stack, clear_site_ma = FALSE)
  raster_conv <- .terra_rrm_get_raster_conv()

  expect_equal(as.vector(terra::values(result[["SDEC_1"]])), c(10, 10, 10))
  expect_equal(as.vector(terra::values(result[["BEUMC_S1"]])), c(1, 19, 2))
  expect_equal(as.vector(terra::values(result[["SDEC_2"]]))[1], 0)
  expect_true(is.na(as.vector(terra::values(result[["REALM_2"]]))[2]))
  expect_true(is.na(as.vector(terra::values(result[["REALM_3"]]))[3]))
})