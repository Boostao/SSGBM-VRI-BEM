source(file.path("..", "..", "R", "terra_rrm_pipeline.R"))
source(file.path("..", "..", "R", "terra_rrm_corrections.R"))
source(file.path("..", "..", "R", "terra_rrm_ecosystem_merge.R"))

test_that("terra_rrm_merge_unique_ecosystem_fields merges lookup fields and derives structure", {
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
    add_levels(make_raster("BGC_ZONE", c(1L, 1L, 1L)), c(1L), c("ICH")),
    add_levels(make_raster("BGC_SUBZON", c(1L, 2L, 1L)), c(1L, 2L), c("dk", "dkp")),
    make_raster("BGC_VRT", c(NA, NA, NA)),
    make_raster("BGC_PHASE", c(NA, NA, NA)),
    add_levels(make_raster("BEUMC_S1", c(1L, 1L, 2L)), c(1L, 2L), c("FF", "WL")),
    make_raster("VRI_AGE_CL_STS", c(20, -1, 20)),
    make_raster("VRI_AGE_CL_STD", c(30, -1, 30)),
    add_levels(make_raster("SPEC_CD_1", c(1L, 3L, 1L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_1", c(80, 10, 10)),
    add_levels(make_raster("SPEC_CD_2", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_2", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_3", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_3", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_4", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_4", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_5", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_5", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_6", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_6", c(0, 0, 0)),
    make_raster("CR_CLOSURE", c(20, 50, 70)),
    add_levels(make_raster("BCLCS_LV_2", c(1L, 1L, 1L)), c(1L, 2L), c("X", "W")),
    add_levels(make_raster("BCLCS_LV_3", c(1L, 1L, 2L)), c(1L, 2L), c("X", "W")),
    add_levels(make_raster("BCLCS_LV_4", c(1L, 1L, 2L)), c(1L, 2L), c("X", "HE")),
    add_levels(make_raster("REALM_1", c(NA, NA, NA)), c(1L, 2L), c("T", "W")),
    add_levels(make_raster("GROUP_1", c(NA, NA, NA)), c(1L), c("G1")),
    add_levels(make_raster("CLASS_1", c(NA, NA, NA)), c(1L), c("C1")),
    add_levels(make_raster("KIND_1", c(NA, NA, NA)), c(1L), c("K1")),
    add_levels(make_raster("FORESTED_1", c(NA, NA, NA)), c(1L, 2L), c("Y", "N")),
    add_levels(make_raster("STS_CLIMAX_1", c(NA, NA, NA)), c(1L, 2L), c("5", "7")),
    add_levels(make_raster("STAND_CLIMAX_1", c(NA, NA, NA)), c(1L, 2L), c("M", "C")),
    add_levels(make_raster("STRCT_S1", c(NA, NA, NA)), c(1L, 2L, 3L), c("2", "5", "7")),
    add_levels(make_raster("STAND_A1", c(NA, NA, NA)), c(1L, 2L, 3L), c("B", "M", "C")),
    add_levels(make_raster("SNOW_CODE", c(NA, NA, NA)), c(1L), c("S1")),
    add_levels(make_raster("STD_VRI", c(NA, NA, NA)), c(1L, 2L, 3L), c("C", "M", "B")),
    add_levels(make_raster("CROWN_ALL", c(NA, NA, NA)), c(1L, 2L, 3L, 4L), c("VL-L", "M", "H", "VH"))
  )
  names(stack) <- c(
    "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC_S1", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD",
    "SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2", "SPEC_PCT_2", "SPEC_CD_3", "SPEC_PCT_3", "SPEC_CD_4", "SPEC_PCT_4",
    "SPEC_CD_5", "SPEC_PCT_5", "SPEC_CD_6", "SPEC_PCT_6", "CR_CLOSURE", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4",
    "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "FORESTED_1", "STS_CLIMAX_1", "STAND_CLIMAX_1", "STRCT_S1",
    "STAND_A1", "SNOW_CODE", "STD_VRI", "CROWN_ALL"
  )

  stack <- terra_rrm_add_ecosystem_keys(stack)

  unique_ecosystem_dt <- data.frame(
    BGC_ZONE = c("ICH", "ICH"),
    BGC_SUBZON = c("dk", "dkp"),
    BGC_VRT = c(NA, NA),
    BGC_PHASE = c(NA, NA),
    BEU_MC = c("FF", "FF"),
    REALM = c("T", "T"),
    GROUP = c("G1", "G1"),
    CLASS = c("C1", "C1"),
    KIND = c("K1", "K1"),
    Snow_Code = c("S1", "S1"),
    `Forested (Y/N)` = c("Y", "N"),
    Strct_Climax = c("7", "7"),
    Stand_Climax = c("C", "C"),
    `Stand_Age_0-15` = c("M", "M"),
    `Stand_Age_16-30` = c("M", "M"),
    `Stand_Age_31-50` = c("M", "M"),
    `Stand_Age_51-80` = c("M", "M"),
    `Stand_Age_80+` = c("M", "M"),
    `Struct_Age_0-3` = c("5", "5"),
    `Struct_Age_4-10` = c("5", "5"),
    `Struct_Age_11-30` = c("5", "5"),
    `Struct_Age_31-40` = c("5", "5"),
    `Struct_Age_41-60` = c("5", "5"),
    `Struct_Age_61-80` = c("5", "5"),
    `Struct_Age_81-139` = c("5", "5"),
    `Struct_Age_140-249` = c("5", "5"),
    `Struct_Age_250+` = c("5", "5"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  wetland_row <- unique_ecosystem_dt[1, , drop = FALSE]
  wetland_row$BEU_MC <- "WL"
  wetland_row[["Forested (Y/N)"]] <- "Y"
  wetland_row$Stand_Climax <- "M"
  wetland_row$Strct_Climax <- "5"
  unique_ecosystem_dt <- rbind(unique_ecosystem_dt, wetland_row)

  stack <- terra_rrm_add_ecosystem_keys(stack)
  result <- terra_rrm_merge_unique_ecosystem_fields(stack, unique_ecosystem_dt)

  expect_equal(as.vector(terra::values(result[["SNOW_CODE"]])), c(1, 1, 1))
  expect_equal(as.vector(terra::values(result[["STD_VRI"]])), c(3, 1, 1))
  expect_equal(as.vector(terra::values(result[["CROWN_ALL"]])), c(1, 3, 4))
  expect_equal(as.vector(terra::values(result[["FORESTED_1"]])), c(1, 2, 1))
  expect_equal(as.vector(terra::values(result[["STRCT_S1"]])), c(2, 3, 1))
  expect_equal(as.vector(terra::values(result[["STAND_A1"]])), c(1, 3, 2))
})

test_that("terra_rrm_merge_unique_ecosystem_fields writes filename paths without category warnings", {
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
    add_levels(make_raster("BGC_ZONE", c(1L, 1L, 1L)), c(1L), c("ICH")),
    add_levels(make_raster("BGC_SUBZON", c(1L, 2L, 1L)), c(1L, 2L), c("dk", "dkp")),
    make_raster("BGC_VRT", c(NA, NA, NA)),
    make_raster("BGC_PHASE", c(NA, NA, NA)),
    add_levels(make_raster("BEUMC_S1", c(1L, 1L, 2L)), c(1L, 2L), c("FF", "WL")),
    make_raster("VRI_AGE_CL_STS", c(20, -1, 20)),
    make_raster("VRI_AGE_CL_STD", c(30, -1, 30)),
    add_levels(make_raster("SPEC_CD_1", c(1L, 3L, 1L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_1", c(80, 10, 10)),
    add_levels(make_raster("SPEC_CD_2", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_2", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_3", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_3", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_4", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_4", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_5", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_5", c(0, 0, 0)),
    add_levels(make_raster("SPEC_CD_6", c(2L, 2L, 2L)), c(1L, 2L, 3L), c("AT", "PL", "SB")),
    make_raster("SPEC_PCT_6", c(0, 0, 0)),
    make_raster("CR_CLOSURE", c(20, 50, 70)),
    add_levels(make_raster("BCLCS_LV_2", c(1L, 1L, 1L)), c(1L, 2L), c("X", "W")),
    add_levels(make_raster("BCLCS_LV_3", c(1L, 1L, 2L)), c(1L, 2L), c("X", "W")),
    add_levels(make_raster("BCLCS_LV_4", c(1L, 1L, 2L)), c(1L, 2L), c("X", "HE")),
    add_levels(make_raster("REALM_1", c(NA, NA, NA)), c(1L, 2L), c("T", "W")),
    add_levels(make_raster("GROUP_1", c(NA, NA, NA)), c(1L), c("G1")),
    add_levels(make_raster("CLASS_1", c(NA, NA, NA)), c(1L), c("C1")),
    add_levels(make_raster("KIND_1", c(NA, NA, NA)), c(1L), c("K1")),
    add_levels(make_raster("FORESTED_1", c(NA, NA, NA)), c(1L, 2L), c("Y", "N")),
    add_levels(make_raster("STS_CLIMAX_1", c(NA, NA, NA)), c(1L, 2L), c("5", "7")),
    add_levels(make_raster("STAND_CLIMAX_1", c(NA, NA, NA)), c(1L, 2L), c("M", "C")),
    add_levels(make_raster("STRCT_S1", c(NA, NA, NA)), c(1L, 2L, 3L), c("2", "5", "7")),
    add_levels(make_raster("STAND_A1", c(NA, NA, NA)), c(1L, 2L, 3L), c("B", "M", "C")),
    add_levels(make_raster("SNOW_CODE", c(NA, NA, NA)), c(1L), c("S1")),
    add_levels(make_raster("STD_VRI", c(NA, NA, NA)), c(1L, 2L, 3L), c("C", "M", "B")),
    add_levels(make_raster("CROWN_ALL", c(NA, NA, NA)), c(1L, 2L, 3L, 4L), c("VL-L", "M", "H", "VH"))
  )
  names(stack) <- c(
    "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC_S1", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD",
    "SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2", "SPEC_PCT_2", "SPEC_CD_3", "SPEC_PCT_3", "SPEC_CD_4", "SPEC_PCT_4",
    "SPEC_CD_5", "SPEC_PCT_5", "SPEC_CD_6", "SPEC_PCT_6", "CR_CLOSURE", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4",
    "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "FORESTED_1", "STS_CLIMAX_1", "STAND_CLIMAX_1", "STRCT_S1",
    "STAND_A1", "SNOW_CODE", "STD_VRI", "CROWN_ALL"
  )

  stack <- terra_rrm_add_ecosystem_keys(stack)

  unique_ecosystem_dt <- data.frame(
    BGC_ZONE = c("ICH", "ICH"),
    BGC_SUBZON = c("dk", "dkp"),
    BGC_VRT = c(NA, NA),
    BGC_PHASE = c(NA, NA),
    BEU_MC = c("FF", "FF"),
    REALM = c("T", "T"),
    GROUP = c("G1", "G1"),
    CLASS = c("C1", "C1"),
    KIND = c("K1", "K1"),
    Snow_Code = c("S1", "S1"),
    `Forested (Y/N)` = c("Y", "N"),
    Strct_Climax = c("7", "7"),
    Stand_Climax = c("C", "C"),
    `Stand_Age_0-15` = c("M", "M"),
    `Stand_Age_16-30` = c("M", "M"),
    `Stand_Age_31-50` = c("M", "M"),
    `Stand_Age_51-80` = c("M", "M"),
    `Stand_Age_80+` = c("M", "M"),
    `Struct_Age_0-3` = c("5", "5"),
    `Struct_Age_4-10` = c("5", "5"),
    `Struct_Age_11-30` = c("5", "5"),
    `Struct_Age_31-40` = c("5", "5"),
    `Struct_Age_41-60` = c("5", "5"),
    `Struct_Age_61-80` = c("5", "5"),
    `Struct_Age_81-139` = c("5", "5"),
    `Struct_Age_140-249` = c("5", "5"),
    `Struct_Age_250+` = c("5", "5"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  temp_file <- tempfile(fileext = ".tif")
  result <- suppressWarnings(
    terra_rrm_merge_unique_ecosystem_fields(stack, unique_ecosystem_dt, filename = temp_file, overwrite = TRUE)
  )
  expect_true(file.exists(temp_file))
  expect_true(nrow(terra::cats(result[["SNOW_CODE"]])[[1]]) > 0L)
  stand_a1_values <- as.vector(terra::values(result[["STAND_A1"]]))
  expect_equal(stand_a1_values[1:2], c(1, 3))
  expect_true(is.na(stand_a1_values[[3]]))
})

test_that("terra_rrm_reduce_unique_ecosystem_dt removes duplicate and irrelevant LUT rows", {
  unique_ecosystem_dt <- data.frame(
    BGC_ZONE = c("ICH", "ICH", "SBS", "ICH"),
    BGC_SUBZON = c("dk", "dk", "dk", "dkp"),
    BGC_VRT = c(NA, NA, NA, NA),
    BGC_PHASE = c(NA, NA, NA, NA),
    BEU_MC = c("FF", "FF", "WL", "FF"),
    REALM = c("T", "T", "T", "T"),
    GROUP = c("G1", "G1", "G2", "G1"),
    CLASS = c("C1", "C1", "C2", "C1"),
    KIND = c("K1", "K1", "K2", "K1"),
    Snow_Code = c("S1", "S1", "S2", "S1"),
    `Forested (Y/N)` = c("Y", "Y", "N", "Y"),
    Strct_Climax = c("7", "7", "5", "7"),
    Stand_Climax = c("C", "C", "M", "C"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  reduced <- terra_rrm_reduce_unique_ecosystem_dt(
    unique_ecosystem_dt,
    available_codes = data.frame(
      BGC_ZONE = c("ICH"),
      BGC_SUBZON = c("dk"),
      BGC_VRT = c(NA),
      BGC_PHASE = c(NA),
      BEU_MC = c("FF")
    )
  )

  expect_equal(nrow(reduced), 1L)
  expect_equal(reduced$BGC_ZONE[[1]], "ICH")
  expect_equal(reduced$BGC_SUBZON[[1]], "dk")
  expect_equal(reduced$BEU_MC[[1]], "FF")
  expect_true("ecosystem_key" %in% names(reduced))
})

test_that(".terra_rrm_available_ecosystem_codes includes BEUMC_S2 and BEUMC_S3 combinations", {
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
    add_levels(make_raster("BGC_ZONE", c(1L, 1L)), c(1L), c("ICH")),
    add_levels(make_raster("BGC_SUBZON", c(1L, 1L)), c(1L), c("dk")),
    make_raster("BGC_VRT", c(NA, NA)),
    make_raster("BGC_PHASE", c(NA, NA)),
    add_levels(make_raster("BEUMC_S1", c(1L, 1L)), c(1L), c("FF")),
    add_levels(make_raster("BEUMC_S2", c(2L, 1L)), c(1L, 2L), c("FF", "WL")),
    add_levels(make_raster("BEUMC_S3", c(1L, 2L)), c(1L, 2L), c("FF", "WL"))
  )
  names(stack) <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3")

  codes <- .terra_rrm_available_ecosystem_codes(stack)
  expect_true(is.data.frame(codes))
  expect_true(all(c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC") %in% names(codes)))
  expect_true("FF" %in% codes$BEU_MC)
  expect_true("WL" %in% codes$BEU_MC)
})

test_that("terra_rrm_merge_unique_ecosystem_fields fails when ecosystem key layers are missing", {
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
    add_levels(make_raster("BGC_ZONE", c(1L)), c(1L), c("ICH")),
    add_levels(make_raster("BGC_SUBZON", c(1L)), c(1L), c("dk")),
    make_raster("BGC_VRT", NA),
    make_raster("BGC_PHASE", NA),
    add_levels(make_raster("BEUMC_S1", c(1L)), c(1L), c("FF")),
    make_raster("VRI_AGE_CL_STS", 20),
    make_raster("VRI_AGE_CL_STD", 30),
    add_levels(make_raster("SPEC_CD_1", c(1L)), c(1L), c("AT")),
    make_raster("SPEC_PCT_1", 80),
    add_levels(make_raster("SPEC_CD_2", c(1L)), c(1L), c("AT")),
    make_raster("SPEC_PCT_2", 0),
    add_levels(make_raster("SPEC_CD_3", c(1L)), c(1L), c("AT")),
    make_raster("SPEC_PCT_3", 0),
    add_levels(make_raster("SPEC_CD_4", c(1L)), c(1L), c("AT")),
    make_raster("SPEC_PCT_4", 0),
    add_levels(make_raster("SPEC_CD_5", c(1L)), c(1L), c("AT")),
    make_raster("SPEC_PCT_5", 0),
    add_levels(make_raster("SPEC_CD_6", c(1L)), c(1L), c("AT")),
    make_raster("SPEC_PCT_6", 0),
    make_raster("CR_CLOSURE", 20),
    add_levels(make_raster("BCLCS_LV_2", c(1L)), c(1L), c("X")),
    add_levels(make_raster("BCLCS_LV_3", c(1L)), c(1L), c("X")),
    add_levels(make_raster("BCLCS_LV_4", c(1L)), c(1L), c("X"))
  )
  names(stack) <- c(
    "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC_S1", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD",
    "SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2", "SPEC_PCT_2", "SPEC_CD_3", "SPEC_PCT_3", "SPEC_CD_4", "SPEC_PCT_4",
    "SPEC_CD_5", "SPEC_PCT_5", "SPEC_CD_6", "SPEC_PCT_6", "CR_CLOSURE", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4"
  )

  unique_ecosystem_dt <- data.frame(
    BGC_ZONE = "ICH",
    BGC_SUBZON = "dk",
    BGC_VRT = NA,
    BGC_PHASE = NA,
    BEU_MC = "FF",
    REALM = "T",
    GROUP = "G1",
    CLASS = "C1",
    KIND = "K1",
    Snow_Code = "S1",
    `Forested (Y/N)` = "Y",
    Strct_Climax = "7",
    Stand_Climax = "C",
    `Stand_Age_0-15` = "M",
    `Stand_Age_16-30` = "M",
    `Stand_Age_31-50` = "M",
    `Stand_Age_51-80` = "M",
    `Stand_Age_80+` = "M",
    `Struct_Age_0-3` = "5",
    `Struct_Age_4-10` = "5",
    `Struct_Age_11-30` = "5",
    `Struct_Age_31-40` = "5",
    `Struct_Age_41-60` = "5",
    `Struct_Age_61-80` = "5",
    `Struct_Age_81-139` = "5",
    `Struct_Age_140-249` = "5",
    `Struct_Age_250+` = "5",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_error(
    terra_rrm_merge_unique_ecosystem_fields(stack, unique_ecosystem_dt),
    "Missing required layers for terra_rrm_merge_unique_ecosystem_fields"
  )
})

test_that("terra_rrm_add_ecosystem_keys builds component-specific numeric key layers", {
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
    add_levels(make_raster("BGC_ZONE", c(1L)), c(1L), c("ICH")),
    add_levels(make_raster("BGC_SUBZON", c(1L)), c(1L), c("dk")),
    make_raster("BGC_VRT", NA),
    make_raster("BGC_PHASE", NA),
    add_levels(make_raster("BEUMC_S1", c(2L)), c(1L, 2L), c("FF", "WL")),
    add_levels(make_raster("BEUMC_S2", c(1L)), c(1L, 2L), c("FF", "WL")),
    make_raster("SDEC_1", 10),
    make_raster("SDEC_2", 0)
  )
  names(stack) <- c(
    "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC_S1", "BEUMC_S2", "SDEC_1", "SDEC_2"
  )

  result <- terra_rrm_add_ecosystem_keys(stack)

  expect_true(all(c("ecosystem_key_s1", "ecosystem_key_s2") %in% names(result)))
  expect_equal(
    .terra_rrm_lookup_factor_label(result[["ecosystem_key_s1"]], as.vector(terra::values(result[["ecosystem_key_s1"]]))),
    "ICH::dk::<NA>::<NA>::WL"
  )
  expect_equal(
    .terra_rrm_lookup_factor_label(result[["ecosystem_key_s2"]], as.vector(terra::values(result[["ecosystem_key_s2"]]))),
    "ICH::dk::<NA>::<NA>::FF"
  )
})