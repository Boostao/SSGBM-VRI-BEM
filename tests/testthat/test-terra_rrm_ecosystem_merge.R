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

  result <- terra_rrm_merge_unique_ecosystem_fields(stack, unique_ecosystem_dt)

  expect_equal(as.vector(terra::values(result[["SNOW_CODE"]])), c(1, 1, 1))
  expect_equal(as.vector(terra::values(result[["STD_VRI"]])), c(3, 1, 1))
  expect_equal(as.vector(terra::values(result[["CROWN_ALL"]])), c(1, 3, 4))
  expect_equal(as.vector(terra::values(result[["FORESTED_1"]])), c(1, 2, 1))
  expect_equal(as.vector(terra::values(result[["STRCT_S1"]])), c(2, 3, 1))
  expect_equal(as.vector(terra::values(result[["STAND_A1"]])), c(1, 3, 2))
})