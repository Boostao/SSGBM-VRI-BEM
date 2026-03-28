source(file.path("..", "..", "R", "terra_rrm_corrections.R"))
source(file.path("..", "..", "R", "terra_rrm_export.R"))

test_that("terra_rrm_create_RRM_ecosystem_moose aggregates actual and projected ecosystems", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values) * 100, ymin = 0, ymax = 100)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  add_levels <- function(raster, values, labels) {
    levels(raster) <- data.frame(value = values, label = labels)
    raster
  }

  stack <- c(
    add_levels(make_raster("ECO_SEC", c(1L, 1L)), c(1L), c("E1")),
    add_levels(make_raster("BGC_ZONE", c(1L, 1L)), c(1L), c("ICH")),
    add_levels(make_raster("BGC_SUBZON", c(1L, 1L)), c(1L), c("dk")),
    make_raster("BGC_VRT", c(NA, NA)),
    make_raster("BGC_PHASE", c(NA, NA)),
    add_levels(make_raster("SLOPE_MOD", c(1L, 1L)), c(1L), c("j")),
    add_levels(make_raster("SITE_M3A", c(1L, 1L)), c(1L), c("a")),
    add_levels(make_raster("SNOW_CODE", c(1L, 1L)), c(1L), c("S1")),
    make_raster("ABOVE_ELEV_THOLD", c(1, 1)),
    make_raster("SDEC_1", c(10, 5)),
    make_raster("SDEC_2", c(0, 0)),
    make_raster("SDEC_3", c(0, 0)),
    add_levels(make_raster("BEUMC_S1", c(1L, 1L)), c(1L), c("FF")),
    make_raster("BEUMC_S2", c(NA, NA)),
    make_raster("BEUMC_S3", c(NA, NA)),
    add_levels(make_raster("FORESTED_1", c(1L, 1L)), c(1L), c("Y")),
    make_raster("FORESTED_2", c(NA, NA)),
    make_raster("FORESTED_3", c(NA, NA)),
    add_levels(make_raster("CROWN_ALL_1", c(1L, 1L)), c(1L), c("H")),
    make_raster("CROWN_ALL_2", c(NA, NA)),
    make_raster("CROWN_ALL_3", c(NA, NA)),
    add_levels(make_raster("STRCT_S1", c(1L, 2L)), c(1L, 2L), c("5", "2")),
    make_raster("STRCT_S2", c(NA, NA)),
    make_raster("STRCT_S3", c(NA, NA)),
    add_levels(make_raster("STAND_A1", c(1L, 2L)), c(1L, 2L), c("B", "M")),
    make_raster("STAND_A2", c(NA, NA)),
    make_raster("STAND_A3", c(NA, NA)),
    add_levels(make_raster("STS_1_Age_0_3", c(1L, 1L)), c(1L), c("1")),
    add_levels(make_raster("STS_1_Age_4_10", c(1L, 1L)), c(1L), c("2")),
    add_levels(make_raster("STS_1_Age_11_30", c(2L, 2L)), c(1L, 2L), c("2", "5")),
    add_levels(make_raster("STS_1_Age_31_40", c(1L, 1L)), c(1L), c("4")),
    add_levels(make_raster("STS_1_Age_41_60", c(1L, 1L)), c(1L), c("6")),
    add_levels(make_raster("STS_1_Age_61_80", c(1L, 1L)), c(1L), c("7")),
    add_levels(make_raster("STS_1_Age_81_139", c(1L, 1L)), c(1L), c("7")),
    add_levels(make_raster("STS_1_Age_140_249", c(1L, 1L)), c(1L), c("7")),
    add_levels(make_raster("STS_1_Age_gt_249", c(1L, 1L)), c(1L), c("7")),
    add_levels(make_raster("STAND_1_Age_0_15", c(1L, 1L)), c(1L), c("B")),
    add_levels(make_raster("STAND_1_Age_16_30", c(2L, 2L)), c(1L, 2L), c("B", "M")),
    add_levels(make_raster("STAND_1_Age_31_50", c(2L, 2L)), c(1L, 2L), c("B", "M")),
    add_levels(make_raster("STAND_1_Age_51_80", c(1L, 1L)), c(1L), c("C")),
    add_levels(make_raster("STAND_1_Age_gt_80", c(1L, 1L)), c(1L), c("C")),
    make_raster("STS_2_Age_0_3", c(NA, NA)), make_raster("STS_2_Age_4_10", c(NA, NA)), make_raster("STS_2_Age_11_30", c(NA, NA)),
    make_raster("STS_2_Age_31_40", c(NA, NA)), make_raster("STS_2_Age_41_60", c(NA, NA)), make_raster("STS_2_Age_61_80", c(NA, NA)),
    make_raster("STS_2_Age_81_139", c(NA, NA)), make_raster("STS_2_Age_140_249", c(NA, NA)), make_raster("STS_2_Age_gt_249", c(NA, NA)),
    make_raster("STS_3_Age_0_3", c(NA, NA)), make_raster("STS_3_Age_4_10", c(NA, NA)), make_raster("STS_3_Age_11_30", c(NA, NA)),
    make_raster("STS_3_Age_31_40", c(NA, NA)), make_raster("STS_3_Age_41_60", c(NA, NA)), make_raster("STS_3_Age_61_80", c(NA, NA)),
    make_raster("STS_3_Age_81_139", c(NA, NA)), make_raster("STS_3_Age_140_249", c(NA, NA)), make_raster("STS_3_Age_gt_249", c(NA, NA)),
    make_raster("STAND_2_Age_0_15", c(NA, NA)), make_raster("STAND_2_Age_16_30", c(NA, NA)), make_raster("STAND_2_Age_31_50", c(NA, NA)), make_raster("STAND_2_Age_51_80", c(NA, NA)), make_raster("STAND_2_Age_gt_80", c(NA, NA)),
    make_raster("STAND_3_Age_0_15", c(NA, NA)), make_raster("STAND_3_Age_16_30", c(NA, NA)), make_raster("STAND_3_Age_31_50", c(NA, NA)), make_raster("STAND_3_Age_51_80", c(NA, NA)), make_raster("STAND_3_Age_gt_80", c(NA, NA))
  )
  names(stack) <- c(
    "ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SLOPE_MOD", "SITE_M3A", "SNOW_CODE", "ABOVE_ELEV_THOLD",
    "SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "FORESTED_1", "FORESTED_2", "FORESTED_3",
    "CROWN_ALL_1", "CROWN_ALL_2", "CROWN_ALL_3", "STRCT_S1", "STRCT_S2", "STRCT_S3", "STAND_A1", "STAND_A2", "STAND_A3",
    "STS_1_Age_0_3", "STS_1_Age_4_10", "STS_1_Age_11_30", "STS_1_Age_31_40", "STS_1_Age_41_60", "STS_1_Age_61_80", "STS_1_Age_81_139", "STS_1_Age_140_249", "STS_1_Age_gt_249",
    "STAND_1_Age_0_15", "STAND_1_Age_16_30", "STAND_1_Age_31_50", "STAND_1_Age_51_80", "STAND_1_Age_gt_80",
    "STS_2_Age_0_3", "STS_2_Age_4_10", "STS_2_Age_11_30", "STS_2_Age_31_40", "STS_2_Age_41_60", "STS_2_Age_61_80", "STS_2_Age_81_139", "STS_2_Age_140_249", "STS_2_Age_gt_249",
    "STS_3_Age_0_3", "STS_3_Age_4_10", "STS_3_Age_11_30", "STS_3_Age_31_40", "STS_3_Age_41_60", "STS_3_Age_61_80", "STS_3_Age_81_139", "STS_3_Age_140_249", "STS_3_Age_gt_249",
    "STAND_2_Age_0_15", "STAND_2_Age_16_30", "STAND_2_Age_31_50", "STAND_2_Age_51_80", "STAND_2_Age_gt_80",
    "STAND_3_Age_0_15", "STAND_3_Age_16_30", "STAND_3_Age_31_50", "STAND_3_Age_51_80", "STAND_3_Age_gt_80"
  )

  result <- terra_rrm_create_RRM_ecosystem_moose(stack)

  actual_row <- result[STRCT == "5" & STAND == "B"]
  expect_equal(actual_row$Hectares, 1)

  actual_row2 <- result[STRCT == "2" & STAND == "M"]
  expect_equal(actual_row2$Hectares, 0.5)

  projected_low <- result[STRCT == "2" & is.na(STAND)]
  expect_true(nrow(projected_low) >= 1)

  expect_true(all(c("ECO_SEC", "BEUMC", "CROWN_ALL", "STRCT", "STAND", "FORESTED", "Hectares") %in% names(result)))
})

test_that("terra_rrm_create_RRM_ecosystem_bear includes Salmon in grouping", {
  skip_if_not_installed("terra")

  make_raster <- function(name, values) {
    raster <- terra::rast(nrows = 1, ncols = length(values), xmin = 0, xmax = length(values) * 100, ymin = 0, ymax = 100)
    terra::values(raster) <- values
    names(raster) <- name
    raster
  }

  add_levels <- function(raster, values, labels) {
    levels(raster) <- data.frame(value = values, label = labels)
    raster
  }

  base <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  stack <- c(
    add_levels(make_raster("ECO_SEC", 1L), 1L, "E1"),
    add_levels(make_raster("BGC_ZONE", 1L), 1L, "ICH"),
    add_levels(make_raster("BGC_SUBZON", 1L), 1L, "dk"),
    make_raster("BGC_VRT", NA),
    make_raster("BGC_PHASE", NA),
    add_levels(make_raster("SLOPE_MOD", 1L), 1L, "j"),
    add_levels(make_raster("SITE_M3A", 1L), 1L, "a"),
    add_levels(make_raster("Salmon", 1L), 1L, "Y"),
    add_levels(make_raster("SNOW_CODE", 1L), 1L, "S1"),
    make_raster("ABOVE_ELEV_THOLD", 1),
    make_raster("SDEC_1", 10), make_raster("SDEC_2", 0), make_raster("SDEC_3", 0),
    add_levels(make_raster("BEUMC_S1", 1L), 1L, "FF"), make_raster("BEUMC_S2", NA), make_raster("BEUMC_S3", NA),
    add_levels(make_raster("FORESTED_1", 1L), 1L, "Y"), make_raster("FORESTED_2", NA), make_raster("FORESTED_3", NA),
    add_levels(make_raster("CROWN_ALL_1", 1L), 1L, "H"), make_raster("CROWN_ALL_2", NA), make_raster("CROWN_ALL_3", NA),
    add_levels(make_raster("STRCT_S1", 1L), 1L, "5"), make_raster("STRCT_S2", NA), make_raster("STRCT_S3", NA),
    add_levels(make_raster("STAND_A1", 1L), 1L, "B"), make_raster("STAND_A2", NA), make_raster("STAND_A3", NA),
    add_levels(make_raster("STS_1_Age_0_3", 1L), 1L, "1"), add_levels(make_raster("STS_1_Age_4_10", 1L), 1L, "2"), add_levels(make_raster("STS_1_Age_11_30", 1L), 1L, "5"), add_levels(make_raster("STS_1_Age_31_40", 1L), 1L, "4"), add_levels(make_raster("STS_1_Age_41_60", 1L), 1L, "6"), add_levels(make_raster("STS_1_Age_61_80", 1L), 1L, "7"), add_levels(make_raster("STS_1_Age_81_139", 1L), 1L, "7"), add_levels(make_raster("STS_1_Age_140_249", 1L), 1L, "7"), add_levels(make_raster("STS_1_Age_gt_249", 1L), 1L, "7"),
    add_levels(make_raster("STAND_1_Age_0_15", 1L), 1L, "B"), add_levels(make_raster("STAND_1_Age_16_30", 1L), 1L, "M"), add_levels(make_raster("STAND_1_Age_31_50", 1L), 1L, "M"), add_levels(make_raster("STAND_1_Age_51_80", 1L), 1L, "C"), add_levels(make_raster("STAND_1_Age_gt_80", 1L), 1L, "C"),
    make_raster("STS_2_Age_0_3", NA), make_raster("STS_2_Age_4_10", NA), make_raster("STS_2_Age_11_30", NA), make_raster("STS_2_Age_31_40", NA), make_raster("STS_2_Age_41_60", NA), make_raster("STS_2_Age_61_80", NA), make_raster("STS_2_Age_81_139", NA), make_raster("STS_2_Age_140_249", NA), make_raster("STS_2_Age_gt_249", NA),
    make_raster("STS_3_Age_0_3", NA), make_raster("STS_3_Age_4_10", NA), make_raster("STS_3_Age_11_30", NA), make_raster("STS_3_Age_31_40", NA), make_raster("STS_3_Age_41_60", NA), make_raster("STS_3_Age_61_80", NA), make_raster("STS_3_Age_81_139", NA), make_raster("STS_3_Age_140_249", NA), make_raster("STS_3_Age_gt_249", NA),
    make_raster("STAND_2_Age_0_15", NA), make_raster("STAND_2_Age_16_30", NA), make_raster("STAND_2_Age_31_50", NA), make_raster("STAND_2_Age_51_80", NA), make_raster("STAND_2_Age_gt_80", NA),
    make_raster("STAND_3_Age_0_15", NA), make_raster("STAND_3_Age_16_30", NA), make_raster("STAND_3_Age_31_50", NA), make_raster("STAND_3_Age_51_80", NA), make_raster("STAND_3_Age_gt_80", NA)
  )
  names(stack) <- c(
    "ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SLOPE_MOD", "SITE_M3A", "Salmon", "SNOW_CODE", "ABOVE_ELEV_THOLD",
    "SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "FORESTED_1", "FORESTED_2", "FORESTED_3",
    "CROWN_ALL_1", "CROWN_ALL_2", "CROWN_ALL_3", "STRCT_S1", "STRCT_S2", "STRCT_S3", "STAND_A1", "STAND_A2", "STAND_A3",
    "STS_1_Age_0_3", "STS_1_Age_4_10", "STS_1_Age_11_30", "STS_1_Age_31_40", "STS_1_Age_41_60", "STS_1_Age_61_80", "STS_1_Age_81_139", "STS_1_Age_140_249", "STS_1_Age_gt_249",
    "STAND_1_Age_0_15", "STAND_1_Age_16_30", "STAND_1_Age_31_50", "STAND_1_Age_51_80", "STAND_1_Age_gt_80",
    "STS_2_Age_0_3", "STS_2_Age_4_10", "STS_2_Age_11_30", "STS_2_Age_31_40", "STS_2_Age_41_60", "STS_2_Age_61_80", "STS_2_Age_81_139", "STS_2_Age_140_249", "STS_2_Age_gt_249",
    "STS_3_Age_0_3", "STS_3_Age_4_10", "STS_3_Age_11_30", "STS_3_Age_31_40", "STS_3_Age_41_60", "STS_3_Age_61_80", "STS_3_Age_81_139", "STS_3_Age_140_249", "STS_3_Age_gt_249",
    "STAND_2_Age_0_15", "STAND_2_Age_16_30", "STAND_2_Age_31_50", "STAND_2_Age_51_80", "STAND_2_Age_gt_80",
    "STAND_3_Age_0_15", "STAND_3_Age_16_30", "STAND_3_Age_31_50", "STAND_3_Age_51_80", "STAND_3_Age_gt_80"
  )

  result <- terra_rrm_create_RRM_ecosystem_bear(stack)
  expect_true("Salmon" %in% names(result))
  expect_equal(unique(result$Salmon), "Y")
})