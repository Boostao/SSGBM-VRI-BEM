library(testthat)
library(sf)

make_clean_vri_bem_fixture <- function(polygons,
                                       bgc_zone,
                                       bgc_subzon,
                                       bgc_vrt,
                                       bgc_phase,
                                       beumc_s1) {
  n <- length(polygons)
  ordered_cols <- c(
    "aoi_name", "FEATURE_ID", "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5",
    "LAND_CD_1", "COV_PCT_1", "LBL_VEGCOV", "SOIL_MOISTURE_REGIME_1", "SOIL_NUTRIENT_REGIME",
    "SITE_POSITION_MESO", "CR_CLOSURE", "SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2", "SPEC_PCT_2",
    "SPEC_CD_3", "SPEC_PCT_3", "SPEC_CD_4", "SPEC_PCT_4", "SPEC_CD_5", "SPEC_PCT_5",
    "SPEC_CD_6", "SPEC_PCT_6", "PROJ_AGE_1", "POLY_COMM", "TEIS_ID", "SITE_INDEX",
    "EST_SITE_INDEX", "ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SDEC_1",
    "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "STRCT_S1", "STAND_A1", "SDEC_2",
    "BEUMC_S2", "REALM_2", "GROUP_2", "CLASS_2", "KIND_2", "STRCT_S2", "STAND_A2", "SDEC_3",
    "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3", "KIND_3", "SITE_M3A", "STRCT_S3", "STAND_A3",
    "AGE_CL_STS", "AGE_CL_STD", "FORESTED_1", "FORESTED_2", "FORESTED_3", "ABOVE_ELEV_THOLD",
    "VRI_SURVEY_YEAR", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD", "CROWN_ALL_1", "CROWN_ALL_2",
    "CROWN_ALL_3", "Salmon", "SLOPE_MOD", "ELEV", "MEAN_SLOPE", "MEAN_TRI", "MEAN_ASP",
    "STS_CLIMAX_1", "STAND_CLIMAX_1", "STS_CLIMAX_2", "STAND_CLIMAX_2", "STS_CLIMAX_3",
    "STAND_CLIMAX_3", "DSTRB_HIST", "MRSRD_Y", "MRSRD_A", "MRSRD_D", "MRSRD_S", "SIFA",
    "most_recent_fire", "percent_burned", "lbl_edit", "Lbl_edit_wl", "Shape_Area", "rrm_merge_ind"
  )

  template <- stats::setNames(rep(list(rep(NA_character_, n)), length(ordered_cols)), ordered_cols)
  template$aoi_name <- rep("Pacific", n)
  template$FEATURE_ID <- seq_len(n)
  template$BCLCS_LV_1 <- rep("T", n)
  template$SPEC_CD_1 <- rep("SW", n)
  template$SPEC_PCT_1 <- rep(100L, n)
  template$PROJ_AGE_1 <- rep(80L, n)
  template$TEIS_ID <- seq_len(n)
  template$BGC_ZONE <- bgc_zone
  template$BGC_SUBZON <- bgc_subzon
  template$BGC_VRT <- bgc_vrt
  template$BGC_PHASE <- bgc_phase
  template$SDEC_1 <- rep(10L, n)
  template$BEUMC_S1 <- beumc_s1
  template$REALM_1 <- rep("T", n)
  template$GROUP_1 <- rep("A", n)
  template$CLASS_1 <- rep("C", n)
  template$KIND_1 <- rep("D", n)
  template$STRCT_S1 <- rep("5", n)
  template$STAND_A1 <- rep("M", n)
  template$SITE_M3A <- rep("a", n)
  template$FORESTED_1 <- rep("Y", n)
  template$CROWN_ALL_1 <- rep("M", n)
  template$lbl_edit <- rep("", n)
  template$Lbl_edit_wl <- rep("", n)
  template$Shape_Area <- rep(NA_real_, n)
  template$rrm_merge_ind <- rep(FALSE, n)

  sf::st_sf(template, Shape = sf::st_as_sfc(polygons, crs = 3005))
}

test_that("clean_vri_bem_output merges slivers into the most similar touching polygon", {
  vri_bem <- make_clean_vri_bem_fixture(
    polygons = c(
      "POLYGON ((0 0, 40 0, 40 100, 0 100, 0 0))",
      "POLYGON ((40 0, 45 0, 45 100, 40 100, 40 0))",
      "POLYGON ((45 0, 85 0, 85 100, 45 100, 45 0))"
    ),
    bgc_zone = c("SBS", "SBS", "ESSF"),
    bgc_subzon = c("mc", "mc", "dk"),
    bgc_vrt = c(NA, NA, "1"),
    bgc_phase = c(NA, NA, "a"),
    beumc_s1 = c("AT", "AT", "BB")
  )

  result <- clean_vri_bem_output(vri_bem)
  areas <- stats::setNames(as.numeric(sf::st_area(result)), result$BEUMC_S1)

  expect_equal(nrow(result), 2L)
  expect_true(all(as.numeric(sf::st_area(result)) >= 1000))
  expect_equal(areas[["AT"]], 4500, tolerance = 1e-8)
  expect_equal(areas[["BB"]], 4000, tolerance = 1e-8)
})

test_that("clean_vri_bem_output falls back to the nearest polygon when no neighbour touches", {
  vri_bem <- make_clean_vri_bem_fixture(
    polygons = c(
      "POLYGON ((0 0, 40 0, 40 100, 0 100, 0 0))",
      "POLYGON ((50 0, 55 0, 55 100, 50 100, 50 0))",
      "POLYGON ((90 0, 130 0, 130 100, 90 100, 90 0))"
    ),
    bgc_zone = c("SBS", "ICH", "ESSF"),
    bgc_subzon = c("mc", "wk", "dk"),
    bgc_vrt = c(NA, NA, "1"),
    bgc_phase = c(NA, NA, "a"),
    beumc_s1 = c("AT", "WL", "BB")
  )

  result <- clean_vri_bem_output(vri_bem)
  areas <- stats::setNames(as.numeric(sf::st_area(result)), result$BEUMC_S1)

  expect_equal(nrow(result), 2L)
  expect_true(all(as.numeric(sf::st_area(result)) >= 1000))
  expect_equal(areas[["AT"]], 4500, tolerance = 1e-8)
  expect_equal(areas[["BB"]], 4000, tolerance = 1e-8)
})