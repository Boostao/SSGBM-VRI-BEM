library(duckplyr)
library(dplyr)
library(testthat)
options(DUCKPLYR_FALLBACK_INFO = TRUE, 
       # DUCKPLYR_FALLBACK_COLLECT = 0,
        DUCKPLYR_FALLBACK_AUTOUPLOAD = FALSE,
      DUCKPLYR_FALLBACK_VERBOSE = TRUE)
# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

eco_char_1 <- c(
  "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1",
  "SITE_S1", "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C",
  "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
  "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
  "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
  "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"
)
eco_char_2 <- sub("1", "2", eco_char_1)
eco_char_3 <- sub("1", "3", eco_char_1)

# Minimal duckplyr tibble for combine_duplicated_BEUMC tests.
make_dup_tbl <- function(beumc_s1, beumc_s2, beumc_s3 = NA_character_,
                         sdec_1 = 5L, sdec_2 = 3L, sdec_3 = 2L,
                         smpl_type = NA_character_) {
  n <- length(beumc_s1)
  df <- data.frame(
    BEUMC_S1    = beumc_s1,
    BEUMC_S2    = beumc_s2,
    BEUMC_S3    = beumc_s3,
    SDEC_1      = rep_len(sdec_1, n),
    SDEC_2      = rep_len(sdec_2, n),
    SDEC_3      = rep_len(sdec_3, n),
    SMPL_TYPE   = rep_len(smpl_type, n),
    lbl_edit    = rep_len("", n),
    row_updated = rep_len(FALSE, n),
    stringsAsFactors = FALSE
  )
  for (v in setdiff(c(eco_char_1, eco_char_2, eco_char_3), names(df))) df[[v]] <- NA_character_
  df[["TREE_C1"]]  <- NA_integer_; df[["TREE_C2"]]  <- NA_integer_; df[["TREE_C3"]]  <- NA_integer_
  df[["SHRUB_C1"]] <- NA_integer_; df[["SHRUB_C2"]] <- NA_integer_; df[["SHRUB_C3"]] <- NA_integer_
  duckplyr::as_duckdb_tibble(df, prudence = "stingy")
}

# Minimal duckplyr tibble for remove_inadequate_wetlands tests.
make_wetland_tbl <- function(beumc_s1, beumc_s2, beumc_s3,
                              sdec_1 = 4L, sdec_2 = 3L, sdec_3 = 3L,
                              bclcs_lv_4 = "TC",
                              smpl_type = NA_character_) {
  n <- length(beumc_s1)
  df <- data.frame(
    BEUMC_S1    = beumc_s1,
    BEUMC_S2    = beumc_s2,
    BEUMC_S3    = beumc_s3,
    SDEC_1      = rep_len(sdec_1, n),
    SDEC_2      = rep_len(sdec_2, n),
    SDEC_3      = rep_len(sdec_3, n),
    BCLCS_LV_4  = rep_len(bclcs_lv_4, n),
    BCLCS_LV_5  = rep_len("SP", n),
    LAND_CD_1   = rep_len("TC", n),
    SMPL_TYPE   = rep_len(smpl_type, n),
    lbl_edit    = rep_len("", n),
    row_updated = rep_len(FALSE, n),
    stringsAsFactors = FALSE
  )
  for (v in setdiff(c(eco_char_1, eco_char_2, eco_char_3), names(df))) df[[v]] <- NA_character_
  df[["TREE_C1"]]  <- NA_integer_; df[["TREE_C2"]]  <- NA_integer_; df[["TREE_C3"]]  <- NA_integer_
  df[["SHRUB_C1"]] <- NA_integer_; df[["SHRUB_C2"]] <- NA_integer_; df[["SHRUB_C3"]] <- NA_integer_
  duckplyr::as_duckdb_tibble(df, prudence = "stingy")
}

# Minimal duckplyr tibble for BCLCS-based correction tests.
make_correction_tbl <- function(bclcs_lv_1  = "N",
                                 bclcs_lv_2  = "N",
                                 bclcs_lv_3  = "U",
                                 bclcs_lv_4  = "SL",
                                 bclcs_lv_5  = "LA",
                                 area_ha     = 1.0,
                                 smpl_type   = NA_character_,
                                 spec_cd_1   = NA_character_,
                                 spec_pct_1  = NA_integer_,
                                 slope_mod   = NA_character_,
                                 land_cd_1   = NA_character_,
                                 cov_pct_1   = NA_integer_,
                                 age_cl_sts  = NA_integer_,
                                 lbl_vegcov  = NA_character_,
                                 sdec_1      = 10L,
                                 sdec_2      = 0L,
                                 sdec_3      = 0L,
                                 stand_a1    = NA_character_,
                                 beumc_s1    = "AT",
                                 row_updated = FALSE) {
  n <- max(lengths(list(bclcs_lv_5, area_ha, smpl_type)))
  df <- data.frame(
    BCLCS_LV_1  = rep_len(bclcs_lv_1,  n),
    BCLCS_LV_2  = rep_len(bclcs_lv_2,  n),
    BCLCS_LV_3  = rep_len(bclcs_lv_3,  n),
    BCLCS_LV_4  = rep_len(bclcs_lv_4,  n),
    BCLCS_LV_5  = rep_len(bclcs_lv_5,  n),
    Area_Ha     = rep_len(area_ha,      n),
    SMPL_TYPE   = rep_len(smpl_type,    n),
    SPEC_CD_1   = rep_len(spec_cd_1,    n),
    SPEC_PCT_1  = rep_len(spec_pct_1,   n),
    SLOPE_MOD   = rep_len(slope_mod,    n),
    LAND_CD_1   = rep_len(land_cd_1,    n),
    COV_PCT_1   = rep_len(cov_pct_1,    n),
    AGE_CL_STS  = rep_len(age_cl_sts,   n),
    LBL_VEGCOV  = rep_len(lbl_vegcov,   n),
    SDEC_1      = rep_len(sdec_1,       n),
    SDEC_2      = rep_len(sdec_2,       n),
    SDEC_3      = rep_len(sdec_3,       n),
    STAND_A1    = rep_len(stand_a1,     n),
    BEUMC_S1    = rep_len(beumc_s1,     n),
    row_updated = rep_len(row_updated,  n),
    lbl_edit    = rep_len("",           n),
    stringsAsFactors = FALSE
  )
  duckplyr::as_duckdb_tibble(df, prudence = "stingy")
}

# ---------------------------------------------------------------------------
# combine_duplicated_BEUMC
# ---------------------------------------------------------------------------

test_that("combine_duplicated_BEUMC: same BEUMC in S1 and S2 merges deciles", {
  tbl <- make_dup_tbl(
    beumc_s1 = c("WL", "SB"),
    beumc_s2 = c("WL", "TC"),
    sdec_1   = c(4L, 6L),
    sdec_2   = c(3L, 2L),
    sdec_3   = c(3L, 2L)
  )
  result <- combine_duplicated_BEUMC(tbl, use_ifelse = FALSE) |> collect()

  # Row 1: duplicate => SDEC_1 = 4+3 = 7, SDEC_2 shifts from SDEC_3
  expect_equal(result$SDEC_1[1], 7L)
  expect_equal(result$SDEC_2[1], 3L)
  expect_equal(result$SDEC_3[1], 0L)
  # Row 2: not duplicated => unchanged
  expect_equal(result$SDEC_1[2], 6L)
  expect_equal(result$SDEC_2[2], 2L)
})

test_that("combine_duplicated_BEUMC: lbl_edit updated for duplicated row", {
  result <- make_dup_tbl(beumc_s1 = "AT", beumc_s2 = "AT") |>
    combine_duplicated_BEUMC() |> collect()
  expect_match(result$lbl_edit[1], "Combined components")
})

test_that("combine_duplicated_BEUMC: non-NA SMPL_TYPE row is NOT combined", {
  result <- make_dup_tbl(
    beumc_s1 = "WL", beumc_s2 = "WL",
    sdec_1 = 4L, sdec_2 = 3L, smpl_type = "X"
  ) |> combine_duplicated_BEUMC() |>  collect()
  expect_equal(result$SDEC_1[1], 4L)
  expect_equal(result$SDEC_2[1], 3L)
  expect_equal(result$lbl_edit[1], "")
})

test_that("combine_duplicated_BEUMC: different BEUMC codes are not combined", {
  result <- make_dup_tbl(beumc_s1 = "AT", beumc_s2 = "SB", sdec_1 = 6L, sdec_2 = 4L) |>
    combine_duplicated_BEUMC() |> collect()
  expect_equal(result$SDEC_1[1], 6L)
  expect_equal(result$SDEC_2[1], 4L)
})

test_that("combine_duplicated_BEUMC: use_ifelse=TRUE marks row_updated on duplicate", {
  result <- make_dup_tbl(beumc_s1 = "AT", beumc_s2 = "AT") |>
    combine_duplicated_BEUMC(use_ifelse = TRUE) |> collect()
  expect_true(result$row_updated[1])
})

# ---------------------------------------------------------------------------
# remove_inadequate_wetlands
# ---------------------------------------------------------------------------

test_that("remove_inadequate_wetlands: WL in 3rd component of treed polygon removed", {
  result <- make_wetland_tbl(
    beumc_s1 = "AT", beumc_s2 = "SB", beumc_s3 = "WL",
    sdec_1 = 5L, sdec_2 = 3L, sdec_3 = 2L, bclcs_lv_4 = "TC"
  ) |> remove_inadequate_wetlands() |> collect()

  expect_equal(result$SDEC_3[1], 0L)
  expect_equal(result$SDEC_2[1], 5L)   # 3 + 2
  expect_true(is.na(result$BEUMC_S3[1]) || result$BEUMC_S3[1] != "WL")
})

test_that("remove_inadequate_wetlands: WL in 2nd component (no 3rd) absorbed into 1st", {
  result <- make_wetland_tbl(
    beumc_s1 = "AT", beumc_s2 = "WL", beumc_s3 = NA_character_,
    sdec_1 = 7L, sdec_2 = 3L, sdec_3 = 0L, bclcs_lv_4 = "TB"
  )  |> remove_inadequate_wetlands() |> collect()

  expect_equal(result$SDEC_1[1], 10L)   # 7 + 3
  expect_equal(result$SDEC_2[1], 0L)
  expect_true(is.na(result$BEUMC_S2[1]) || result$BEUMC_S2[1] != "WL")
})

test_that("remove_inadequate_wetlands: WL in 2nd component with 3rd present => shift from 3rd", {
  result <- make_wetland_tbl(
    beumc_s1 = "AT", beumc_s2 = "WL", beumc_s3 = "SB",
    sdec_1 = 5L, sdec_2 = 3L, sdec_3 = 2L, bclcs_lv_4 = "TC"
  ) |> remove_inadequate_wetlands() |> collect()

  expect_true(is.na(result$BEUMC_S2[1]) || result$BEUMC_S2[1] == "SB")
  expect_equal(result$SDEC_3[1], 0L)
})

test_that("remove_inadequate_wetlands: non-treed polygon (BCLCS_LV_4='SL') leaves WL intact", {
  result <- make_wetland_tbl(
    beumc_s1 = "AT", beumc_s2 = "SB", beumc_s3 = "WL",
    sdec_1 = 5L, sdec_2 = 3L, sdec_3 = 2L, bclcs_lv_4 = "SL"
  ) |> remove_inadequate_wetlands() |> collect()

  expect_equal(result$BEUMC_S3[1], "WL")
  expect_equal(result$SDEC_3[1], 2L)
})

test_that("remove_inadequate_wetlands: STRCT_S and STAND_A cleared for non-structural BEU types", {
  result <- make_wetland_tbl(
    beumc_s1 = "LL", beumc_s2 = "TC", beumc_s3 = NA_character_,
    sdec_1 = 10L, sdec_2 = 0L, sdec_3 = 0L, bclcs_lv_4 = "SL"
  ) |>
    mutate(STRCT_S1 = "7", STAND_A1 = "C", STRCT_S2 = "5", STAND_A2 = "M", BCLCS_LV_5 = "LA", LAND_CD_1 = "LA") |>
    remove_inadequate_wetlands() |> collect()

  expect_true(is.na(result$STRCT_S1[1]))
  expect_true(is.na(result$STAND_A1[1]))
  expect_true(is.na(result$STRCT_S2[1]))
  expect_true(is.na(result$STAND_A2[1]))
})

# ---------------------------------------------------------------------------
# check_allowed_bec_beu
# ---------------------------------------------------------------------------

make_bec_beu_tbl <- function(bgc_zone, bgc_subzon, beumc_s1,
                              beumc_s2 = NA_character_,
                              beumc_s3 = NA_character_) {
  n <- length(beumc_s1)
  df <- data.frame(
    BGC_ZONE    = bgc_zone,
    BGC_SUBZON  = bgc_subzon,
    BEUMC_S1    = beumc_s1,
    BEUMC_S2    = beumc_s2,
    BEUMC_S3    = beumc_s3,
    lbl_edit    = rep_len("", n),
    row_updated = rep_len(FALSE, n),
    stringsAsFactors = FALSE
  )
  duckplyr::as_duckdb_tibble(df, prudence = "stingy")
}

test_that("check_allowed_bec_beu: valid BEC/BEU combo leaves lbl_edit unchanged", {
  lut <- data.frame(
    BGC_Subzone = "SBSdk", BEU = "AT",
    Script_Rule = "OK", Change_to_BEU = NA_character_, Name = "n"
  )|> duckplyr::as_tbl()
  result <- make_bec_beu_tbl("SBS", "dk", "AT") |>
    check_allowed_bec_beu(beu_bec = lut) |> collect()
  expect_equal(result$lbl_edit[1], "")
  expect_equal(result$BEUMC_S1[1], "AT")
})

test_that("check_allowed_bec_beu: Error rule with 2-char Change_to_BEU corrects BEUMC_S1", {
  lut <- data.frame(
    BGC_Subzone = "SBSdk", BEU = "GL",
    Script_Rule = "Error", Change_to_BEU = "RO", Name = "n"
  )|> duckplyr::as_tbl()
  result <- make_bec_beu_tbl("SBS", "dk", "GL") |>
    check_allowed_bec_beu(beu_bec = lut) |> collect()
  expect_equal(result$BEUMC_S1[1], "RO")
  expect_match(result$lbl_edit[1], "corrected to RO in decile 1")
})

test_that("check_allowed_bec_beu: Error rule with NA Change_to_BEU flags lbl_edit, no BEUMC change", {
  lut <- data.frame(
    BGC_Subzone = "SBSdk", BEU = "GL",
    Script_Rule = "Error", Change_to_BEU = NA_character_, Name = "n"
  )|> duckplyr::as_tbl()
  result <- make_bec_beu_tbl("SBS", "dk", "GL") |>
    check_allowed_bec_beu(beu_bec = lut) |> collect()
  expect_equal(result$BEUMC_S1[1], "GL")
  expect_gt(nchar(result$lbl_edit[1]), 0)
})

# ---------------------------------------------------------------------------
# Lake / water body corrections (inline pipeline mirroring update_bem_from_vri)
# ---------------------------------------------------------------------------

apply_lake_corrections <- function(tbl) {
  tbl |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "LA" & Area_Ha <= 2  & !row_updated, "OW", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "OW", 10L, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "OW", "OW", BEUMC_S1),
           row_updated = if_else(correction_cd == "OW", TRUE, row_updated)) |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "LA" & Area_Ha > 2 & Area_Ha <= 60 & !row_updated, "LS", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "LS", 10L, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "LS", "LS", BEUMC_S1),
           row_updated = if_else(correction_cd == "LS", TRUE, row_updated)) |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "LA" & Area_Ha > 60 & !row_updated, "LL", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "LL", 10L, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "LL", "LL", BEUMC_S1),
           row_updated = if_else(correction_cd == "LL", TRUE, row_updated))
}

test_that("OW correction: BCLCS_LV_5='LA', Area_Ha <= 2 => BEUMC_S1='OW'", {
  result <- make_correction_tbl(bclcs_lv_1 = "N", bclcs_lv_5 = "LA", area_ha = 1.0) |>
    apply_lake_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "OW")
  expect_equal(result$SDEC_1[1], 10L)
})

test_that("LS correction: BCLCS_LV_5='LA', Area_Ha in (2,60] => BEUMC_S1='LS'", {
  result <- make_correction_tbl(bclcs_lv_1 = "N", bclcs_lv_5 = "LA", area_ha = 30.0) |>
    apply_lake_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "LS")
})

test_that("LL correction: BCLCS_LV_5='LA', Area_Ha > 60 => BEUMC_S1='LL'", {
  result <- make_correction_tbl(bclcs_lv_1 = "N", bclcs_lv_5 = "LA", area_ha = 100.0) |>
    apply_lake_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "LL")
})

test_that("lake corrections: row already updated is NOT overwritten", {
  result <- make_correction_tbl(
    bclcs_lv_1 = "N", bclcs_lv_5 = "LA", area_ha = 1.0,
    row_updated = TRUE, beumc_s1 = "ORIG"
  ) |> apply_lake_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "ORIG")
})

test_that("lake corrections: non-NA SMPL_TYPE row is skipped", {
  result <- make_correction_tbl(
    bclcs_lv_1 = "N", bclcs_lv_5 = "LA", area_ha = 1.0, smpl_type = "S"
  ) |> apply_lake_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "AT")
})

test_that("RI correction: BCLCS_LV_5 in c('RI','RS') => BEUMC_S1='RI'", {
  for (lv5 in c("RI", "RS")) {
    result <- make_correction_tbl(bclcs_lv_1 = "N", bclcs_lv_5 = lv5) |>
      mutate(correction_cd = if_else(
        is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 %in% c("RI", "RS") & !row_updated, "RI", "")) |>
      mutate(BEUMC_S1 = if_else(correction_cd == "RI", "RI", BEUMC_S1)) |>
      collect()
    expect_equal(result$BEUMC_S1[1], "RI", info = paste("BCLCS_LV_5 =", lv5))
  }
})

# ---------------------------------------------------------------------------
# STAND_A1 corrections (inline pipeline)
# ---------------------------------------------------------------------------

apply_stand_a1_corrections <- function(tbl) {
  tbl |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) & SPEC_CD_1 %in% c("AC", "ACB", "ACT", "AT", "EP") &
        SPEC_PCT_1 >= 75 & STAND_A1 %in% c("C", "M"), "STAND_A1_B", "")) |>
    mutate(STAND_A1 = if_else(correction_cd == "STAND_A1_B", "B", STAND_A1)) |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) & SPEC_CD_1 %in% c("AC", "ACB", "ACT", "AT", "EP") &
        SPEC_PCT_1 >= 50 & SPEC_PCT_1 < 75 & STAND_A1 %in% c("C", "B"),
      "STAND_A1_M", correction_cd)) |>
    mutate(STAND_A1 = if_else(correction_cd == "STAND_A1_M", "M", STAND_A1)) |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) &
        SPEC_CD_1 %in% c("B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW", "PA",
                          "PL", "PLI", "S", "SB", "SE", "SS", "SW", "SX", "SXW") &
        SPEC_PCT_1 >= 75 & STAND_A1 == "M", "STAND_A1_C", correction_cd)) |>
    mutate(STAND_A1 = if_else(correction_cd == "STAND_A1_C", "C", STAND_A1))
}

test_that("STAND_A1: deciduous >= 75% with C/M stand => changed to 'B'", {
  result <- make_correction_tbl(spec_cd_1 = "AT", spec_pct_1 = 80L, stand_a1 = "C") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "B")
})

test_that("STAND_A1: deciduous at exactly 75% => 'B'", {
  result <- make_correction_tbl(spec_cd_1 = "AT", spec_pct_1 = 75L, stand_a1 = "C") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "B")
})

test_that("STAND_A1: deciduous 50-74% with C/B stand => changed to 'M'", {
  result <- make_correction_tbl(spec_cd_1 = "AT", spec_pct_1 = 60L, stand_a1 = "C") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "M")
})

test_that("STAND_A1: deciduous at exactly 50% => 'M'", {
  result <- make_correction_tbl(spec_cd_1 = "AT", spec_pct_1 = 50L, stand_a1 = "C") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "M")
})

test_that("STAND_A1: conifer >= 75% with M stand => changed to 'C'", {
  result <- make_correction_tbl(spec_cd_1 = "PLI", spec_pct_1 = 80L, stand_a1 = "M") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "C")
})

test_that("STAND_A1: deciduous < 50% leaves stand unchanged", {
  result <- make_correction_tbl(spec_cd_1 = "AT", spec_pct_1 = 40L, stand_a1 = "C") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "C")
})

test_that("STAND_A1: conifer < 75% does not trigger 'C' override", {
  result <- make_correction_tbl(spec_cd_1 = "PLI", spec_pct_1 = 60L, stand_a1 = "M") |>
    apply_stand_a1_corrections() |> collect()
  expect_equal(result$STAND_A1[1], "M")
})

# ---------------------------------------------------------------------------
# Cliff (CL) correction (inline)
# ---------------------------------------------------------------------------

apply_cliff_correction <- function(tbl) {
  tbl |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) & SLOPE_MOD %in% c("q", "z") & !row_updated, "CL", "")) |>
    mutate(SDEC_1      = if_else(correction_cd == "CL", 10L, SDEC_1),
           BEUMC_S1    = if_else(correction_cd == "CL", "CL", BEUMC_S1),
           row_updated = if_else(correction_cd == "CL", TRUE, row_updated))
}

test_that("CL correction: SLOPE_MOD='q' => BEUMC_S1='CL'", {
  result <- make_correction_tbl(slope_mod = "q") |>
    apply_cliff_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "CL")
  expect_equal(result$SDEC_1[1], 10L)
})

test_that("CL correction: SLOPE_MOD='z' => BEUMC_S1='CL'", {
  result <- make_correction_tbl(slope_mod = "z") |>
    apply_cliff_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "CL")
})

test_that("CL correction: other SLOPE_MOD => unchanged", {
  result <- make_correction_tbl(slope_mod = "m") |>
    apply_cliff_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "AT")
})

# ---------------------------------------------------------------------------
# BB - Black Spruce Bog (inline)
# ---------------------------------------------------------------------------

apply_bb_correction <- function(tbl) {
  tbl |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) & SPEC_CD_1 == "SB" & SPEC_PCT_1 >= 90 & !row_updated, "BB", "")) |>
    mutate(SDEC_1      = if_else(correction_cd == "BB", 10L, SDEC_1),
           BEUMC_S1    = if_else(correction_cd == "BB", "BB", BEUMC_S1),
           row_updated = if_else(correction_cd == "BB", TRUE, row_updated))
}

test_that("BB correction: SB >= 90% => BEUMC_S1='BB'", {
  result <- make_correction_tbl(spec_cd_1 = "SB", spec_pct_1 = 95L) |>
    apply_bb_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "BB")
})

test_that("BB correction: SB at exactly 90% => BEUMC_S1='BB'", {
  result <- make_correction_tbl(spec_cd_1 = "SB", spec_pct_1 = 90L) |>
    apply_bb_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "BB")
})

test_that("BB correction: SB < 90% => unchanged", {
  result <- make_correction_tbl(spec_cd_1 = "SB", spec_pct_1 = 89L) |>
    apply_bb_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "AT")
})

test_that("BB correction: non-SB species >= 90% => unchanged", {
  result <- make_correction_tbl(spec_cd_1 = "PLI", spec_pct_1 = 95L) |>
    apply_bb_correction() |> collect()
  expect_equal(result$BEUMC_S1[1], "AT")
})

# ---------------------------------------------------------------------------
# UV corrections (inline)
# ---------------------------------------------------------------------------

apply_uv_corrections <- function(tbl) {
  uv_lv5 <- c("UV", "RS", "MU", "ES", "CB", "MN", "RM", "LL")
  uv_lcd  <- c("UV", "RS", "MU", "ES", "CB", "MN", "RM")
  tbl |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) & BCLCS_LV_5 %in% uv_lv5 & !row_updated, "UV", "")) |>
    mutate(SDEC_1      = if_else(correction_cd == "UV", 10L, SDEC_1),
           BEUMC_S1    = if_else(correction_cd == "UV", "UV", BEUMC_S1),
           row_updated = if_else(correction_cd == "UV", TRUE, row_updated)) |>
    mutate(correction_cd = if_else(
      is.na(SMPL_TYPE) & LAND_CD_1 %in% uv_lcd & COV_PCT_1 >= 95 & !row_updated, "UV_LANDCD", correction_cd)) |>
    mutate(SDEC_1      = if_else(correction_cd == "UV_LANDCD", 10L, SDEC_1),
           BEUMC_S1    = if_else(correction_cd == "UV_LANDCD", "UV", BEUMC_S1),
           row_updated = if_else(correction_cd == "UV_LANDCD", TRUE, row_updated))
}

test_that("UV correction: each BCLCS_LV_5 UV code triggers correction", {
  for (lv5 in c("UV", "RS", "MU", "ES", "CB", "MN", "RM", "LL")) {
    result <- make_correction_tbl(bclcs_lv_5 = lv5, bclcs_lv_1 = "V") |>
      apply_uv_corrections() |> collect()
    expect_equal(result$BEUMC_S1[1], "UV", info = paste("BCLCS_LV_5 =", lv5))
  }
})

test_that("UV correction via LAND_CD_1 and COV_PCT_1 >= 95 => BEUMC_S1='UV'", {
  result <- make_correction_tbl(bclcs_lv_5 = "SP", land_cd_1 = "MU", cov_pct_1 = 96L) |>
    apply_uv_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "UV")
})

test_that("UV correction via LAND_CD_1: COV_PCT_1 < 95 => unchanged", {
  result <- make_correction_tbl(bclcs_lv_5 = "SP", land_cd_1 = "MU", cov_pct_1 = 90L) |>
    apply_uv_corrections() |> collect()
  expect_equal(result$BEUMC_S1[1], "AT")
})

# ---------------------------------------------------------------------------
# validate_required_attributes
# ---------------------------------------------------------------------------

test_that("validate_required_attributes: passes when all columns present", {
  expect_silent(validate_required_attributes(data.frame(A = 1, B = 2), c("A", "B")))
})

test_that("validate_required_attributes: errors when column missing", {
  expect_error(validate_required_attributes(data.frame(A = 1), c("A", "B")), "B")
})

test_that("validate_required_attributes: error lists all missing columns", {
  expect_error(validate_required_attributes(data.frame(A = 1), c("B", "C")), "B")
  expect_error(validate_required_attributes(data.frame(A = 1), c("B", "C")), "C")
})
