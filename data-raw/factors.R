# BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3, BCLCS_LEVEL_4, BCLCS_LEVEL_5,
# SPECIES_CD_1, SPECIES_CD_2, SPECIES_CD_3, SPECIES_CD_4, SPECIES_CD_5, SPECIES_CD_6,
# SPECIES_PCT_1, SPECIES_PCT_2, SPECIES_PCT_3, SPECIES_PCT_4, SPECIES_PCT_5, SPECIES_PCT_6,
# CROWN_CLOSURE, LAND_COVER_CLASS_CD_1, EST_COVERAGE_PCT_1, LINE_5_VEGETATION_COVER,
# HARVEST_DATE, PROJ_AGE_1

library(data.table)

# VRI ----

# BCLCS_LV_1 ----
BCLCS_LV_1 <- data.table(value = c(NA_character_, "V", "N", "U"))
BCLCS_LV_1[, factor := 0:(.N-1)]
usethis::use_data(BCLCS_LV_1, overwrite = TRUE)

# BCLCS_LV_2 ----
BCLCS_LV_2 <- data.table(value = c(NA_character_, "T", "N", "L", "W"))
BCLCS_LV_2[, factor := 0:(.N-1)]
usethis::use_data(BCLCS_LV_2, overwrite = TRUE)

# BCLCS_LV_3 ----
BCLCS_LV_3 <- data.table(value = c(NA_character_, "W", "U", "A"))
BCLCS_LV_3[, factor := 0:(.N-1)]
usethis::use_data(BCLCS_LV_3, overwrite = TRUE)


# BCLCS_LV_4 ----
BCLCS_LV_4 <- data.table(value = c(NA_character_, "TC", "TB", "TM", "ST", "SL", "HE", "HF", "HG", "BY",
                                      "BM", "BL", "SI", "RO", "EL"))
BCLCS_LV_4[, factor := 0:(.N-1)]
usethis::use_data(BCLCS_LV_4, overwrite = TRUE)

# BCLCS_LV_5 ----
BCLCS_LV_5 <- data.table(value = c(NA_character_, "DE", "OP", "SP", "CL", "OP", "GL", "PN", "BR", "TA",
                                      "BI", "MZ", "LB", "RS", "ES", "LS", "RM", "BE", "LL",
                                      "BU", "RZ", "MU", "CB", "MN", "GP", "TZ", "RN", "UR",
                                      "AP", "MI", "OT", "LA", "RE", "RI", "OC"))
BCLCS_LV_5[, factor := 0:(.N-1)]
usethis::use_data(BCLCS_LV_5, overwrite = TRUE)

# SPEC_CD_1 ----
SPEC_CD_1 <- data.table(value = c(NA_character_, "AC", "AT", "B", "BL", "BA", "BG", "CW", "DR", "E", "EP",
                                     "EA", "FD", "H", "HW", "HM", "L", "LA", "LT", "LW", "MB",
                                     "PF", "PL", "PW", "PA", "PY", "PJ", "S", "SB", "SE", "SS",
                                     "SW", "YC", "DM", "R", "EW", "Cw", "Yc",
                                     "Fd", "Fdc", "Fdi", "Ba", "Bg", "BI", "Hm", "Hw",
                                     "Hxm", "Jr", "La", "Lt", "Lw", "Pj", "Pf",
                                     "Pl", "Pli", "Pxj", "Py", "Plc", "Pw", "Pa", "Sb", "Se",
                                     "Ss", "Sw", "Sx", "Sxw", "Sxl", "Sxs", "Tw", "Dr",
                                     "Up", "Ac", "Acb", "Act", "Ax", "At", "Ra",
                                     "Ea", "Exp", "Ep", "Ew", "Kc", "Vb", "Vv",
                                     "Vp", "Gp", "Mb", "Mv","Qg", "Wb", "Wp",
                                     "Wa", "Wd", "Ws", "Wt", "Xc", "Xh", "Zc", "Zh",
                                     "Ua", "Ad", "Ee", "Es", "Ey", "Vs", "Yp", "Bb", "Bp",
                                     "Bm", " Bc", "Ld", "Me", "Mn", "Ms", "Qe", "Qw",
                                     "Oa", "Ob", "Oc", "Od", "Oe", "Of", "Og", "Oh", "Oi",
                                     "Pm", "Pr", "Ps", "Sn"))

SPEC_CD_1[, factor := 0:(.N-1)]
usethis::use_data(SPEC_CD_1, overwrite = TRUE)

# SPEC_CD_2 ----
SPEC_CD_2 <- copy(SPEC_CD_1)
usethis::use_data(SPEC_CD_2, overwrite = TRUE)

# SPEC_CD_3 ----
SPEC_CD_3 <- copy(SPEC_CD_1)
usethis::use_data(SPEC_CD_3, overwrite = TRUE)

# SPEC_CD_4 ----
SPEC_CD_4 <- copy(SPEC_CD_1)
usethis::use_data(SPEC_CD_4, overwrite = TRUE)

# SPEC_CD_5 ----
SPEC_CD_5 <- copy(SPEC_CD_1)
usethis::use_data(SPEC_CD_5, overwrite = TRUE)

# SPEC_CD_6 ----
SPEC_CD_6 <- copy(SPEC_CD_1)
usethis::use_data(SPEC_CD_6, overwrite = TRUE)

# LAND_CD_1 ----
LAND_CD_1 <- data.table(value = c(NA_character_, "TB", "TC", "TM", "ST", "SL", "HE", "HF", "HG",
                                              "BY", "BM", "BL", "SI", "GL", "PN", "RO, BR",
                                              "TA", "BI", "MZ", "LB", "EL", "RS", "ES", "LS",
                                              "RM", "BE", "LL", "BU", "RZ", "MU", "CB", "MN", "GP",
                                              "TZ", "RN", "UR", "AP", "MI", "OT", "LA", "RE", "RI",
                                              "A", "OC"))
LAND_CD_1[, factor := 0:(.N-1)]
usethis::use_data(LAND_CD_1, overwrite = TRUE)

# LBL_VEGCOV ----
LBL_VEGCOV <- data.table(value = c(NA_character_, "sh", "he", "by"))
LBL_VEGCOV[, factor := 0:(.N-1)]
usethis::use_data(LBL_VEGCOV, overwrite = TRUE)


# BEM ----

c("SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
  "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1",
  "STRCT_M1", "STAND_A1", "SERAL_1", "TREE_C1", "SHRUB_C1", "DISTCLS_1", "DISTSCLS_1",
  "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "SDEC_2", "BEUMC_S2", "REALM_2",
  "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C",
  "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2",
  "SERAL_2", "TREE_C2", "SHRUB_C2", "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2",
  "SESUBCL_2", "COND_2", "VIAB_2", "SDEC_3", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3",
  "KIND_3", "SITE_S3", "SITEAM_S3A", "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3",
  "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "TREE_C3", "SHRUB_C3",
  "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3", "COND_3", "VIAB_3", "SLOPE_MOD",
  "FORESTED_1", "FORESTED_2", "FORESTED_3", "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3",
  "BCLCS_LV_4", "BCLCS_LV_5", "SPEC_CD_1", "AGE_CL_STS", "LAND_CD_1",
  "COV_PCT_1", "LBL_VEGCOV", "Area_Ha", "BGC_ZONE", "BGC_SUBZON",
  "SPEC_PCT_1")
