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

# [1] "Area_Ha"    "BGC_ZONE"   "BGC_SUBZON" "SDEC_1"     "BEUMC_S1"   "REALM_1"    "GROUP_1"    "CLASS_1"    "KIND_1" SITE_S3     "SITEAM_S1A"    "SITEAM_S1A" "SITEAM_S1B" "SITEAM_S1C" "SITEAM_S1D" "SITEMC_S1"
# [16] "SITE_M1A"   "SITE_M1B"   "STRCT_S1"   "STRCT_M1"   "STAND_A1"   "SERAL_1"    "TREE_C1"    "SHRUB_C1"   "DISTCLS_1"  "DISTSCLS_1" "DISSSCLS_1" "SECL_1"     "SESUBCL_1"  "COND_1"     "VIAB_1"
# [31] "SDEC_2"     "BEUMC_S2"   "REALM_2"    "GROUP_2"    "CLASS_2"    "KIND_2"     "SITE_S2"    "SITEAM_S2A" "SITEAM_S2B" "SITEAM_S2C" "SITEAM_S2D" "SITEMC_S2"  "SITE_M2A"   "SITE_M2B"   "STRCT_S2"
# [46] "STRCT_M2"   "STAND_A2"   "SERAL_2"    "TREE_C2"    "SHRUB_C2"   "DISTCLS_2"  "DISTSCLS_2" "DISSSCLS_2" "SECL_2"     "SESUBCL_2"  "COND_2"     "VIAB_2"     "SDEC_3"     "BEUMC_S3"   "REALM_3"
# [61] "GROUP_3"    "CLASS_3"    "KIND_3"     "SITE_S3"    "SITEAM_S3A" "SITEAM_S3B" "SITEAM_S3C" "SITEAM_S3D" "SITEMC_S3"  "SITE_M3A"   "SITE_M3B"   "STRCT_S3"   "STRCT_M3"   "STAND_A3"   "SERAL_3"
# [76] "TREE_C3"    "SHRUB_C3"   "DISTCLS_3"  "DISTSCLS_3" "DISSSCLS_3" "SECL_3"     "SESUBCL_3"  "COND_3"     "VIAB_3"     "SLOPE_MOD"  "AGE_CL_STS" "FORESTED_1" "FORESTED_2" "FORESTED_3"

# BGC_ZONE ----
BGC_ZONE <- data.table(value = c(NA_character_,"SWB", "BAFA", "BWBS", "ESSF", "CMA", "MH", "SBS", "CWH", "ICH", "SBPS"))
BGC_ZONE[, factor := 0:(.N-1)]
usethis::use_data(BGC_ZONE, overwrite = TRUE)

# BGC_SUBZON ----
BGC_SUBZON <- data.table(value = c(NA_character_,"vks", "un", "vk", "dk",  "uns", "unp", "wm",  "mk",  "mm",  "mks", "wc",  "mmp", "mc",  "mcp",
                                   "wv",  "wvp", "vc",  "vh",  "ws",  "mv",  "wk",  "whp", "vm",  "wh",  "mvp", "dw",  "mkp"))
BGC_SUBZON[, factor := 0:(.N-1)]
usethis::use_data(BGC_SUBZON, overwrite = TRUE)

# BEUMC_S1 ----
BEUMC_S1 <- data.table(value = c(NA_character_,"GL", "AU", "SU", "PR", "AT", "AV", "BP", "FB", "MS", "WL", "AM", "EW", "SF", "EF", "GB", "LL", "SM", "LP", "LS", "ER", "MF", "HS", "AG", "SR", "SP", "HP", "BK", "WR", "ME",
                                 "RO", "ST", "FS", "CS", "RR", "IS", "WP", "UV", "AH", "SL", "ES","FP", "FR", "WB", "CH", "YM", "MI", "DF", "HL", "UR", "YB", "CB", "YS", "CF", "RE",
                                 "SD", "RS", "BG", "OW"))
BEUMC_S1[, factor := 0:(.N-1)]
usethis::use_data(BEUMC_S1, overwrite = TRUE)

# BEUMC_S2 ----
BEUMC_S2 <- copy(BEUMC_S1)
usethis::use_data(BEUMC_S2, overwrite = TRUE)

# BEUMC_S3 ----
BEUMC_S3 <- copy(BEUMC_S1)
usethis::use_data(BEUMC_S3, overwrite = TRUE)

# REALM_1 ----
REALM_1 <- data.table(value = c(NA_character_, "T", "W", "O", "M", "E"))
REALM_1[, factor := 0:(.N-1)]
usethis::use_data(REALM_1, overwrite = TRUE)

# REALM_2 ----
REALM_2 <- copy(REALM_1)
usethis::use_data(REALM_2, overwrite = TRUE)

# REALM_3 ----
REALM_3 <- copy(REALM_1)
usethis::use_data(REALM_3, overwrite = TRUE)

# GROUP_1 ----
GROUP_1 <- data.table(value = c(NA_character_, "A", "S", "F", "V", "G", "M", "R", "E", "X", "P"))
GROUP_1[, factor := 0:(.N-1)]
usethis::use_data(GROUP_1, overwrite = TRUE)

# GROUP_2 ----
GROUP_2 <- copy(GROUP_1)
usethis::use_data(GROUP_2, overwrite = TRUE)

# GROUP_3 ----
GROUP_3 <- copy(GROUP_1)
usethis::use_data(GROUP_3, overwrite = TRUE)

# CLASS_1 ----
CLASS_1 <- data.table(value = c(NA_character_, "c", "t", "s", "b", "m", "a", "g", "k", "o", "h"))
CLASS_1[, factor := 0:(.N-1)]
usethis::use_data(CLASS_1, overwrite = TRUE)

# CLASS_2 ----
CLASS_2 <- copy(CLASS_1)
usethis::use_data(CLASS_2, overwrite = TRUE)

# CLASS_3 ----
CLASS_3 <- copy(CLASS_1)
usethis::use_data(CLASS_3, overwrite = TRUE)

# KIND_1 ----
KIND_1 <- data.table(value = c(NA_character_, "N", "M", "U", "C", "O"))
KIND_1[, factor := 0:(.N-1)]
usethis::use_data(KIND_1, overwrite = TRUE)

# KIND_2 ----
KIND_2 <- copy(KIND_1)
usethis::use_data(KIND_2, overwrite = TRUE)

# KIND_3 ----
KIND_3 <- copy(KIND_1)
usethis::use_data(KIND_3, overwrite = TRUE)

# SITE_S1 ----
SITE_S1 <- data.table(value = c(NA_character_))
SITE_S1[, factor := 0:(.N-1)]
usethis::use_data(SITE_S1, overwrite = TRUE)

# SITE_S2 ----
SITE_S2 <- copy(SITE_S1)
usethis::use_data(SITE_S2, overwrite = TRUE)

# SITE_S3 ----
SITE_S3 <- copy(SITE_S1)
usethis::use_data(SITE_S3, overwrite = TRUE)

# SITEAM_S1A ----
SITEAM_S1A <- data.table(value = c(NA_character_))
SITEAM_S1A[, factor := 0:(.N-1)]
usethis::use_data(SITEAM_S1A, overwrite = TRUE)

# SITEAM_S2A ----
SITEAM_S2A <- copy(SITEAM_S1A)
usethis::use_data(SITEAM_S2A, overwrite = TRUE)

# SITEAM_S3A ----
SITEAM_S3A <- copy(SITEAM_S1A)
usethis::use_data(SITEAM_S3A, overwrite = TRUE)

# SITEAM_S1B ----
SITEAM_S1B <- data.table(value = c(NA_character_))
SITEAM_S1B[, factor := 0:(.N-1)]
usethis::use_data(SITEAM_S1B, overwrite = TRUE)

# SITEAM_S2B ----
SITEAM_S2B <- copy(SITEAM_S1B)
usethis::use_data(SITEAM_S2B, overwrite = TRUE)

# SITEAM_S3B ----
SITEAM_S3B <- copy(SITEAM_S1B)
usethis::use_data(SITEAM_S3B, overwrite = TRUE)

# SITEAM_S1C ----
SITEAM_S1C <- data.table(value = c(NA_character_))
SITEAM_S1C[, factor := 0:(.N-1)]
usethis::use_data(SITEAM_S1C, overwrite = TRUE)

# SITEAM_S2C ----
SITEAM_S2C <- copy(SITEAM_S1C)
usethis::use_data(SITEAM_S2C, overwrite = TRUE)

# SITEAM_S3C ----
SITEAM_S3C <- copy(SITEAM_S1C)
usethis::use_data(SITEAM_S3C, overwrite = TRUE)

# SITEAM_S1D ----
SITEAM_S1D <- data.table(value = c(NA_character_))
SITEAM_S1D[, factor := 0:(.N-1)]
usethis::use_data(SITEAM_S1D, overwrite = TRUE)

# SITEAM_S2D ----
SITEAM_S2D <- copy(SITEAM_S1D)
usethis::use_data(SITEAM_S2D, overwrite = TRUE)

# SITEAM_S3D ----
SITEAM_S3D <- copy(SITEAM_S1D)
usethis::use_data(SITEAM_S3D, overwrite = TRUE)

# SITEMC_S1 ----
SITEMC_S1 <- data.table(value = c(NA_character_))
SITEMC_S1[, factor := 0:(.N-1)]
usethis::use_data(SITEMC_S1, overwrite = TRUE)

# SITEMC_S2 ----
SITEMC_S2 <- copy(SITEMC_S1)
usethis::use_data(SITEMC_S2, overwrite = TRUE)

# SITEMC_S3 ----
SITEMC_S3 <- copy(SITEMC_S1)
usethis::use_data(SITEMC_S3, overwrite = TRUE)

# SITE_M1A ----
SITE_M1A <- data.table(value = c(NA_character_, "a"))
SITE_M1A[, factor := 0:(.N-1)]
usethis::use_data(SITE_M1A, overwrite = TRUE)

# SITE_M2A ----
SITE_M2A <- copy(SITE_M1A)
usethis::use_data(SITE_M2A, overwrite = TRUE)

# SITE_M3A ----
SITE_M3A <- copy(SITE_M1A)
usethis::use_data(SITE_M3A, overwrite = TRUE)

# SITE_M1B ----
SITE_M1B <- data.table(value = c(NA_character_))
SITE_M1B[, factor := 0:(.N-1)]
usethis::use_data(SITE_M1B, overwrite = TRUE)

# SITE_M2B ----
SITE_M2B <- copy(SITE_M1B)
usethis::use_data(SITE_M2B, overwrite = TRUE)

# SITE_M3B ----
SITE_M3B <- copy(SITE_M1B)
usethis::use_data(SITE_M3B, overwrite = TRUE)

# STRCT_S1 ----
STRCT_S1 <- data.table(value = c(NA_character_, "1a", "3a", "6",  "2d", "3b", "7",  "4",  "5",  "3",  "2a", "2b", "7a", "3c", "1",  "2"))
STRCT_S1[, factor := 0:(.N-1)]
usethis::use_data(STRCT_S1, overwrite = TRUE)

# STRCT_S2 ----
STRCT_S2 <- copy(STRCT_S1)
usethis::use_data(STRCT_S2, overwrite = TRUE)

# STRCT_S3 ----
STRCT_S3 <- copy(STRCT_S1)
usethis::use_data(STRCT_S3, overwrite = TRUE)

# STRCT_M1 ----
STRCT_M1 <- data.table(value = c(NA_character_))
STRCT_M1[, factor := 0:(.N-1)]
usethis::use_data(STRCT_M1, overwrite = TRUE)

# STRCT_M2 ----
STRCT_M2 <- copy(STRCT_M1)
usethis::use_data(STRCT_M2, overwrite = TRUE)

# STRCT_M3 ----
STRCT_M3 <- copy(STRCT_M1)
usethis::use_data(STRCT_M3, overwrite = TRUE)

# STAND_A1 ----
STAND_A1 <- data.table(value = c(NA_character_,"M", "C", "B"))
STAND_A1[, factor := 0:(.N-1)]
usethis::use_data(STAND_A1, overwrite = TRUE)

# STAND_A2 ----
STAND_A2 <- copy(STAND_A1)
usethis::use_data(STAND_A2, overwrite = TRUE)

# STAND_A3 ----
STAND_A3 <- copy(STAND_A1)
usethis::use_data(STAND_A3, overwrite = TRUE)

# SERAL_1 ----
SERAL_1 <- data.table(value = c(NA_character_))
SERAL_1[, factor := 0:(.N-1)]
usethis::use_data(SERAL_1, overwrite = TRUE)

# SERAL_2 ----
SERAL_2 <- copy(SERAL_1)
usethis::use_data(SERAL_2, overwrite = TRUE)

# SERAL_3 ----
SERAL_3 <- copy(SERAL_1)
usethis::use_data(SERAL_3, overwrite = TRUE)

# TREE_C1 ----
TREE_C1 <- data.table(value = c(NA_character_))
TREE_C1[, factor := 0:(.N-1)]
usethis::use_data(TREE_C1, overwrite = TRUE)

# TREE_C2 ----
TREE_C2 <- copy(TREE_C1)
usethis::use_data(TREE_C2, overwrite = TRUE)

# TREE_C3 ----
TREE_C3 <- copy(TREE_C1)
usethis::use_data(TREE_C3, overwrite = TRUE)

# SHRUB_C1 ----
SHRUB_C1 <- data.table(value = c(NA_character_))
SHRUB_C1[, factor := 0:(.N-1)]
usethis::use_data(SHRUB_C1, overwrite = TRUE)

# SHRUB_C2 ----
SHRUB_C2 <- copy(SHRUB_C1)
usethis::use_data(SHRUB_C2, overwrite = TRUE)

# SHRUB_C3 ----
SHRUB_C3 <- copy(SHRUB_C1)
usethis::use_data(SHRUB_C3, overwrite = TRUE)

# DISTCLS_1 ----
DISTCLS_1 <- data.table(value = c(NA_character_))
DISTCLS_1[, factor := 0:(.N-1)]
usethis::use_data(DISTCLS_1, overwrite = TRUE)

# DISTCLS_2 ----
DISTCLS_2 <- copy(DISTCLS_1)
usethis::use_data(DISTCLS_2, overwrite = TRUE)

# DISTCLS_3 ----
DISTCLS_3 <- copy(DISTCLS_1)
usethis::use_data(DISTCLS_3, overwrite = TRUE)

# DISTSCLS_1 ----
DISTSCLS_1 <- data.table(value = c(NA_character_))
DISTSCLS_1[, factor := 0:(.N-1)]
usethis::use_data(DISTSCLS_1, overwrite = TRUE)

# DISTSCLS_2 ----
DISTSCLS_2 <- copy(DISTSCLS_1)
usethis::use_data(DISTSCLS_2, overwrite = TRUE)

# DISTSCLS_3 ----
DISTSCLS_3 <- copy(DISTSCLS_1)
usethis::use_data(DISTSCLS_3, overwrite = TRUE)

# DISSSCLS_1 ----
DISSSCLS_1 <- data.table(value = c(NA_character_))
DISSSCLS_1[, factor := 0:(.N-1)]
usethis::use_data(DISSSCLS_1, overwrite = TRUE)

# DISSSCLS_2 ----
DISSSCLS_2 <- copy(DISSSCLS_1)
usethis::use_data(DISSSCLS_2, overwrite = TRUE)

# DISSSCLS_3 ----
DISSSCLS_3 <- copy(DISSSCLS_1)
usethis::use_data(DISSSCLS_3, overwrite = TRUE)

# SECL_1 ----
SECL_1 <- data.table(value = c(NA_character_))
SECL_1[, factor := 0:(.N-1)]
usethis::use_data(SECL_1, overwrite = TRUE)

# SECL_2 ----
SECL_2 <- copy(SECL_1)
usethis::use_data(SECL_2, overwrite = TRUE)

# SECL_3 ----
SECL_3 <- copy(SECL_1)
usethis::use_data(SECL_3, overwrite = TRUE)

# SESUBCL_1 ----
SESUBCL_1 <- data.table(value = c(NA_character_))
SESUBCL_1[, factor := 0:(.N-1)]
usethis::use_data(SESUBCL_1, overwrite = TRUE)

# SESUBCL_2 ----
SESUBCL_2 <- copy(SESUBCL_1)
usethis::use_data(SESUBCL_2, overwrite = TRUE)

# SESUBCL_3 ----
SESUBCL_3 <- copy(SESUBCL_1)
usethis::use_data(SESUBCL_3, overwrite = TRUE)

# COND_1 ----
COND_1 <- data.table(value = c(NA_character_))
COND_1[, factor := 0:(.N-1)]
usethis::use_data(COND_1, overwrite = TRUE)

# COND_2 ----
COND_2 <- copy(COND_1)
usethis::use_data(COND_2, overwrite = TRUE)

# COND_3 ----
COND_3 <- copy(COND_1)
usethis::use_data(COND_3, overwrite = TRUE)

# VIAB_1 ----
VIAB_1 <- data.table(value = c(NA_character_))
VIAB_1[, factor := 0:(.N-1)]
usethis::use_data(VIAB_1, overwrite = TRUE)

# VIAB_2 ----
VIAB_2 <- copy(VIAB_1)
usethis::use_data(VIAB_2, overwrite = TRUE)

# VIAB_3 ----
VIAB_3 <- copy(VIAB_1)
usethis::use_data(VIAB_3, overwrite = TRUE)

# SLOPE_MOD ----
SLOPE_MOD <- data.table(value = c(NA_character_, "", "k", "w"))
SLOPE_MOD[, factor := 0:(.N-1)]
usethis::use_data(SLOPE_MOD, overwrite = TRUE)

# FORESTED_1 ----
FORESTED_1 <- data.table(value = c(NA_character_, "N", "Y"))
FORESTED_1[, factor := 0:(.N-1)]
usethis::use_data(FORESTED_1, overwrite = TRUE)

# FORESTED_2 ----
FORESTED_2 <- copy(FORESTED_1)
usethis::use_data(FORESTED_2, overwrite = TRUE)

# FORESTED_3 ----
FORESTED_3 <- copy(FORESTED_1)
usethis::use_data(FORESTED_3, overwrite = TRUE)

# BGC_PHASE ----
BGC_PHASE <- data.table(value = c(NA_character_, "a"))
BGC_PHASE[, factor := 0:(.N-1)]
usethis::use_data(BGC_PHASE, overwrite = TRUE)

# ECO_SEC ----
ECO_SEC <- data.table(value = c(NA_character_, "BUB", "BAU", "NEU", "BUR", "NAU"))
ECO_SEC[, factor := 0:(.N-1)]
usethis::use_data(ECO_SEC, overwrite = TRUE)

