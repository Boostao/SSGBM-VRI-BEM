# BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3, BCLCS_LEVEL_4, BCLCS_LEVEL_5,
# SPECIES_CD_1, SPECIES_CD_2, SPECIES_CD_3, SPECIES_CD_4, SPECIES_CD_5, SPECIES_CD_6,
# SPECIES_PCT_1, SPECIES_PCT_2, SPECIES_PCT_3, SPECIES_PCT_4, SPECIES_PCT_5, SPECIES_PCT_6,
# CROWN_CLOSURE, LAND_COVER_CLASS_CD_1, EST_COVERAGE_PCT_1, LINE_5_VEGETATION_COVER,
# HARVEST_DATE, PROJ_AGE_1

library(data.table)

raster_conv <- list(vri = list(),
                    bem = list())
# VRI ----

# BCLCS_LV_1 ----
raster_conv$vri$BCLCS_LEVEL_1 <- data.table(value = c(NA_character_, "V", "N", "U"))
raster_conv$vri$BCLCS_LEVEL_1[, factor := 0:(.N-1)]

# BCLCS_LV_2 ----
raster_conv$vri$BCLCS_LEVEL_2 <- data.table(value = c(NA_character_, "T", "N", "L", "W"))
raster_conv$vri$BCLCS_LEVEL_2[, factor := 0:(.N-1)]

# BCLCS_LV_3 ----
raster_conv$vri$BCLCS_LEVEL_3 <- data.table(value = c(NA_character_, "W", "U", "A"))
raster_conv$vri$BCLCS_LEVEL_3[, factor := 0:(.N-1)]


# BCLCS_LV_4 ----
raster_conv$vri$BCLCS_LEVEL_4 <- data.table(value = c(NA_character_, "TC", "TB", "TM", "ST", "SL", "HE", "HF", "HG", "BY",
                                      "BM", "BL", "SI", "RO", "EL"))
raster_conv$vri$BCLCS_LEVEL_4[, factor := 0:(.N-1)]

# BCLCS_LV_5 ----
raster_conv$vri$BCLCS_LEVEL_5 <- data.table(value = c(NA_character_, "DE", "OP", "SP", "CL", "OP", "GL", "PN", "BR", "TA",
                                                   "BI", "MZ", "LB", "RS", "ES", "LS", "RM", "BE", "LL",
                                                   "BU", "RZ", "MU", "CB", "MN", "GP", "TZ", "RN", "UR",
                                                   "AP", "MI", "OT", "LA", "RE", "RI", "OC"))
raster_conv$vri$BCLCS_LEVEL_5[, factor := 0:(.N-1)]

# SPECIES_CD_1 ----
raster_conv$vri$SPECIES_CD_1 <- data.table(value = c(NA_character_, "AC", "AT", "B", "BL", "BA", "BG", "CW", "DR", "E", "EP",
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

raster_conv$vri$SPECIES_CD_1[, factor := 0:(.N-1)]

# SPECIES_CD_2 ----
raster_conv$vri$SPECIES_CD_2 <- copy(raster_conv$vri$SPECIES_CD_1)

# SPECIES_CD_3 ----
raster_conv$vri$SPECIES_CD_3 <- copy(raster_conv$vri$SPECIES_CD_1)

# SPECIES_CD_4 ----
raster_conv$vri$SPECIES_CD_4 <- copy(raster_conv$vri$SPECIES_CD_1)

# SPECIES_CD_5 ----
raster_conv$vri$SPECIES_CD_5 <- copy(raster_conv$vri$SPECIES_CD_1)

# SPECIES_CD_6 ----
raster_conv$vri$SPECIES_CD_6 <- copy(raster_conv$vri$SPECIES_CD_1)

# LAND_COVER_CLASS_CD_1 ----
raster_conv$vri$LAND_COVER_CLASS_CD_1 <- data.table(value = c(NA_character_, "TB", "TC", "TM", "ST", "SL", "HE", "HF", "HG",
                                                              "BY", "BM", "BL", "SI", "GL", "PN", "RO, BR",
                                                              "TA", "BI", "MZ", "LB", "EL", "RS", "ES", "LS",
                                                              "RM", "BE", "LL", "BU", "RZ", "MU", "CB", "MN", "GP",
                                                              "TZ", "RN", "UR", "AP", "MI", "OT", "LA", "RE", "RI",
                                                              "A", "OC"))
raster_conv$vri$LAND_COVER_CLASS_CD_1[, factor := 0:(.N-1)]

# LINE_5_VEGETATION_COVER ----
raster_conv$vri$LINE_5_VEGETATION_COVER <- data.table(value = c(NA_character_, "sh", "he", "by"))
raster_conv$vri$LINE_5_VEGETATION_COVER[, factor := 0:(.N-1)]


# BEM ----

# [1] "Area_Ha"    "BGC_ZONE"   "BGC_SUBZON" "SDEC_1"     "BEUMC_S1"   "REALM_1"    "GROUP_1"    "CLASS_1"    "KIND_1" SITE_S3     "SITEAM_S1A"    "SITEAM_S1A" "SITEAM_S1B" "SITEAM_S1C" "SITEAM_S1D" "SITEMC_S1"
# [16] "SITE_M1A"   "SITE_M1B"   "STRCT_S1"   "STRCT_M1"   "STAND_A1"   "SERAL_1"    "TREE_C1"    "SHRUB_C1"   "DISTCLS_1"  "DISTSCLS_1" "DISSSCLS_1" "SECL_1"     "SESUBCL_1"  "COND_1"     "VIAB_1"
# [31] "SDEC_2"     "BEUMC_S2"   "REALM_2"    "GROUP_2"    "CLASS_2"    "KIND_2"     "SITE_S2"    "SITEAM_S2A" "SITEAM_S2B" "SITEAM_S2C" "SITEAM_S2D" "SITEMC_S2"  "SITE_M2A"   "SITE_M2B"   "STRCT_S2"
# [46] "STRCT_M2"   "STAND_A2"   "SERAL_2"    "TREE_C2"    "SHRUB_C2"   "DISTCLS_2"  "DISTSCLS_2" "DISSSCLS_2" "SECL_2"     "SESUBCL_2"  "COND_2"     "VIAB_2"     "SDEC_3"     "BEUMC_S3"   "REALM_3"
# [61] "GROUP_3"    "CLASS_3"    "KIND_3"     "SITE_S3"    "SITEAM_S3A" "SITEAM_S3B" "SITEAM_S3C" "SITEAM_S3D" "SITEMC_S3"  "SITE_M3A"   "SITE_M3B"   "STRCT_S3"   "STRCT_M3"   "STAND_A3"   "SERAL_3"
# [76] "TREE_C3"    "SHRUB_C3"   "DISTCLS_3"  "DISTSCLS_3" "DISSSCLS_3" "SECL_3"     "SESUBCL_3"  "COND_3"     "VIAB_3"     "SLOPE_MOD"  "AGE_CL_STS" "FORESTED_1" "FORESTED_2" "FORESTED_3"

# BGC_ZONE ----
raster_conv$bem$BGC_ZONE <- data.table(value = c(NA_character_,"SWB", "BAFA", "BWBS", "ESSF", "CMA", "MH", "SBS", "CWH", "ICH", "SBPS"))
raster_conv$bem$BGC_ZONE[, factor := 0:(.N-1)]

# BGC_SUBZON ----
raster_conv$bem$BGC_SUBZON <- data.table(value = c(NA_character_,"vks", "un", "vk", "dk",  "uns", "unp", "wm",  "mk",  "mm",  "mks", "wc",  "mmp", "mc",  "mcp",
                                                   "wv",  "wvp", "vc",  "vh",  "ws",  "mv",  "wk",  "whp", "vm",  "wh",  "mvp", "dw",  "mkp"))
raster_conv$bem$BGC_SUBZON[, factor := 0:(.N-1)]

# BEUMC_S1 ----
raster_conv$bem$BEUMC_S1 <- data.table(value = c(NA_character_,"GL", "AU", "SU", "PR", "AT", "AV", "BP", "FB", "MS", "WL", "AM", "EW", "SF", "EF", "GB", "LL", "SM", "LP", "LS", "ER", "MF", "HS", "AG", "SR", "SP", "HP", "BK", "WR", "ME",
                                 "RO", "ST", "FS", "CS", "RR", "IS", "WP", "UV", "AH", "SL", "ES","FP", "FR", "WB", "CH", "YM", "MI", "DF", "HL", "UR", "YB", "CB", "YS", "CF", "RE",
                                 "SD", "RS", "BG", "OW"))
raster_conv$bem$BEUMC_S1[, factor := 0:(.N-1)]

# BEUMC_S2 ----
raster_conv$bem$BEUMC_S2 <- copy(raster_conv$bem$BEUMC_S1)

# BEUMC_S3 ----
raster_conv$bem$BEUMC_S3 <- copy(raster_conv$bem$BEUMC_S1)

# REALM_1 ----
raster_conv$bem$REALM_1 <- data.table(value = c(NA_character_, "T", "W", "O", "M", "E"))
raster_conv$bem$REALM_1[, factor := 0:(.N-1)]

# REALM_2 ----
raster_conv$bem$REALM_2 <- copy(raster_conv$bem$REALM_1)

# REALM_3 ----
raster_conv$bem$REALM_3 <- copy(raster_conv$bem$REALM_1)

# GROUP_1 ----
raster_conv$bem$GROUP_1 <- data.table(value = c(NA_character_, "A", "S", "F", "V", "G", "M", "R", "E", "X", "P"))
raster_conv$bem$GROUP_1[, factor := 0:(.N-1)]

# GROUP_2 ----
raster_conv$bem$GROUP_2 <- copy(raster_conv$bem$GROUP_1)

# GROUP_3 ----
raster_conv$bem$GROUP_3 <- copy(raster_conv$bem$GROUP_1)

# CLASS_1 ----
raster_conv$bem$CLASS_1 <- data.table(value = c(NA_character_, "c", "t", "s", "b", "m", "a", "g", "k", "o", "h"))
raster_conv$bem$CLASS_1[, factor := 0:(.N-1)]

# CLASS_2 ----
raster_conv$bem$CLASS_2 <- copy(raster_conv$bem$CLASS_1)

# CLASS_3 ----
raster_conv$bem$CLASS_3 <- copy(raster_conv$bem$CLASS_1)

# KIND_1 ----
raster_conv$bem$KIND_1 <- data.table(value = c(NA_character_, "N", "M", "U", "C", "O"))
raster_conv$bem$KIND_1[, factor := 0:(.N-1)]

# KIND_2 ----
raster_conv$bem$KIND_2 <- copy(raster_conv$bem$KIND_1)

# KIND_3 ----
raster_conv$bem$KIND_3 <- copy(raster_conv$bem$KIND_1)

# SITE_S1 ----
raster_conv$bem$SITE_S1 <- data.table(value = c(NA_character_))
raster_conv$bem$SITE_S1[, factor := 0:(.N-1)]

# SITE_S2 ----
raster_conv$bem$SITE_S2 <- copy(raster_conv$bem$SITE_S1)

# SITE_S3 ----
raster_conv$bem$SITE_S3 <- copy(raster_conv$bem$SITE_S1)

# SITEAM_S1A ----
raster_conv$bem$SITEAM_S1A <- data.table(value = c(NA_character_))
raster_conv$bem$SITEAM_S1A[, factor := 0:(.N-1)]

# SITEAM_S2A ----
raster_conv$bem$SITEAM_S2A <- copy(raster_conv$bem$SITEAM_S1A)

# SITEAM_S3A ----
raster_conv$bem$SITEAM_S3A <- copy(raster_conv$bem$SITEAM_S1A)

# SITEAM_S1B ----
raster_conv$bem$SITEAM_S1B <- data.table(value = c(NA_character_))
raster_conv$bem$SITEAM_S1B[, factor := 0:(.N-1)]

# SITEAM_S2B ----
raster_conv$bem$SITEAM_S2B <- copy(raster_conv$bem$SITEAM_S1B)

# SITEAM_S3B ----
raster_conv$bem$SITEAM_S3B <- copy(raster_conv$bem$SITEAM_S1B)

# SITEAM_S1C ----
raster_conv$bem$SITEAM_S1C <- data.table(value = c(NA_character_))
raster_conv$bem$SITEAM_S1C[, factor := 0:(.N-1)]

# SITEAM_S2C ----
raster_conv$bem$SITEAM_S2C <- copy(raster_conv$bem$SITEAM_S1C)

# SITEAM_S3C ----
raster_conv$bem$SITEAM_S3C <- copy(raster_conv$bem$SITEAM_S1C)

# SITEAM_S1D ----
raster_conv$bem$SITEAM_S1D <- data.table(value = c(NA_character_))
raster_conv$bem$SITEAM_S1D[, factor := 0:(.N-1)]

# SITEAM_S2D ----
raster_conv$bem$SITEAM_S2D <- copy(raster_conv$bem$SITEAM_S1D)

# SITEAM_S3D ----
raster_conv$bem$SITEAM_S3D <- copy(raster_conv$bem$SITEAM_S1D)

# SITEMC_S1 ----
raster_conv$bem$SITEMC_S1 <- data.table(value = c(NA_character_))
raster_conv$bem$SITEMC_S1[, factor := 0:(.N-1)]

# SITEMC_S2 ----
raster_conv$bem$SITEMC_S2 <- copy(raster_conv$bem$SITEMC_S1)

# SITEMC_S3 ----
raster_conv$bem$SITEMC_S3 <- copy(raster_conv$bem$SITEMC_S1)

# SITE_M1A ----
raster_conv$bem$SITE_M1A <- data.table(value = c(NA_character_, "a"))
raster_conv$bem$SITE_M1A[, factor := 0:(.N-1)]

# SITE_M2A ----
raster_conv$bem$SITE_M2A <- copy(raster_conv$bem$SITE_M1A)

# SITE_M3A ----
raster_conv$bem$SITE_M3A <- copy(raster_conv$bem$SITE_M1A)

# SITE_M1B ----
raster_conv$bem$SITE_M1B <- data.table(value = c(NA_character_))
raster_conv$bem$SITE_M1B[, factor := 0:(.N-1)]

# SITE_M2B ----
raster_conv$bem$SITE_M2B <- copy(raster_conv$bem$SITE_M1B)

# SITE_M3B ----
raster_conv$bem$SITE_M3B <- copy(raster_conv$bem$SITE_M1B)

# STRCT_S1 ----
raster_conv$bem$STRCT_S1 <- data.table(value = c(NA_character_, "1a", "3a", "6",  "2d", "3b", "7",  "4",  "5",  "3",  "2a", "2b", "7a", "3c", "1",  "2"))
raster_conv$bem$STRCT_S1[, factor := 0:(.N-1)]

# STRCT_S2 ----
raster_conv$bem$STRCT_S2 <- copy(raster_conv$bem$STRCT_S1)

# STRCT_S3 ----
raster_conv$bem$STRCT_S3 <- copy(raster_conv$bem$STRCT_S1)

# STRCT_M1 ----
raster_conv$bem$STRCT_M1 <- data.table(value = c(NA_character_))
raster_conv$bem$STRCT_M1[, factor := 0:(.N-1)]

# STRCT_M2 ----
raster_conv$bem$STRCT_M2 <- copy(raster_conv$bem$STRCT_M1)

# STRCT_M3 ----
raster_conv$bem$STRCT_M3 <- copy(raster_conv$bem$STRCT_M1)

# STAND_A1 ----
raster_conv$bem$STAND_A1 <- data.table(value = c(NA_character_,"M", "C", "B"))
raster_conv$bem$STAND_A1[, factor := 0:(.N-1)]

# STAND_A2 ----
raster_conv$bem$STAND_A2 <- copy(raster_conv$bem$STAND_A1)

# STAND_A3 ----
raster_conv$bem$STAND_A3 <- copy(raster_conv$bem$STAND_A1)

# SERAL_1 ----
raster_conv$bem$SERAL_1 <- data.table(value = c(NA_character_))
raster_conv$bem$SERAL_1[, factor := 0:(.N-1)]

# SERAL_2 ----
raster_conv$bem$SERAL_2 <- copy(raster_conv$bem$SERAL_1)

# SERAL_3 ----
raster_conv$bem$SERAL_3 <- copy(raster_conv$bem$SERAL_1)

# TREE_C1 ----
raster_conv$bem$TREE_C1 <- data.table(value = c(NA_character_))
raster_conv$bem$TREE_C1[, factor := 0:(.N-1)]

# TREE_C2 ----
raster_conv$bem$TREE_C2 <- copy(raster_conv$bem$TREE_C1)

# TREE_C3 ----
raster_conv$bem$TREE_C3 <- copy(raster_conv$bem$TREE_C1)

# SHRUB_C1 ----
raster_conv$bem$SHRUB_C1 <- data.table(value = c(NA_character_))
raster_conv$bem$SHRUB_C1[, factor := 0:(.N-1)]

# SHRUB_C2 ----
raster_conv$bem$SHRUB_C2 <- copy(raster_conv$bem$SHRUB_C1)

# SHRUB_C3 ----
raster_conv$bem$SHRUB_C3 <- copy(raster_conv$bem$SHRUB_C1)

# DISTCLS_1 ----
raster_conv$bem$DISTCLS_1 <- data.table(value = c(NA_character_))
raster_conv$bem$DISTCLS_1[, factor := 0:(.N-1)]

# DISTCLS_2 ----
raster_conv$bem$DISTCLS_2 <- copy(raster_conv$bem$DISTCLS_1)

# DISTCLS_3 ----
raster_conv$bem$DISTCLS_3 <- copy(raster_conv$bem$DISTCLS_1)

# DISTSCLS_1 ----
raster_conv$bem$DISTSCLS_1 <- data.table(value = c(NA_character_))
raster_conv$bem$DISTSCLS_1[, factor := 0:(.N-1)]

# DISTSCLS_2 ----
raster_conv$bem$DISTSCLS_2 <- copy(raster_conv$bem$DISTSCLS_1)

# DISTSCLS_3 ----
raster_conv$bem$DISTSCLS_3 <- copy(raster_conv$bem$DISTSCLS_1)

# DISSSCLS_1 ----
raster_conv$bem$DISSSCLS_1 <- data.table(value = c(NA_character_))
raster_conv$bem$DISSSCLS_1[, factor := 0:(.N-1)]

# DISSSCLS_2 ----
raster_conv$bem$DISSSCLS_2 <- copy(raster_conv$bem$DISSSCLS_1)

# DISSSCLS_3 ----
raster_conv$bem$DISSSCLS_3 <- copy(raster_conv$bem$DISSSCLS_1)

# SECL_1 ----
raster_conv$bem$SECL_1 <- data.table(value = c(NA_character_))
raster_conv$bem$SECL_1[, factor := 0:(.N-1)]

# SECL_2 ----
raster_conv$bem$SECL_2 <- copy(raster_conv$bem$SECL_1)

# SECL_3 ----
raster_conv$bem$SECL_3 <- copy(raster_conv$bem$SECL_1)

# SESUBCL_1 ----
raster_conv$bem$SESUBCL_1 <- data.table(value = c(NA_character_))
raster_conv$bem$SESUBCL_1[, factor := 0:(.N-1)]

# SESUBCL_2 ----
raster_conv$bem$SESUBCL_2 <- copy(raster_conv$bem$SESUBCL_1)

# SESUBCL_3 ----
raster_conv$bem$SESUBCL_3 <- copy(raster_conv$bem$SESUBCL_1)

# COND_1 ----
raster_conv$bem$COND_1 <- data.table(value = c(NA_character_))
raster_conv$bem$COND_1[, factor := 0:(.N-1)]

# COND_2 ----
raster_conv$bem$COND_2 <- copy(raster_conv$bem$COND_1)

# COND_3 ----
raster_conv$bem$COND_3 <- copy(raster_conv$bem$COND_1)

# VIAB_1 ----
raster_conv$bem$VIAB_1 <- data.table(value = c(NA_character_))
raster_conv$bem$VIAB_1[, factor := 0:(.N-1)]

# VIAB_2 ----
raster_conv$bem$VIAB_2 <- copy(raster_conv$bem$VIAB_1)

# VIAB_3 ----
raster_conv$bem$VIAB_3 <- copy(raster_conv$bem$VIAB_1)

# SLOPE_MOD ----
raster_conv$bem$SLOPE_MOD <- data.table(value = c(NA_character_, "", "k", "w"))
raster_conv$bem$SLOPE_MOD[, factor := 0:(.N-1)]

# FORESTED_1 ----
raster_conv$bem$FORESTED_1 <- data.table(value = c(NA_character_, "N", "Y"))
raster_conv$bem$FORESTED_1[, factor := 0:(.N-1)]

# FORESTED_2 ----
raster_conv$bem$FORESTED_2 <- copy(raster_conv$bem$FORESTED_1)

# FORESTED_3 ----
raster_conv$bem$FORESTED_3 <- copy(raster_conv$bem$FORESTED_1)

# BGC_PHASE ----
raster_conv$bem$BGC_PHASE <- data.table(value = c(NA_character_, "a"))
raster_conv$bem$BGC_PHASE[, factor := 0:(.N-1)]


# ECO_SEC ----
raster_conv$bem$ECO_SEC <- data.table(value = c(NA_character_, "BUB", "BAU", "NEU", "BUR", "NAU"))
raster_conv$bem$ECO_SEC[, factor := 0:(.N-1)]


usethis::use_data(raster_conv, overwrite = TRUE, internal = TRUE)

