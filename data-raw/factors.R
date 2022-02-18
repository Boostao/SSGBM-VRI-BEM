# BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3, BCLCS_LEVEL_4, BCLCS_LEVEL_5,
# SPECIES_CD_1, SPECIES_CD_2, SPECIES_CD_3, SPECIES_CD_4, SPECIES_CD_5, SPECIES_CD_6,
# SPECIES_PCT_1, SPECIES_PCT_2, SPECIES_PCT_3, SPECIES_PCT_4, SPECIES_PCT_5, SPECIES_PCT_6,
# CROWN_CLOSURE, LAND_COVER_CLASS_CD_1, EST_COVERAGE_PCT_1, LINE_5_VEGETATION_COVER,
# HARVEST_DATE, PROJ_AGE_1

# BCLCS_LEVEL_1 ----
bclcs_level_1 <- data.table(value = c("V", "N", "U"))
bclcs_level_1[, factor := 1:.N]
usethis::use_data(bclcs_level_1, overwrite = TRUE)

# BCLCS_LEVEL_2 ----
bclcs_level_2 <- data.table(value = c("T", "N", "L", "W"))
bclcs_level_2[, factor := 1:.N]
usethis::use_data(bclcs_level_2, overwrite = TRUE)

# BCLCS_LEVEL_3 ----
bclcs_level_3 <- data.table(value = c("W", "U", "A"))
bclcs_level_3[, factor := 1:.N]
usethis::use_data(bclcs_level_3, overwrite = TRUE)


# BCLCS_LEVEL_4 ----
bclcs_level_4 <- data.table(value = c("TC", "TB", "TM", "ST", "SL", "HE", "HF", "HG", "BY",
                                      "BM", "BL", "SI", "RO", "EL"))
bclcs_level_4[, factor := 1:.N]
usethis::use_data(bclcs_level_4, overwrite = TRUE)

# BCLCS_LEVEL_5 ----
bclcs_level_5 <- data.table(value = c("DE", "OP", "SP", "CL", "OP", "GL", "PN", "BR", "TA",
                                      "BI", "MZ", "LB", "RS", "ES", "LS", "RM", "BE", "LL",
                                      "BU", "RZ", "MU", "CB", "MN", "GP", "TZ", "RN", "UR",
                                      "AP", "MI", "OT", "LA", "RE", "RI", "OC"))
bclcs_level_5[, factor := 1:.N]
usethis::use_data(bclcs_level_5, overwrite = TRUE)

# SPECIES_CD_1 ----
species_cd_1 <- data.table(value = c("AC", "AT", "B", "BL", "BA", "BG", "CW", "DR", "E", "EP",
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
                                     "Pm", "Pr", "Ps", "S", "Sn"))

species_cd_1[, factor := 1:.N]
usethis::use_data(species_cd_1, overwrite = TRUE)

# SPECIES_CD_2 ----
species_cd_2 <- copy(species_cd_1)
usethis::use_data(species_cd_2, overwrite = TRUE)

# SPECIES_CD_3 ----
species_cd_3 <- copy(species_cd_1)
usethis::use_data(species_cd_3, overwrite = TRUE)

# SPECIES_CD_4 ----
species_cd_4 <- copy(species_cd_1)
usethis::use_data(species_cd_4, overwrite = TRUE)

# SPECIES_CD_5 ----
species_cd_5 <- copy(species_cd_1)
usethis::use_data(species_cd_5, overwrite = TRUE)

# SPECIES_CD_6 ----
species_cd_6 <- copy(species_cd_1)
usethis::use_data(species_cd_6, overwrite = TRUE)

# LAND_COVER_CLASS_CD_1 ----
land_cover_class_cd_1 <- data.table(value = c("TB", "TC", "TM", "ST", "SL", "HE", "HF", "HG",
                                              "BY", "BM", "BL", "SI", "GL", "PN", "RO, BR",
                                              "TA", "BI", "MZ", "LB", "EL", "RS", "ES", "LS",
                                              "RM", "BE", "LL", "BU", "RZ", "MU", "CB", "MN", "GP",
                                              "TZ", "RN", "UR", "AP", "MI", "OT", "LA", "RE", "RI",
                                              "A", "OC"))
land_cover_class_cd_1[, factor := 1:.N]
usethis::use_data(land_cover_class_cd_1, overwrite = TRUE)

# LINE_5_VEGETATION_COVER ----
land_cover_class_cd_1 <- data.table(value = c("sh", "he", "by"))
land_cover_class_cd_1[, factor := 1:.N]
usethis::use_data(land_cover_class_cd_1, overwrite = TRUE)


