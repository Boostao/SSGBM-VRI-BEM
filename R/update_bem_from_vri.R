#' Update BEM attributes based on VRI attributes
#'
#'This function updates BEM (broad ecosystem mapping) attributes based on VRI (vegetation resource inventory) attributes, according to the document "Corrections to the BEM map codes".
#'
#' @param vri_bem sf object that represent the combined vri & bem polygon feature class
#' @param rivers sf object that represent Rivers polygon feature class (FWA_Rivers)
#' @param beu_bec data.table object of allowed BEC and BEM Code Combos
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @param use_ifelse boolean, if TRUE correction done after the combine_duplicated_BEUMC will only be applied on rows that were not affected by the correction of duplicated BEUMC
#' @details
#' This function performs some adjustments to BEM attributes based on VRI attributes :
#'   * Combine together components with duplicated BEUMC
#'   * Perform corrections based on BC Land Cover Classification Scheme (BCLCS) and land : assign related SDEC_1 to 10
#'   * Remove inadequate wetlands
#'   * Update STAND_... based on species
#'
#' @return sf object which contains adjusted map codes
#' @import sf
#' @import dbplyr
#' @import data.table
#' @export

update_bem_from_vri <- function(conn, beu_bec, clear_site_ma = TRUE, use_ifelse = TRUE) {

  vribem_columns <- duckdb::dbListFields(conn, "V_VRIBEM")
  
  vri_bem <- tbl(conn, "V_VRIBEM") |> as_duckdb_tibble(prudence = "stingy")

  # validate inputs ----
  validate_views_column_names(conn = conn, 
                              obj = "V_VRIBEM",
                              required_names = c("SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
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
                                                 "SPEC_PCT_1"))

  if (!"lbl_edit" %in% vribem_columns) {
    vri_bem <- mutate(vri_bem, lbl_edit = "") 
  }

  if (!"DEC_Total" %in% vribem_columns) {
    vri_bem <- mutate(vri_bem, DEC_Total = 0L)
  }

  if (!"SMPL_TYPE" %in% vribem_columns) {
    vri_bem <- mutate(vri_bem, SMPL_TYPE = NA_character_)
  }

 # perform corrections ----

  if (clear_site_ma) {
    vri_bem <- mutate(vri_bem, SITE_M1A = NA_character_, SITE_M2A = NA_character_)
  }
  vri_bem <- mutate(vri_bem, 
    SITE_M3A = NA_character_, #M3A is always cleared
    row_updated = FALSE, #helper column to track which rows were updated by corrections
    blank_eco_variables = FALSE #helper column to track which rows had ecological variables blanked out by corrections
    )

  ## Remove duplicate labels (line 259) ----
  # In BEM may have had two of the same forested unit; one associated with one set
  # of site conditions and the other representing different conditions.
  # Site modifiers are NOT updated in this product. Therefore, duplicate labels were combined.

  vri_bem <- combine_duplicated_BEUMC(ifc = vri_bem, use_ifelse = use_ifelse)

  ## OW - Shallow Open Water (line 279) ----
  # -shallow open water typically associated with floating vegetation
  # -For LIW for moose, LS is rated 0.05, and OW is rated 0.25 because of its common association with a
  # shrub fringe

  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "LA" & Area_Ha <= 2 & !row_updated, "OW", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "OW", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "OW", "OW", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "OW", "Updated to 10 OW because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'LA', Area <= 10 ha", lbl_edit),
           row_updated = if_else(correction_cd == "OW", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "OW", TRUE, blank_eco_variables))
           
  ## LS - Small Lake (line 291) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "LA" & Area_Ha > 2 & Area_Ha <= 60 & !row_updated, "LS", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "LS", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "LS", "LS", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "LS", "Updated to 10 LS because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'LA', Area <= 60 ha", lbl_edit),
           row_updated = if_else(correction_cd == "LS", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "LS", TRUE, blank_eco_variables))
  

  ## LL - Large Lake (line 303) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "LA" & Area_Ha > 60 & !row_updated, "LL", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "LL", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "LL", "LL", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "LL", "Updated to 10 LL because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'LA', Area > 60 ha", lbl_edit),
           row_updated = if_else(correction_cd == "LL", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "LL", TRUE, blank_eco_variables))
  
  ## RE - Reservoir (line 315) ----
    vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 == "RE"  & !row_updated, "RE", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "RE", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "RE", "RE", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "RE", "Updated to 10 RE because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'RE'", lbl_edit),
           row_updated = if_else(correction_cd == "RE", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "RE", TRUE, blank_eco_variables))
  
  ## RI - Rivers (line 331) ----
  # There are two VRI codes (labels) that apply to rivers; river (RI) and river sediments (RS)
  # The default applied was to assign 'FP' (Fast Perennial Stream) to BEU_MC where rivers were identified by
  # this query.
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "N" & BCLCS_LV_5 %in% c("RI", "RS")  & !row_updated, "RI", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "RI", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "RI", "RI", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "RI", "Updated to 10 RI because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'RI' or 'RS'", lbl_edit),
           row_updated = if_else(correction_cd == "RI", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "RI", TRUE, blank_eco_variables))
  

  ## WL - Wetland (line 367) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & AGE_CL_STS == -1 & !row_updated, "WL", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "WL", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "WL", "WL", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "WL", "Updated to 10 WL because BCLCS_LV_1/2/3 = 'V'/'N'/'W' and AGE_CL_STS = -1", lbl_edit),
           row_updated = if_else(correction_cd == "WL", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "WL", TRUE, blank_eco_variables))
  
  ## Remove Wetland - Forested (line 380) ----
  # should not contain a wetland (WL) label component.
  # Remove WL decile component and update value in Decile
  vri_bem <- remove_inadequate_wetlands(ifc = vri_bem)


  ## BB - Black Spruce Bog (line 448) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & SPEC_CD_1 == "SB" & SPEC_PCT_1 >= 90 & !row_updated, "BB", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "BB", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "BB", "BB", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "BB", "Updated to 10 BB because SPEC_CD_1 = 'SB' and SPEC_PCT_1 >= 90", lbl_edit),
           row_updated = if_else(correction_cd == "BB", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "BB", TRUE, blank_eco_variables))

  ## AP - Anthropogenic and Non-vegetated (line 457) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "AP"  & !row_updated, "AP", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "AP", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "AP", "AP", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "AP", "Updated to 10 AP because BCLCS_LV_5 = 'AP'", lbl_edit),
           row_updated = if_else(correction_cd == "AP", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "AP", TRUE, blank_eco_variables))
  
  ## BU - (line 464) -----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "BU"  & !row_updated, "BU", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "BU", 10, SDEC_1),
           DISTCLS_1 = if_else(correction_cd == "BU", "F", DISTCLS_1),
           lbl_edit = if_else(correction_cd == "BU", "Updated to 10 BU because BCLCS_LV_5 = 'BU'", lbl_edit),
           row_updated = if_else(correction_cd == "BU", TRUE, row_updated))  

  ## CL - Cliff (line 470) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & SLOPE_MOD %in% c("q", "z")  & !row_updated, "CL", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "CL", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "CL", "CL", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "CL", "Updated to 10 CL because Slope Mod is q or z", lbl_edit),
           row_updated = if_else(correction_cd == "CL", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "CL", TRUE, blank_eco_variables))

  #TODO see if comment below from python script still applies
  # TO BE ADDED
  # if  If Site Mod 3A = "a" and STS_AGE_CL >= 1 then assign 10 PR, ER OR WR as per which
  # BEC zone those units are allowed (ER can only go in the ESSF, etc.).  In common language
  # I'm saying that forested units adjacent to floodplains should be one of these riparian
  # forest types as the dominant forest ecosystem.

  ## GB - Gravel Bar (line 484) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "GB"  & !row_updated, "GB", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "GB", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "GB", "GB", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "GB", "Updated to 10 GB because BCLCS_LV_5 = 'GB'", lbl_edit),
           row_updated = if_else(correction_cd == "GB", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "GB", TRUE, blank_eco_variables))
  
  ## GL - Glacier (line 491) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 %in% c("GL", "PN")  & !row_updated, "GL", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "GL", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "GL", "GL", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "GL", "Updated to 10 GL because BCLCS_LV_5 = 'GL' or 'PN'", lbl_edit),
           row_updated = if_else(correction_cd == "GL", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "GL", TRUE, blank_eco_variables))


  ## GP - Gravel Pit (line 498) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "GP"  & !row_updated, "GP", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "GP", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "GP", "GP", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "GP", "Updated to 10 GP because BCLCS_LV_5 = 'GP'", lbl_edit),
           row_updated = if_else(correction_cd == "GP", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "GP", TRUE, blank_eco_variables))

  ## MI - Mine (line 512) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 %in% c("MI", "TZ", "MZ")  & !row_updated, "MI", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "MI", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "MI", "MI", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "MI", "Updated to 10 MI because BCLCS_LV_5 = 'MI', 'TZ' or 'MZ'", lbl_edit),
           row_updated = if_else(correction_cd == "MI", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "MI", TRUE, blank_eco_variables))

  ## RO - Rock (line 519) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 %in% c("RO", "BR", "BI")  & !row_updated, "RO", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "RO", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "RO", "RO", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "RO", "Updated to 10 RO because BCLCS_LV_5 = 'RO', 'BR' or 'BI'", lbl_edit),
           row_updated = if_else(correction_cd == "RO", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "RO", TRUE, blank_eco_variables))
  
  ## TA - Talus (line 526) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "TA"  & !row_updated, "TA", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "TA", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "TA", "TA", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "TA", "Updated to 10 TA because BCLCS_LV_5 = 'TA'", lbl_edit),
           row_updated = if_else(correction_cd == "TA", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "TA", TRUE, blank_eco_variables))

  ## TC - Transportation Corridor (line 533) ----
  vri_bem <- vri_bem |> 
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 %in% c("TC", "RN", "RZ")  & !row_updated, "TC", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "TC", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "TC", "TC", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "TC", "Updated to 10 TC because BCLCS_LV_5 = 'TC', 'RN' or 'RZ'", lbl_edit),
           row_updated = if_else(correction_cd == "TC", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "TC", TRUE, blank_eco_variables))
  

  ##  TR - Transmission Corridor (line 540) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "TR"  & !row_updated, "TR", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "TR", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "TR", "TR", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "TR", "Updated to 10 TR because BCLCS_LV_5 = 'TR'", lbl_edit),
           row_updated = if_else(correction_cd == "TR", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "TR", TRUE, blank_eco_variables))
  
  
  ## UV - Unvegetated (line 547) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 %in% c("UV", "RS", "MU", "ES", "CB", "MN", "RM","LL")  & !row_updated, "UV", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "UV", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "UV", "UV", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "UV", "Updated to 10 UV because BCLCS_LV_5 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN','LL' or 'RM'", lbl_edit),
           row_updated = if_else(correction_cd == "UV", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "UV", TRUE, blank_eco_variables))
  
  vri_bem <- vri_bem |> 
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & LAND_CD_1 %in% c("UV", "RS", "MU", "ES", "CB", "MN", "RM") & COV_PCT_1 >= 95  & !row_updated, "UV_LANDCD", correction_cd)) |>
    mutate(SDEC_1 = if_else(correction_cd == "UV_LANDCD", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "UV_LANDCD", "UV", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "UV_LANDCD", "Updated to 10 UV because LAND_CD_1 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN' or 'RM' and COV_PCT_1 >= 95", lbl_edit),
           row_updated = if_else(correction_cd == "UV_LANDCD", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "UV_LANDCD", TRUE, blank_eco_variables))

  
  ## UR - Urban (line 564) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_5 == "UR"  & !row_updated, "UR", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "UR", 10, SDEC_1),
           BEUMC_S1 = if_else(correction_cd == "UR", "UR", BEUMC_S1),
           lbl_edit = if_else(correction_cd == "UR", "Updated to 10 UR because BCLCS_LV_5 = 'UR'", lbl_edit),
           row_updated = if_else(correction_cd == "UR", TRUE, row_updated),
           blank_eco_variables = if_else(correction_cd == "UR", TRUE, blank_eco_variables))

  
  ## TC - Transportation Corridor (Component 2) (line 564) ----
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & BCLCS_LV_2 == "T" & SDEC_1 == 10 &
                                    LBL_VEGCOV %in% c('rz', 'rz,by', 'rz,by,he', 'rz,by,he,sl', 'rz,by,sl', 'rz,by,sl,he', 'rz,by,st', 'rz,he',
                                                      'rz,by,sl,he', 'rz,by,st', 'rz,he', 'rz,he,by', 'rz,he,by,sl', 'rz,he,sl', 'rz,he,sl,by',
                                                      'rz,he,st', 'rz,he,st,by', 'rz,hf,by', 'rz,hf,sl,by', 'rz,hg', 'rz,hg,sl', 'rz,sl',
                                                      'rz,sl,by', 'rz,sl,by,he', 'rz,sl,he', 'rz,sl,he,by', 'rz,sl,hf', 'rz,sl,hf,by', 'rz,sl,hg',
                                                      'rz,st', 'rz,st,he', 'rz,st,hf', 'rz,st,hg')  & !row_updated,
                                    "TC", "")) |>
    mutate(SDEC_1 = if_else(correction_cd == "TC", 8L , SDEC_1),
           SDEC_2 = if_else(correction_cd == "TC", 2L , SDEC_2),
           BEUMC_S2 = if_else(correction_cd == "TC", "TC", BEUMC_S2),
           lbl_edit = if_else(correction_cd == "TC", "Added 2nd component 2 TC because BCLCS_LV_2 = 'T' and LBL_VEGCOV begins with 'rz'", lbl_edit),
           row_updated = if_else(correction_cd == "TC", TRUE , row_updated))
  
  # Update STAND_A1 ----
  # line 608 (no `else if` be careful! it's a simple if)

  vri_bem <- vri_bem |> 
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & SPEC_CD_1 %in% c("AC", "ACB", "ACT", "AT", "EP") & SPEC_PCT_1 >= 75 & STAND_A1 %in% c("C", "M") , 
                                    "STAND_A1_B", correction_cd)) |>
    mutate(STAND_A1 = if_else(correction_cd == "STAND_A1_B", "B", STAND_A1),
           lbl_edit = if_else(correction_cd == "STAND_A1_B", dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "),
                                                                    "Updated STAND_A1 to 'B' because SPEC_CD_1 = '", SPEC_CD_1, "' and SPEC_PCT_1 >= 75 and STAND_A1 was 'C' or 'M'"), lbl_edit),
           row_updated = if_else(correction_cd == "STAND_A1_B", TRUE, row_updated))
  
  # line 618
  vri_bem <- vri_bem |>
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & SPEC_CD_1 %in% c("AC", "ACB", "ACT", "AT", "EP") &
                                    SPEC_PCT_1 >= 50 & SPEC_PCT_1 < 75 & STAND_A1 %in% c("C", "B") , 
                                    "STAND_A1_M", correction_cd)) |>
    mutate(STAND_A1 = if_else(correction_cd == "STAND_A1_M", "M", STAND_A1),
           lbl_edit = if_else(correction_cd == "STAND_A1_M", dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "),
                                                                    "Updated STAND_A1 to 'M' because SPEC_CD_1 = '", SPEC_CD_1, "' and SPEC_PCT_1 >= 50 and < 75 and STAND_A1 was 'C' or 'B'"), lbl_edit),
           row_updated = if_else(correction_cd == "STAND_A1_M", TRUE, row_updated)
          )

  # line 627
  vri_bem <- vri_bem |> 
    mutate(correction_cd = if_else(is.na(SMPL_TYPE) & SPEC_CD_1 %in% c("B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW", "PA", "PL", "PLI",
                                                                        "S", "SB", "SE", "SS", "SW", "SX", "SXW") &
                                    SPEC_PCT_1 >= 75 & STAND_A1 %in% "M" , 
                                    "STAND_A1_C", correction_cd)) |>
    mutate(STAND_A1 = if_else(correction_cd == "STAND_A1_C", "C", STAND_A1),
           lbl_edit = if_else(correction_cd == "STAND_A1_C", dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "),
                                                                    "Updated STAND_A1 to 'C' because SPEC_CD_1 = '", SPEC_CD_1, "' and SPEC_PCT_1 >= 75 and STAND_A1 was 'M'"), lbl_edit),
           row_updated = if_else(correction_cd == "STAND_A1_C", TRUE, row_updated))

 
  #Blank Eco Fields (line 639) ----
  vri_bem <- shift_eco_variables(vri_bem, cond_var = blank_eco_variables, 
                                 shift_pattern = list(c(1,NA), c(2,NA), c(3,NA)), 
                                 char_vars_1 = c("REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                                                  "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", 
                                                  "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                                                  "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                                                  "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"))|>
    mutate(SDEC_2 = if_else(blank_eco_variables, 0L, SDEC_2), 
           SDEC_3 = if_else(blank_eco_variables, 0L, SDEC_3)) 
  

  # line 654
  vri_bem <- vri_bem |>
    mutate(DEC_Total = if_else(is.na(SMPL_TYPE), SDEC_1 + SDEC_2 + SDEC_3, DEC_Total)) |> # Should we put NA for rows with SMPL_TYPE not NA?
    mutate(lbl_edit = if_else(is.na(SMPL_TYPE) & DEC_Total != 10, dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "),
                             "**** DECILE TOTAL ", SDEC_1, "+", SDEC_2, "+", SDEC_3, "=", DEC_Total), lbl_edit),
           row_updated = if_else(is.na(SMPL_TYPE) & DEC_Total != 10, TRUE, row_updated))
  
  
  #Check for allowed BEC/BEU combinations
  vri_bem <- check_allowed_bec_beu(vri_bem, beu_bec)

  # for all feature that intersect with rivers
  # SITE_M3A becomes "a"
  # and lbl is updated to say the old value became "a"

  #TODO
  # maybe reverse the geometry and the unique ( need to test)
  # just need to find the line that intersect with rivers
  
  vri_bem <- mutate(vri_bem, 
    SITE_M3A = if_else(INTERSECTS_RIVER, "a", SITE_M3A),
    lbl_edit = if_else(INTERSECTS_RIVER, dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "),
                             "Updated SITE_M3A from '", SITE_M3A, "' to 'a' because polygon is adjacent to river"), lbl_edit))
  
    # remove temp variables
  vri_bem |>
    select(-c(correction_cd, row_updated, blank_eco_variables, merge_key))

}

combine_duplicated_BEUMC <- function(ifc, use_ifelse = FALSE) { 
  ifc <- ifc |>
    mutate(duplicated = coalesce(BEUMC_S1 == BEUMC_S2 & !!sql("SMPL_TYPE IS NULL"), FALSE)) |>
    mutate(
      SDEC_1 = if_else(duplicated, SDEC_1 + SDEC_2, SDEC_1),
      SDEC_2 = if_else(duplicated, SDEC_3, SDEC_2),
      SDEC_3 = if_else(duplicated, 0L, SDEC_3),
      lbl_edit = if_else(duplicated & lbl_edit != '', dd$concat(lbl_edit, '; Combined components 1 and 2 with same BEUMC_S# code into single component 1'), 
                         if_else(duplicated, 'Combined components 1 and 2 with same BEUMC_S# code into single component 1', lbl_edit)),                             
      row_updated = if_else(duplicated, use_ifelse, row_updated)
    ) |>
    shift_eco_variables(cond_var = duplicated, shift_pattern = list(c(2,3), c(3,NA))) |>
    select(-duplicated) 
  
  ifc
}

remove_inadequate_wetlands <- function(ifc){

  #validate variables exists in ifc

  validate_required_attributes(ifc,
                               required_attributes = c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "BCLCS_LV_4", "SDEC_1", "SDEC_2", "SDEC_3"))
  
  #Replace wetland in 3rd component ----
  ifc <- ifc |>
    mutate(treed_WL_3 = coalesce(is.na(SMPL_TYPE) & BCLCS_LV_4 %in% c("TB", "TC", "TM") & BEUMC_S3 == "WL" & !row_updated, FALSE)) |>
    mutate(SDEC_2 = if_else(treed_WL_3, SDEC_2 + SDEC_3, SDEC_2),
           SDEC_3 = if_else(treed_WL_3, 0L, SDEC_3),
           lbl_edit = if_else(treed_WL_3, "Removed WL in component 3 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'", lbl_edit),
           row_updated = if_else(treed_WL_3, TRUE, row_updated))|>
    shift_eco_variables(cond_var = treed_WL_3, shift_pattern = list(c(3, NA)))|>
    select(-treed_WL_3)


  #Replace wetlands in 2nd component ----
  ifc <- ifc |>
    mutate(treed_WL_2_from_3 = coalesce(is.na(SMPL_TYPE) & BCLCS_LV_4 %in% c("TB", "TC", "TM") & BEUMC_S2 == "WL" & SDEC_3 > 0 & !row_updated, FALSE)) |>
    mutate(treed_WL_2_to_1 = coalesce(is.na(SMPL_TYPE) & BCLCS_LV_4 %in% c("TB", "TC", "TM") & BEUMC_S2 == "WL" & SDEC_3 %in% c(0, NA_integer_) & !row_updated, FALSE))
    
  ## When there is a value in 3rd component update 2nd from 3rd ----
  ifc <- shift_eco_variables(ifc, cond_var = treed_WL_2_from_3, shift_pattern = list(c(2,3), c(3,NA))) |>
    mutate(SDEC_2 = if_else(treed_WL_2_from_3, SDEC_2 + SDEC_3, SDEC_2),
           SDEC_3 = if_else(treed_WL_2_from_3, 0L, SDEC_3),
           lbl_edit = if_else(treed_WL_2_from_3, "Removed WL in component 2 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'", lbl_edit),
           row_updated = if_else(treed_WL_2_from_3, TRUE, row_updated)) |>
    select(-treed_WL_2_from_3)
    
  ## When there is no value in 3rd component update 1st from 2nd -----
  ifc <- ifc |>
    mutate(SDEC_1 = if_else(treed_WL_2_to_1, SDEC_1 + SDEC_2, SDEC_1)) |>
    shift_eco_variables(cond_var = treed_WL_2_to_1, shift_pattern = list(c(2, NA))) |>
    mutate(SDEC_2 = if_else(treed_WL_2_to_1, 0L, SDEC_2),
           lbl_edit = if_else(treed_WL_2_to_1, "Removed WL in component 2 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'", lbl_edit),
           row_updated = if_else(treed_WL_2_to_1, TRUE, row_updated)) |>
    select(-treed_WL_2_to_1)
  

  #Replace wetlands from 1st component -----
  ifc <- ifc |>
    mutate(treed_WL_1_from_2 = coalesce(is.na(SMPL_TYPE) & BCLCS_LV_4 %in% c("TB", "TC", "TM") & BEUMC_S1 == "WL" & SDEC_2 > 0 & !row_updated, FALSE)) |>
    shift_eco_variables(cond_var = treed_WL_1_from_2, shift_pattern = list(c(1,2), c(2,3), c(3,NA))) |>
    mutate(SDEC_3 = if_else(treed_WL_1_from_2, 0L, SDEC_3),
           lbl_edit = if_else(treed_WL_1_from_2, "Removed WL in component 1 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'", lbl_edit),
           row_updated = if_else(treed_WL_1_from_2, TRUE, row_updated)) |>
    select(-treed_WL_1_from_2)
      

  #Warning if polygon is pule WL ----
  ifc <- ifc |>
    mutate(treed_pure_WL = coalesce(is.na(SMPL_TYPE) & BCLCS_LV_4 %in% c("TB", "TC", "TM") & BEUMC_S1 %in% c('0', NA_integer_), FALSE)) |>
    mutate(lbl_edit = if_else(treed_pure_WL, "**** Warning: Polygon is pure WL, but BCLCS_LV_4 = 'TB', 'TC' or 'TM'", lbl_edit),
           row_updated = if_else(treed_pure_WL, TRUE, row_updated)) |>
    select(-treed_pure_WL)


  # Remove BEU for lakes ----
  # In cases for small lakes (LS), large lakes (LL), and open water (OW) where BCLCS_LV_5 AND LAND_CD_1 DO NOT equal LA,
  # remove BEU label for lakes -- will need to be manually assigned. Otherwise, if BCLCS_LV_5 OR LAND_CD_1 = LA, leave BEU as-is.
  # include "OT" with "LA". Sometimes lakes are assigned BCLCS = "OT"
  ifc <- ifc |>
    mutate(BEUMC_S1 = if_else(BEUMC_S1 %in% c("LS", "LL", "OW") & !BCLCS_LV_5 %in% c("LA","OT") & !LAND_CD_1 %in% c("LA","OT"), NA_character_, BEUMC_S1))

  #If the BEU was correctly assigned to an ecosystem which should not have a structural stage (as identified in the lookup table)
  # remove associated structure/stand information so it correctly populates suitability

  ifc <- ifc |>
    mutate(BEUMC_S1_in_list = BEUMC_S1 %in% c("LS", "LL", "OW","MI","GL","TC","UR","RE","RI","ES","ST","UR"),
           BEUMC_S2_in_list = BEUMC_S2 %in% c("LS", "LL", "OW","MI","GL","TC","UR","RE","RI","ES","ST","UR"),
           BEUMC_S3_in_list = BEUMC_S3 %in% c("LS", "LL", "OW","MI","GL","TC","UR","RE","RI","ES","ST","UR")) |>
    mutate(STRCT_S1 = if_else(BEUMC_S1_in_list, NA_character_, STRCT_S1),
           STAND_A1 = if_else(BEUMC_S1_in_list, NA_character_, STAND_A1),
           STRCT_S2 = if_else(BEUMC_S2_in_list, NA_character_, STRCT_S2),
           STAND_A2 = if_else(BEUMC_S2_in_list, NA_character_, STAND_A2),
           STRCT_S3 = if_else(BEUMC_S3_in_list, NA_character_, STRCT_S3),
           STAND_A3 = if_else(BEUMC_S3_in_list, NA_character_, STAND_A3)) |>
    #make sure non-forested features are not indicated as forested
    mutate(FORESTED_1 = if_else(BEUMC_S1_in_list, "N", FORESTED_1),
           FORESTED_2 = if_else(BEUMC_S2_in_list, "N", FORESTED_2),
           FORESTED_3 = if_else(BEUMC_S3_in_list, "N", FORESTED_3)) |>
    select(-c(BEUMC_S1_in_list, BEUMC_S2_in_list, BEUMC_S3_in_list))


  ifc

}


#' Check allowed BEC/BEU combinations
#'
#'This function uses a table of allowed BEC/BEU combinations and updates BEUs which are identified as mismatches.
#
#' @param vri_bem sf object that represent the combined vri & bem polygon feature class
#' @param beu_bec data.table object of allowed BEC and BEM Code Combos
#' @details
#' This function checks all BEC/BEU combinations.
#' In cases where BEUs are assigned to BEC units they can't occur in, they will be updated to the correct BEU.
#' Note that mismatches may still occur if they aren't reflected in the allowed BEC/BEU code combos. Make sure to update regularly.
#'
#' @return sf object which contains adjusted map codes
#' @import sf
#' @import data.table
#' @export

check_allowed_bec_beu <- function(vri_bem, beu_bec) {

  
  # bgc subzone and beu mapcode
  vri_bem <- mutate(vri_bem, merge_key = dd$concat(BGC_ZONE, BGC_SUBZON))|>
    left_join(select(beu_bec, -Name), by = c("merge_key" = "BGC_Subzone", "BEUMC_S1" = "BEU"), suffix = c("", "_bec_1"), copy = TRUE) |>
    # merge and change beu for decile 1
    mutate(BEUMC_S1 = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), Change_to_BEU, BEUMC_S1),
           lbl_edit = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S1, " corrected to ", Change_to_BEU, " in decile 1"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), TRUE, row_updated)) |>
    mutate(lbl_edit = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) != 2, FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S1, " in decile 1 is invalid combination (mapper needs to assess)"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) != 2, FALSE), TRUE, row_updated)) |>
    mutate(lbl_edit = if_else(coalesce(Script_Rule == "Error" & is.na(Change_to_BEU), FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S1, " in decile 1 combination is not listed"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & is.na(Change_to_BEU), FALSE), TRUE, row_updated)) |>
    select(-c(Script_Rule, Change_to_BEU)) |>
    # merge and change beu for decile 2
    left_join(select(beu_bec, -Name), by = c("merge_key" = "BGC_Subzone", "BEUMC_S2" = "BEU"), suffix = c("", "_bec_2"), copy = TRUE) |> 
    mutate(BEUMC_S2 = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), Change_to_BEU, BEUMC_S2),
           lbl_edit = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S2, " corrected to ", Change_to_BEU, " in decile 2"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), TRUE, row_updated)) |>
    mutate(lbl_edit = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) != 2, FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S2, " in decile 2 is invalid combination (mapper needs to assess)"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) != 2, FALSE), TRUE, row_updated)) |>
    mutate(lbl_edit = if_else(coalesce(Script_Rule == "Error" & is.na(Change_to_BEU), FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S2, " in decile 2 combination is not listed"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & is.na(Change_to_BEU), FALSE), TRUE, row_updated)) |>
    select(-c(Script_Rule, Change_to_BEU)) |>
    # merge and change beu for decile 3
    left_join(select(beu_bec, -Name), by = c("merge_key" = "BGC_Subzone", "BEUMC_S3" = "BEU"), suffix = c("", "_bec_3"), copy = TRUE) |>
    mutate(BEUMC_S3 = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), Change_to_BEU, BEUMC_S3),
           lbl_edit = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S3, " corrected to ", Change_to_BEU, " in decile 3"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) == 2, FALSE), TRUE, row_updated)) |>
    mutate(lbl_edit = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) != 2, FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S3, " in decile 3 is invalid combination (mapper needs to assess)"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & dd$len(Change_to_BEU) != 2, FALSE), TRUE, row_updated)) |>
    mutate(lbl_edit = if_else(coalesce(Script_Rule == "Error" & is.na(Change_to_BEU), FALSE), dd$concat(lbl_edit, if_else(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S3, " in decile 3 combination is not listed"), lbl_edit),
           row_updated = if_else(coalesce(Script_Rule == "Error" & is.na(Change_to_BEU), FALSE), TRUE, row_updated)) |>  
    select(-c(Script_Rule, Change_to_BEU))

  vri_bem
}
