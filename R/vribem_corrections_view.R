#' Apply VRI/BEM corrections view
#'
#' @param conn A DuckDB connection.
#' @param vri_bem_tbl Name of the source table (default \code{"V_VRIBEM"}).
#' @param beu_bec Name of the allowed BEC/BEU combinations table.
#' @param clear_site_ma Logical; if TRUE, clears SITE_M1A and SITE_M2A (default TRUE).
#' @param use_ifelse Logical; reserved for future use (default TRUE).
#' @param result_tbl Name of the result table (default same as \code{vri_bem_tbl}).
#' @param skip_river_adjacency Logical; if TRUE, skips the INTERSECTS_RIVER update so
#'   that river adjacency can be applied per-cell in the raster phase (default FALSE).
#' @return Invisibly returns \code{result_tbl}.
#' @export
vribem_corrections_view <- function(conn, vri_bem_tbl = "V_VRIBEM", beu_bec, clear_site_ma = TRUE, use_ifelse = TRUE, result_tbl = vri_bem_tbl, skip_river_adjacency = FALSE){
  
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

  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TEMP TABLE %s AS (SELECT vri_bem.* FROM %s vri_bem);", result_tbl, vri_bem_tbl))
  
  add_col_to_tbl(conn, tbl_name = result_tbl, col = "lbl_edit", type = "VARCHAR DEFAULT ''")
  add_col_to_tbl(conn, tbl_name = result_tbl, col = "DEC_Total", type = "INTEGER DEFAULT 0")
  add_col_to_tbl(conn, tbl_name = result_tbl, col = "SMPL_TYPE", type = "VARCHAR DEFAULT NULL")
  add_col_to_tbl(conn, tbl_name = result_tbl, col = "row_updated", type = "BOOLEAN DEFAULT FALSE")
  add_col_to_tbl(conn, tbl_name = result_tbl, col = "blank_eco_variables", type = "BOOLEAN DEFAULT FALSE")
 
  

  
  # Clear SITE_M#A based on param but SITE_M3A is always cleared regardless of param.
  DBI::dbExecute(conn, 
    sprintf("UPDATE %s SET %s SITE_M3A = NULL;",
        result_tbl, 
        ifelse(clear_site_ma, "SITE_M1A = NULL, SITE_M2A = NULL,", ""))) 
  
  combine_duplicated_BEUMC_view(conn, view_name = result_tbl)
  
  # OW - Shallow Open Water (line 279) ----
  # -shallow open water typically associated with floating vegetation
  # -For LIW for moose, LS is rated 0.05, and OW is rated 0.25 because of its common association with a
  # shrub fringe
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_1 = 'N' AND BCLCS_LV_5 = 'LA' AND Area_Ha <= 2 AND NOT row_updated", 
    beumc_s1 = "OW", 
    lbl_edit = "Updated to 10 OW because BCLCS_LV_1 = N, BCLCS_LV_5 = LA, Area <= 10 ha")
  
   ## LS - Small Lake (line 291) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_1 = 'N' AND BCLCS_LV_5 = 'LA' AND Area_Ha >2 AND Area_Ha <= 60 AND NOT row_updated", 
    beumc_s1 = "LS", 
    lbl_edit = "Updated to 10 LS because BCLCS_LV_1 = N, BCLCS_LV_5 = LA, Area <= 60 ha")

  ## LL - Large Lake (line 303) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_1 = 'N' AND BCLCS_LV_5 = 'LA' AND Area_Ha >60 AND NOT row_updated", 
    beumc_s1 = "LL", 
    lbl_edit = "Updated to 10 LL because BCLCS_LV_1 = N, BCLCS_LV_5 = LA, Area > 60 ha")

  ## RE - Reservoir (line 315) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_1 = 'N' AND BCLCS_LV_5 = 'RE' AND NOT row_updated", 
    beumc_s1 = "RE", 
    lbl_edit = "Updated to 10 RE because BCLCS_LV_1 = N, BCLCS_LV_5 = RE")
  
  ## RI - Rivers (line 331) ----
  # There are two VRI codes (labels) that apply to rivers; river (RI) and river sediments (RS)
  # The default applied was to assign 'FP' (Fast Perennial Stream) to BEU_MC where rivers were identified by
  # this query.
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_1 = 'N' AND BCLCS_LV_5 IN('RI', 'RS') AND NOT row_updated", 
    beumc_s1 = "RI", 
    lbl_edit = "Updated to 10 RI because BCLCS_LV_1 = N, BCLCS_LV_5 = RI or RS")
  
  ## WL - Wetland (line 367) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_1 = 'V' AND BCLCS_LV_2 = 'N' AND BCLCS_LV_3 = 'W' AND AGE_CL_STS = -1  AND NOT row_updated", 
    beumc_s1 = "WL", 
    lbl_edit = "Updated to 10 WL because BCLCS_LV_1/2/3 = V/N/W and AGE_CL_STS = -1")

  ## Remove Wetland - Forested (line 380) ----
  # should not contain a wetland (WL) label component.
  # Remove WL decile component and update value in Decile
  remove_inadequate_wetlands_view(conn, view_name = result_tbl)

  ## BB - Black Spruce Bog (line 448) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND SPEC_CD_1 = 'SB' AND SPEC_PCT_1 >= 90 AND NOT row_updated", 
    beumc_s1 = "BB", 
    lbl_edit = "Updated to 10 BB because SPEC_CD_1 = SB and SPEC_PCT_1 >= 90")

  ## AP - Anthropogenic and Non-vegetated (line 457) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'AP' AND NOT row_updated", 
    beumc_s1 = "AP", 
    lbl_edit = "Updated to 10 AP because BCLCS_LV_5 = AP")

  ## BU - (line 464) -----
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET SDEC_1 = 10,
       DISTCLS_1 = 'F',
       lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Updated to 10 BU because BCLCS_LV_5 = BU' ELSE 'Updated to 10 BU because BCLCS_LV_5 = BU' END, 
       row_updated = TRUE,
       blank_eco_variables = TRUE
      WHERE SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'BU' AND NOT row_updated;")
  )

  ## CL - Cliff (line 470) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND SLOPE_MOD IN ('q', 'z', ) AND NOT row_updated", 
    beumc_s1 = "CL", 
    lbl_edit = "Updated to 10 CL because Slope Mod is q or z")
  
  #TODO see if comment below from python script still applies
  # TO BE ADDED
  # if  If Site Mod 3A = "a" and STS_AGE_CL >= 1 then assign 10 PR, ER OR WR as per which
  # BEC zone those units are allowed (ER can only go in the ESSF, etc.).  In common language
  # I'm saying that forested units adjacent to floodplains should be one of these riparian
  # forest types as the dominant forest ecosystem.

  ## GB - Gravel Bar (line 484) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'GB' AND NOT row_updated", 
    beumc_s1 = "GB", 
    lbl_edit = "Updated to 10 GB because BCLCS_LV_5 = GB")
  
  ## GL - Glacier (line 491) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 IN ('GL', 'PN')  AND NOT row_updated", 
    beumc_s1 = "GL", 
    lbl_edit = "Updated to 10 GL because BCLCS_LV_5 IN (GL, PN)")
  
   ## GP - Gravel Pit (line 498) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'GP' AND NOT row_updated", 
    beumc_s1 = "GP", 
    lbl_edit = "Updated to 10 GP because BCLCS_LV_5 = GP")
  
  ## MI - Mine (line 512) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 IN ('MI', 'TZ', 'MZ') AND NOT row_updated", 
    beumc_s1 = "MI", 
    lbl_edit = "Updated to 10 MI because BCLCS_LV_5 IN (MI, TZ, MZ)")
  
  ## RO - Rock (line 519) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 IN ('RO', 'BR', 'BI') AND NOT row_updated", 
    beumc_s1 = "RO", 
    lbl_edit = "Updated to 10 RO because BCLCS_LV_5 IN (RO, BR, BI)")
  
  ## TA - Talus (line 526) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'TA' AND NOT row_updated", 
    beumc_s1 = "TA", 
    lbl_edit = "Updated to 10 TA because BCLCS_LV_5 = TA")
  
  ## TC - Transportation Corridor (line 533) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 IN ('TC', 'RN', 'RZ') AND NOT row_updated", 
    beumc_s1 = "TC", 
    lbl_edit = "Updated to 10 TC because BCLCS_LV_5 IN (TC, RN, RZ)")
  
  ##  TR - Transmission Corridor (line 540) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'TR' AND NOT row_updated", 
    beumc_s1 = "TR", 
    lbl_edit = "Updated to 10 TR because BCLCS_LV_5 = TR")
  
  ## UV - Unvegetated (line 547) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 IN ('UV', 'RS', 'MU', 'ES', 'CB', 'MN', 'RM','LL') AND NOT row_updated", 
    beumc_s1 = "UV", 
    lbl_edit = "Updated to 10 UV because BCLCS_LV_5 IN (UV, RS, MU, ES, CB, MN, RM, LL)")

  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND LAND_CD_1 IN ('UV', 'RS', 'MU', 'ES', 'CB', 'MN', 'RM') AND COV_PCT_1 >= 95 AND NOT row_updated", 
    beumc_s1 = "UV", 
    lbl_edit = "Updated to 10 UV because LAND_CD_1 IN (UV, RS, MU, ES, CB, MN, RM) and COV_PCT_1 >= 95")

  ## UR - Urban (line 564) ----
  fix_vri_bem_1st_eco(conn, tbl_name = result_tbl,
    cond = "SMPL_TYPE IS NULL AND BCLCS_LV_5 = 'UR' AND NOT row_updated", 
    beumc_s1 = "UR", 
    lbl_edit = "Updated to 10 UR because BCLCS_LV_5 = UR")
  
  ## TC - Transportation Corridor (Component 2) (line 564) ----
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, " 
      SET SDEC_1 = 8,
       SDEC_2 = 2, 
       BEUMC_S2 = 'TC',
       DISTCLS_1 = 'F',
       lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'UAdded 2nd component 2 TC because BCLCS_LV_2 = T and LBL_VEGCOV begins with rz' ELSE 'Added 2nd component 2 TC because BCLCS_LV_2 = T and LBL_VEGCOV begins with rz' END, 
       row_updated = TRUE
      WHERE SMPL_TYPE IS NULL AND BCLCS_LV_2 = 'T' AND SDEC_1 = 10 AND 
       LBL_VEGCOV IN ('rz', 'rz,by', 'rz,by,he', 'rz,by,he,sl', 'rz,by,sl', 'rz,by,sl,he', 'rz,by,st', 'rz,he',
                      'rz,by,sl,he', 'rz,by,st', 'rz,he', 'rz,he,by', 'rz,he,by,sl', 'rz,he,sl', 'rz,he,sl,by',
                      'rz,he,st', 'rz,he,st,by', 'rz,hf,by', 'rz,hf,sl,by', 'rz,hg', 'rz,hg,sl', 'rz,sl',
                      'rz,sl,by', 'rz,sl,by,he', 'rz,sl,he', 'rz,sl,he,by', 'rz,sl,hf', 'rz,sl,hf,by', 'rz,sl,hg',
                      'rz,st', 'rz,st,he', 'rz,st,hf', 'rz,st,hg') AND NOT row_updated;")
    
  )

  # Update STAND_A1 ----
  # line 608 (no `else if` be careful! it's a simple if)
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET STAND_A1 = 'B', 
       lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Updated STAND_A1 to B because SPEC_CD_1 = ' || SPEC_CD_1 || ' and SPEC_PCT_1 >= 75 and STAND_A1 was C or M' ELSE 'Updated STAND_A1 to B because SPEC_CD_1 = ' || SPEC_CD_1 || ' and SPEC_PCT_1 >= 75 and STAND_A1 was C or M' END, 
       row_updated = TRUE
      WHERE SMPL_TYPE IS NULL AND SPEC_CD_1 IN ('AC', 'ACB', 'ACT', 'AT', 'EP') AND SPEC_PCT_1 >= 75  AND STAND_A1 IN('C','M') ;")
  )

  # line 618
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET STAND_A1 = 'M', 
       lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Updated STAND_A1 to M because SPEC_CD_1 = ' || SPEC_CD_1 || ' and SPEC_PCT_1 >= 50 and SPEC_PCT_1 < 75 and STAND_A1 was C or M' ELSE 'Updated STAND_A1 to M because SPEC_CD_1 = ' || SPEC_CD_1 || ' and SPEC_PCT_1 >= 50 and SPEC_PCT_1 < 75 and STAND_A1 was C or M' END, 
       row_updated = TRUE
      WHERE SMPL_TYPE IS NULL AND SPEC_CD_1 IN ('AC', 'ACB', 'ACT', 'AT', 'EP') AND SPEC_PCT_1 >= 50 AND SPEC_PCT_1 < 75  AND STAND_A1 IN('C','M') ;")
  )

  # line 627
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET STAND_A1 = 'C', 
       lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Updated STAND_A1 to C because SPEC_CD_1 = ' || SPEC_CD_1 || ' and SPEC_PCT_1 >= 75 and STAND_A1 was M' ELSE 'Updated STAND_A1 to C because SPEC_CD_1 = ' || SPEC_CD_1 || ' and SPEC_PCT_1 >= 75 and STAND_A1 was M' END, 
       row_updated = TRUE
      WHERE SMPL_TYPE IS NULL AND SPEC_CD_1 IN ('B', 'BB', 'BL', 'CW', 'FD', 'FDI', 'HM', 'HW', 'PA', 'PL', 'PLI', 'S', 'SB', 'SE', 'SS', 'SW', 'SX', 'SXW') AND SPEC_PCT_1 >= 75 AND STAND_A1 = 'M' ;")
  )

  #Blank Eco Fields (line 639) ----
  shift_eco_variables_in_view(conn = conn, 
                              view_name = result_tbl, 
                              cond = "blank_eco_variables = TRUE", 
                              shift_pattern = list(c("1", NA), c("2", NA), c("3", NA)), 
                              eco_vars_1 = c("REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                                                  "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", 
                                                  "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                                                  "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                                                  "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1", "TREE_C1", "SHRUB_C1"))
  
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET SDEC_2 = 0, SDEC_3 = 0 
      WHERE blank_eco_variables = TRUE;"))

  # Validate total deciles line 654 
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET DEC_Total = IFNULL(SDEC_1, 0) + IFNULL(SDEC_2, 0) + IFNULL(SDEC_3, 0), 
       row_updated = TRUE
      WHERE SMPL_TYPE IS NULL;"))
  
  DBI::dbExecute(conn, paste0("
    UPDATE ", result_tbl, "
      SET lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || '**** DECILE TOTAL ' || IFNULL(SDEC_1, 0) || '+' || IFNULL(SDEC_2, 0) || '+' || IFNULL(SDEC_3, 0) || '=' || DEC_Total ELSE '**** DECILE TOTAL ' || IFNULL(SDEC_1, 0) || '+' || IFNULL(SDEC_2, 0) || '+' || IFNULL(SDEC_3, 0) || '=' || DEC_Total END
      WHERE SMPL_TYPE IS NULL;"))

  #Check for allowed BEC/BEU combinations
  check_allowed_bec_beu_view(conn = conn, vri_bem_view = result_tbl, beu_bec_view = beu_bec)

  # for all feature that intersect with rivers
  # SITE_M3A becomes "a"
  # and lbl is updated to say the old value became "a"
  if (!skip_river_adjacency) {
    DBI::dbExecute(conn, paste0("
      UPDATE ", result_tbl, "
        SET SITE_M3A = 'a', 
         lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Updated SITE_M3A from ' || SITE_M3A || ' to a because unit intersects with river feature' ELSE 'Updated SITE_M3A from ' || SITE_M3A || ' to a because unit intersects with river feature' END
        WHERE INTERSECTS_RIVER;"))
  }

  # remove temp variables 
  rm_cols_from_tbl(conn, tbl_name = result_tbl, c("row_updated", "blank_eco_variables"))

  DBI::dbExecute(conn, paste0("CREATE INDEX IF NOT EXISTS idx_vribem ON ", result_tbl," USING RTREE (Shape);"))

  invisible(result_tbl)
}















combine_duplicated_BEUMC_view <- function(conn, view_name){
  
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET SDEC_1 = SDEC_1 + SDEC_2,
        SDEC_2 = SDEC_3,
        SDEC_3 = NULL,
        lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Combined components 1 and 2 with same BEUMC_S# code into single component 1'
                          ELSE 'Combined components 1 and 2 with same BEUMC_S# code into single component 1'
                   END,
        row_updated = TRUE
    WHERE BEUMC_S1 = BEUMC_S2 AND SMPL_TYPE IS NULL;",
     view_name))
  
  shift_eco_variables_in_view(conn = conn, 
                              view_name = view_name, 
                              cond = "BEUMC_S1 = BEUMC_S2 AND SMPL_TYPE IS NULL", 
                              shift_pattern = list(c("2", "3"), c("3", NA)))

}
  
shift_eco_variables_in_view <- function(
  conn, 
  view_name, 
  cond, 
  shift_pattern,
  eco_vars_1 = c("REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                  "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
                  "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", "DISTCLS_1", "DISTSCLS_1",
                  "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1",
                  "TREE_C1", "SHRUB_C1"))  {
  
  set_clause <- ""

  for (comb in shift_pattern){
    lhs_nbr <- as.character(comb[1])
    rhs_nbr <- as.character(comb[2])

    stopifnot(rhs_nbr %in% c("1", "2", "3", NA_character_))
    stopifnot(lhs_nbr %in% c("1", "2", "3"))

    if (is.na(rhs_nbr)){
      set_clause_i <- paste(paste0(sub("1", lhs_nbr, eco_vars_1), " = NULL"), collapse = ", ")
    } else {
      set_clause_i <- paste(paste0(sub("1", lhs_nbr, eco_vars_1), " = ", sub("1", rhs_nbr, eco_vars_1)), collapse = ", ")
    }
    set_clause <- paste(set_clause, set_clause_i, sep = ifelse(set_clause == "", "", ", "))
  }

  if (set_clause != ""){
    DBI::dbExecute(conn, sprintf("UPDATE %s SET %s WHERE %s;", view_name, set_clause, cond))
  } 
  
}

fix_vri_bem_1st_eco <- function(conn, tbl_name = "V_VRIBEM", cond, sdec_1 = 10, beumc_s1, lbl_edit){
  DBI::dbExecute(conn, 
    sprintf("UPDATE %s 
      SET SDEC_1 = %s,
       BEUMC_S1 = '%s', 
       lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || '%s' ELSE '%s' END, 
       row_updated = TRUE,
       blank_eco_variables = TRUE
      WHERE %s;", 
    tbl_name, sdec_1, beumc_s1, lbl_edit, lbl_edit, cond)
  )
}

remove_inadequate_wetlands_view <- function(conn, view_name = "V_VRIBEM"){
  
  validate_views_column_names(conn = conn, 
                              obj = view_name,
                              required_names = c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "BCLCS_LV_4", "SDEC_1", "SDEC_2", "SDEC_3"))

  # Replace wetland in 3rd component ---
  add_col_to_tbl(conn, tbl_name = view_name, col = "treed_WL_3", type = "BOOLEAN DEFAULT FALSE")
  add_col_to_tbl(conn, tbl_name = view_name, col = "treed_WL_2_from_3", type = "BOOLEAN DEFAULT FALSE")
  add_col_to_tbl(conn, tbl_name = view_name, col = "treed_WL_2_to_1", type = "BOOLEAN DEFAULT FALSE")
  add_col_to_tbl(conn, tbl_name = view_name, col = "treed_WL_1_from_2", type = "BOOLEAN DEFAULT FALSE")
  add_col_to_tbl(conn, tbl_name = view_name, col = "treed_pure_WL", type = "BOOLEAN DEFAULT FALSE")

  
  DBI::dbExecute(conn,
     sprintf("
    UPDATE %s 
    SET SDEC_2 = SDEC_2 + SDEC_3,
        SDEC_3 = 0,
        lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Removed WL in component 3 because BCLCS_LV_4 IN (TB, TC, TM)' ELSE 'Removed WL in component 3 because BCLCS_LV_4 IN (TB, TC, TM)' END, 
        row_updated = TRUE, 
        treed_WL_3 = TRUE
    WHERE BCLCS_LV_4 IN ('TB', 'TC', 'TM') AND BEUMC_S3 = 'WL' AND SMPL_TYPE IS NULL AND NOT row_updated;",
    view_name))
  
  shift_eco_variables_in_view(conn = conn, 
                              view_name = view_name, 
                              cond = "treed_WL_3 = TRUE", 
                              shift_pattern = list(c("3", NA)))
  
  #Replace wetlands in 2nd component ----
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET treed_WL_2_from_3 = TRUE, 
    WHERE BCLCS_LV_4 IN ('TB', 'TC', 'TM') AND BEUMC_S2 = 'WL' AND SDEC_3 > 0 AND SMPL_TYPE IS NULL AND NOT row_updated;",
    view_name))
  
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET treed_WL_2_to_1 = TRUE, 
    WHERE BCLCS_LV_4 IN ('TB', 'TC', 'TM') AND BEUMC_S2 = 'WL' AND (SDEC_3 = 0 OR SDEC_3 IS NULL) AND SMPL_TYPE IS NULL AND NOT row_updated;",
    view_name))
  
  shift_eco_variables_in_view(conn = conn, 
                              view_name = view_name, 
                              cond = "treed_WL_2_from_3 = TRUE", 
                              shift_pattern = list(c(2, 3), c(3, NA)))
  
  ## When there is a value in 3rd component update 2nd from 3rd ----
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET SDEC_2 = SDEC_2 + SDEC_3,
        SDEC_3 = 0,
        lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Removed WL in component 2 because BCLCS_LV_4 IN (TB, TC, TM)' ELSE 'Removed WL in component 2 because BCLCS_LV_4 IN (TB, TC, TM)' END, 
        row_updated = TRUE
    WHERE treed_WL_2_from_3;",
    view_name))
  
  ## When there is no value in 3rd component update 1st from 2nd -----
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET SDEC_1 = SDEC_1 + SDEC_2,
        SDEC_2 = 0,
        lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Removed WL in component 2 because BCLCS_LV_4 IN (TB, TC, TM)' ELSE 'Removed WL in component 2 because BCLCS_LV_4 IN (TB, TC, TM)' END, 
        row_updated = TRUE
    WHERE treed_WL_2_to_1;",
    view_name))
  
  shift_eco_variables_in_view(conn = conn, 
                              view_name = view_name, 
                              cond = "treed_WL_2_to_1 = TRUE", 
                              shift_pattern = list(c(2, NA)))
  
  #Replace wetlands from 1st component -----
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET treed_WL_1_from_2 = TRUE, 
      SDEC_3 = 0, 
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || 'Removed WL in component 1 because BCLCS_LV_4 IN (TB, TC, TM)' ELSE 'Removed WL in component 1 because BCLCS_LV_4 IN (TB, TC, TM)' END,
      row_updated = TRUE
    WHERE BCLCS_LV_4 IN ('TB', 'TC', 'TM') AND BEUMC_S1 = 'WL' AND SDEC_2 > 0 AND NOT row_updated;",
    view_name))
  
  shift_eco_variables_in_view(conn = conn, 
                              view_name = view_name, 
                              cond = "treed_WL_1_from_2 = TRUE", 
                              shift_pattern = list(c(1,2), c(2,3), c(3,NA)))
  
  #Warning if polygon is pule WL ----
  DBI::dbExecute(conn, sprintf("
    UPDATE %s 
    SET treed_pure_WL = TRUE,
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || '**** Warning: Polygon is pure WL, but BCLCS_LV_4 IN (TB, TC, TM).' ELSE '**** Warning: Polygon is pure WL, but BCLCS_LV_4 IN (TB, TC, TM).' END, 
      row_updated = TRUE
    WHERE BCLCS_LV_4 IN ('TB', 'TC', 'TM') AND (BEUMC_S1 = '0' OR BEUMC_S1 IS NULL) ;",
    view_name))
  
  # Remove BEU for lakes ----
  # In cases for small lakes (LS), large lakes (LL), and open water (OW) where BCLCS_LV_5 AND LAND_CD_1 DO NOT equal LA,
  # remove BEU label for lakes -- will need to be manually assigned. Otherwise, if BCLCS_LV_5 OR LAND_CD_1 = LA, leave BEU as-is.
  # include "OT" with "LA". Sometimes lakes are assigned BCLCS = "OT"
    DBI::dbExecute(conn, sprintf("
      UPDATE %s 
      SET BEUMC_S1 = NULL
      WHERE BEUMC_S1 IN('LS', 'LL', 'OW') AND BCLCS_LV_5 NOT IN ('LA','OT') AND LAND_CD_1 NOT IN ('LA','OT');",
      view_name))
  
  #If the BEU was correctly assigned to an ecosystem which should not have a structural stage (as identified in the lookup table)
  # remove associated structure/stand information so it correctly populates suitability
  # make sure non-forested features are not indicated as forested
   
  DBI::dbExecute(conn, sprintf("
      UPDATE %s 
      SET STRCT_S1 = NULL, STAND_A1 = NULL, FORESTED_1 = 'N'
      WHERE BEUMC_S1 IN ('LS', 'LL', 'OW', 'MI','GL', 'TC','UR','RE','RI','ES','ST','UR') ;",
      view_name))

  DBI::dbExecute(conn, sprintf("
      UPDATE %s 
      SET STRCT_S2 = NULL, STAND_A2 = NULL, FORESTED_2 = 'N'
      WHERE BEUMC_S2 IN ('LS', 'LL', 'OW', 'MI','GL', 'TC','UR','RE','RI','ES','ST','UR') ;",
      view_name))
  
  DBI::dbExecute(conn, sprintf("
      UPDATE %s 
      SET STRCT_S3 = NULL, STAND_A3 = NULL, FORESTED_3 = 'N'
      WHERE BEUMC_S3 IN ('LS', 'LL', 'OW', 'MI','GL', 'TC','UR','RE','RI','ES','ST','UR') ;",
      view_name))
  
  rm_cols_from_tbl(conn, tbl_name = view_name, c("treed_WL_3", "treed_WL_2_from_3", "treed_WL_2_to_1", "treed_WL_1_from_2", "treed_pure_WL"))

}

check_allowed_bec_beu_view <- function(conn, vri_bem_view, beu_bec_view) {

  add_col_to_tbl(conn, tbl_name = vri_bem_view, col = "merge_key", type = "VARCHAR DEFAULT''")
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s 
  SET merge_key = IFNULL(BGC_ZONE, '') || IFNULL(BGC_SUBZON, '');",
  vri_bem_view))

  #Value 1
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      BEUMC_S1 = beu_bec.Change_to_BEU, 
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S1 || ' corrected to ' || beu_bec.Change_to_BEU || '  in decile 1' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S1 || ' corrected to ' || beu_bec.Change_to_BEU || '  in decile 1' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S1 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND len(Change_to_BEU) = 2",
    vri_bem_view, beu_bec_view))
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S1 || '  in decile 1 is invalid combination (mapper needs to assess)' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S1 || '  in decile 1 is invalid combination (mapper needs to assess)' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S1 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND len(Change_to_BEU) != 2",
    vri_bem_view, beu_bec_view))
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S1 || ' in decile 1 combination is not listed' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S1 || ' in decile 1 combination is not listed' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S1 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND beu_bec.Change_to_BEU IS NULL",
    vri_bem_view, beu_bec_view))
  
  #Value 2
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      BEUMC_S2 = beu_bec.Change_to_BEU, 
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S2 || ' corrected to ' || beu_bec.Change_to_BEU || '  in decile 2' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S2 || ' corrected to ' || beu_bec.Change_to_BEU || '  in decile 2' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S2 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND len(Change_to_BEU) = 2",
    vri_bem_view, beu_bec_view))
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S2 || '  in decile 2 is invalid combination (mapper needs to assess)' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S2 || '  in decile 2 is invalid combination (mapper needs to assess)' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S2 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND len(Change_to_BEU) != 2",
    vri_bem_view, beu_bec_view))
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S2 || ' in decile 2 combination is not listed' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S2 || ' in decile 2 combination is not listed' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S2 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND beu_bec.Change_to_BEU IS NULL",
    vri_bem_view, beu_bec_view))
  
  #Value 3
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      BEUMC_S3 = beu_bec.Change_to_BEU, 
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S3 || ' corrected to ' || beu_bec.Change_to_BEU || '  in decile 3' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S3 || ' corrected to ' || beu_bec.Change_to_BEU || '  in decile 3' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S3 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND len(Change_to_BEU) = 2",
    vri_bem_view, beu_bec_view))
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S3 || '  in decile 3 is invalid combination (mapper needs to assess)' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S3 || '  in decile 3 is invalid combination (mapper needs to assess)' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S3 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND len(Change_to_BEU) != 2",
    vri_bem_view, beu_bec_view))
  
  DBI::dbExecute(conn, sprintf("
  UPDATE %s as vri_bem
    SET
      lbl_edit = CASE WHEN lbl_edit != '' THEN lbl_edit || '; ' || vri_bem.merge_key || ' ' || vri_bem.BEUMC_S3 || ' in decile 3 combination is not listed' 
                      ELSE vri_bem.merge_key || ' ' || vri_bem.BEUMC_S3 || ' in decile 3 combination is not listed' END,
      row_updated = TRUE
    FROM %s beu_bec
    WHERE vri_bem.merge_key = beu_bec.BGC_Subzone 
      AND vri_bem.BEUMC_S3 = beu_bec.BEU
      AND beu_bec.Script_Rule = 'Error' 
      AND beu_bec.Change_to_BEU IS NULL",
    vri_bem_view, beu_bec_view))

  rm_cols_from_tbl(conn, tbl_name = vri_bem_view, cols = "merge_key")

}
