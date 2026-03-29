vri_bem_wetlands_corrections_view <- function(conn, vri_bem = "VRIBEM_CORRECTIONS", wetlands = "V_WETLANDS", beu_wetland_updates = "beu_wetland_updates") {
  
  # validate inputs ----
  validate_views_column_names(conn = conn, 
                              obj = vri_bem,
                              required_names = c("TEIS_ID", "SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1",
                                                 "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
                                                 "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", "TREE_C1", "SHRUB_C1", "DISTCLS_1",
                                                 "DISTSCLS_1", "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "SDEC_2", "BEUMC_S2",
                                                 "REALM_2", "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C",
                                                 "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2",
                                                 "SERAL_2", "TREE_C2", "SHRUB_C2", "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2",
                                                 "SESUBCL_2", "COND_2", "VIAB_2", "SDEC_3", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3",
                                                 "KIND_3", "SITE_S3", "SITEAM_S3A", "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3",
                                                 "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "TREE_C3", "SHRUB_C3",
                                                 "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3", "COND_3", "VIAB_3", "BGC_ZONE",
                                                 "MEAN_SLOPE", "BCLCS_LV_4"))
  
  # output table name
  tbl_name <- "VRIBEM_WETLANDS_CORRECTIONS"

  # Ensure the wetlands source is materialized with an RTREE index so the
  # spatial aggregation below can use the index.  If `filtered_views()` was
  # called with `materialize = TRUE` (default), V_WETLANDS is already an
  # indexed table and we use it as-is.  Otherwise we materialize it here.
  wl_src <- wetlands
  tmp_wetlands <- "_tmp_wc_wetlands"
  wl_is_view <- tryCatch(
    {
      DBI::dbGetQuery(conn, sprintf("SELECT * FROM duckdb_views() WHERE view_name = '%s'", wetlands))$view_name
    },
    error = function(e) character(0L)
  )
  if (length(wl_is_view) > 0L && wl_is_view == wetlands) {
    # Source is a view: materialise + index it once for this call
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TEMP TABLE %s AS SELECT * FROM %s",
      tmp_wetlands, wetlands
    ))
    DBI::dbExecute(conn, sprintf(
      "CREATE INDEX %s_rtree ON %s USING RTREE (Shape)",
      tmp_wetlands, tmp_wetlands
    ))
    wl_src <- tmp_wetlands
    on.exit(
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_wetlands)),
          silent = TRUE),
      add = TRUE
    )
  }

  DBI::dbExecute(conn, sprintf("
    CREATE OR REPLACE TEMP TABLE %s AS (
      WITH vri_bem AS (SELECT *, ROW_NUMBER() OVER (ORDER BY TEIS_ID, FEATURE_ID) AS row_id FROM %s)           
      SELECT vri_bem.*, 
        wl.Area_wetland,
        IFNULL(wl.Area_wetland, 0) / vri_bem.Shape_Area AS wl_pct
      FROM ( 
        SELECT vri_bem.row_id,
            sum(ST_Area(ST_Intersection(vri_bem.Shape, wetlands.Shape))) AS Area_wetland
        FROM vri_bem
        LEFT JOIN %s wetlands
          ON ST_Intersects(vri_bem.Shape, wetlands.Shape)
        GROUP BY vri_bem.row_id
      ) wl
      JOIN vri_bem
        ON wl.row_id = vri_bem.row_id
  );", tbl_name, vri_bem, wl_src))

  add_col_to_tbl(conn, tbl_name = tbl_name, col = "Lbl_edit_wl", type = "VARCHAR DEFAULT ''")
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "SMPL_TYPE", type = "VARCHAR DEFAULT ''")

  # Make note of which polygons have SITE_M3A == 'a' for later (before the 'a' is potentially moved to SITE_M1A or SITE_M2A)
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "init_SITE_M3a", type = "VARCHAR DEFAULT NULL")
  update_tbl(conn, tbl_name = tbl_name, set_expr = "init_SITE_M3a = SITE_M3A", where_expr = NULL)
  
  # compute % of wetland area for each BEM ----
  update_tbl(conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "Lbl_edit_wl = CASE WHEN wl_pct > 0 THEN wl_pct || '% of polygon occupied by wetland.' ELSE 'No Wetland.' END")
  
  update_tbl(conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "Lbl_edit_wl = Lbl_edit_wl || ' Current BEU: ' || SDEC_1 || ' ' || BEUMC_S1")
  
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "Lbl_edit_wl = Lbl_edit_wl || ', ' || SDEC_2 || ' ' || BEUMC_S2", 
             where_expr = "SDEC_2 > 0")
  
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "Lbl_edit_wl = Lbl_edit_wl || ', ' || SDEC_3 || ' ' || BEUMC_S3", 
             where_expr = "SDEC_3 > 0")
  
  # Remove all 'WL' mapcodes from Dec3 - these were added during the BEM process and no longer apply at
  # the VRI scale
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "wl_3_ind", type = "BOOLEAN DEFAULT FALSE")
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "SDEC_1 = SDEC_1 + SDEC_3, 
                         SDEC_3 = 0, 
                         BEUMC_S3 = NULL, 
                         wl_3_ind = TRUE", 
             where_expr = "SDEC_3 > 0 AND BEUMC_S3 = 'WL'")
  
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "wl_3_ind = TRUE", 
                              shift_pattern = list(c("3", NA))) 
  
  
  #ensure SDEC_2 and SDEC_3 have zeroes and not NA
  update_tbl(conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "SDEC_2 = IFNULL(SDEC_2, 0), SDEC_3 = IFNULL(SDEC_3, 0)")


  # If curr_beu_code is 4 digits: 1 digit for each decile, with the 4th digit:
  #     0 if none of the 3 components is WL
  #     1 if the 1st component is WL
  #     2 if the 2nd component is WL
  #     3 if the 3rd component is WL
  # If curr_beu_code is 5 digits: the first decile value must 10, the 2nd and 3rd are 0's,
  # and the 5th digit is:
  #     0 if the first and only component is not WL
  #     1 if the first and only component is WL
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "curr_wl_zone", type = "INTEGER DEFAULT 0")
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "curr_beu_code", type = "INTEGER DEFAULT 0")

  update_tbl(conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "curr_wl_zone = CASE 
                          WHEN BEUMC_S1 = 'WL' AND SDEC_1 > 0 THEN 1
                          WHEN BEUMC_S2 = 'WL' AND SDEC_2 > 0 THEN 2
                          WHEN BEUMC_S3 = 'WL' AND SDEC_3 > 0 THEN 3
                          ELSE 0 END")
  
  update_tbl(conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "curr_beu_code = SDEC_1 * 1000 + SDEC_2 * 100 + SDEC_3 * 10 + curr_wl_zone")
  
  update_tbl(conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "Lbl_edit_wl = Lbl_edit_wl || ' ('  || curr_beu_code || ')'")
    
  # Merge allowed BEU codes  -----
  # no WL 9 but it's normal
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "new_beu_code", type = "INTEGER")

  # Allowed BEU codes adjustments (line 364) -----
  DBI::dbExecute(conn, sprintf("
    UPDATE %s as vri_bem
    SET
      new_beu_code = CASE 
        WHEN vri_bem.wl_pct < 14 THEN Code_WL1
        WHEN vri_bem.wl_pct < 25 THEN Code_WL2
        WHEN vri_bem.wl_pct < 35 THEN Code_WL3
        WHEN vri_bem.wl_pct < 45 THEN Code_WL4
        WHEN vri_bem.wl_pct < 55 THEN Code_WL5
        WHEN vri_bem.wl_pct < 65 THEN Code_WL6
        WHEN vri_bem.wl_pct < 75 THEN Code_WL7
        WHEN vri_bem.wl_pct < 80 THEN Code_WL8
        WHEN vri_bem.wl_pct >= 80 THEN Code_WL10
        ELSE NULL 
      END
    FROM %s beu_upd
    WHERE vri_bem.curr_beu_code = beu_upd.Code_Orig
      AND NOT (vri_bem.SDEC_1 = 10 AND BEUMC_S1 IN ('BB','CB','CR','ER','PB','PR','RR','RS','SK','SR','TF','WG','WR','YB','YS','BG', 'ES', 'FE', 'ME', 'MR','SC','SH','ST','SW','WL','FS', 'IM', 'IN', 'LL', 'LS', 'RE','SP','OW'))
      AND vri_bem.wl_pct >= 8 ",
    tbl_name, beu_wetland_updates))
  
  # overwrite SDEC using new_beu_code
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "new_wl_zone", type = "INTEGER")
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "SDEC_1 = (new_beu_code - (new_beu_code // 1000)) / 1000, 
                         SDEC_2 = ((new_beu_code // 1000) - (new_beu_code // 100)) / 100, 
                         SDEC_3 = ((new_beu_code // 100) - (new_beu_code // 10)) / 10, 
                         new_wl_zone = new_beu_code // 10",
            where_expr = "new_beu_code != curr_beu_code ")
  
  
   # Reassign ecosystems ----
  #  - reassign eco components
  #  - make adjustment below when creating a WL component
  #  - create label specifying which rows where updated


  # When new code says that is 100% wetland in unit 1 , blank all variables for unit 1 , 2 and 3
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "new_wl_zone = 1 AND SDEC_1 = 10 AND curr_wl_zone = 0", 
                              shift_pattern = list(c("1", NA), c("2", NA), c("3", NA)))
  
  #When the new code says that there is no wetland at all, blank any wetland units
  
  # Old 1 / New 0 :  Remove WL from component 1, (2 & 3 move up toward 1)
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "curr_wl_zone = 1 AND new_wl_zone = 0", 
                              shift_pattern = list(c(1, 2), c(2, 3), c(3, NA)))
  # Old 2 / New 0 : Remove WL from component 2  (3 move up to 2)
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "curr_wl_zone = 2 AND new_wl_zone = 0", 
                              shift_pattern = list(c(2, 3), c(3, NA)))
  # Old 3 / New 0 : Remove WL from component 3
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "curr_wl_zone = 3 AND new_wl_zone = 0", 
                              shift_pattern = list(c(3, NA)))

  
  #If more specific wetland category exists, defer to that
  #If adding new WL component, also make REALM_# = "W", GROUP_# = "W" and KIND_# = "U"
  wl_feats <- "('BB','CB','CR','ER','PB','PR','RR','RS','SK','SR','TF','WG','WR','YB','YS','BG', 'ES', 'FE', 'ME', 'MR','SC','SH','ST','SW','WL', 'FS', 'IM', 'IN', 'LL', 'LS', 'RE','SP','OW')"
  
  #OLD 0/new 1 when BEUMC_S1 or BEUMC_S2 include wetland features
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = paste0("
              BEUMC_S1 = CASE WHEN BEUMC_S1 IN ", wl_feats, " THEN BEUMC_S1 
                              WHEN BEUMC_S2 IN ", wl_feats, " THEN BEUMC_S2 
                              ELSE 'WL' END,
              BEUMC_S2 = CASE WHEN BEUMC_S1 IN ", wl_feats, " THEN BEUMC_S2 
                              WHEN BEUMC_S2 IN ", wl_feats, " AND BEUMC_S3 IS NOT NULL THEN BEUMC_S3
                              WHEN BEUMC_S2 IN ", wl_feats, " AND BEUMC_S3 IS NULL THEN NULL
                              ELSE BEUMC_S2 END,
              SDEC_1 = CASE WHEN BEUMC_S2 IS NOT NULL AND BEUMC_S1 IS NOT NULL THEN SDEC_1
                            WHEN BEUMC_S2 IS NULL AND BEUMC_S3 IS NOT NULL THEN SDEC_1 + SDEC_2
                            WHEN BEUMC_S2 IS NULL and BEUMC_S3 IS NULL THEN 10 
                            ELSE SDEC_1 END,
              SDEC_2 = CASE WHEN SDEC_1 = 0 OR BEUMC_S2 IS NULL THEN 0
                            WHEN BEUMC_S2 IS NOT NULL AND BEUMC_S3 IS NULL THEN SDEC_2 + SDEC_3
                            ELSE SDEC_2 END,
              SDEC_3 = CASE WHEN SDEC_1 = 10 THEN 0 
                            WHEN SDEC_1 + SDEC_2 = 10 THEN 0
                            WHEN BEUMC_S3 IS NULL THEN 0
                            ELSE SDEC_3 END,
              BEUMC_S3 = CASE WHEN SDEC_3 = 0 THEN NULL ELSE BEUMC_S3 END,
              REALM_1 = 'W', 
              GROUP_1 = 'W',
              KIND_1 = 'U'"), 
             where_expr = sprintf("new_wl_zone = 1 AND curr_wl_zone = 0 AND (BEUMC_S1 IN %s OR BEUMC_S2 IN %s)", wl_feats, wl_feats))
  
  # Old 0 / New 1 when BEUMC_S1 or BEUMC_S2 do not include wetland features: add WL to component 1, (2 & 3 move down toward 3)
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "ind_0_to_1", type = "BOOLEAN DEFAULT FALSE")
  
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "ind_0_to_1 = TRUE", 
             where_expr = sprintf("new_wl_zone = 1 AND curr_wl_zone = 0 AND NOT (BEUMC_S1 IN %s OR BEUMC_S2 IN %s)", wl_feats, wl_feats))
  
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = " ind_0_to_1 = TRUE", 
                              shift_pattern = list(c(2,1), c(3,2)))
  

  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = paste0("
              BEUMC_S1 = 'WL',
              REALM_1 = 'W', 
              GROUP_1 = 'W', 
              KIND_1 = 'U'"), 
             where_expr = "ind_0_to_1 = TRUE")
  
  
  # Old 0 / New 2 : Add WL to component 2, (2 move down to 3)
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = paste0("
              BEUMC_S3 = CASE WHEN SDEC_3 = 0 THEN NULL 
                              WHEN BEUMC_S2 NOT IN ", wl_feats, " AND SDEC_3 != 0 THEN BEUMC_S2 
                              ELSE BEUMC_S3 END,
              BEUMC_S2 = CASE WHEN BEUMC_S2 NOT IN ", wl_feats, " THEN 'WL' 
                              ELSE BEUMC_S2 END,
              SDEC_2 = CASE WHEN BEUMC_S3 IS NULL THEN SDEC_2 + SDEC_3
                            ELSE SDEC_2 END,
              SDEC_3 = CASE WHEN SDEC_1 + SDEC_2 = 10 THEN 0
                            WHEN BEUMC_S3 IS NULL THEN 0
                            ELSE SDEC_3 END,
              REALM_1 = 'W', 
              GROUP_1 = 'W',
              KIND_1 = 'U'"), 
             where_expr = "new_wl_zone = 2 AND curr_wl_zone = 0")
  
  # Old 0 / New 3 : add WL to component 3
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "ind_0_to_3", type = "BOOLEAN DEFAULT FALSE")

  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "ind_0_to_3 = TRUE", 
             where_expr = "new_wl_zone = 3 AND curr_wl_zone = 0")
  
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = " ind_0_to_3 = TRUE", 
                              shift_pattern = list(c(3,NA)))
  
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = paste0("
              BEUMC_S3 = 'WL',
              REALM_3 = 'W', 
              GROUP_3 = 'W', 
              KIND_3 = 'U'"), 
             where_expr = "ind_0_to_3 = TRUE")
  
  # When there is already a wetland but in the wrong unit

  # invert eco_vars 1 & 2
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "
              BEUMC_S1 = BEUMC_S2,
              BEUMC_S2 = BEUMC_S1",
             where_expr = "(curr_wl_zone = 2 AND new_wl_zone = 1) OR (curr_wl_zone = 1 AND new_wl_zone = 2 AND BEUMC_S2 IS NOT NULL)")

  # invert eco_vars 1 & 3
  # note: should not be wl in 3 because it was removed earlier in this script as remnant of BEM creation process
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "(curr_wl_zone = 3 AND new_wl_zone = 1) OR (curr_wl_zone = 1 AND new_wl_zone = 3)", 
                              shift_pattern = list(c(1,3), c(3,1)))
  
  
  # invert eco_vars 2 & 3
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "BEUMC_S2 = BEUMC_S3, BEUMC_S3 = BEUMC_S2",
             where_expr = "(curr_wl_zone = 2 AND new_wl_zone = 3 AND BEUMC_S3 IS NOT NULL) OR (curr_wl_zone = 3 AND new_wl_zone = 2)")

  # update Label to reflect changes
  update_tbl(conn, tbl_name = tbl_name, where_expr = "curr_beu_code != new_beu_code",
             set_expr = "Lbl_edit_wl = Lbl_edit_wl || '; Updated BEU: ' || SDEC_1 || ' ' || BEUMC_S1 || ', ' || SDEC_2 || ' ' || BEUMC_S2 || ', ' || SDEC_3 || ' ' || BEUMC_S3 || ' ('  || new_beu_code || ')'")
  
  
  # If more specific wetland category exists with WL, combine into one category
  # If BEUMC_S1 has generic WL and BEUMC_S2 has wetland feature, combine under specific wetland feature
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "
              BEUMC_S1 = BEUMC_S2, 
              BEUMC_S2 = CASE WHEN BEUMC_S3 IS NOT NULL THEN BEUMC_S3 ELSE NULL END,
              BEUMC_S3 = NULL,
              SDEC_1 = SDEC_1 + SDEC_2,
              SDEC_2 = SDEC_3,
              SDEC_3 = 0", 
             where_expr = paste0("BEUMC_S1 = 'WL' AND BEUMC_S2 IN ", wl_feats))
  #If BEUMC_S2 has generic WL and BEUMC_S1 has specific wetland/riparian type, combine within BEUMC_S1
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "
              BEUMC_S2 = CASE WHEN BEUMC_S3 IS NOT NULL THEN BEUMC_S3 ELSE NULL END,
              BEUMC_S3 = NULL,
              SDEC_1 = SDEC_1 + SDEC_2,
              SDEC_2 = SDEC_3,
              SDEC_3 = 0", 
             where_expr = paste0("BEUMC_S2 = 'WL' AND BEUMC_S1 IN ", wl_feats))  
  #If BEUMC_S3 is generic WL and BEUMC_S1 or BEUMC_S2 more specific wetland type, combine within those (deferring to BEUMC_S1)
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = paste0("
              BEUMC_S3 = NULL,
              SDEC_1 = CASE WHEN BEUMC_S1 IN ", wl_feats, " THEN SDEC_1 + SDEC_3 ELSE SDEC_1 END,
              SDEC_2 = CASE WHEN BEUMC_S1 NOT IN ", wl_feats, " AND BEUMC_S2 IN ", wl_feats, " THEN SDEC_2 + SDEC_3 ELSE SDEC_2 END,
              SDEC_3 = 0"), 
             where_expr = paste0("((BEUMC_S3 = 'WL' AND (BEUMC_S1 IN ", wl_feats, ")) OR (BEUMC_S3 = 'WL' AND (BEUMC_S2 IN ", wl_feats, ")))"))
  
  #correct cases where wetland has been attributed to both BEUMC_S1 and _S2 or _S3
  update_tbl(conn, tbl_name = tbl_name, 
             set_expr = "
              BEUMC_S2 = CASE WHEN BEUMC_S1 = BEUMC_S2 AND BEUMC_S3 IS NULL THEN NULL 
                              WHEN BEUMC_S1 = BEUMC_S2 AND BEUMC_S3 IS NOT NULL THEN BEUMC_S3
                               ELSE BEUMC_S2 END,
              BEUMC_S3 = CASE WHEN BEUMC_S1 = BEUMC_S3 OR BEUMC_S2 = BEUMC_S3 OR SDEC_3 = 0 THEN NULL ELSE BEUMC_S3 END,
              SDEC_1 = CASE WHEN BEUMC_S1 = BEUMC_S2 THEN SDEC_1 + SDEC_2 
                            WHEN BEUMC_S1 = BEUMC_S3 THEN SDEC_1 + SDEC_3
                            ELSE SDEC_1 END,
              SDEC_2 = CASE WHEN BEUMC_S1 = BEUMC_S2 and BEUMC_S3 IS NULL THEN 0
                            WHEN BEUMC_S1 = BEUMC_S2 AND BEUMC_S3 IS NOT NULL THEN SDEC_3 
                            ELSE SDEC_2 END, 
              SDEC_3 = CASE WHEN (BEUMC_S1 = BEUMC_S2) OR (BEUMC_S1 = BEUMC_S3) OR (BEUMC_S2 = BEUMC_S3) OR BEUMC_S3 IS NULL THEN 0 ELSE SDEC_3 END",
              where_expr = "(BEUMC_S1 = BEUMC_S2) OR (BEUMC_S1 = BEUMC_S3) OR (BEUMC_S2 = BEUMC_S3)")
  
  
  # Riparian Mapcode adjustments (line 681) -----
  non_veg <- "('RI','WL','BB','UR','OW','LS','LL','RE','CL','GB','GL','GP','MI','RO','TA','TC','TR','UV','BG','CB','FE','MR','PB','RS','SH','SK','SW','WG','TF','YB','YS','AU','AV','ES','IM','ME','OV','RM','SC','SM','ST')"

  riparian_mapcode <- data.frame(bgc_zone = c("CDF", "BWBS", "SWB", "ESSF", "ICH", "CWH", "SBPS", "SBS"),
                                    beumc_s1 = c("CR", "PR", "PR", "ER", "RR", "SR", "WR", "WR"))
  
  
  DBI::dbWriteTable(conn, "riparian_mapcode", riparian_mapcode, overwrite = TRUE, temporary = TRUE)
  add_col_to_tbl(conn, tbl_name = tbl_name, col = "riparian_adj_ind", type = "BOOLEAN DEFAULT FALSE")

  DBI::dbExecute(conn, paste0("
     UPDATE ", tbl_name, " vri_bem
     SET 
       SDEC_1 = 10, 
       SDEC_2 = 0, 
       SDEC_3 = 0,  
       BEUMC_S1 = IFNULL(rp.BEUMC_S1, vri_bem.BEUMC_S1),
       riparian_adj_ind = TRUE
     FROM riparian_mapcode rp
     WHERE vri_bem.init_SITE_M3a = 'a' 
       AND vri_bem.MEAN_SLOPE < 10 
       AND vri_bem.BEUMC_S1 NOT IN ", non_veg, "
       AND vri_bem.BGC_ZONE = rp.bgc_zone"))
  
  shift_eco_variables_in_view(conn = conn, view_name = tbl_name, 
                              cond = "riparian_adj_ind = TRUE", 
                              shift_pattern = list(c("2", NA), c("3", NA)))
  
  update_tbl(conn, tbl_name = tbl_name, where_expr = "riparian_adj_ind = TRUE",
             set_expr = "Lbl_edit_wl = Lbl_edit_wl || '; Updated to 10 ' || BEUMC_S1 || ' because SITE_M3A = a, Slope < 10, and BGC_ZONE = ' || BGC_ZONE || '.'")
  
  update_tbl(conn, tbl_name = tbl_name,
             set_expr = "SITE_M3A = 'a'", 
             where_expr = "init_SITE_M3A = 'a'")
  
  update_tbl(conn = conn, tbl_name = tbl_name, where_expr = NULL,
             set_expr = "
             BEUMC_S2 = CASE WHEN SDEC_2 = 0 THEN NULL ELSE BEUMC_S2 END,
             BEUMC_S3 = CASE WHEN SDEC_3 = 0 THEN NULL ELSE BEUMC_S3 END,
             SDEC_1 = CASE WHEN BEUMC_S2 IS NULL THEN SDEC_1 + IFNULL(SDEC_2, 0)
                           WHEN BEUMC_S3 IS NULL THEN SDEC_1 + IFNULL(SDEC_3, 0)
                           WHEN BEUMC_S2 IS NULL AND BEUMC_S3 IS NULL THEN 10
                           ELSE SDEC_1 END,
             SDEC_2 = CASE WHEN BEUMC_S2 IS NULL THEN 0 ELSE SDEC_2 END,
             SDEC_3 = CASE WHEN BEUMC_S3 IS NULL THEN 0 ELSE SDEC_3 END"
            )
  
  # clean temp vars 
  rm_cols_from_tbl(conn, tbl_name = tbl_name, 
    cols = c("init_SITE_M3a", "wl_3_ind", "curr_beu_code", "new_beu_code", "curr_wl_zone", "new_wl_zone", "ind_0_to_1", "ind_0_to_3", "riparian_adj_ind")) 
}