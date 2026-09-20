
merge_vri_pem_duckdb <- function(conn) {
  logger::log_info("Merging VRI and PEM data...")
  
  DBI::dbExecute(conn, "
    CREATE OR REPLACE TABLE VRI_AND_PEM AS (
      WITH VRI_PEM AS (
        SELECT
          vri.FEATURE_ID, 
          vri.BCLCS_LV_1, 
          vri.BCLCS_LV_2,
          vri.BCLCS_LV_3, 
          vri.BCLCS_LV_4,
          vri.BCLCS_LV_5, 
          vri.VRI_BEC_PHAS, 
          vri.VRI_BEC_SUBZON, 
          vri.VRI_BEC_VRT, 
          vri.VRI_BEC_ZONE, 
          vri.CR_CLOSURE, 
          vri.COV_PCT_1, 
          vri.HRVSTDT, 
          vri.INVENTORY_STANDARD_CD, 
          vri.LAND_CD_1, 
          vri.LBL_VEGCOV, 
          vri.PROJ_AGE_1, 
          vri.VRI_SURVEY_YEAR, 
          vri.SOIL_MOISTURE_REGIME_1, 
          vri.SOIL_NUTRIENT_REGIME, 
          vri.SPEC_CD_1, 
          vri.SPEC_CD_2, 
          vri.SPEC_CD_3, 
          vri.SPEC_CD_4,
          vri.SPEC_CD_5,
          vri.SPEC_CD_6, 
          vri.SPEC_PCT_1, 
          vri.SPEC_PCT_2, 
          vri.SPEC_PCT_3, 
          vri.SPEC_PCT_4, 
          vri.SPEC_PCT_5, 
          vri.SPEC_PCT_6, 
          pem.PEM_PRED_CLASS, 
          ST_Intersection(pem.Shape, vri.Shape) AS Shape,
          ST_Area(VRI.Shape) AS VRI_Area 
        FROM V_VRI vri
        LEFT JOIN PEM pem
          ON ST_Intersects(pem.Shape, vri.Shape)
      )
      SELECT *, 
        round(ST_Area(Shape)/10000, 2) AS Area_Ha, 
        ST_Area(Shape) AS Shape_Area
      FROM VRI_PEM
    ;")

  any_null_pem <- duckdb_any_null(conn, "VRI_AND_PEM", "PEM_PRED_CLASS")

  if (isTRUE(any_null_pem)) {
    logger::log_warn("Some VRI features do not have a corresponding PEM_PRED_CLASS. Check the VRI_AND_PEM table for details.")
  }


  any_non_forested_pem <- duckdb_any_value(conn, "VRI_AND_PEM", "PEM_PRED_CLASS", "non-forested")
  #Check if any non-forested if so use BEM  
  ## Otherwise determine which steps of process only apply to BEM and skip them if possible.

  

  
}
