
merge_vri_pem_duckdb <- function(conn) {
  logger::log_info("Merging VRI and PEM data...")
  
  tmp_intersect_raw <- "_merge_vri_pem_intersect_raw"
  tmp_intersect_san <- "_merge_vri_pem_intersect_san"
  tmp_pem_union <- "_merge_vri_pem_union"
  tmp_remainder <- "_merge_vri_pem_remainder"

  on.exit({
    for (tbl in c(tmp_intersect_raw, tmp_intersect_san, tmp_pem_union, tmp_remainder)) {
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tbl)), silent = TRUE)
    }
  }, add = TRUE)

  DBI::dbExecute(conn, sprintf(" 
    CREATE OR REPLACE TEMP TABLE %s AS (
      SELECT
        vri.rowid AS vri_rowid,
        vri.FEATURE_ID,
        vri.BCLCS_LV_1,
        vri.BCLCS_LV_2,
        vri.BCLCS_LV_3,
        vri.BCLCS_LV_4,
        vri.BCLCS_LV_5,
        vri.VRI_BEC_PHASE,
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
        ST_Intersection(ST_MakeValid(pem.Shape), ST_MakeValid(vri.Shape)) AS Shape,
        ST_Area(vri.Shape) AS VRI_Area
      FROM V_VRI vri
      JOIN PEM pem
        ON ST_Intersects(pem.Shape, vri.Shape)
    );", tmp_intersect_raw))

  sanitize_geometry_duckdb(conn, tmp_intersect_raw, tmp_intersect_san, tolerance_m2 = 0)

  DBI::dbExecute(conn, sprintf(" 
    CREATE OR REPLACE TEMP TABLE %s AS (
      SELECT
        vri_rowid,
        ST_Union_Agg(ST_MakeValid(Shape)) AS pem_union
      FROM %s
      GROUP BY vri_rowid
    );", tmp_pem_union, tmp_intersect_raw))

  DBI::dbExecute(conn, sprintf(" 
    CREATE OR REPLACE TEMP TABLE %s AS (
      SELECT *
      FROM (
        SELECT
          vri.FEATURE_ID,
          vri.BCLCS_LV_1,
          vri.BCLCS_LV_2,
          vri.BCLCS_LV_3,
          vri.BCLCS_LV_4,
          vri.BCLCS_LV_5,
          vri.VRI_BEC_PHASE,
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
          CAST(NULL AS VARCHAR) AS PEM_PRED_CLASS,
          ST_CollectionExtract(
            ST_MakeValid(
              CASE
                WHEN pem.pem_union IS NULL THEN vri.Shape
                ELSE ST_Difference(ST_MakeValid(vri.Shape), pem.pem_union)
              END
            ),
            3
          ) AS Shape,
          ST_Area(vri.Shape) AS VRI_Area
        FROM V_VRI vri
        LEFT JOIN %s pem
          ON vri.rowid = pem.vri_rowid
      ) remainder
      WHERE NOT ST_IsEmpty(Shape) AND ST_Area(Shape) > 0
    );", tmp_remainder, tmp_pem_union))

  DBI::dbExecute(conn, sprintf(" 
    CREATE OR REPLACE TABLE VRI_AND_PEM AS (
      SELECT
        *,
        round(ST_Area(Shape) / 10000, 2) AS Area_Ha,
        ST_Area(Shape) AS Shape_Area
      FROM (
        SELECT
          FEATURE_ID,
          BCLCS_LV_1,
          BCLCS_LV_2,
          BCLCS_LV_3,
          BCLCS_LV_4,
          BCLCS_LV_5,
          VRI_BEC_PHASE,
          VRI_BEC_SUBZON,
          VRI_BEC_VRT,
          VRI_BEC_ZONE,
          CR_CLOSURE,
          COV_PCT_1,
          HRVSTDT,
          INVENTORY_STANDARD_CD,
          LAND_CD_1,
          LBL_VEGCOV,
          PROJ_AGE_1,
          VRI_SURVEY_YEAR,
          SOIL_MOISTURE_REGIME_1,
          SOIL_NUTRIENT_REGIME,
          SPEC_CD_1,
          SPEC_CD_2,
          SPEC_CD_3,
          SPEC_CD_4,
          SPEC_CD_5,
          SPEC_CD_6,
          SPEC_PCT_1,
          SPEC_PCT_2,
          SPEC_PCT_3,
          SPEC_PCT_4,
          SPEC_PCT_5,
          SPEC_PCT_6,
          PEM_PRED_CLASS,
          Shape,
          VRI_Area
        FROM %s
        UNION ALL
        SELECT
          FEATURE_ID,
          BCLCS_LV_1,
          BCLCS_LV_2,
          BCLCS_LV_3,
          BCLCS_LV_4,
          BCLCS_LV_5,
          VRI_BEC_PHASE,
          VRI_BEC_SUBZON,
          VRI_BEC_VRT,
          VRI_BEC_ZONE,
          CR_CLOSURE,
          COV_PCT_1,
          HRVSTDT,
          INVENTORY_STANDARD_CD,
          LAND_CD_1,
          LBL_VEGCOV,
          PROJ_AGE_1,
          VRI_SURVEY_YEAR,
          SOIL_MOISTURE_REGIME_1,
          SOIL_NUTRIENT_REGIME,
          SPEC_CD_1,
          SPEC_CD_2,
          SPEC_CD_3,
          SPEC_CD_4,
          SPEC_CD_5,
          SPEC_CD_6,
          SPEC_PCT_1,
          SPEC_PCT_2,
          SPEC_PCT_3,
          SPEC_PCT_4,
          SPEC_PCT_5,
          SPEC_PCT_6,
          PEM_PRED_CLASS,
          Shape,
          VRI_Area
        FROM %s
      ) vri_pem
    );", tmp_intersect_san, tmp_remainder))

  any_null_pem <- duckdb_any_null(conn, "VRI_AND_PEM", "PEM_PRED_CLASS")

  if (isTRUE(any_null_pem)) {
    logger::log_warn("Some VRI geometry is not covered by PEM. VRI_AND_PEM includes those remainder polygons with PEM_PRED_CLASS = NULL.")
  }


  any_non_forested_pem <- duckdb_any_value(conn, "VRI_AND_PEM", "PEM_PRED_CLASS", "non-forested")
  #Check if any non-forested if so use BEM  
  ## Otherwise determine which steps of process only apply to BEM and skip them if possible.

  

  
}
