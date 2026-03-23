#' Calculate all HEM fields inside DuckDB (in-place)
#'
#' Replicates \code{calc_hem_fields()} entirely inside DuckDB, modifying
#' \code{vri_bem_tbl} in-place rather than returning a new object.
#'
#' @param conn A DuckDB connection with the spatial extension loaded.
#' @param vri_bem_tbl Character. Name of the VRI-BEM table to modify in-place.
#'   The table must already contain a \code{Shape} geometry column as well as
#'   \code{BCLCS_LV_1–5}, \code{SPEC_CD_1–2}, \code{SPEC_PCT_1–2},
#'   \code{PROJ_AGE_1}, \code{ELEV}, \code{MEAN_SLOPE}, \code{LBL_VEGCOV},
#'   and \code{CR_CLOSURE}.
#' @param fire_tbl Character. Name of the fire table (or view) in DuckDB.
#'   Default \code{"V_FIRE"}.
#' @param harvest_year_col Character. Column in \code{vri_bem_tbl} that holds
#'   the harvest year (default \code{"HARVEST_YEAR"}). If the column is absent,
#'   \code{Dynamic_L} is set to \code{0} and a warning is issued.
#' @param age_cl_sts_col Character. Column in \code{vri_bem_tbl} that holds the
#'   structural-stage age class (default \code{"VRI_AGE_CL_STS"}).
#' @param current_year Integer. Year used to compute \code{Dynamic_L} (default:
#'   current calendar year).
#' @return Invisibly returns \code{vri_bem_tbl}.
#' @details
#' The 23 HEM output columns are:
#' \code{fire_pct} (intermediate, DOUBLE), plus INTEGER flags:
#' \code{Static_Wetland_ST}, \code{Static_Wetland_SL}, \code{Static_Wetland_HE},
#' \code{Static_Wetland_Shrub_Riparian}, \code{Static_Upland}, \code{Static_Willow},
#' \code{Static_Sb_Bog}, \code{Static_Riparian}, \code{Waterbody},
#' \code{Elev_Threshold}, \code{Slope_Limit}, \code{W_Site_Conditions_Met},
#' \code{Age_Class8_9}, \code{Static_Persist_Decid}, \code{W_Shelter_1},
#' \code{Dynamic_WFD_4to10}, \code{Dynamic_WFD_11to30}, \code{Security_1},
#' \code{Static_Brush}, \code{Static_WFD_All}, \code{Dynamic_WFD_All},
#' \code{Dynamic_L}, \code{Dynamic_F}.
#'
#' All operations are idempotent.
#'
#' @note The original R function \code{calc_hem_fields()} contains a logic
#'   inversion in \code{fire_pct} that makes \code{Dynamic_F} always \code{0}.
#'   This DuckDB version implements the correct intent:
#'   \code{fire_pct = intersection_area / vri_area * 100}.
#'
#' @export
calc_hem_fields_duckdb <- function(conn,
                                   vri_bem_tbl,
                                   fire_tbl         = "V_FIRE",
                                   harvest_year_col = "HARVEST_START_YEAR_CALENDAR",
                                   age_cl_sts_col   = "VRI_AGE_CL_STS",
                                   current_year     = as.integer(format(Sys.Date(), "%Y"))) {

  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(vri_bem_tbl), nchar(vri_bem_tbl) > 0L)
  stopifnot(is.character(fire_tbl),    nchar(fire_tbl)    > 0L)
  current_year <- as.integer(current_year)

  v  <- vri_bem_tbl
  sq <- function(x) paste0("'", paste(x, collapse = "','"), "'")

  # ── 1. Add all output columns (no-op if already present) ─────────────────
  DBI::dbExecute(conn, sprintf(
    "ALTER TABLE %s ADD COLUMN IF NOT EXISTS fire_pct DOUBLE", v
  ))
  for (col in c(
    "Static_Wetland_ST", "Static_Wetland_SL", "Static_Wetland_HE",
    "Static_Wetland_Shrub_Riparian", "Static_Upland", "Static_Willow",
    "Static_Sb_Bog", "Static_Riparian", "Waterbody", "Elev_Threshold",
    "Slope_Limit", "W_Site_Conditions_Met", "Age_Class8_9",
    "Static_Persist_Decid", "W_Shelter_1", "Dynamic_WFD_4to10",
    "Dynamic_WFD_11to30", "Security_1", "Static_Brush",
    "Static_WFD_All", "Dynamic_WFD_All", "Dynamic_L", "Dynamic_F"
  )) {
    DBI::dbExecute(conn, sprintf(
      "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s INTEGER", v, col
    ))
  }

  # ── 2. fire_pct via spatial intersection ─────────────────────────────────
  tmp_fire <- "_tmp_hemf_fire"
  on.exit(
    try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_fire)),
        silent = TRUE),
    add = TRUE
  )
  DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_fire))
  DBI::dbExecute(conn, sprintf(
    "CREATE TEMP TABLE %s AS
     SELECT v.rowid AS _vid,
            SUM(ST_Area(ST_Intersection(v.Shape, f.Shape))) /
              NULLIF(ST_Area(v.Shape), 0) * 100.0 AS pct
     FROM %s v
     JOIN %s f ON ST_Intersects(v.Shape, f.Shape)
     GROUP BY v.rowid, ST_Area(v.Shape)",
    tmp_fire, v, fire_tbl
  ))
  # Default to 0, then override fire rows
  DBI::dbExecute(conn, sprintf("UPDATE %s SET fire_pct = 0.0", v))
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET fire_pct = t.pct
     FROM %s t
     WHERE %s.rowid = t._vid",
    v, tmp_fire, v
  ))

  # ── 3. Batch 1: fields with no cross-field dependencies ──────────────────
  riparian_vals <- c(
    "sl,ri",       "sl,ri,by",    "sl,ri,he",    "sl,ri,he,by",
    "sl,ri,hf",    "sl,ri,hg",
    "st,ri",       "st,ri,by",    "st,ri,he",    "st,ri,he,by",
    "st,ri,hf",    "st,ri,hg",    "st,ri,hg,by",
    "ri,by",       "ri,by,sl,he", "ri,he",       "ri,he,st",
    "ri,hg",       "ri,hg,sl",
    "ri,sl",       "ri,sl,by",    "ri,sl,by,he", "ri,sl,he",
    "ri,sl,he,by", "ri,sl,hf",    "ri,sl,hf,by",
    "ri,st",       "ri,st,by",    "ri,st,he",    "ri,st,he,by",
    "ri,st,hf",    "ri,st,hg",    "ri,st,hg,by"
  )

  decid_sp <- c("D","DG","DR","A","AC","AT","ACT","ACB","AX","E","EA","EX","EP")

  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET
       Static_Wetland_ST = CASE
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'
              AND BCLCS_LV_4='ST' AND BCLCS_LV_5 IN ('OP','ST','DE') THEN 1
         ELSE 0 END,

       Static_Wetland_SL = CASE
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'
              AND BCLCS_LV_4='SL' AND BCLCS_LV_5 IN ('OP','ST','DE') THEN 1
         ELSE 0 END,

       Static_Wetland_HE = CASE
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'
              AND BCLCS_LV_4='HE' AND BCLCS_LV_5 IN ('OP','ST','DE') THEN 1
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'
              AND BCLCS_LV_4='BY' AND BCLCS_LV_5='CL' THEN 1
         ELSE 0 END,

       Static_Upland = CASE
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='U'
              AND BCLCS_LV_4 IN ('ST','HE') AND BCLCS_LV_5 IN ('OP','DE') THEN 1
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='U'
              AND BCLCS_LV_4='SL' AND BCLCS_LV_5='OP' THEN 1
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='U'
              AND BCLCS_LV_4='BY' AND BCLCS_LV_5='CL' THEN 1
         WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='T' AND BCLCS_LV_3='U'
              AND BCLCS_LV_4='TC' AND BCLCS_LV_5 IN ('SP','OP') THEN 1
         ELSE 0 END,

       Static_Willow = CASE
         WHEN SPEC_CD_1 IN ('W','WS') OR SPEC_CD_2 IN ('W','WS') THEN 1
         ELSE 0 END,

       Static_Sb_Bog = CASE
         WHEN SPEC_CD_1='SB' AND TRY_CAST(SPEC_PCT_1 AS DOUBLE) > 89 THEN 1
         ELSE 0 END,

       Static_Riparian = CASE
         WHEN LBL_VEGCOV IN (%s) THEN 1
         ELSE 0 END,

       Waterbody = CASE
         WHEN BCLCS_LV_5 = 'LA' THEN 0
         ELSE 1 END,

       Elev_Threshold = CASE
         WHEN TRY_CAST(ELEV AS DOUBLE) < 1501 THEN 1
         ELSE 0 END,

       Slope_Limit = CASE
         WHEN TRY_CAST(MEAN_SLOPE AS DOUBLE) < 81 THEN 1
         ELSE 0 END,

       Age_Class8_9 = CASE
         WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 140 THEN 1
         ELSE 0 END,

       Static_Persist_Decid = CASE
         WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 60
              AND SPEC_CD_1 IN (%s)
              AND TRY_CAST(SPEC_PCT_1 AS DOUBLE) > 60 THEN 1
         WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 60
              AND SPEC_CD_2 IN (%s)
              AND TRY_CAST(SPEC_PCT_2 AS DOUBLE) > 60 THEN 1
         ELSE 0 END,

       Static_Brush = CASE
         WHEN LBL_VEGCOV IN ('NC','NCBR','NP','NPBR','NPBU','NSR') THEN 1
         ELSE 0 END",
    v,
    sq(riparian_vals),
    sq(decid_sp), sq(decid_sp)
  ))

  # ── Dynamic_L (guarded: column may not exist) ────────────────────────────
  existing_cols <- toupper(names(DBI::dbGetQuery(
    conn, sprintf("SELECT * FROM %s LIMIT 0", v)
  )))
  if (toupper(harvest_year_col) %in% existing_cols) {
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s SET Dynamic_L = CASE
         WHEN (%s - TRY_CAST(%s AS INTEGER)) <= 31 THEN 1
         ELSE 0 END",
      v, current_year, harvest_year_col
    ))
  } else {
    DBI::dbExecute(conn, sprintf("UPDATE %s SET Dynamic_L = 0", v))
    warning(sprintf(
      "calc_hem_fields_duckdb: column '%s' not found in %s; Dynamic_L set to 0.",
      harvest_year_col, v
    ))
  }

  # ── 4. Batch 2: fields depending on Batch 1 ──────────────────────────────
  shelter_sp <- c("BA","BB","BL","C","FDI","H","HM","HW","HX","HXM","P","PA",
                  "PL","PLC","PLI","S","SB","SE","SS","SW","SX","SXE","SXL",
                  "SXS","SXW","SXX")

  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET
       Static_Wetland_Shrub_Riparian = CASE
         WHEN Static_Wetland_ST=1 OR Static_Wetland_SL=1 OR Static_Wetland_HE=1 THEN 1
         ELSE 0 END,

       W_Site_Conditions_Met = CASE
         WHEN Waterbody=1 AND Elev_Threshold=1 AND Slope_Limit=1 THEN 1
         ELSE 0 END,

       W_Shelter_1 = CASE
         WHEN SPEC_CD_1 IN (%s)
              AND TRY_CAST(SPEC_PCT_1 AS DOUBLE)  > 40
              AND TRY_CAST(PROJ_AGE_1 AS DOUBLE)  > 120
              AND Elev_Threshold = 1
              AND Slope_Limit    = 1
              AND TRY_CAST(CR_CLOSURE AS DOUBLE) > 35
              AND Static_Persist_Decid = 0 THEN 1
         ELSE 0 END,

       Dynamic_WFD_4to10 = CASE
         WHEN TRY_CAST(%s AS DOUBLE) = 7
              AND Elev_Threshold=1 AND Slope_Limit=1 THEN 1
         ELSE 0 END,

       Dynamic_WFD_11to30 = CASE
         WHEN TRY_CAST(%s AS DOUBLE) = 20
              AND Elev_Threshold=1 AND Slope_Limit=1 THEN 1
         ELSE 0 END,

       Security_1 = CASE
         WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 40
              AND Elev_Threshold=1 AND Slope_Limit=1 THEN 1
         ELSE 0 END,

       Static_WFD_All = CASE
         WHEN (Static_Brush=1 OR Static_Persist_Decid=1 OR Static_Sb_Bog=1)
              AND Elev_Threshold=1 AND Slope_Limit=1 THEN 1
         ELSE 0 END",
    v,
    sq(shelter_sp),
    age_cl_sts_col, age_cl_sts_col
  ))

  # ── 5. Dynamic_WFD_All ────────────────────────────────────────────────────
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET Dynamic_WFD_All = CASE
       WHEN (Dynamic_WFD_4to10=1 OR Dynamic_WFD_11to30=1)
            AND Elev_Threshold=1 AND Slope_Limit=1 THEN 1
       ELSE 0 END",
    v
  ))

  # ── 6. Dynamic_F ─────────────────────────────────────────────────────────
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET Dynamic_F = CASE
       WHEN fire_pct > 50 AND Dynamic_WFD_All = 1 THEN 1
       ELSE 0 END",
    v
  ))

  invisible(vri_bem_tbl)
}
