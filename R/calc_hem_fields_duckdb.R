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
  logger::log_info("Adding HEM output columns to %s", v)
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
  # Ensure the fire source is a materialized table with an RTREE index so
  # DuckDB can prune candidates before computing ST_Intersection.
  # If filtered_views() already produced an indexed table (materialize = TRUE),
  # we use it directly; otherwise we materialise here.
  tmp_fire_geom <- "_tmp_hemf_fire_geom"
  tmp_fire      <- "_tmp_hemf_fire"
  logger::log_info("Preparing fire source for spatial join")
  fire_is_view <- tryCatch(
    {
      DBI::dbGetQuery(conn, sprintf("SELECT * FROM duckdb_views() WHERE view_name = '%s'", fire_tbl))$view_name
    },
    error = function(e) character(0L)
  )
  fire_src <- fire_tbl
  if (length(fire_is_view) > 0L && fire_is_view == fire_tbl) {
    on.exit(
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_fire_geom)),
          silent = TRUE),
      add = TRUE
    )
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_fire_geom))
    DBI::dbExecute(conn, sprintf(
      "CREATE TEMP TABLE %s AS SELECT * FROM %s",
      tmp_fire_geom, fire_tbl
    ))
    DBI::dbExecute(conn, sprintf(
      "CREATE INDEX IF NOT EXISTS %s_rtree ON %s USING RTREE (Shape)",
      tmp_fire_geom, tmp_fire_geom
    ))
    fire_src <- tmp_fire_geom
  }
  on.exit(
    try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_fire)),
        silent = TRUE),
    add = TRUE
  )
  # Use a CTE to compute ST_Area(polygon) once per VRI row rather than once
  # per (polygon x fire) join row; avoids redundant geometry-area calls when
  # a VRI polygon overlaps multiple fire records.
  logger::log_info("Computing fire_pct: spatial intersection with %s", fire_tbl)
  DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_fire))
  DBI::dbExecute(conn, sprintf(
    "CREATE TEMP TABLE %s AS
     WITH vri_area AS (
       SELECT rowid AS _vid, Shape, ST_Area(Shape) AS _vri_area FROM %s
     )
     SELECT v._vid,
            SUM(ST_Area(ST_Intersection(v.Shape, f.Shape))) /
              NULLIF(v._vri_area, 0) * 100.0 AS pct
     FROM vri_area v
     JOIN %s f ON ST_Intersects(v.Shape, f.Shape)
     GROUP BY v._vid, v._vri_area",
    tmp_fire, v, fire_src
  ))
  logger::log_info("Updating fire_pct in %s", v)
  DBI::dbExecute(conn, sprintf("UPDATE %s SET fire_pct = 0.0", v))
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET fire_pct = t.pct
     FROM %s t
     WHERE %s.rowid = t._vid",
    v, tmp_fire, v
  ))

  # ── 3. All HEM fields in a single combined UPDATE ────────────────────────
  # By inlining Batch-1 expressions into dependent SET clauses (Batch 2+3),
  # all 23 fields are computed in one table scan instead of four sequential
  # UPDATE passes.
  logger::log_info("Computing all HEM fields in a single combined pass on %s", v)

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

  decid_sp   <- c("D","DG","DR","A","AC","AT","ACT","ACB","AX","E","EA","EX","EP")

  shelter_sp <- c("BA","BB","BL","C","FDI","H","HM","HW","HX","HXM","P","PA",
                  "PL","PLC","PLI","S","SB","SE","SS","SW","SX","SXE","SXL",
                  "SXS","SXW","SXX")

  # Build Dynamic_L SQL expression (guarded: harvest_year_col may not exist)
  existing_cols <- toupper(DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", v)
  )$name)
  if (toupper(harvest_year_col) %in% existing_cols) {
    dynamic_l_expr <- sprintf(
      "CASE WHEN (%s - TRY_CAST(%s AS INTEGER)) <= 31 THEN 1 ELSE 0 END",
      current_year, harvest_year_col
    )
  } else {
    warning(sprintf(
      "calc_hem_fields_duckdb: column '%s' not found in %s; Dynamic_L set to 0.",
      harvest_year_col, v
    ))
    dynamic_l_expr <- "0"
  }

  # Inline sub-expressions shared across multiple SET clauses.
  # In a single UPDATE all SET expressions use OLD column values, so dependent
  # fields (Batch 2+3) must re-express their conditions using raw VRI columns.
  elev_ok     <- "TRY_CAST(ELEV AS DOUBLE) < 1501"
  slope_ok    <- "TRY_CAST(MEAN_SLOPE AS DOUBLE) < 81"
  age7_expr   <- paste0("TRY_CAST(", age_cl_sts_col, " AS DOUBLE) = 7")
  age20_expr  <- paste0("TRY_CAST(", age_cl_sts_col, " AS DOUBLE) = 20")
  persist_cond <- paste0(
    "((TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 60",
    " AND SPEC_CD_1 IN (", sq(decid_sp), ")",
    " AND TRY_CAST(SPEC_PCT_1 AS DOUBLE) > 60)",
    " OR (TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 60",
    " AND SPEC_CD_2 IN (", sq(decid_sp), ")",
    " AND TRY_CAST(SPEC_PCT_2 AS DOUBLE) > 60))"
  )

  sql_combined <- paste0(
    "UPDATE ", v, " SET\n",
    # ── Batch 1: fields with no cross-field dependencies ─────────────────
    "  Static_Wetland_ST = CASE\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'\n",
    "         AND BCLCS_LV_4='ST' AND BCLCS_LV_5 IN ('OP','ST','DE') THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_Wetland_SL = CASE\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'\n",
    "         AND BCLCS_LV_4='SL' AND BCLCS_LV_5 IN ('OP','ST','DE') THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_Wetland_HE = CASE\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'\n",
    "         AND BCLCS_LV_4='HE' AND BCLCS_LV_5 IN ('OP','ST','DE') THEN 1\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W'\n",
    "         AND BCLCS_LV_4='BY' AND BCLCS_LV_5='CL' THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_Upland = CASE\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='U'\n",
    "         AND BCLCS_LV_4 IN ('ST','HE') AND BCLCS_LV_5 IN ('OP','DE') THEN 1\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='U'\n",
    "         AND BCLCS_LV_4='SL' AND BCLCS_LV_5='OP' THEN 1\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='U'\n",
    "         AND BCLCS_LV_4='BY' AND BCLCS_LV_5='CL' THEN 1\n",
    "    WHEN BCLCS_LV_1='V' AND BCLCS_LV_2='T' AND BCLCS_LV_3='U'\n",
    "         AND BCLCS_LV_4='TC' AND BCLCS_LV_5 IN ('SP','OP') THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_Willow = CASE\n",
    "    WHEN SPEC_CD_1 IN ('W','WS') OR SPEC_CD_2 IN ('W','WS') THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_Sb_Bog = CASE\n",
    "    WHEN SPEC_CD_1='SB' AND TRY_CAST(SPEC_PCT_1 AS DOUBLE) > 89 THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_Riparian = CASE WHEN LBL_VEGCOV IN (", sq(riparian_vals), ") THEN 1 ELSE 0 END,\n",
    "  Waterbody       = CASE WHEN BCLCS_LV_5 = 'LA' THEN 0 ELSE 1 END,\n",
    "  Elev_Threshold  = CASE WHEN ", elev_ok,  " THEN 1 ELSE 0 END,\n",
    "  Slope_Limit     = CASE WHEN ", slope_ok, " THEN 1 ELSE 0 END,\n",
    "  Age_Class8_9    = CASE WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 140 THEN 1 ELSE 0 END,\n",
    "  Static_Persist_Decid = CASE WHEN ", persist_cond, " THEN 1 ELSE 0 END,\n",
    "  Static_Brush = CASE WHEN LBL_VEGCOV IN ('NC','NCBR','NP','NPBR','NPBU','NSR') THEN 1 ELSE 0 END,\n",
    "  Dynamic_L = ", dynamic_l_expr, ",\n",
    # ── Batch 2+3: dependent fields with inline-expanded conditions ───────
    "  Static_Wetland_Shrub_Riparian = CASE\n",
    "    WHEN (BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W' AND BCLCS_LV_4='ST' AND BCLCS_LV_5 IN ('OP','ST','DE'))\n",
    "      OR (BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W' AND BCLCS_LV_4='SL' AND BCLCS_LV_5 IN ('OP','ST','DE'))\n",
    "      OR (BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W' AND BCLCS_LV_4='HE' AND BCLCS_LV_5 IN ('OP','ST','DE'))\n",
    "      OR (BCLCS_LV_1='V' AND BCLCS_LV_2='N' AND BCLCS_LV_3='W' AND BCLCS_LV_4='BY' AND BCLCS_LV_5='CL')\n",
    "    THEN 1 ELSE 0 END,\n",
    "  W_Site_Conditions_Met = CASE\n",
    "    WHEN BCLCS_LV_5 != 'LA' AND ", elev_ok, " AND ", slope_ok, " THEN 1\n",
    "    ELSE 0 END,\n",
    "  W_Shelter_1 = CASE\n",
    "    WHEN SPEC_CD_1 IN (", sq(shelter_sp), ")\n",
    "         AND TRY_CAST(SPEC_PCT_1 AS DOUBLE) > 40\n",
    "         AND TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 120\n",
    "         AND ", elev_ok, "\n",
    "         AND ", slope_ok, "\n",
    "         AND TRY_CAST(CR_CLOSURE AS DOUBLE) > 35\n",
    "         AND NOT ", persist_cond, " THEN 1\n",
    "    ELSE 0 END,\n",
    "  Dynamic_WFD_4to10  = CASE WHEN ", age7_expr,  " AND ", elev_ok, " AND ", slope_ok, " THEN 1 ELSE 0 END,\n",
    "  Dynamic_WFD_11to30 = CASE WHEN ", age20_expr, " AND ", elev_ok, " AND ", slope_ok, " THEN 1 ELSE 0 END,\n",
    "  Security_1 = CASE\n",
    "    WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) > 40 AND ", elev_ok, " AND ", slope_ok, " THEN 1\n",
    "    ELSE 0 END,\n",
    "  Static_WFD_All = CASE\n",
    "    WHEN (LBL_VEGCOV IN ('NC','NCBR','NP','NPBR','NPBU','NSR')\n",
    "         OR ", persist_cond, "\n",
    "         OR (SPEC_CD_1='SB' AND TRY_CAST(SPEC_PCT_1 AS DOUBLE) > 89))\n",
    "         AND ", elev_ok, " AND ", slope_ok, " THEN 1\n",
    "    ELSE 0 END,\n",
    "  Dynamic_WFD_All = CASE\n",
    "    WHEN (", age7_expr, " OR ", age20_expr, ")\n",
    "         AND ", elev_ok, " AND ", slope_ok, " THEN 1 ELSE 0 END,\n",
    "  Dynamic_F = CASE\n",
    "    WHEN fire_pct > 50\n",
    "         AND (", age7_expr, " OR ", age20_expr, ")\n",
    "         AND ", elev_ok, " AND ", slope_ok, " THEN 1\n",
    "    ELSE 0 END"
  )

  DBI::dbExecute(conn, sql_combined)
  logger::log_info("HEM fields computation complete on %s", v)

  invisible(vri_bem_tbl)
}
