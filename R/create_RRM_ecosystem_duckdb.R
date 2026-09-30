# Internal helper ─────────────────────────────────────────────────────────────
# Builds the UNION ALL body for the moose/bear variants.
# Each decile contributes one "actual" row (real area) plus 11 projections
# (area = 0) covering the full age-class range.
.rrm_union_sql <- function(v, salmon = FALSE) {

  salmon_sel <- if (salmon) "Salmon, " else ""

  # 11 (STRCT template, STAND template) pairs; %d → decile index
  
  proj_pairs <- rbindlist(list(
    list(STS="STS_1_Age_0_3",    STD="STAND_1_Age_0_15"),
    list(STS="STS_1_Age_4_10",   STD="STAND_1_Age_0_15"),
    list(STS="STS_1_Age_11_30",  STD="STAND_1_Age_0_15"),
    list(STS="STS_1_Age_11_30",  STD="STAND_1_Age_16_30"),
    list(STS="STS_1_Age_31_40",  STD="STAND_1_Age_31_50"),
    list(STS="STS_1_Age_41_60",  STD="STAND_1_Age_31_50"),
    list(STS="STS_1_Age_41_60",  STD="STAND_1_Age_51_80"),
    list(STS="STS_1_Age_61_80",  STD="STAND_1_Age_51_80"),
    list(STS="STS_1_Age_81_139", STD="STAND_1_Age_gt_80"),
    list(STS="STS_1_Age_140_249",STD="STAND_1_Age_gt_80"),
    list(STS="STS_1_Age_gt_249", STD="STAND_1_Age_gt_80")
  ))

  one_part <- function(i, strct_col, stand_col, area_expr, proj_val) {
    sprintf(
      "SELECT ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
              BEUMC_S1 AS BEUMC, SLOPE_MOD, SITE_M3A, %s SNOW_CODE,
              ABOVE_ELEV_THOLD, CROWN_ALL,
              STRCT_S1 AS STRCT, STAND_A1 AS STAND, FORESTED_1 AS FORESTED,
              CAST(ST_Area(Shape) AS DOUBLE) AS area_sum, FALSE AS projection
       FROM %s
       WHERE SDEC_1 > 0 AND BEUMC_S1 IS NOT NULL AND FORESTED_1 IS NOT NULL",
      salmon_sel, 
      v
    )
  }

  
  # Actual
    actual_query <- sprintf(
      "SELECT ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
              BEUMC_S1 AS BEUMC, SLOPE_MOD, SITE_M3A, %s SNOW_CODE,
              ABOVE_ELEV_THOLD, CROWN_ALL,
              STRCT_S1 AS STRCT, STAND_A1 AS STAND, FORESTED_1 AS FORESTED,
              CAST(ST_Area(Shape) AS DOUBLE) AS area_sum, FALSE AS projection
       FROM %s
       WHERE SDEC_1 > 0 AND BEUMC_S1 IS NOT NULL AND FORESTED_1 IS NOT NULL",
      salmon_sel, 
      v
    )
  
  proj_query <- paste0(
    "SELECT ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
              BEUMC_S1 AS BEUMC, SLOPE_MOD, SITE_M3A, ", salmon_sel, " SNOW_CODE,
              ABOVE_ELEV_THOLD, CROWN_ALL,
              ", proj_pairs$STS," AS STRCT, ", proj_pairs$STD," AS STAND, FORESTED_1 AS FORESTED,
              CAST(0.0 AS DOUBLE)AS area_sum, TRUE AS projection
       FROM ", v, "
       WHERE BEUMC_S1 IS NOT NULL AND FORESTED_1 IS NOT NULL"
    )
  paste(c(actual_query, proj_query), collapse = "\nUNION ALL\n")
  
}


# Internal helper ─────────────────────────────────────────────────────────────
# Executes the full moose / bear query and returns a data.frame.
.rrm_eco_query <- function(conn, v, salmon = FALSE) {
  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(v), nchar(v) > 0L)

  union_sql <- .rrm_union_sql(v, salmon = salmon)

  # Group-by key columns (ordered to match the R source)
  gc_fixed <- ifelse(salmon, 
                     "ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A, Salmon, SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL",
                     "ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A, SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL")

  gc_all_str      <- sprintf("%s, STRCT, STAND, FORESTED", gc_fixed)
  gc_no_stand_str <- sprintf("%s, STRCT, FORESTED", gc_fixed)

  sql <- sprintf(
    "SELECT %s, SUM(area_sum) / 10000.0 AS Hectares
     FROM (
       SELECT %s, 
        CASE WHEN projection AND STAND IN ('B','C','M')
                  AND STRCT NOT IN ('4','5','6','7')
             THEN NULL ELSE STAND END AS STAND,
        area_sum
       FROM ( %s ) AS raw
     ) AS corrected
     GROUP BY %s",
    gc_all_str, gc_no_stand_str, union_sql, gc_all_str
  )

  DBI::dbGetQuery(conn, sql)
}


# ── Exported functions ────────────────────────────────────────────────────────

#' Create RRM ecosystem summary for moose inside DuckDB
#'
#' Replicates \code{create_RRM_ecosystem()} entirely inside DuckDB, returning a
#' \code{data.frame} with one row per unique ecosystem combination.
#'
#' @param conn A DuckDB connection with the spatial extension loaded.
#' @param vri_bem_tbl Character. Name of the VRI-BEM table. Must contain a
#'   \code{Shape} geometry column, decile fields (\code{SDEC_1–3},
#'   \code{BEUMC_S1–3}, \code{FORESTED_1–3}, \code{CROWN_ALL_1–3},
#'   \code{STRCT_S1–3}, \code{STAND_A1–3}), projected age-class columns
#'   (\code{STS_i_Age_*}, \code{STAND_i_Age_*} for \code{i} in 1–3), and
#'   grouping columns (\code{ECO_SEC}, \code{BGC_ZONE}, \code{BGC_SUBZON},
#'   \code{BGC_VRT}, \code{BGC_PHASE}, \code{SLOPE_MOD}, \code{SITE_M3A},
#'   \code{SNOW_CODE}, \code{ABOVE_ELEV_THOLD}).
#' @return A \code{data.frame} with columns \code{ECO_SEC}, \code{BGC_ZONE},
#'   \code{BGC_SUBZON}, \code{BGC_VRT}, \code{BGC_PHASE}, \code{BEUMC},
#'   \code{SLOPE_MOD}, \code{SITE_M3A}, \code{SNOW_CODE},
#'   \code{ABOVE_ELEV_THOLD}, \code{CROWN_ALL}, \code{STRCT}, \code{STAND},
#'   \code{FORESTED}, \code{Hectares}.
#' @details
#' For each decile 1–3, one "actual" row (real polygon area weighted by
#' \code{SDEC_i}) and 11 projected age-class rows (area = 0) are generated.
#' A \code{STAND} correction is applied before aggregation: projected rows
#' where \code{STAND} is \code{"B"}, \code{"C"}, or \code{"M"} and \code{STRCT}
#' is not in \code{("4","5","6","7")} have \code{STAND} set to \code{NULL}.
#' @export
create_RRM_ecosystem_duckdb <- function(conn, vri_bem_tbl) {
  .rrm_eco_query(conn, vri_bem_tbl, salmon = FALSE)
}


#' Create RRM ecosystem summary for moose inside DuckDB
#'
#' Thin wrapper around \code{create_RRM_ecosystem_duckdb()}.
#'
#' @inheritParams create_RRM_ecosystem_duckdb
#' @return See \code{\link{create_RRM_ecosystem_duckdb}}.
#' @export
create_RRM_ecosystem_moose_duckdb <- function(conn, vri_bem_tbl) {
  create_RRM_ecosystem_duckdb(conn = conn, vri_bem_tbl = vri_bem_tbl)
}


#' Create RRM ecosystem summary for bear inside DuckDB
#'
#' Replicates \code{create_RRM_ecosystem_bear()} entirely inside DuckDB.
#' Identical to \code{create_RRM_ecosystem_duckdb()} but includes \code{Salmon}
#' as an additional group-by key.
#'
#' @inheritParams create_RRM_ecosystem_duckdb
#' @return A \code{data.frame} like \code{\link{create_RRM_ecosystem_duckdb}}
#'   with an additional \code{Salmon} column.
#' @export
create_RRM_ecosystem_bear_duckdb <- function(conn, vri_bem_tbl) {
  .rrm_eco_query(conn, vri_bem_tbl, salmon = TRUE)
}


#' Create RRM ecosystem summary for huckleberry inside DuckDB
#'
#' Replicates \code{create_RRM_ecosystem_huckleberry()} entirely inside DuckDB.
#' Only actual (non-projected) ecosystems are included, and the group-by keys
#' differ from the moose/bear variants.
#'
#' @inheritParams create_RRM_ecosystem_duckdb
#'   The table must additionally contain \code{HUCK_ASP} and
#'   \code{HUCK_ELEV_Thold} columns.
#' @return A \code{data.frame} with columns \code{ECO_SEC}, \code{BGC_ZONE},
#'   \code{BGC_SUBZON}, \code{BGC_VRT}, \code{BGC_PHASE}, \code{HUCK_ASP},
#'   \code{HUCK_ELEV_Thold}, \code{CROWN_ALL}, \code{STRCT}, \code{STAND},
#'   \code{FORESTED}, \code{Hectares}.
#' @export
create_RRM_ecosystem_huckleberry_duckdb <- function(conn, vri_bem_tbl) {
  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(vri_bem_tbl), nchar(vri_bem_tbl) > 0L)

  gc_str <- "ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, HUCK_ASP, HUCK_ELEV_Thold, CROWN_ALL, STRCT, STAND, FORESTED"

  sql <- sprintf(
    "SELECT %s, SUM(area_sum) / 10000.0 AS Hectares
     FROM ( SELECT ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
              HUCK_ASP, HUCK_ELEV_Thold,
              CROWN_ALL AS CROWN_ALL,
              STRCT_S1 AS STRCT, STAND_A1 AS STAND,
              FORESTED_1 AS FORESTED,
              ST_Area(Shape) AS area_sum
           FROM %s
           WHERE SDEC_1 > 0 AND BEUMC_S1 IS NOT NULL AND FORESTED_1 IS NOT NULL ) AS raw
     GROUP BY %s",
    gc_str, vri_bem_tbl, gc_str
  )

  DBI::dbGetQuery(conn, sql)
}
