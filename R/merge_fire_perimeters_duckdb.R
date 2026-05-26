#' Merge fire perimeter attributes into a DuckDB VRI-BEM table
#'
#' DuckDB-native counterpart of [merge_fire_perimeters()].
#' Adds two columns to `vri_bem_tbl` **in-place**:
#' \describe{
#'   \item{`percent_burned`}{Percent of the polygon area overlapping the most
#'     recent fire perimeter that intersects it. `0` when no overlap, or when
#'     `BEUMC_S1` is a water code (`OW`, `LL`, `LS`).}
#'   \item{`most_recent_fire`}{\code{FIRE_YEAR} of the most recent fire
#'     intersecting the polygon. `NULL` when no overlap or for water polygons.}
#' }
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param vri_bem_tbl Character. Name of the VRI-BEM table to modify in-place.
#'   Default `"VRIBEM_CCB"`.
#' @param fire_tbl Character. Name of the fire perimeters table/view in `conn`.
#'   Must contain a `Shape` geometry column and a `FIRE_YEAR` integer column.
#'   Default `"V_FIRE"`.
#' @return The name of `vri_bem_tbl` invisibly.
#' @details
#' Logic matches [merge_fire_perimeters()] exactly:
#' \enumerate{
#'   \item For each (VRI polygon × fire polygon) pair the intersection area is
#'         computed and grouped by `(polygon rowid, FIRE_YEAR)`.
#'   \item The row with the maximum `FIRE_YEAR` per polygon is kept (using
#'         the DuckDB `QUALIFY` clause).
#'   \item `percent_burned` is derived as
#'         `intersect_area / ST_Area(polygon) * 100`.
#'   \item Polygons with no fire overlap receive `percent_burned = 0` and
#'         `most_recent_fire = NULL` (the column default).
#'   \item Water polygons (`BEUMC_S1 IN ('OW','LL','LS')`) are reset to
#'         `percent_burned = 0`, `most_recent_fire = NULL`.
#' }
#' @import DBI
#' @import duckdb
#' @export
merge_fire_perimeters_duckdb <- function(conn,
                                         vri_bem_tbl = "VRIBEM_CCB",
                                         fire_tbl    = "V_FIRE") {

  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(vri_bem_tbl), length(vri_bem_tbl) == 1L)
  stopifnot(is.character(fire_tbl),    length(fire_tbl)    == 1L)

  t0 <- proc.time()[["elapsed"]]
  logger::log_info("merge_fire_perimeters_duckdb: starting (vri_bem_tbl={vri_bem_tbl})")

  # ------------------------------------------------------------------
  # Guard: skip gracefully when fire table is absent
  # ------------------------------------------------------------------
  if (!tbl_exists(conn, fire_tbl)) {
    logger::log_warn(
      "merge_fire_perimeters_duckdb: fire table '{fire_tbl}' not found. Skipping."
    )
    return(invisible(vri_bem_tbl))
  }

  n_fire <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s;", fire_tbl))$n
  if (n_fire == 0L) {
    logger::log_warn(
      "merge_fire_perimeters_duckdb: fire table '{fire_tbl}' is empty. Skipping."
    )
    return(invisible(vri_bem_tbl))
  }

  # ------------------------------------------------------------------
  # 1. Add output columns (idempotent)
  # ------------------------------------------------------------------
  add_col_to_tbl(conn, vri_bem_tbl, "percent_burned",   "DOUBLE  DEFAULT 0")
  add_col_to_tbl(conn, vri_bem_tbl, "most_recent_fire", "INTEGER DEFAULT NULL")

  # ------------------------------------------------------------------
  # 2. Build per-polygon fire summary:
  #    - group by (rowid, FIRE_YEAR), sum intersection areas
  #    - keep only the most recent FIRE_YEAR per rowid (QUALIFY)
  # ------------------------------------------------------------------
  tmp_fire_agg <- paste0("_mfp_", vri_bem_tbl, "_agg")

  on.exit(
    try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_fire_agg)),
        silent = TRUE),
    add = TRUE
  )

  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT
       vb_rowid,
       FIRE_YEAR AS most_recent_year,
       intersect_area,
       vb_area
     FROM (
       SELECT
         v.rowid  AS vb_rowid,
         ST_Area(v.Shape) AS vb_area,
         f.FIRE_YEAR,
         SUM(
           ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(v.Shape, f.Shape)), 3))
         ) AS intersect_area
       FROM %s v
       JOIN %s f ON ST_Intersects(v.Shape, f.Shape)
       GROUP BY v.rowid, ST_Area(v.Shape), f.FIRE_YEAR
     ) grouped
     QUALIFY FIRE_YEAR = MAX(FIRE_YEAR) OVER (PARTITION BY vb_rowid);",
    tmp_fire_agg, vri_bem_tbl, fire_tbl
  ))
  logger::log_info(
    "merge_fire_perimeters_duckdb: fire aggregation done ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 3. UPDATE main table with percent_burned and most_recent_fire
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s AS v
     SET
       percent_burned   = agg.intersect_area / agg.vb_area * 100,
       most_recent_fire = agg.most_recent_year
     FROM %s agg
     WHERE v.rowid = agg.vb_rowid;",
    vri_bem_tbl, tmp_fire_agg
  ))
  logger::log_info(
    "merge_fire_perimeters_duckdb: percent_burned / most_recent_fire updated ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 4. Reset water polygons (OW, LL, LS)
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s
     SET percent_burned = 0, most_recent_fire = NULL
     WHERE BEUMC_S1 IN ('OW', 'LL', 'LS');",
    vri_bem_tbl
  ))
  logger::log_info("merge_fire_perimeters_duckdb: water polygons reset")

  logger::log_info(
    "merge_fire_perimeters_duckdb: done (total {round(proc.time()[['elapsed']] - t0, 1)}s)"
  )

  invisible(vri_bem_tbl)
}
