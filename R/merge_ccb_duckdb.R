#' Sanitize geometry in a DuckDB table or view
#'
#' Applies the DuckDB-native equivalent of [sanitize_geometry()]:
#' \enumerate{
#'   \item Applies `ST_MakeValid` to the `Shape` column.
#'   \item Strips non-polygon sub-geometries via `ST_CollectionExtract(..., 3)`.
#'   \item Drops rows whose resulting polygon area is ≤ `tolerance_m2`.
#' }
#'
#' The result is written as a new temporary table named `result_tbl`.  Any
#' existing table with that name is replaced.  All non-geometry columns are
#' carried over unchanged via `* EXCLUDE (Shape)`.
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param input_tbl Character. Name of the source table or view in `conn`.
#'   Must contain a `Shape` geometry column.
#' @param result_tbl Character. Name of the temporary table to create.
#' @param tolerance_m2 Numeric. Minimum polygon area (m²) to retain.
#'   Rows whose sanitized area is ≤ this value are dropped.  Default `10`.
#' @return The name of the result table (`result_tbl`) invisibly.
#' @import DBI
#' @import duckdb
#' @export
sanitize_geometry_duckdb <- function(conn,
                                      input_tbl,
                                      result_tbl,
                                      tolerance_m2 = 10) {
  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(input_tbl),  length(input_tbl)  == 1L)
  stopifnot(is.character(result_tbl), length(result_tbl) == 1L)
  stopifnot(is.numeric(tolerance_m2), length(tolerance_m2) == 1L)

  tol <- as.numeric(tolerance_m2)

  DBI::dbExecute(conn, paste0(
    "CREATE OR REPLACE TEMP TABLE ", result_tbl, " AS (
      SELECT * 
      FROM (SELECT * EXCLUDE (Shape),
              ST_CollectionExtract(ST_MakeValid(Shape), 3) AS Shape
            FROM ", input_tbl, ") tmp
      WHERE NOT ST_IsEmpty(Shape) AND ST_Area(Shape) > ", tol, "
    );"
  ))
    
  invisible(result_tbl)
}

#' Spatial union-split of two polygon tables inside DuckDB
#'
#' This is the DuckDB-native counterpart of [merge_geometry()].
#' All geometry work (intersection, difference, area filtering) happens
#' entirely inside the database via DuckDB Spatial — no R sf objects are
#' created.
#'
#' The operation is equivalent to a spatial union:
#' * `x` polygons that overlap `y` polygons are split into
#'   (a) the intersection piece (carries all `y` attributes), and
#'   (b) the remaining difference piece (`y` attributes are `NULL`).
#' * `x` polygons with no `y` overlap are passed through unchanged
#'   (`y` attributes are `NULL`).
#' * Any resulting polygon whose area is ≤ `tolerance_m2` is discarded.
#' * Geometry collections are reduced to plain `POLYGON` geometry via
#'   [sanitize_geometry_duckdb()].
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param x_tbl Character. Name of the base table / view in `conn` (e.g.
#'   the VRI-BEM polygons).  Must contain a `Shape` geometry column.
#' @param y_tbl Character. Name of the overlay table / view in `conn` (e.g.
#'   CCB, burn perimeters).  Must contain a `Shape` geometry column.
#' @param tolerance_m2 Numeric. Minimum polygon area (m²) to retain.
#'   Fragments smaller than or equal to this value are dropped.
#'   Default `10`.
#' @param result_tbl Character. Name of the (temp) table written to `conn`
#'   containing the final result.
#' @return The name of the result table (`result_tbl`) invisibly.
#' @import DBI
#' @import duckdb
#' @export
merge_geometry_duckdb <- function(conn,
                                   x_tbl,
                                   y_tbl,
                                   tolerance_m2 = 10,
                                   result_tbl) {

  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(x_tbl), length(x_tbl) == 1L)
  stopifnot(is.character(y_tbl), length(y_tbl) == 1L)
  stopifnot(is.numeric(tolerance_m2), length(tolerance_m2) == 1L)
  stopifnot(is.character(result_tbl), length(result_tbl) == 1L)

  # ------------------------------------------------------------------
  # 1. Discover column names so we can build UNION-compatible SELECT
  #    lists where y columns are NULLed out for non-overlap rows.
  # ------------------------------------------------------------------
  x_cols <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", x_tbl)
  )$name

  y_cols <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", y_tbl)
  )$name

  x_non_geom <- x_cols[x_cols != "Shape"]
  y_non_geom <- y_cols[y_cols != "Shape"]
  # y columns not already present in x (avoids duplicate column names)
  new_y_cols <- y_non_geom[!y_non_geom %in% x_non_geom]

  qi <- function(x) paste0('"', x, '"')
  indented <- function(exprs, indent = "      ") {
    paste(exprs, collapse = paste0(",\n", indent))
  }

  # ------------------------------------------------------------------
  # 2. Build SELECT column expressions for each intermediate step
  # ------------------------------------------------------------------

  # intersect_raw: x_rowid (for union aggregation) + x cols + y cols + raw intersection
  # x_rowid is stored here so step 4c can aggregate the y_union directly from this
  # table instead of re-running the expensive ST_Intersects spatial join.
  intersect_raw_select <- indented(c(
    "v.rowid AS x_rowid",
    sprintf("v.%s", qi(x_non_geom)),
    if (length(new_y_cols) > 0L) sprintf("y.%s", qi(new_y_cols)),
    "ST_Intersection(ST_MakeValid(v.Shape), ST_MakeValid(y.Shape)) AS Shape"
  ))

  # diff_raw / no_overlap: x cols + NULL y cols
  null_y_exprs <- c(
    sprintf("v.%s", qi(x_non_geom)),
    if (length(new_y_cols) > 0L) sprintf("NULL AS %s", qi(new_y_cols))
  )
 
  # diff_raw: + raw (not yet sanitized) difference geometry
  diff_raw_select <- indented(c(
    null_y_exprs,
    "ST_Difference(ST_MakeValid(v.Shape), u.y_union) AS Shape"
  ))

  # no_overlap: + original x geometry (already valid, no sanitize needed)
  no_overlap_select <- indented(c(null_y_exprs, "v.Shape"))

  # Explicit column list for the final UNION ALL (ensures column alignment)
  all_cols         <- c(x_non_geom, new_y_cols, "Shape")
  final_select_sql <- paste(qi(all_cols), collapse = ",\n    ")

  # ------------------------------------------------------------------
  # 3. Intermediate temp table names (prefixed from result_tbl to avoid
  #    collisions when the function is called multiple times)
  # ------------------------------------------------------------------
  pfx               <- paste0("_mgd_", result_tbl, "_")
  tmp_intersect_raw <- paste0(pfx, "iraw")
  tmp_intersect_san <- paste0(pfx, "isan")
  tmp_unioned       <- paste0(pfx, "union")
  tmp_diff_san      <- paste0(pfx, "dsan")
  tmp_no_overlap    <- paste0(pfx, "noovlp")

  on.exit({
    for (t in c(tmp_intersect_raw, tmp_intersect_san, tmp_unioned,
                tmp_diff_san, tmp_no_overlap)) {
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", t)), silent = TRUE)
    }
  }, add = TRUE)

  # ------------------------------------------------------------------
  # 4. Execute step by step
  # ------------------------------------------------------------------

  # 4a. One raw row per (x, y) overlapping pair
  logger::log_info("Computing raw intersections of overlapping x and y polygons")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
      SELECT %s
      FROM %s v
      JOIN %s y 
        ON ST_Intersects(v.Shape, y.Shape)",
    tmp_intersect_raw, intersect_raw_select, x_tbl, y_tbl
  ))

  # 4b. Sanitize: collection-extract to polygons only + area filter
   logger::log_info("Sanitizing intersected geometries: collection-extract to polygons only + area filter")
  sanitize_geometry_duckdb(conn, tmp_intersect_raw, tmp_intersect_san, tolerance_m2)

  # 4c. Per-x union of intersection geometries (feeds the difference step)
  # Reuses tmp_intersect_raw instead of re-running ST_Intersects — saves one
  # full spatial index scan.  Union of intersections ≡ union of original y
  # shapes for the purpose of ST_Difference(x, union) because the only
  # relevant part of any y polygon is the part that overlaps x.
  logger::log_info("Computing per-x union from intersection geometries (no spatial re-scan)")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
      SELECT x_rowid, ST_Union_Agg(ST_MakeValid(Shape)) AS y_union
      FROM %s
      GROUP BY x_rowid",
    tmp_unioned, tmp_intersect_raw
  ))

  # 4d+4e. x remainder (x minus overlapping y union) + inline sanitize
  # Combining the difference computation and the collection-extract/area filter
  # into one query avoids writing and reading back an intermediate table.
  logger::log_info("Computing x remainder (x poly minus its overlapping y union) + sanitizing")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
      SELECT * FROM (
        SELECT * EXCLUDE (Shape),
               ST_CollectionExtract(ST_MakeValid(Shape), 3) AS Shape
        FROM (
          SELECT %s
          FROM %s v
          JOIN %s u ON v.rowid = u.x_rowid
        ) raw_diff
      ) san_diff
      WHERE NOT ST_IsEmpty(Shape) AND ST_Area(Shape) > %s",
    tmp_diff_san, diff_raw_select, x_tbl, tmp_unioned, as.numeric(tolerance_m2)
  ))

  # 4f. x polygons with no y overlap at all (pass through unchanged)
  # LEFT JOIN anti-join is cheaper than NOT IN (...) for large tables because
  # it avoids building a full hash set and handles NULL rowids correctly.
  logger::log_info("Finding non-overlapping x polygons")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
      SELECT %s
      FROM %s v
      LEFT JOIN %s u ON v.rowid = u.x_rowid
      WHERE u.x_rowid IS NULL",
    tmp_no_overlap, no_overlap_select, x_tbl, tmp_unioned
  ))

  # 4g. UNION ALL three parts into the final result table
  logger::log_info("UNION ALL three parts into the final result table")

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
      SELECT %s FROM %s
      UNION ALL
      SELECT %s FROM %s
      UNION ALL
      SELECT %s FROM %s",
    result_tbl,
    final_select_sql, tmp_intersect_san,
    final_select_sql, tmp_diff_san,
    final_select_sql, tmp_no_overlap
  ))

  logger::log_info("Created table {result_tbl} with geometry union-split results.")

  invisible(result_tbl)
}

#' Spatial union-split of VRI-BEM polygons by CCB polygons inside DuckDB
#'
#' Thin wrapper around [merge_geometry_duckdb()] with CCB-specific defaults.
#' See that function for full details of the union-split algorithm.
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param vri_bem_tbl Character. Name of the VRI-BEM table / view in `conn`.
#'   Defaults to `"VRIBEM_ELEVATION"`.
#' @param ccb_tbl Character. Name of the CCB table / view in `conn`.
#'   Defaults to `"V_CCB"` (the AOI-filtered view created by
#'   [filtered_views()]).
#' @param tolerance_m2 Numeric. Minimum polygon area (m²) to retain.
#'   Default `10`.
#' @param result_tbl Character. Name of the result temp table.
#'   Defaults to `"VRIBEM_CCB"`.
#' @return The name of the result table (`result_tbl`) invisibly.
#' @import DBI
#' @import duckdb
#' @export
merge_ccb_duckdb <- function(conn,
                              vri_bem_tbl  = "VRIBEM_ELEVATION",
                              ccb_tbl      = "V_CCB",
                              tolerance_m2 = 10,
                              result_tbl   = "VRIBEM_CCB") {
  merge_geometry_duckdb(
    conn         = conn,
    x_tbl        = vri_bem_tbl,
    y_tbl        = ccb_tbl,
    tolerance_m2 = tolerance_m2,
    result_tbl   = result_tbl
  )

  DBI::dbExecute(conn, sprintf("CREATE INDEX IF NOT EXISTS idx_%s ON %s USING RTREE (Shape);", tolower(result_tbl), result_tbl))
}
