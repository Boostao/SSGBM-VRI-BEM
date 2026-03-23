#' Compute slope, aspect, elevation and their derivatives inside DuckDB
#'
#' This is the DuckDB-optimised counterpart of [merge_elevation_raster_on_sf()].
#' Raster cells are exported to temporary **Parquet files on disk** via
#' [arrow::write_parquet()].  DuckDB then reads those files natively with
#' `read_parquet()` and performs the spatial join (`ST_Contains(polygon,
#' ST_Point(x, y))`), aggregation, and `SLOPE_MOD` / `ABOVE_ELEV_THOLD`
#' derivation entirely in SQL — R never holds the raster as an in-memory vector
#' object.
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param vri_bem_tbl Character. Name of the table / view in `conn` that holds
#'   the VRI-BEM polygons.  The table **must** contain a `Shape` geometry
#'   column and the fields `BEUMC_S1`, `BEUMC_S2`, `BEUMC_S3` and `BGC_ZONE`.
#' @param elev_raster `SpatRaster` representing the elevation model.
#' @param elevation_threshold Numeric. Elevation (m) above which
#'   `ABOVE_ELEV_THOLD` is set to `"Y"`. Default `1500`.
#' @param terrain_raster Optional `SpatRaster` with layers `slope` and
#'   `aspect` expressed in **radians**.  When `NULL` (default) the terrain
#'   raster is derived from `elev_raster` via [terra::terrain()].
#' @param result_tbl Character. Name given to the (temp) table written to
#'   `conn` that will hold the final result including the new columns.
#'   Defaults to `"VRIBEM_ELEVATION"`.
#' @return The name of the result table (`result_tbl`) invisibly.  The caller
#'   can query `conn` directly, e.g. `duckdb::dbReadTable(conn, result_tbl)`.
#' @details
#' **Memory model**: each raster layer is converted to a flat `(x, y, value)`
#' data frame via [terra::as.data.frame()] and written to a temp Parquet file.
#' DuckDB streams those files directly from disk via `read_parquet()`, so the
#' peak R memory is one data frame at a time rather than three full
#' `SpatVector` polygon objects.
#'
#' **SLOPE_MOD** derivation rules (identical to [merge_elevation_raster_on_sf()]):
#'  * `j` - gentle slope: 10-25 % (non CWH/MH) or 10-35 % (CWH/MH)
#'  * `k` - cool aspect (285-359 or 0-134 deg) AND moderate slope
#'  * `q` - very steep cool aspect (slope > 100 %)
#'  * `w` - warm aspect (135-284 deg) AND moderate slope
#'  * `z` - very steep warm aspect (slope > 100 %)
#'
#' Polygons whose `BEUMC_S1`, `BEUMC_S2`, or `BEUMC_S3` belongs to the
#' *no-slope-mod* list (water / non-terrestrial codes) receive `NULL`.
#'
#' @importFrom terra terrain `add<-`
#' @importFrom arrow write_parquet
#' @import DBI
#' @import duckdb
#' @export
merge_elevation_duckdb <- function(conn,
                                   vri_bem_tbl,
                                   elev_raster,
                                   elevation_threshold = 1500,
                                   terrain_raster = NULL,
                                   result_tbl = "VRIBEM_ELEVATION") {

  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(vri_bem_tbl), length(vri_bem_tbl) == 1L)

  on.exit({
    # Clean up temp parquet files even on error
    if (exists("f_dem",    inherits = FALSE)) try(unlink(f_dem),    silent = TRUE)
    if (exists("f_slope",  inherits = FALSE)) try(unlink(f_slope),  silent = TRUE)
    if (exists("f_aspect", inherits = FALSE)) try(unlink(f_aspect), silent = TRUE)
  }, add = TRUE)

  # ------------------------------------------------------------------
  # 1. Build terrain raster (slope + aspect in radians) ---------------
  # ------------------------------------------------------------------
  if (is.null(terrain_raster)) {
    terrain_raster <- terra::terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")
  }

  # ------------------------------------------------------------------
  # 2. Write each layer to a temp Parquet file ------------------------
  #
  # terra::as.data.frame(xy = TRUE) produces a flat (x, y, value) table —
  # far smaller than SpatVector polygons.  Writing to Parquet lets DuckDB
  # stream it from disk with read_parquet() so the file never fully lives
  # in DuckDB's memory either.
  # ------------------------------------------------------------------
  f_dem   <- tempfile(fileext = ".parquet")
  f_slope <- tempfile(fileext = ".parquet")
  f_aspect <- tempfile(fileext = ".parquet")

  arrow::write_parquet(
    terra::as.data.frame(elev_raster,                   xy = TRUE, na.rm = FALSE),
    f_dem
  )
  arrow::write_parquet(
    terra::as.data.frame(terrain_raster[["slope"]],     xy = TRUE, na.rm = FALSE),
    f_slope
  )
  arrow::write_parquet(
    terra::as.data.frame(terrain_raster[["aspect"]],    xy = TRUE, na.rm = FALSE),
    f_aspect
  )

  # ------------------------------------------------------------------
  # 3. SQL: spatial join (ST_Point) + aggregate -----------------------
  #
  # DuckDB reads the parquet files directly from disk — no dbWriteTable.
  # ST_Contains(polygon, ST_Point(x, y)) replaces a polygon-polygon
  # intersection; it is cheaper and requires no cell geometry at all.
  #
  # Constant: radians → percent slope (preserves original formula exactly).
  # Circular mean aspect only over cells where slope > 0.
  # ------------------------------------------------------------------
  constant1_sql <- 57.29578 / 90 * 100
  tmp_stats_tbl <- "_elev_stats_tmp"

  sql_agg <- sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
    SELECT
      v.rowid                                          AS row_id,
      AVG(d.%s)                                                         AS ELEV,
      AVG(s.slope) * %.10f                                              AS MEAN_SLOPE,
      CASE
        WHEN SUM(CASE WHEN s.slope > 0 AND s.slope IS NOT NULL THEN 1 ELSE 0 END) = 0
          THEN NULL
        ELSE
          (degrees(
            atan2(
              SUM(CASE WHEN s.slope > 0 AND s.slope IS NOT NULL
                       THEN sin(a.aspect) ELSE 0.0 END)
              / SUM(CASE WHEN s.slope > 0 AND s.slope IS NOT NULL THEN 1 ELSE 0 END),
              SUM(CASE WHEN s.slope > 0 AND s.slope IS NOT NULL
                       THEN cos(a.aspect) ELSE 0.0 END)
              / SUM(CASE WHEN s.slope > 0 AND s.slope IS NOT NULL THEN 1 ELSE 0 END)
            )
          ) + 360) %% 360
      END                                                               AS MEAN_ASP
    FROM %s v
    JOIN read_parquet('%s') d ON ST_Contains(v.Shape, ST_Point(d.x, d.y))
    JOIN read_parquet('%s') s ON s.x = d.x AND s.y = d.y
    JOIN read_parquet('%s') a ON a.x = d.x AND a.y = d.y
    GROUP BY v.rowid",
    tmp_stats_tbl,
    names(elev_raster)[1L],
    constant1_sql,
    vri_bem_tbl,
    gsub("\\\\", "/", f_dem),
    gsub("\\\\", "/", f_slope),
    gsub("\\\\", "/", f_aspect)
  )

  DBI::dbExecute(conn, sql_agg)

  # Warn about polygons with no elevation
  no_elev_rows <- DBI::dbGetQuery(
    conn,
    sprintf("SELECT row_id FROM %s WHERE ELEV IS NULL", tmp_stats_tbl)
  )
  if (nrow(no_elev_rows) > 0L) {
    warning("could not calculate elevation for the following row ids: ",
            paste(no_elev_rows$row_id, collapse = ", "))
  }

  # ------------------------------------------------------------------
  # 4. SQL: join stats + derive ABOVE_ELEV_THOLD and SLOPE_MOD -------
  # ------------------------------------------------------------------
  no_slope_mod_MC <- paste0(
    "'",
    paste(
      c("LL", "LS", "LA", "La", "OW", "Pd", "PD", "RE", "RI", "Ri", "Wa", "WE", "Wm", "Ww", "Ws", "WL"),
      collapse = "','"
    ),
    "'"
  )

  sql_result <- sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
    WITH joined AS (
      SELECT
        v.*,
        s.ELEV,
        s.MEAN_SLOPE,
        s.MEAN_ASP
      FROM %s v
      LEFT JOIN %s s ON v.rowid = s.row_id
    )
    SELECT
      * EXCLUDE (ELEV, MEAN_SLOPE, MEAN_ASP),
      ELEV,
      MEAN_SLOPE,
      MEAN_ASP,
      CASE
        WHEN ELEV > %s THEN 'Y'
        ELSE 'N'
      END AS ABOVE_ELEV_THOLD,
      CASE
        -- Exclude water / non-terrestrial map codes
        WHEN BEUMC_S1 IN (%s) OR BEUMC_S2 IN (%s) OR BEUMC_S3 IN (%s) THEN NULL
        WHEN MEAN_SLOPE IS NULL OR MEAN_ASP IS NULL                     THEN NULL

        -- CWH / MH zones, cool aspect
        WHEN BGC_ZONE IN ('CWH','MH')
             AND (MEAN_ASP >= 285 OR MEAN_ASP <= 134)
             AND (MEAN_SLOPE >= 10  AND MEAN_SLOPE < 35)              THEN 'j'
        WHEN BGC_ZONE IN ('CWH','MH')
             AND (MEAN_ASP >= 285 OR MEAN_ASP <= 134)
             AND (MEAN_SLOPE >= 35  AND MEAN_SLOPE <= 100)            THEN 'k'
        WHEN BGC_ZONE IN ('CWH','MH')
             AND (MEAN_ASP >= 285 OR MEAN_ASP <= 134)
             AND MEAN_SLOPE > 100                                     THEN 'q'

        -- CWH / MH zones, warm aspect
        WHEN BGC_ZONE IN ('CWH','MH')
             AND (MEAN_ASP >= 135 AND MEAN_ASP <= 284)
             AND (MEAN_SLOPE >= 10  AND MEAN_SLOPE < 35)              THEN 'j'
        WHEN BGC_ZONE IN ('CWH','MH')
             AND (MEAN_ASP >= 135 AND MEAN_ASP <= 284)
             AND (MEAN_SLOPE >= 35  AND MEAN_SLOPE <= 100)            THEN 'w'
        WHEN BGC_ZONE IN ('CWH','MH')
             AND (MEAN_ASP >= 135 AND MEAN_ASP <= 284)
             AND MEAN_SLOPE > 100                                     THEN 'z'

        -- Other zones, cool aspect
        WHEN (MEAN_ASP >= 285 OR MEAN_ASP <= 134)
             AND (MEAN_SLOPE >= 10  AND MEAN_SLOPE < 25)              THEN 'j'
        WHEN (MEAN_ASP >= 285 OR MEAN_ASP <= 134)
             AND (MEAN_SLOPE >= 25  AND MEAN_SLOPE <= 100)            THEN 'k'
        WHEN (MEAN_ASP >= 285 OR MEAN_ASP <= 134)
             AND MEAN_SLOPE > 100                                     THEN 'q'

        -- Other zones, warm aspect
        WHEN (MEAN_ASP >= 135 AND MEAN_ASP <= 284)
             AND (MEAN_SLOPE >= 10  AND MEAN_SLOPE < 25)              THEN 'j'
        WHEN (MEAN_ASP >= 135 AND MEAN_ASP <= 284)
             AND (MEAN_SLOPE >= 25  AND MEAN_SLOPE <= 100)            THEN 'w'
        WHEN (MEAN_ASP >= 135 AND MEAN_ASP <= 284)
             AND MEAN_SLOPE > 100                                     THEN 'z'

        ELSE NULL
      END AS SLOPE_MOD
    FROM joined",
    result_tbl,
    vri_bem_tbl,
    tmp_stats_tbl,
    elevation_threshold,
    no_slope_mod_MC, no_slope_mod_MC, no_slope_mod_MC
  )

  DBI::dbExecute(conn, sql_result)

  invisible(result_tbl)
}
