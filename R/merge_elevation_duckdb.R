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
#' @importFrom sf st_as_sfc st_crs<- st_sf
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

  t0_total <- proc.time()[["elapsed"]]
  logger::log_info("merge_elevation_duckdb: starting (vri_bem_tbl={vri_bem_tbl}, threshold={elevation_threshold}m)")

  on.exit({
    # Clean up temp parquet file and temp table even on error
    if (exists("f_terrain", inherits = FALSE)) try(unlink(f_terrain), silent = TRUE)
    try(DBI::dbExecute(conn, "DROP TABLE IF EXISTS _elev_vri_tmp;"), silent = TRUE)
  }, add = TRUE)

  # ------------------------------------------------------------------
  # 1. Build terrain raster (slope + aspect in radians) ---------------
  # ------------------------------------------------------------------
  t0 <- proc.time()[["elapsed"]]
  if (is.null(terrain_raster)) {
    terrain_raster <- terra::terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")
  }
  logger::log_info("merge_elevation_duckdb: terrain computed ({round(proc.time()[['elapsed']] - t0, 1)}s)")

  # ------------------------------------------------------------------
  # 2. Crop rasters to the VRI-BEM bounding box + 1-cell buffer ------
  #
  # For large AOIs the full raster can be enormous.  Cropping before
  # converting to Parquet means only the cells that can possibly fall
  # inside a VRI-BEM polygon are materialised on disk, dramatically
  # reducing both Parquet file size and the number of rows DuckDB
  # must evaluate in the spatial join.
  # ------------------------------------------------------------------
  t0 <- proc.time()[["elapsed"]]
  bbox_row <- DBI::dbGetQuery(conn, sprintf(
    "SELECT MIN(ST_XMin(Shape)) AS xmin, MAX(ST_XMax(Shape)) AS xmax,
            MIN(ST_YMin(Shape)) AS ymin, MAX(ST_YMax(Shape)) AS ymax
     FROM %s",
    vri_bem_tbl
  ))
  raster_res <- terra::res(elev_raster)[1L]
  crop_ext   <- terra::ext(
    bbox_row$xmin - raster_res,
    bbox_row$xmax + raster_res,
    bbox_row$ymin - raster_res,
    bbox_row$ymax + raster_res
  )
  elev_raster    <- terra::crop(elev_raster,    crop_ext)
  terrain_raster <- terra::crop(terrain_raster, crop_ext)
  logger::log_info("merge_elevation_duckdb: raster cropped to bbox ({round(proc.time()[['elapsed']] - t0, 1)}s)")

  # ------------------------------------------------------------------
  # 3. Stack layers + materialize polygons + rasterize row_id --------
  #
  # Key insight: terra::rasterize() (C++ scanline fill) assigns each
  # raster cell a polygon row_id orders of magnitude faster than
  # DuckDB's per-cell ST_Intersects, even with an RTREE.  By burning
  # row_id into the Parquet, the DuckDB aggregation becomes a plain
  # GROUP BY integer — no geometry math at all.
  # ------------------------------------------------------------------
  t0 <- proc.time()[["elapsed"]]
  elev_col <- names(elev_raster)[1L]          # save before add() renames
  terra::add(elev_raster) <- terrain_raster   # stack: elev, slope, aspect

  # Materialize the view into a temp table so rowid is stable and we
  # only evaluate the view chain once.
  n_poly <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) AS n FROM %s;", vri_bem_tbl))$n
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS _elev_vri_tmp;")
  DBI::dbExecute(conn, sprintf(
    "CREATE TEMP TABLE _elev_vri_tmp AS
     SELECT rowid AS row_id, Shape, BEUMC_S1, BEUMC_S2, BEUMC_S3, BGC_ZONE
     FROM %s;",
    vri_bem_tbl
  ))
  logger::log_info("merge_elevation_duckdb: {n_poly} polygons materialized ({round(proc.time()[['elapsed']] - t0, 1)}s)")

  # Export polygon geometries as WKB → SpatVector, rasterize to assign
  # row_id to each cell.  terra uses a C++ scanline fill: ~10-20s for
  # 150K polygons vs ~100s for DuckDB ST_Intersects per cell.
  t0 <- proc.time()[["elapsed"]]
  poly_wkb <- DBI::dbGetQuery(conn,
    "SELECT row_id, ST_AsWKB(Shape) AS wkb FROM _elev_vri_tmp ORDER BY row_id;")
  polys_sfc <- sf::st_as_sfc(poly_wkb$wkb)
  sf::st_crs(polys_sfc) <- terra::crs(elev_raster)
  polys_sv  <- terra::vect(sf::st_sf(row_id = poly_wkb$row_id, geometry = polys_sfc))
  rm(poly_wkb, polys_sfc)
  id_raster <- terra::rasterize(polys_sv, elev_raster[[1L]], field = "row_id")
  names(id_raster) <- "row_id"
  rm(polys_sv)
  logger::log_info("merge_elevation_duckdb: {n_poly} polygons rasterized ({round(proc.time()[['elapsed']] - t0, 1)}s)")

  # Stack row_id as 4th layer.  na.rm = TRUE drops cells not in any
  # polygon (row_id = NA) as well as raster no-data, so the Parquet
  # contains only cells inside polygons — smaller file, faster hash join.
  # xy coordinates are no longer needed (no spatial join in SQL).
  t0 <- proc.time()[["elapsed"]]
  terra::add(elev_raster) <- id_raster        # 4th layer: row_id
  rm(id_raster, terrain_raster)
  f_terrain <- tempfile(fileext = ".parquet")
  raster_df <- terra::as.data.frame(elev_raster, xy = FALSE, na.rm = TRUE)
  arrow::write_parquet(raster_df, f_terrain)
  logger::log_info("merge_elevation_duckdb: parquet written ({nrow(raster_df)} cells inside polygons, {round(proc.time()[['elapsed']] - t0, 1)}s)")
  rm(raster_df)

  # ------------------------------------------------------------------
  # 4. Pure hash-join aggregation in DuckDB (no geometry) ------------
  #
  # The stats CTE groups the Parquet by integer row_id — no ST_*
  # functions, no RTREE, no geometry columns needed.  The joined CTE
  # and final SELECT are unchanged from before.
  # ------------------------------------------------------------------
  t0 <- proc.time()[["elapsed"]]
  logger::log_info("merge_elevation_duckdb: running hash-join aggregation (no geometry)...")
  constant1_sql <- 57.29578 / 90 * 100
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
    WITH stats AS (
      SELECT
        CAST(row_id AS INTEGER)                                           AS row_id,
        AVG(%s)                                                           AS ELEV,
        AVG(slope) * %.10f                                                AS MEAN_SLOPE,
        CASE
          WHEN SUM(CASE WHEN slope > 0 AND slope IS NOT NULL THEN 1 ELSE 0 END) = 0
            THEN NULL
          ELSE
            (degrees(
              atan2(
                SUM(CASE WHEN slope > 0 AND slope IS NOT NULL
                         THEN sin(aspect) ELSE 0.0 END)
                / SUM(CASE WHEN slope > 0 AND slope IS NOT NULL THEN 1 ELSE 0 END),
                SUM(CASE WHEN slope > 0 AND slope IS NOT NULL
                         THEN cos(aspect) ELSE 0.0 END)
                / SUM(CASE WHEN slope > 0 AND slope IS NOT NULL THEN 1 ELSE 0 END)
              )
            ) + 360) %% 360
        END                                                               AS MEAN_ASP
      FROM read_parquet('%s')
      GROUP BY CAST(row_id AS INTEGER)
    ),
    joined AS (
      SELECT
        v.*,
        s.ELEV,
        s.MEAN_SLOPE,
        s.MEAN_ASP
      FROM %s v
      LEFT JOIN stats s ON v.rowid = s.row_id
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
    elev_col,
    constant1_sql,
    gsub("\\\\", "/", f_terrain),
    vri_bem_tbl,
    elevation_threshold,
    no_slope_mod_MC, no_slope_mod_MC, no_slope_mod_MC
  )

  DBI::dbExecute(conn, sql_result)
  logger::log_info("merge_elevation_duckdb: hash-join aggregation done ({round(proc.time()[['elapsed']] - t0, 1)}s)")

  # Warn about polygons with no elevation
  no_elev_rows <- DBI::dbGetQuery(
    conn,
    sprintf("SELECT rowid FROM %s WHERE ELEV IS NULL", result_tbl)
  )
  if (nrow(no_elev_rows) > 0L) {
    warning("could not calculate elevation for the following row ids: ",
            paste(no_elev_rows$rowid, collapse = ", "))
  }

  logger::log_info("merge_elevation_duckdb: done -> {result_tbl} (total {round(proc.time()[['elapsed']] - t0_total, 1)}s)")
  invisible(result_tbl)
}
