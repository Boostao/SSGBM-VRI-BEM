#' Compute slope, aspect, elevation and their derivatives inside DuckDB
#'
#' This is the DuckDB-optimised counterpart of [merge_elevation_raster_on_sf()].
#' Raster cells are exported to temporary **Parquet files on disk** via
#' [arrow::write_parquet()]. DuckDB then reads those files natively with
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
#'   Defaults to `vri_bem_tbl`.
#' @return The name of the result table (`result_tbl`) invisibly.  The caller
#'   can query `conn` directly, e.g. `duckdb::dbReadTable(conn, result_tbl)`.
#' @details
#' **Memory model**: raster values are read block-by-block from terra and
#' written as Parquet parts on disk. DuckDB streams those files directly
#' from disk via `read_parquet()`, so peak R memory is bounded to one raster
#' block plus one polygon batch rather than the full cropped raster or three
#' full `SpatVector` polygon objects. Polygon-to-cell ID assignment is
#' rasterized in batches to avoid materializing all source polygons in memory
#' at once.
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
                                   result_tbl = vri_bem_tbl) {

  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(vri_bem_tbl), length(vri_bem_tbl) == 1L)

  t0_total <- proc.time()[["elapsed"]]
  logger::log_info("merge_elevation_duckdb: starting (vri_bem_tbl={vri_bem_tbl}, threshold={elevation_threshold}m)")

  on.exit({
    # Clean up temp parquet file and temp table even on error
    if (exists("terrain_dataset_dir", inherits = FALSE)) {
      try(unlink(terrain_dataset_dir, recursive = TRUE, force = TRUE), silent = TRUE)
    }
    try(DBI::dbExecute(conn, "DROP TABLE IF EXISTS _elev_vri_tmp;"), silent = TRUE)
  }, add = TRUE)

  # ------------------------------------------------------------------
  # 1. Build terrain raster (slope + aspect in radians) ---------------
  # ------------------------------------------------------------------
  t0 <- proc.time()[["elapsed"]]
  if (is.null(terrain_raster)) {
    terrain_raster <- terra::terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")
  }
  terrain_secs <- round(proc.time()[["elapsed"]] - t0, 1)
  logger::log_info(sprintf("merge_elevation_duckdb: terrain computed (%.1fs)", terrain_secs))

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
  crop_secs <- round(proc.time()[["elapsed"]] - t0, 1)
  logger::log_info(sprintf("merge_elevation_duckdb: raster cropped to bbox (%.1fs)", crop_secs))

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

  rasterize_batch_size <- getOption("ssgbm.merge_elevation_batch_size", 10000L)
  if (!is.numeric(rasterize_batch_size) || length(rasterize_batch_size) != 1L ||
      is.na(rasterize_batch_size) || rasterize_batch_size < 1L) {
    rasterize_batch_size <- 10000L
  }
  rasterize_batch_size <- as.integer(rasterize_batch_size)

  # Materialize the view into a temp table so rowid is stable and we
  # only evaluate the view chain once.
  n_poly <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) AS n FROM %s;", vri_bem_tbl))$n
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS _elev_vri_tmp;")
  DBI::dbExecute(conn, sprintf(
    "CREATE TEMP TABLE _elev_vri_tmp AS
     SELECT row_number() OVER () AS raster_row_id, rowid AS src_row_id, Shape
     FROM %s;",
    vri_bem_tbl
  ))
  materialize_secs <- round(proc.time()[["elapsed"]] - t0, 1)
  logger::log_info(sprintf(
    "merge_elevation_duckdb: %d polygons materialized (%.1fs)",
    n_poly,
    materialize_secs
  ))

  # Export polygon geometries as WKB in batches, rasterize each chunk into the
  # same ID raster, and update only the cells touched by that batch. This keeps
  # peak R memory bounded even for million-polygon tables.
  t0 <- proc.time()[["elapsed"]]
  id_raster <- elev_raster[[1L]]
  terra::values(id_raster) <- NA_real_
  n_batches <- max(as.integer(ceiling(n_poly / rasterize_batch_size)), 1L)

  for (batch_idx in seq_len(n_batches)) {
    batch_start <- (batch_idx - 1L) * rasterize_batch_size + 1L
    batch_end <- min(batch_start + rasterize_batch_size - 1L, n_poly)

    poly_wkb <- DBI::dbGetQuery(conn, sprintf(
      "SELECT raster_row_id, ST_AsWKB(Shape) AS wkb
       FROM _elev_vri_tmp
       WHERE raster_row_id BETWEEN %d AND %d
       ORDER BY raster_row_id;",
      batch_start, batch_end
    ))

    if (nrow(poly_wkb) == 0L) {
      next
    }

    polys_sfc <- sf::st_as_sfc(poly_wkb$wkb)
    sf::st_crs(polys_sfc) <- terra::crs(elev_raster)
    polys_sv <- terra::vect(sf::st_sf(row_id = poly_wkb$raster_row_id, geometry = polys_sfc))
    id_raster <- terra::rasterize(polys_sv, id_raster, field = "row_id", update = TRUE)
    rm(poly_wkb, polys_sfc, polys_sv)

    if (batch_idx %% 10L == 0L || batch_idx == n_batches) {
      logger::log_info(
        "merge_elevation_duckdb: rasterized polygon batch {batch_idx}/{n_batches} ({batch_end}/{n_poly} rows)"
      )
      gc(verbose = FALSE)
    }
  }

  names(id_raster) <- "row_id"
  rasterize_secs <- round(proc.time()[["elapsed"]] - t0, 1)
  logger::log_info(sprintf(
    "merge_elevation_duckdb: %d polygons rasterized (%.1fs)",
    n_poly,
    rasterize_secs
  ))

  # Stack row_id as 4th layer.  na.rm = TRUE drops cells not in any
  # polygon (row_id = NA) as well as raster no-data, so the Parquet
  # contains only cells inside polygons — smaller file, faster hash join.
  # Export is streamed block-by-block to avoid a full-raster allocation.
  t0 <- proc.time()[["elapsed"]]
  terra::add(elev_raster) <- id_raster        # 4th layer: row_id
  rm(id_raster, terrain_raster)
  terrain_dataset_dir <- tempfile(pattern = "merge_elevation_parquet_")
  dir.create(terrain_dataset_dir)
  export_block_target <- getOption("ssgbm.merge_elevation_export_blocks", 64L)
  if (!is.numeric(export_block_target) || length(export_block_target) != 1L ||
      is.na(export_block_target) || export_block_target < 1L) {
    export_block_target <- 64L
  }
  export_block_target <- as.integer(export_block_target)

  block_plan <- terra::blocks(elev_raster, n = export_block_target)
  n_cells_inside <- 0L
  part_idx <- 0L
  read_started <- FALSE
  terra::readStart(elev_raster)
  read_started <- TRUE
  on.exit({
    if (exists("read_started", inherits = FALSE) && isTRUE(read_started)) {
      try(terra::readStop(elev_raster), silent = TRUE)
    }
  }, add = TRUE)

  for (block_idx in seq_len(block_plan$n)) {
    block_vals <- terra::readValues(
      elev_raster,
      row = block_plan$row[block_idx],
      nrows = block_plan$nrows[block_idx],
      dataframe = TRUE,
      mat = FALSE
    )

    if (nrow(block_vals) == 0L) {
      next
    }

    keep_idx <- stats::complete.cases(block_vals)
    if (!any(keep_idx)) {
      rm(block_vals, keep_idx)
      next
    }

    block_df <- block_vals[keep_idx, , drop = FALSE]
    block_df$row_id <- as.integer(block_df$row_id)
    part_idx <- part_idx + 1L
    arrow::write_parquet(
      tibble::as_tibble(block_df),
      sink = file.path(terrain_dataset_dir, sprintf("part-%05d.parquet", part_idx))
    )
    n_cells_inside <- n_cells_inside + nrow(block_df)
    rm(block_vals, keep_idx, block_df)

    if (block_idx %% 10L == 0L || block_idx == block_plan$n) {
      logger::log_info(
        "merge_elevation_duckdb: parquet export block {block_idx}/{block_plan$n} ({n_cells_inside} cells written)"
      )
      gc(verbose = FALSE)
    }
  }

  terra::readStop(elev_raster)
  read_started <- FALSE
  parquet_secs <- round(proc.time()[["elapsed"]] - t0, 1)
  logger::log_info(sprintf(
    "merge_elevation_duckdb: parquet dataset written (%d cells inside polygons, %.1fs)",
    n_cells_inside,
    parquet_secs
  ))

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
      FROM read_parquet('%s/*.parquet')
      GROUP BY CAST(row_id AS INTEGER)
    ),
    mapstats AS (
      SELECT
        src_row_id,
        s.ELEV,
        s.MEAN_SLOPE,
        s.MEAN_ASP
      FROM _elev_vri_tmp m
      LEFT JOIN stats s ON m.raster_row_id = s.row_id
    ),
    joined AS (
      SELECT
        v.*,
        m.ELEV,
        m.MEAN_SLOPE,
        m.MEAN_ASP
      FROM %s v
      LEFT JOIN mapstats m ON v.rowid = m.src_row_id
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
    gsub("\\\\", "/", terrain_dataset_dir),
    vri_bem_tbl,
    elevation_threshold,
    no_slope_mod_MC, no_slope_mod_MC, no_slope_mod_MC
  )

  DBI::dbExecute(conn, sql_result)
  hash_secs <- round(proc.time()[["elapsed"]] - t0, 1)
  logger::log_info(sprintf("merge_elevation_duckdb: hash-join aggregation done (%.1fs)", hash_secs))

  # Warn about polygons with no elevation
  no_elev_rows <- DBI::dbGetQuery(
    conn,
    sprintf("SELECT rowid FROM %s WHERE ELEV IS NULL", result_tbl)
  )
  if (nrow(no_elev_rows) > 0L) {
    warning("could not calculate elevation for the following row ids: ",
            paste(no_elev_rows$rowid, collapse = ", "))
  }

  total_secs <- round(proc.time()[["elapsed"]] - t0_total, 1)
  logger::log_info(sprintf("merge_elevation_duckdb: done -> %s (total %.1fs)", result_tbl, total_secs))
  invisible(result_tbl)
}
