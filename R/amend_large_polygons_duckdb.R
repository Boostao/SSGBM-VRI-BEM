#' Amend large polygons in a DuckDB VRI-BEM table
#'
#' DuckDB-native counterpart of [amend_large_polygons()].
#' Polygons larger than 3500 ha are extracted, dissolved by VRI identifier
#' attributes, re-split by glaciers, lakes, and wetlands, re-joined with BEM,
#' and merged back into the VRI-BEM table replacing the original large polygons.
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param vri_bem_tbl Character. Name of the source VRI-BEM table or view.
#'   Default `"V_VRIBEM"`.
#' @param lakes_tbl Character. Name of the lakes table/view in `conn`.
#'   Default `"V_LAKES"`.
#' @param glaciers_tbl Character. Name of the glaciers table/view in `conn`.
#'   Default `"V_GLACIERS"`.
#' @param wetlands_tbl Character. Name of the wetlands table/view in `conn`.
#'   Default `"V_WETLANDS"`.
#' @param bem_tbl Character. Name of the BEM table used for the re-join of XL
#'   pieces.  Default `"V_BEM"`.
#' @param tolerance_m2 Numeric. Minimum polygon area (m²) to retain after
#'   splitting.  Default `100`.
#' @param result_tbl Character. Name of the (temp) table written to `conn`
#'   with the amended result.  Defaults to `"V_VRIBEM"`, which drops the
#'   original view and replaces it in-place with a temp table.
#' @return The name of the result table (`result_tbl`) invisibly.
#' @details
#' The logic mirrors [amend_large_polygons()]:
#' \enumerate{
#'   \item If no polygon exceeds 3500 ha the function is a no-op (copies
#'         `vri_bem_tbl` to `result_tbl` when they differ).
#'   \item Large polygons (>3500 ha) and their touching neighbours are combined
#'         and dissolved by VRI identifier attributes
#'         (\code{INVENTORY_STANDARD_CD}, \code{BCLCS_LV_1-5},
#'         \code{SPEC_CD_1}, \code{BGC_ZONE/SUBZON/VRT/PHASE}).
#'   \item Dissolved regions still exceeding 3500 ha are intersected with
#'         glaciers (\code{BEUMC_S1 = "GL"}), lakes (\code{BEUMC_S1 = "LL"}),
#'         and wetlands (\code{BEUMC_S1 = "WL"}).  Residual pieces (XL area
#'         minus all overlays) receive no override.
#'   \item All XL pieces (overlay + residual) are spatially re-joined with
#'         \code{bem_tbl}: one row is created per XL-piece × BEM intersection,
#'         mirroring the VRI × BEM join performed by [vribem_view()].
#'   \item Overlay pieces receive the appropriate \code{BEUMC_S1} code,
#'         \code{SDEC_1 = 10}, \code{SDEC_2 = SDEC_3 = 0}, and
#'         \code{POLY_COMM = "XL edit"}.
#'   \item The XL footprint is subtracted from the original \code{vri_bem_tbl}
#'         rows and the new XL pieces are appended.
#' }
#' @import DBI
#' @import duckdb
#' @export
amend_large_polygons_duckdb <- function(conn,
                                        vri_bem_tbl  = "V_VRIBEM",
                                        lakes_tbl    = "V_LAKES",
                                        glaciers_tbl = "V_GLACIERS",
                                        wetlands_tbl = "V_WETLANDS",
                                        bem_tbl      = "V_BEM",
                                        tolerance_m2 = 100,
                                        result_tbl   = vri_bem_tbl) {

  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(vri_bem_tbl),  length(vri_bem_tbl)  == 1L)
  stopifnot(is.character(lakes_tbl),    length(lakes_tbl)    == 1L)
  stopifnot(is.character(glaciers_tbl), length(glaciers_tbl) == 1L)
  stopifnot(is.character(wetlands_tbl), length(wetlands_tbl) == 1L)
  stopifnot(is.character(bem_tbl),      length(bem_tbl)      == 1L)
  stopifnot(is.numeric(tolerance_m2),   length(tolerance_m2) == 1L)
  stopifnot(is.character(result_tbl),   length(result_tbl)   == 1L)

  t0 <- proc.time()[["elapsed"]]
  logger::log_info("amend_large_polygons_duckdb: starting (vri_bem_tbl={vri_bem_tbl})")

  # ------------------------------------------------------------------
  # 0.  Early exit when no large polygons exist
  # ------------------------------------------------------------------
  max_area <- DBI::dbGetQuery(conn, sprintf(
    "SELECT COALESCE(MAX(ST_Area(Shape)), 0) AS max_area FROM %s",
    vri_bem_tbl
  ))$max_area

  if (max_area <= 35000000) {
    logger::log_info(
      "amend_large_polygons_duckdb: no large polygons (max = {round(max_area / 10000, 0)} ha). Skipping."
    )
    if (!identical(result_tbl, vri_bem_tbl)) {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s;",
        result_tbl, vri_bem_tbl
      ))
      try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    }
    return(invisible(result_tbl))
  }

  logger::log_info(
    "amend_large_polygons_duckdb: large polygons found (max = {round(max_area / 10000, 0)} ha). Processing."
  )

  # ------------------------------------------------------------------
  # Column introspection
  # ------------------------------------------------------------------
  vribem_cols  <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", vri_bem_tbl)
  )$name

  bem_all_cols <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", bem_tbl)
  )$name
  bem_non_geom <- bem_all_cols[bem_all_cols != "Shape"]

  # BEM columns that are handled separately via COALESCE (BGC fields from VRI take priority)
  bem_bgc_cols <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE")
  bem_other    <- setdiff(bem_non_geom, bem_bgc_cols)

  qi <- function(x) paste0('"', x, '"')
  .relation_exists <- function(name) {
    nrow(DBI::dbGetQuery(conn, sprintf(
      "SELECT 1 FROM information_schema.tables WHERE table_name = '%s'\n       UNION ALL\n       SELECT 1 FROM duckdb_views() WHERE view_name = '%s'\n       LIMIT 1;",
      name, name
    ))) > 0L
  }

  vribem_non_geom <- vribem_cols[vribem_cols != "Shape"]

  # Explicit SELECT of all vri_bem_tbl non-geom cols qualified with alias v
  orig_non_geom_select <- paste(sprintf("v.%s", qi(vribem_non_geom)), collapse = ",\n      ")

  # ------------------------------------------------------------------
  # Temp table names (prefixed to avoid collisions across calls)
  # ------------------------------------------------------------------
  pfx          <- paste0("_alp_", result_tbl, "_")
  tmp_src      <- paste0(pfx, "src")   # materialised copy of vri_bem_tbl if it is a VIEW
  tmp_large    <- paste0(pfx, "large")
  tmp_adj      <- paste0(pfx, "adj")
  tmp_combined <- paste0(pfx, "combined") # step 3 intermediate: UNION of large+adj
  tmp_dissolv  <- paste0(pfx, "dissolv")
  tmp_vrixl    <- paste0(pfx, "vrixl")
  tmp_overlay  <- paste0(pfx, "overlay")
  tmp_nonovl   <- paste0(pfx, "nonovl")
  tmp_xlpieces <- paste0(pfx, "xlpieces")
  tmp_bemjoin  <- paste0(pfx, "bemjoin")
  tmp_xlfinal  <- paste0(pfx, "xlfinal")
  tmp_glacsrc  <- paste0(pfx, "glacsrc")
  tmp_lakesrc  <- paste0(pfx, "lakesrc")
  tmp_wetlsrc  <- paste0(pfx, "wetlsrc")

  on.exit({
    for (t in c(tmp_src, tmp_large, tmp_adj, tmp_combined, tmp_dissolv, tmp_vrixl,
                tmp_overlay, tmp_nonovl, tmp_xlpieces, tmp_bemjoin,
                tmp_xlfinal, tmp_glacsrc, tmp_lakesrc, tmp_wetlsrc)) {
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", t)), silent = TRUE)
    }
  }, add = TRUE)

  # Disable insertion-order preservation so DuckDB doesn't allocate large sort
  # buffers during CTAS of wide spatial tables.  Restore on exit.
  orig_pio <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT current_setting('preserve_insertion_order') AS v")$v,
    error = function(e) NULL
  )
  DBI::dbExecute(conn, "SET preserve_insertion_order=false")
  on.exit(
    if (!is.null(orig_pio)) {
      DBI::dbExecute(conn, sprintf(
        "SET preserve_insertion_order=%s",
        if (tolower(orig_pio) == "false") "false" else "true"
      ))
    },
    add = TRUE
  )

  # ------------------------------------------------------------------
  # 0b.  Materialise vri_bem_tbl if it is a VIEW
  #
  # V_VRIBEM is a TEMP VIEW that re-evaluates a VRI×BEM spatial join
  # plus a correlated EXISTS (INTERSECTS_RIVER) subquery on every access.
  # Steps 1, 2, and 11 each re-scan it, tripling I/O and peak memory.
  # Writing it once to a persistent TABLE (checkpointed to disk so pages
  # are evictable by the buffer manager) eliminates repeated evaluation.
  # ------------------------------------------------------------------
  src_is_view <- nrow(DBI::dbGetQuery(conn, sprintf(
    "SELECT 1 FROM duckdb_views() WHERE view_name = '%s'", vri_bem_tbl
  ))) > 0L

  if (src_is_view) {
    logger::log_info(
      "amend_large_polygons_duckdb: materialising {vri_bem_tbl} into {tmp_src}"
    )
    # Speed up the INTERSECTS_RIVER correlated subquery in V_VRIBEM by
    # ensuring V_RIVERS has an RTREE index before the full-table scan.
    if (.relation_exists("V_RIVERS")) {
      has_riv_idx <- nrow(DBI::dbGetQuery(conn,
        "SELECT 1 FROM duckdb_indexes() WHERE index_name = 'V_RIVERS_rtree'"
      )) > 0L
      if (!has_riv_idx) {
        n_riv <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_RIVERS")$n
        if (n_riv > 0L) {
          tryCatch(
            DBI::dbExecute(conn,
              "CREATE INDEX V_RIVERS_rtree ON V_RIVERS USING RTREE (Shape);"
            ),
            error = function(e) logger::log_warn(
              "amend_large_polygons_duckdb: RTREE on V_RIVERS failed: {conditionMessage(e)}"
            )
          )
        }
      }
    }
    t_src <- proc.time()[["elapsed"]]
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s;", tmp_src, vri_bem_tbl
    ))
    try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    n_src <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) AS n FROM %s;", tmp_src))$n
    if (n_src > 0L) {
      tryCatch(
        DBI::dbExecute(conn, sprintf(
          "CREATE INDEX %s_rtree ON %s USING RTREE (Shape);", tmp_src, tmp_src
        )),
        error = function(e) logger::log_warn(
          "amend_large_polygons_duckdb: RTREE on {tmp_src} failed: {conditionMessage(e)}"
        )
      )
    }
    vri_bem_tbl <- tmp_src
    logger::log_info(
      "amend_large_polygons_duckdb: materialised to {tmp_src} ({n_src} rows, {round(proc.time()[['elapsed']] - t_src, 1)}s)"
    )
  }

  # ------------------------------------------------------------------
  # 1.  Extract polygons larger than 3500 ha
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT * FROM %s WHERE ST_Area(Shape) > 35000000;",
    tmp_large, vri_bem_tbl
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 1 – large polygons extracted ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 2.  Find adjacent (touching) polygons
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT DISTINCT v.*
     FROM %s v
     JOIN %s xl ON ST_Touches(v.Shape, xl.Shape);",
    tmp_adj, vri_bem_tbl, tmp_large
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 2 – adjacent polygons found ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )
  # tmp_src RTREE is no longer needed after step 2 (steps 3-10 don't use it,
  # step 11 scans tmp_src sequentially with NOT EXISTS against tmp_vrixl RTREE).
  # Dropping it here marks those buffer pages as evictable, freeing space for
  # the per-rowid overlay intersection computations.
  try(DBI::dbExecute(conn, sprintf(
    "DROP INDEX IF EXISTS %s_rtree;", tmp_src
  )), silent = TRUE)

  # ------------------------------------------------------------------
  # 3.  Dissolve combined large + adjacent by VRI identifier attributes.
  #     Split into two SQL steps to work around a DuckDB query-planner
  #     bug ("Attempted to access index -1 within vector of size N")
  #     triggered when a UNION ALL subquery is combined with GROUP BY +
  #     ST_Union_Agg in a single CREATE TABLE AS SELECT.  Materialising
  #     the UNION ALL first into a plain table avoids the bad plan.
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
            SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, Shape
     FROM %s
     UNION ALL
     SELECT INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
            SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, Shape
     FROM %s;",
    tmp_combined, tmp_large, tmp_adj
  ))
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT
       INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
       SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
       ST_Union_Agg(ST_MakeValid(Shape)) AS Shape
     FROM %s
     GROUP BY
       INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
       SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE;",
    tmp_dissolv, tmp_combined
  ))
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_combined)), silent = TRUE)
  try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
  logger::log_info(
    "amend_large_polygons_duckdb: step 3 – dissolved ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )
  # large/adj no longer needed — free buffer memory now
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_large)),   silent = TRUE)
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_adj)),     silent = TRUE)

  # ------------------------------------------------------------------
  # 4.  Filter dissolved > 3500 ha; extract polygon geometry only
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT
       INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
       SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
       ST_CollectionExtract(ST_MakeValid(Shape), 3) AS Shape
     FROM %s
     WHERE ST_Area(ST_CollectionExtract(ST_MakeValid(Shape), 3)) > 35000000;",
    tmp_vrixl, tmp_dissolv
  ))

  n_xl <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s;", tmp_vrixl))$n
  logger::log_info("amend_large_polygons_duckdb: {n_xl} dissolved XL region(s) identified.")
  # dissolv no longer needed — free buffer memory now
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_dissolv)),  silent = TRUE)

  if (n_xl == 0L) {
    logger::log_info("amend_large_polygons_duckdb: no XL regions after dissolving. Skipping.")
    if (!identical(result_tbl, vri_bem_tbl)) {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s;",
        result_tbl, vri_bem_tbl
      ))
      try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    }
    return(invisible(result_tbl))
  }
  # tmp_vrixl is kept alive through steps 6-7 (overlay creation) AND the
  # combined step 11/12 (XL footprint erasure).  It is dropped there.

  # RTREE index on tmp_vrixl so spatial joins in steps 5 and 11 can use it.
  tryCatch(
    DBI::dbExecute(conn, sprintf(
      "CREATE INDEX %s_rtree ON %s USING RTREE (Shape);", tmp_vrixl, tmp_vrixl
    )),
    error = function(e) logger::log_warn(
      "amend_large_polygons_duckdb: RTREE on {tmp_vrixl} failed: {conditionMessage(e)}"
    )
  )

  # NOTE: Overlay RTREEs (V_LAKES 134K, V_WETLANDS 99K, etc.) are built
  # one-at-a-time inside the step 5 overlay loop below and dropped immediately
  # after each overlay is processed.  DuckDB RTREEs are in-memory only structures
  # that cannot be spilled to disk; building all of them simultaneously alongside
  # the 1.4M-row _alp_VRIBEM_src buffer exhausts the available memory budget.
  # V_BEM RTREE is built just before step 9 where it is first needed.

  xl_rowids <- DBI::dbGetQuery(conn, sprintf(
    "SELECT rowid AS rid FROM %s ORDER BY rowid", tmp_vrixl
  ))$rid

  # Build a spatial tile grid over all XL polygons.
  # Each overlay INSERT is clipped to one 20 km × 20 km tile, bounding the
  # working geometry and preventing the per-query memory spikes that occur
  # when a single 127 000 ha XL polygon is intersected with all overlay
  # features at once.
  #
  # We compute one bbox per XL polygon and generate only the tiles that can
  # actually intersect that polygon.  This avoids thousands of empty tile
  # queries on broad AOIs while keeping each intersection bounded to a small
  # working geometry.
  .tile_sz      <- 10000  # 10 km in BC Albers metres
  .xl_bounds    <- tryCatch(
    DBI::dbGetQuery(conn, sprintf(
      "SELECT rowid AS rid,
              ST_XMin(Shape) AS xmin,
              ST_YMin(Shape) AS ymin,
              ST_XMax(Shape) AS xmax,
              ST_YMax(Shape) AS ymax
       FROM %s
       ORDER BY rowid",
      tmp_vrixl
    )),
    error = function(e) {
      logger::log_warn(
        "amend_large_polygons_duckdb: XL bbox query failed ({conditionMessage(e)}); using fallback single-tile bounds"
      )
      data.frame(rid = xl_rowids, xmin = 200000, ymin = 360000, xmax = 210000, ymax = 370000)
    }
  )
  xl_tiles <- do.call(rbind, lapply(seq_len(nrow(.xl_bounds)), function(i) {
    bb <- .xl_bounds[i, ]
    xs <- seq(floor(bb$xmin / .tile_sz) * .tile_sz,
              floor(bb$xmax / .tile_sz) * .tile_sz, by = .tile_sz)
    ys <- seq(floor(bb$ymin / .tile_sz) * .tile_sz,
              floor(bb$ymax / .tile_sz) * .tile_sz, by = .tile_sz)
    grid <- expand.grid(x0 = xs, y0 = ys, KEEP.OUT.ATTRS = FALSE)
    data.frame(
      rid = bb$rid,
      x0 = grid$x0,
      y0 = grid$y0,
      x1 = grid$x0 + .tile_sz,
      y1 = grid$y0 + .tile_sz,
      stringsAsFactors = FALSE
    )
  }))
  logger::log_info(
    "amend_large_polygons_duckdb: tile grid built ({nrow(xl_tiles)} occupied tiles over {nrow(.xl_bounds)} XL regions)"
  )

  # NOTE: We deliberately do NOT reduce memory_limit before the per-rowid loops.
  # Each per-rowid query processes one XL polygon (~500 MB working set), which
  # is trivially small compared to the full 13 GiB budget.  Reducing the limit
  # would set it below the static data footprint (tmp_vrixl complex geometries +
  # overlay RTREE indexes already exceed 3.8 GiB), leaving zero room for any
  # allocation and causing immediate OOM on the very first per-rowid query.

  # Reduce thread count during per-rowid geometry loops: fewer parallel
  # threads means less concurrent geometry buffer use per DuckDB operation.
  .orig_threads <- tryCatch(
    as.integer(DBI::dbGetQuery(conn,
      "SELECT value FROM duckdb_settings() WHERE name = 'threads'"
    )[[1L]]),
    error = function(e) NULL
  )
  .reduce_threads <- FALSE
  if (!is.null(.orig_threads) && .orig_threads > 2L) {
    tryCatch({
      DBI::dbExecute(conn, "SET threads = 2;")
      .reduce_threads <- TRUE
    }, error = function(e) {
      logger::log_warn(
        "amend_large_polygons_duckdb: could not reduce threads: {conditionMessage(e)}"
      )
    })
  }

  # ------------------------------------------------------------------
  # 5.  Overlay pieces: intersect XL with glaciers, lakes, wetlands.
  #     Each overlay RTREE is built immediately before its rowid loop and
  #     DROPPED immediately after, so only ONE large RTREE is in memory at
  #     a time.  This prevents simultaneous V_LAKES + V_WETLANDS RTREEs
  #     from exhausting the buffer pool alongside _alp_VRIBEM_src.
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]

  # Create the overlay table upfront with an explicit schema that includes
  # xl_rowid (the rowid of the source XL region in tmp_vrixl).  This column
  # lets step 7 compute a per-XL-region overlay union instead of one global
  # ST_Union_Agg over all overlay pieces.
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s (
       xl_rowid              BIGINT,
       INVENTORY_STANDARD_CD VARCHAR,
       BCLCS_LV_1            VARCHAR,
       BCLCS_LV_2            VARCHAR,
       BCLCS_LV_3            VARCHAR,
       BCLCS_LV_4            VARCHAR,
       BCLCS_LV_5            VARCHAR,
       SPEC_CD_1             VARCHAR,
       BGC_ZONE              VARCHAR,
       BGC_SUBZON            VARCHAR,
       BGC_VRT               VARCHAR,
       BGC_PHASE             VARCHAR,
       overlay_beumc         VARCHAR,
       overlay_dec           INTEGER,
       Shape                 GEOMETRY
     );",
    tmp_overlay
  ))

  # Read the current memory_limit once so we can restore it after buffer flushes.
  .orig_mem_limit <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT value FROM duckdb_settings() WHERE name = 'memory_limit'")[[1L]],
    error = function(e) NULL
  )

  # Helper to build an RTREE if it does not already exist.
  .ensure_rtree <- function(tbl) {
    idx <- paste0(tbl, "_rtree")
    if (nrow(DBI::dbGetQuery(conn, sprintf(
      "SELECT 1 FROM duckdb_indexes() WHERE index_name = '%s'", idx
    ))) == 0L) {
      n <- tryCatch(
        DBI::dbGetQuery(conn, sprintf("SELECT count(*) AS n FROM %s;", tbl))$n,
        error = function(e) 0L
      )
      if (n > 0L) tryCatch(
        DBI::dbExecute(conn, sprintf(
          "CREATE INDEX %s ON %s USING RTREE (Shape);", idx, tbl
        )),
        error = function(e) logger::log_warn(
          "amend_large_polygons_duckdb: RTREE on {tbl} failed: {conditionMessage(e)}"
        )
      )
    }
    invisible(idx)
  }
  .drop_rtree <- function(tbl) {
    idx <- paste0(tbl, "_rtree")
    try(DBI::dbExecute(conn, sprintf("DROP INDEX IF EXISTS %s;", idx)), silent = TRUE)
  }
  .materialize_if_view <- function(tbl, tmp_tbl) {
    if (nrow(DBI::dbGetQuery(conn, sprintf(
      "SELECT 1 FROM duckdb_views() WHERE view_name = '%s'", tbl
    ))) > 0L) {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s;", tmp_tbl, tbl
      ))
      try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
      return(tmp_tbl)
    }
    tbl
  }
  # After dropping an overlay RTREE + CHECKPOINT, the large table pages
  # (tmp_src 1.4M rows) remain hot in the DuckDB buffer pool.  Temporarily
  # shrinking memory_limit forces the buffer manager to evict all clean
  # (already-checkpointed) pages before we build the next overlay RTREE.
  # Without this, LAKES RTREE (134K) + tmp_src buffer fills the 13 GiB budget,
  # leaving zero room for WETLANDS RTREE or per-rowid geometry allocations.
  .flush_buffer <- function() {
    try(DBI::dbExecute(conn, "SET memory_limit = '256MB';"), silent = TRUE)
    try(DBI::dbExecute(conn, "CHECKPOINT;"),                 silent = TRUE)
    if (!is.null(.orig_mem_limit))
      try(DBI::dbExecute(conn, sprintf("SET memory_limit = '%s';", .orig_mem_limit)), silent = TRUE)
  }

  # Flush the buffer pool before building any overlay RTREEs.
  # _alp_VRIBEM_src was just materialised (1.4M rows) and its pages fill the
  # DuckDB buffer.  Without this flush the very first RTREE build (GLACIERS)
  # already exhausts the 13 GiB DuckDB-internal limit.
  .flush_buffer()

  # --- GLACIERS overlay ---
  if (.relation_exists(glaciers_tbl)) {
    glaciers_src <- .materialize_if_view(glaciers_tbl, tmp_glacsrc)
    .ensure_rtree(glaciers_src)
    .step5g_i <- 0L
    for (i in seq_len(nrow(xl_tiles))) {
      tile <- xl_tiles[i, ]
      DBI::dbExecute(conn, sprintf(
        "INSERT INTO %s
         SELECT
           xl.rowid AS xl_rowid,
           xl.INVENTORY_STANDARD_CD,
           'N'  AS BCLCS_LV_1,
           'L'  AS BCLCS_LV_2,
           'A'  AS BCLCS_LV_3,
           'SI' AS BCLCS_LV_4,
           'OT' AS BCLCS_LV_5,
           xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
           'GL' AS overlay_beumc,
           10   AS overlay_dec,
           ST_CollectionExtract(ST_MakeValid(ST_Intersection(
             ST_Intersection(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f)),
             g.Shape
           )), 3) AS Shape
         FROM (
           SELECT rowid, INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3,
                  BCLCS_LV_4, BCLCS_LV_5, SPEC_CD_1, BGC_ZONE, BGC_SUBZON,
                  BGC_VRT, BGC_PHASE, Shape
           FROM %s
           WHERE rowid = %.0f
         ) xl
         JOIN %s g ON ST_Intersects(xl.Shape, g.Shape)
                   AND ST_Intersects(g.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f))
         WHERE ST_Intersects(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f))
           AND ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(
             ST_Intersection(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f)),
             g.Shape
           )), 3)) > %.0f;",
        tmp_overlay,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tmp_vrixl, tile$rid, glaciers_src,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tolerance_m2
      ))
      .step5g_i <- .step5g_i + 1L
      if (.step5g_i %% 10L == 0L) .flush_buffer()
    }
    .drop_rtree(glaciers_src)
    try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    .flush_buffer()
  }

  # --- LAKES overlay ---
  if (.relation_exists(lakes_tbl)) {
    lakes_src <- .materialize_if_view(lakes_tbl, tmp_lakesrc)
    .ensure_rtree(lakes_src)
    .step5l_i <- 0L
    for (i in seq_len(nrow(xl_tiles))) {
      tile <- xl_tiles[i, ]
      DBI::dbExecute(conn, sprintf(
        "INSERT INTO %s
         SELECT
           xl.rowid AS xl_rowid,
           xl.INVENTORY_STANDARD_CD,
           'N'  AS BCLCS_LV_1,
           'W'  AS BCLCS_LV_2,
           'W'  AS BCLCS_LV_3,
           NULL AS BCLCS_LV_4,
           'LA' AS BCLCS_LV_5,
           xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
           'LL' AS overlay_beumc,
           10   AS overlay_dec,
           ST_CollectionExtract(ST_MakeValid(ST_Intersection(
             ST_Intersection(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f)),
             l.Shape
           )), 3) AS Shape
         FROM (
           SELECT rowid, INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3,
                  BCLCS_LV_4, BCLCS_LV_5, SPEC_CD_1, BGC_ZONE, BGC_SUBZON,
                  BGC_VRT, BGC_PHASE, Shape
           FROM %s
           WHERE rowid = %.0f
         ) xl
         JOIN %s l ON ST_Intersects(xl.Shape, l.Shape)
                   AND ST_Intersects(l.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f))
         WHERE ST_Intersects(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f))
           AND ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(
             ST_Intersection(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f)),
             l.Shape
           )), 3)) > %.0f;",
        tmp_overlay,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tmp_vrixl, tile$rid, lakes_src,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tolerance_m2
      ))
      .step5l_i <- .step5l_i + 1L
      if (.step5l_i %% 10L == 0L) .flush_buffer()
    }
    .drop_rtree(lakes_src)
    try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    .flush_buffer()
  }

  # --- WETLANDS overlay ---
  if (.relation_exists(wetlands_tbl)) {
    wetlands_src <- .materialize_if_view(wetlands_tbl, tmp_wetlsrc)
    .ensure_rtree(wetlands_src)
    .step5w_i <- 0L
    for (i in seq_len(nrow(xl_tiles))) {
      tile <- xl_tiles[i, ]
      DBI::dbExecute(conn, sprintf(
        "INSERT INTO %s
         SELECT
           xl.rowid AS xl_rowid,
           xl.INVENTORY_STANDARD_CD,
           'V'  AS BCLCS_LV_1,
           'N'  AS BCLCS_LV_2,
           'W'  AS BCLCS_LV_3,
           NULL AS BCLCS_LV_4,
           NULL AS BCLCS_LV_5,
           xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
           'WL' AS overlay_beumc,
           10   AS overlay_dec,
           ST_CollectionExtract(ST_MakeValid(ST_Intersection(
             ST_Intersection(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f)),
             w.Shape
           )), 3) AS Shape
         FROM (
           SELECT rowid, INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3,
                  BCLCS_LV_4, BCLCS_LV_5, SPEC_CD_1, BGC_ZONE, BGC_SUBZON,
                  BGC_VRT, BGC_PHASE, Shape
           FROM %s
           WHERE rowid = %.0f
         ) xl
         JOIN %s w ON ST_Intersects(xl.Shape, w.Shape)
                   AND ST_Intersects(w.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f))
         WHERE ST_Intersects(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f))
           AND ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(
             ST_Intersection(xl.Shape, ST_MakeEnvelope(%.0f, %.0f, %.0f, %.0f)),
             w.Shape
           )), 3)) > %.0f;",
        tmp_overlay,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tmp_vrixl, tile$rid, wetlands_src,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tile$x0, tile$y0, tile$x1, tile$y1,
        tolerance_m2
      ))
      .step5w_i <- .step5w_i + 1L
      if (.step5w_i %% 10L == 0L) .flush_buffer()
    }
    .drop_rtree(wetlands_src)
    try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    .flush_buffer()
  }

  logger::log_info(
    "amend_large_polygons_duckdb: step 5 \u2013 overlay pieces created ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 7.  Non-overlay residual: XL area minus the union of all overlay shapes
  #     Processed one XL region at a time to limit ST_Difference memory use.
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s (
       INVENTORY_STANDARD_CD VARCHAR,
       BCLCS_LV_1            VARCHAR,
       BCLCS_LV_2            VARCHAR,
       BCLCS_LV_3            VARCHAR,
       BCLCS_LV_4            VARCHAR,
       BCLCS_LV_5            VARCHAR,
       SPEC_CD_1             VARCHAR,
       BGC_ZONE              VARCHAR,
       BGC_SUBZON            VARCHAR,
       BGC_VRT               VARCHAR,
       BGC_PHASE             VARCHAR,
       overlay_beumc         VARCHAR,
       overlay_dec           INTEGER,
       Shape                 GEOMETRY
     );",
    tmp_nonovl
  ))
  .step7_i <- 0L
  for (rid in xl_rowids) {
    .step7_i <- .step7_i + 1L
    DBI::dbExecute(conn, sprintf(
      "INSERT INTO %s
       SELECT
         xl.INVENTORY_STANDARD_CD,
         xl.BCLCS_LV_1, xl.BCLCS_LV_2, xl.BCLCS_LV_3, xl.BCLCS_LV_4, xl.BCLCS_LV_5,
         xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
         CAST(NULL AS VARCHAR) AS overlay_beumc,
         CAST(NULL AS INTEGER) AS overlay_dec,
         CASE
           WHEN ov.overlay_geom IS NULL
             THEN xl.Shape
           ELSE
             ST_CollectionExtract(ST_MakeValid(ST_Difference(xl.Shape, ov.overlay_geom)), 3)
         END AS Shape
       FROM %s xl
       LEFT JOIN (
         SELECT xl_rowid, ST_Union_Agg(ST_MakeValid(Shape)) AS overlay_geom
         FROM %s
         WHERE xl_rowid = %s
         GROUP BY xl_rowid
       ) ov ON xl.rowid = ov.xl_rowid
       WHERE xl.rowid = %s
         AND CASE
           WHEN ov.overlay_geom IS NULL THEN ST_Area(xl.Shape)
           ELSE ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Difference(xl.Shape, ov.overlay_geom)), 3))
         END > %s;",
      tmp_nonovl, tmp_vrixl, tmp_overlay, rid, rid, tolerance_m2
    ))
    if (.step7_i %% 10L == 0L) try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
  }
  logger::log_info(
    "amend_large_polygons_duckdb: step 7 \u2013 non-overlay residuals ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )
  # Restore the original thread count now that the per-rowid loops are done.
  if (!is.null(.orig_threads) && .reduce_threads) {
    tryCatch(
      DBI::dbExecute(conn, sprintf("SET threads = %d;", .orig_threads)),
      error = function(e) NULL
    )
  }
  # NOTE: tmp_vrixl is intentionally kept alive here — it is still needed in
  # the combined step 11/12 to identify and erase the XL footprint from the
  # original vri_bem_tbl without needing a global ST_Union_Agg.

  # ------------------------------------------------------------------
  # 8.  Combine overlay + residual pieces
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT * EXCLUDE (xl_rowid) FROM %s
     UNION ALL
     SELECT * FROM %s;",
    tmp_xlpieces, tmp_overlay, tmp_nonovl
  ))
  # overlay/nonovl no longer needed — free buffer memory now
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_overlay)),  silent = TRUE)
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_nonovl)),   silent = TRUE)

  # RTREE on tmp_xlpieces so the BEM re-join in step 9 can use index lookups.
  tryCatch(
    DBI::dbExecute(conn, sprintf(
      "CREATE INDEX %s_rtree ON %s USING RTREE (Shape);", tmp_xlpieces, tmp_xlpieces
    )),
    error = function(e) logger::log_warn(
      "amend_large_polygons_duckdb: RTREE on {tmp_xlpieces} failed: {conditionMessage(e)}"
    )
  )

  # ------------------------------------------------------------------
  # 9.  Re-join XL pieces with BEM
  #     One row per (XL piece × BEM polygon intersection) — mirrors vribem_view.
  #     Build the V_BEM RTREE here, just before it is first needed, so that
  #     it does not occupy memory during steps 5-7.
  # ------------------------------------------------------------------
  .ensure_rtree(bem_tbl)
  t1 <- proc.time()[["elapsed"]]

  bem_other_select <- paste(sprintf("b.%s", qi(bem_other)), collapse = ",\n         ")

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT
       xl.INVENTORY_STANDARD_CD,
       xl.BCLCS_LV_1, xl.BCLCS_LV_2, xl.BCLCS_LV_3, xl.BCLCS_LV_4, xl.BCLCS_LV_5,
       xl.SPEC_CD_1,
       COALESCE(xl.BGC_ZONE,             b.BGC_ZONE)             AS BGC_ZONE,
       COALESCE(xl.BGC_SUBZON,           b.BGC_SUBZON)           AS BGC_SUBZON,
       COALESCE(xl.BGC_VRT::VARCHAR,     b.BGC_VRT::VARCHAR)     AS BGC_VRT,
       COALESCE(xl.BGC_PHASE,            b.BGC_PHASE)            AS BGC_PHASE,
       xl.overlay_beumc,
       xl.overlay_dec,
       %s,
       ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, b.Shape)), 3) AS Shape
     FROM %s xl
     JOIN %s b ON ST_Intersects(xl.Shape, b.Shape)
     WHERE ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, b.Shape)), 3)) > %s;",
    tmp_bemjoin,
    bem_other_select,
    tmp_xlpieces, bem_tbl, tolerance_m2
  ))
  try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
  logger::log_info(
    "amend_large_polygons_duckdb: step 9 – BEM re-join done ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )
  # xlpieces no longer needed — free buffer memory now
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_xlpieces)), silent = TRUE)

  # ------------------------------------------------------------------
  # 10. Apply overlay overrides: BEUMC_S1, BEUMC_S2/S3, SDEC, POLY_COMM
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT
       * EXCLUDE (BEUMC_S1, BEUMC_S2, BEUMC_S3, SDEC_1, SDEC_2, SDEC_3,
                  POLY_COMM, overlay_beumc, overlay_dec),
       CASE WHEN overlay_beumc IS NOT NULL THEN overlay_beumc ELSE BEUMC_S1  END AS BEUMC_S1,
       CASE WHEN overlay_beumc IS NOT NULL THEN NULL           ELSE BEUMC_S2  END AS BEUMC_S2,
       CASE WHEN overlay_beumc IS NOT NULL THEN NULL           ELSE BEUMC_S3  END AS BEUMC_S3,
       CASE WHEN overlay_beumc IS NOT NULL THEN overlay_dec    ELSE SDEC_1    END AS SDEC_1,
       CASE WHEN overlay_beumc IS NOT NULL THEN 0              ELSE SDEC_2    END AS SDEC_2,
       CASE WHEN overlay_beumc IS NOT NULL THEN 0              ELSE SDEC_3    END AS SDEC_3,
       CASE WHEN overlay_beumc IS NOT NULL THEN 'XL edit'      ELSE POLY_COMM END AS POLY_COMM
     FROM %s;",
    tmp_xlfinal, tmp_bemjoin
  ))
  try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
  # bemjoin no longer needed — free buffer memory now
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_bemjoin)),  silent = TRUE)

  # ------------------------------------------------------------------
  # 11+12. Erase XL footprint from original vri_bem_tbl and write result_tbl
  #
  # Previous approach (OOM): pre-compute ST_Union_Agg of 90+ large dissolved
  # polygons into one enormous geometry, then CROSS JOIN all of vri_bem_tbl
  # against it.  The ST_Union_Agg step alone exhausted all available RAM.
  #
  # New approach: use a correlated NOT EXISTS against tmp_vrixl (90 rows).
  # For tessellated polygon data (VRI/BEM) every vri_bem row is either:
  #   - fully within the XL footprint  → excluded by NOT EXISTS, replaced by
  #     the re-attributed pieces in tmp_xlfinal
  #   - entirely outside the XL footprint → passed through unchanged
  # No ST_Difference computation or intermediate tmp_origdiff table required.
  # ------------------------------------------------------------------
  xl_final_cols <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", tmp_xlfinal)
  )$name

  final_select_xl <- paste(sapply(vribem_cols, function(col) {
    if (col == "Area_Ha") {
      sprintf("round(ST_Area(\"Shape\") / 10000, 2) AS %s", qi(col))
    } else if (col == "Shape_Area") {
      sprintf("ST_Area(\"Shape\") AS %s", qi(col))
    } else if (col %in% xl_final_cols) {
      qi(col)
    } else {
      sprintf("NULL AS %s", qi(col))
    }
  }), collapse = ",\n    ")

  # Drop any existing VIEW before creating the result as a TABLE
  try(DBI::dbExecute(conn, sprintf("DROP VIEW IF EXISTS %s;", result_tbl)), silent = TRUE)

  t1 <- proc.time()[["elapsed"]]
  # ------------------------------------------------------------------
  # Step 11a: precompute the rowids of vri_bem_tbl rows that fall inside
  # any XL footprint.  Reading only the Shape column (late materialisation)
  # is MUCH faster than a correlated NOT EXISTS that forces DuckDB to buffer
  # the full wide result (including large VRI_Shape blobs) in RAM before
  # committing.  The result is a small integer table (<< 1 MB).
  # ------------------------------------------------------------------
  tmp_xl_rowids <- paste0(pfx, "xlrowids")
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_xl_rowids)), silent = TRUE)
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT DISTINCT v.rowid AS src_rowid
     FROM %s v
     JOIN %s x ON ST_Intersects(x.Shape, v.Shape);",
    tmp_xl_rowids, vri_bem_tbl, tmp_vrixl
  ))
  try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
  logger::log_info(
    "amend_large_polygons_duckdb: step 11a – XL rowids precomputed ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # Step 11b/12: write result_tbl using rowid anti-join (no per-row
  # geometry computation in the outer query's predicate).  DuckDB turns
  # the NOT IN into a hash anti-join that streams rows without requiring
  # the full wide result to be buffered in RAM before commit.
  # ------------------------------------------------------------------
  t2 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     -- Non-XL rows: pass through unchanged.
     SELECT %s FROM %s v
     WHERE v.rowid NOT IN (SELECT src_rowid FROM %s)
     UNION ALL
     -- XL replacement pieces aligned to vri_bem_tbl schema.
     SELECT %s FROM %s;",
    result_tbl,
    paste(qi(vribem_cols), collapse = ", "), vri_bem_tbl, tmp_xl_rowids,
    final_select_xl,                         tmp_xlfinal
  ))
  try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
  # vrixl, xlfinal, and the precomputed rowid table are no longer needed
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_vrixl)),    silent = TRUE)
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_xlfinal)),  silent = TRUE)
  try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", tmp_xl_rowids)), silent = TRUE)
  logger::log_info(
    "amend_large_polygons_duckdb: step 11+12 – XL erased, result written ({round(proc.time()[['elapsed']] - t2, 1)}s)"
  )

  logger::log_info(
    "amend_large_polygons_duckdb: done (total {round(proc.time()[['elapsed']] - t0, 1)}s)"
  )

  invisible(result_tbl)
}
