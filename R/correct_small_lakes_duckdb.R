#' Correct small lakes in a DuckDB VRI-BEM table
#'
#' DuckDB-native counterpart of [correct_small_lakes()].
#' Polygons whose `BEUMC_S1` is not already a lake code (`LS`, `LL`, `OW`)
#' are split against the FWA Lakes layer:
#' \itemize{
#'   \item **Intersection pieces** receive an updated `BEUMC_S1` (`OW` for
#'         areas < 10 ha, `LS` otherwise), `BCLCS_LV_*` overrides, and cleared
#'         `SPEC_CD_*` / `SPEC_PCT_*` columns.
#'   \item **Difference pieces** (residual non-lake area) keep their original
#'         attributes; tiny slivers below `tolerance_m2` are discarded.
#'   \item **Already-classified lake polygons** pass through unchanged.
#' }
#' After recombining, all rows where `BEUMC_S1 IN ('LS','LL','OW')` receive
#' `SDEC_1 = 10`, `SDEC_2 = 0`, `SDEC_3 = 0`, and `SITE_M3A = NULL` — matching
#' the clean-up performed at the end of [correct_small_lakes()].
#'
#' @param conn A `duckdb_connection` object (as returned by [init_conn()]).
#' @param vri_bem_tbl Character. Name of the source VRI-BEM table in `conn`.
#'   Default `"VRIBEM_CORRECTIONS"`.
#' @param lakes_tbl Character. Name of the lakes table/view in `conn`.
#'   Default `"V_LAKES"`.
#' @param tolerance_m2 Numeric. Minimum polygon area (m²) to retain after
#'   splitting.  Default `10`.
#' @param batch_size Integer. Number of non-lake VRI rows to process per
#'   iteration when computing intersections and differences.  Smaller values
#'   use less peak memory at the cost of more round-trips; larger values are
#'   faster but require more RAM.  Default `500L`.
#' @param result_tbl Character. Name of the (temp) table written to `conn`
#'   with the corrected result.  Defaults to `"VRIBEM_CORRECTIONS"`, which
#'   replaces the input table in-place.
#' @return The name of the result table (`result_tbl`) invisibly.
#' @import DBI
#' @import duckdb
#' @export
correct_small_lakes_duckdb <- function(conn,
                                       vri_bem_tbl  = "VRIBEM_CORRECTIONS",
                                       lakes_tbl    = "V_LAKES",
                                       tolerance_m2 = 10,
                                       batch_size   = 500L,
                                       result_tbl   = "VRIBEM_CORRECTIONS") {

  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(vri_bem_tbl),  length(vri_bem_tbl)  == 1L)
  stopifnot(is.character(lakes_tbl),    length(lakes_tbl)    == 1L)
  stopifnot(is.numeric(tolerance_m2),   length(tolerance_m2) == 1L)
  stopifnot(is.character(result_tbl),   length(result_tbl)   == 1L)

  t0 <- proc.time()[["elapsed"]]
  logger::log_info("correct_small_lakes_duckdb: starting (vri_bem_tbl={vri_bem_tbl})")

  # ------------------------------------------------------------------
  # Guard: if lakes table is missing, skip gracefully
  # ------------------------------------------------------------------
  if (!tbl_exists(conn, lakes_tbl)) {
    logger::log_warn(
      "correct_small_lakes_duckdb: lakes table '{lakes_tbl}' not found. Skipping."
    )
    if (!identical(result_tbl, vri_bem_tbl)) {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TEMP TABLE %s AS SELECT * FROM %s;",
        result_tbl, vri_bem_tbl
      ))
    }
    return(invisible(result_tbl))
  }

  # ------------------------------------------------------------------
  # Column introspection
  # ------------------------------------------------------------------
  all_cols  <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", vri_bem_tbl)
  )$name
  non_geom  <- all_cols[all_cols != "Shape"]
  spec_cols <- grep("^SPEC_CD_|^SPEC_PCT_", non_geom, value = TRUE)
  qi        <- function(x) paste0('"', x, '"')

  # ------------------------------------------------------------------
  # Build column SELECT expressions for each branch of the UNION ALL.
  # All branches iterate over all_cols so the column ORDER is identical,
  # which is required for UNION ALL positional alignment.
  # ------------------------------------------------------------------

  # Branch 1: Intersection pieces from tmp_iraw
  # - BEUMC_S1:    OW (<10 ha) / LS (>=10 ha) by intersection area
  # - BCLCS_LV_*:  override
  # - SPEC_CD_*/SPEC_PCT_*: NULL
  # - lbl_edit:    'Corrected with FWA Lakes polygons' (if column exists)
  # - Area_Ha / Shape_Area: recomputed from new Shape
  # - everything else: carry original value
  intersect_exprs <- sapply(all_cols, function(col) {
    qc <- qi(col)
    switch(col,
      "Shape"      = qc,   # already intersection geometry in tmp_iraw
      "Area_Ha"    = "round(ST_Area(\"Shape\") / 10000, 2) AS \"Area_Ha\"",
      "Shape_Area" = "ST_Area(\"Shape\") AS \"Shape_Area\"",
      "BEUMC_S1"   = "CASE WHEN ST_Area(\"Shape\") < 100000 THEN 'OW' ELSE 'LS' END AS \"BEUMC_S1\"",
      "BCLCS_LV_1" = "'N' AS \"BCLCS_LV_1\"",
      "BCLCS_LV_2" = "'W' AS \"BCLCS_LV_2\"",
      "BCLCS_LV_3" = sprintf("NULL AS %s", qc),
      "BCLCS_LV_4" = sprintf("NULL AS %s", qc),
      "BCLCS_LV_5" = "'LA' AS \"BCLCS_LV_5\"",
      "lbl_edit"   = "'Corrected with FWA Lakes polygons' AS \"lbl_edit\"",
      {
        if (col %in% spec_cols) sprintf("NULL AS %s", qc) else qc
      }
    )
  })

  # Branch 2: Difference pieces from tmp_diff
  # - Area_Ha / Shape_Area: recomputed from new Shape
  # - everything else (including BEUMC_S1, BCLCS, etc.): carry original
  diff_exprs <- sapply(all_cols, function(col) {
    qc <- qi(col)
    switch(col,
      "Area_Ha"    = "round(ST_Area(\"Shape\") / 10000, 2) AS \"Area_Ha\"",
      "Shape_Area" = "ST_Area(\"Shape\") AS \"Shape_Area\"",
      qc
    )
  })

  # Branch 3: Pass-through (already-classified lake polygons)
  # No column changes needed.
  passthru_exprs <- qi(all_cols)

  fmt_select <- function(exprs) paste(exprs, collapse = ",\n      ")

  # ------------------------------------------------------------------
  # Temp table names
  # ------------------------------------------------------------------
  pfx          <- paste0("_csl_", result_tbl, "_")
  tmp_lku      <- paste0(pfx, "lku")       # lake union geometry
  tmp_src      <- paste0(pfx, "src")       # numbered non-lake source rows
  tmp_iraw     <- paste0(pfx, "iraw")      # intersection pieces (raw geometry)
  tmp_diff     <- paste0(pfx, "diff")      # difference pieces
  tmp_passthru <- paste0(pfx, "passthru")  # known-lake pass-through

  on.exit({
    for (t in c(tmp_lku, tmp_src, tmp_iraw, tmp_diff, tmp_passthru)) {
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", t)), silent = TRUE)
    }
  }, add = TRUE)

  # Disable insertion-order preservation for the heavy geo operations to
  # reduce peak memory usage (restored in on.exit below).
  DBI::dbExecute(conn, "SET preserve_insertion_order=false;")
  on.exit(
    try(DBI::dbExecute(conn, "SET preserve_insertion_order=true;"), silent = TRUE),
    add = TRUE
  )

  # ------------------------------------------------------------------
  # 1.  Union of all lake geometries (used for the ST_Difference step)
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT ST_Union_Agg(ST_MakeValid(Shape)) AS lake_union FROM %s;",
    tmp_lku, lakes_tbl
  ))
  logger::log_info(
    "correct_small_lakes_duckdb: step 1 – lake union ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 2 & 3.  Intersection and difference pieces -- batched
  #
  #  Processing all non-lake VRI rows in a single query can exhaust
  #  memory on large datasets even after reducing per-row work.  The
  #  fix is to materialise the non-lake rows once with a sequential
  #  row-number, then drive ST_Intersection / ST_Difference from R in
  #  chunks of `batch_size` rows, appending results to tmp_iraw /
  #  tmp_diff.  Peak memory per operation is therefore bounded by
  #  batch_size x (average polygon complexity).
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]

  # Materialise all non-lake rows with a stable row number for slicing.
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT row_number() OVER () AS _rn, *
     FROM %s
     WHERE \"BEUMC_S1\" NOT IN ('LS', 'LL', 'OW');",
    tmp_src, vri_bem_tbl
  ))
  n_nonlake <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s;", tmp_src))$n
  logger::log_info(
    "correct_small_lakes_duckdb: step 2 - {n_nonlake} non-lake rows staged ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # Inner-SELECT expressions (alias c = tmp_src, u = tmp_lku).
  # The outer selects (intersect_exprs / diff_exprs) apply attribute
  # overrides and recompute Area_Ha / Shape_Area from the resulting "Shape".
  iraw_batch_exprs <- sapply(all_cols, function(col) {
    if (col == "Shape")
      "ST_CollectionExtract(ST_MakeValid(ST_Intersection(c.\"Shape\", u.lake_union)), 3) AS \"Shape\""
    else
      sprintf("c.%s", qi(col))
  })

  diff_batch_geom_exprs <- sapply(all_cols, function(col) {
    if (col == "Shape")
      paste0(
        "CASE WHEN u.lake_union IS NULL THEN c.\"Shape\" ",
        "ELSE ST_CollectionExtract(ST_MakeValid(ST_Difference(c.\"Shape\", u.lake_union)), 3) ",
        "END AS \"Shape\""
      )
    else
      sprintf("c.%s", qi(col))
  })

  # Create empty result tables with the correct column schema.
  empty_schema_sql <- sprintf(
    "SELECT %s FROM %s WHERE FALSE;",
    fmt_select(qi(all_cols)), vri_bem_tbl
  )
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TEMP TABLE %s AS %s", tmp_iraw, empty_schema_sql))
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TEMP TABLE %s AS %s", tmp_diff,  empty_schema_sql))

  # Batch loop: each iteration processes batch_size rows.
  if (n_nonlake > 0L) {
    for (batch_start in seq(1L, n_nonlake, by = batch_size)) {
      batch_end <- min(batch_start + batch_size - 1L, n_nonlake)

      # Intersection pieces for this chunk.
      # Store raw intersection geometry + original attributes; step 5
      # applies the attribute overrides (BEUMC_S1, BCLCS_LV_*, etc.)
      # via intersect_exprs when assembling the final result.
      DBI::dbExecute(conn, sprintf(
        "INSERT INTO %s
         SELECT *
         FROM (
           SELECT %s
           FROM %s c CROSS JOIN %s u
           WHERE c._rn BETWEEN %d AND %d
             AND ST_Intersects(c.\"Shape\", u.lake_union)
         ) sub
         WHERE NOT ST_IsEmpty(\"Shape\") AND ST_Area(\"Shape\") > %s;",
        tmp_iraw,
        fmt_select(iraw_batch_exprs),
        tmp_src, tmp_lku, batch_start, batch_end, tolerance_m2
      ))

      # Difference pieces for this chunk
      DBI::dbExecute(conn, sprintf(
        "INSERT INTO %s
         SELECT %s
         FROM (
           SELECT %s
           FROM %s c CROSS JOIN %s u
           WHERE c._rn BETWEEN %d AND %d
         ) sub
         WHERE NOT ST_IsEmpty(\"Shape\") AND ST_Area(\"Shape\") > %s;",
        tmp_diff,
        fmt_select(diff_exprs),
        fmt_select(diff_batch_geom_exprs),
        tmp_src, tmp_lku, batch_start, batch_end, tolerance_m2
      ))
    }
  }
  logger::log_info(
    "correct_small_lakes_duckdb: step 3 - intersection+difference done ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 4.  Capture already-classified lake polygons BEFORE overwriting result_tbl
  #     (required when result_tbl == vri_bem_tbl to avoid a self-reference).
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT %s FROM %s
     WHERE \"BEUMC_S1\" IN ('LS', 'LL', 'OW');",
    tmp_passthru,
    fmt_select(passthru_exprs),
    vri_bem_tbl
  ))

  # ------------------------------------------------------------------
  # 5.  Assemble result: intersection + difference + pass-through
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  try(DBI::dbExecute(conn, sprintf("DROP VIEW IF EXISTS %s;", result_tbl)), silent = TRUE)

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     -- Intersection pieces with attribute overrides
     SELECT %s FROM %s
     UNION ALL
     -- Difference (residual non-lake) pieces
     SELECT * FROM %s
     UNION ALL
     -- Already-classified lake polygons, unchanged
     SELECT * FROM %s;",
    result_tbl,
    fmt_select(intersect_exprs), tmp_iraw,
    tmp_diff,
    tmp_passthru
  ))
  logger::log_info(
    "correct_small_lakes_duckdb: step 5 – result assembled ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 6.  Fix SDEC / SITE_M3A for all lake rows
  #     Mirrors the final mutate() in correct_small_lakes().
  # ------------------------------------------------------------------
  set_exprs <- "SDEC_1 = 10"
  if ("SDEC_2"   %in% non_geom) set_exprs <- paste0(set_exprs, ", SDEC_2 = 0")
  if ("SDEC_3"   %in% non_geom) set_exprs <- paste0(set_exprs, ", SDEC_3 = 0")
  if ("SITE_M3A" %in% non_geom) set_exprs <- paste0(set_exprs, ", SITE_M3A = NULL")

  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET %s WHERE \"BEUMC_S1\" IN ('LS', 'LL', 'OW');",
    result_tbl, set_exprs
  ))
  logger::log_info(
    "correct_small_lakes_duckdb: step 6 – SDEC/SITE_M3A updated"
  )

  logger::log_info(
    "correct_small_lakes_duckdb: done (total {round(proc.time()[['elapsed']] - t0, 1)}s)"
  )

  invisible(result_tbl)
}
