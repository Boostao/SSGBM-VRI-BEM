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
#' @param mem_limit Character. DuckDB memory limit string (e.g. `"6GB"`) to
#'   use as a floor for the spatial batch operations. If the current connection
#'   limit is already higher, it is left unchanged. By the time this function is
#'   called the buffer pool may be near-full from prior steps (`V_VRI`, `VRIBEM`,
#'   etc.), leaving no headroom for even a small `ST_Intersection` allocation.
#'   Raising the limit lets DuckDB evict clean (checkpointed) pages from the
#'   `V_` tables on demand. Restored to the original value when the function
#'   returns. Default `"6GB"`.
#' @param result_tbl Character. Name of the (temp) table written to `conn`
#'   with the corrected result.  Defaults to `"VRIBEM_CORRECTIONS"`, which
#'   replaces the input table in-place.
#' @return The name of the result table (`result_tbl`) invisibly.
#' @import DBI
#' @import duckdb
#' @export
correct_small_lakes_duckdb <- function(conn,
                                       vri_bem_tbl = "VRIBEM",
                                       lakes_tbl = "V_LAKES",
                                       tolerance_m2 = 10,
                                       batch_size = 500L,
                                       mem_limit = "6GB",
                                       result_tbl = "VRIBEM") {
  stopifnot(inherits(conn, "duckdb_connection"))
  stopifnot(is.character(vri_bem_tbl), length(vri_bem_tbl) == 1L)
  stopifnot(is.character(lakes_tbl), length(lakes_tbl) == 1L)
  stopifnot(is.numeric(tolerance_m2), length(tolerance_m2) == 1L)
  stopifnot(is.character(result_tbl), length(result_tbl) == 1L)

  t0 <- proc.time()[["elapsed"]]
  logger::log_info("correct_small_lakes_duckdb: starting (vri_bem_tbl={vri_bem_tbl})")
  .relation_exists <- function(name) {
    nrow(DBI::dbGetQuery(conn, sprintf(
      "SELECT 1 FROM information_schema.tables WHERE table_name = '%s'\n       UNION ALL\n       SELECT 1 FROM duckdb_views() WHERE view_name = '%s'\n       LIMIT 1;",
      name, name
    ))) > 0L
  }

  # ------------------------------------------------------------------
  # Guard: if lakes table is missing, skip gracefully
  # ------------------------------------------------------------------
  if (!.relation_exists(lakes_tbl)) {
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
  all_cols <- DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", vri_bem_tbl)
  )$name
  non_geom <- all_cols[all_cols != "Shape"]
  spec_cols <- grep("^SPEC_CD_|^SPEC_PCT_", non_geom, value = TRUE)
  qi <- function(x) paste0('"', x, '"')

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
      "Shape" = qc, # already intersection geometry in tmp_iraw
      "Area_Ha" = "round(ST_Area(\"Shape\") / 10000, 2) AS \"Area_Ha\"",
      "Shape_Area" = "ST_Area(\"Shape\") AS \"Shape_Area\"",
      "BEUMC_S1" = "CASE WHEN ST_Area(\"Shape\") < 100000 THEN 'OW' ELSE 'LS' END AS \"BEUMC_S1\"",
      "BCLCS_LV_1" = "'N' AS \"BCLCS_LV_1\"",
      "BCLCS_LV_2" = "'W' AS \"BCLCS_LV_2\"",
      "BCLCS_LV_3" = sprintf("NULL AS %s", qc),
      "BCLCS_LV_4" = sprintf("NULL AS %s", qc),
      "BCLCS_LV_5" = "'LA' AS \"BCLCS_LV_5\"",
      "lbl_edit" = "'Corrected with FWA Lakes polygons' AS \"lbl_edit\"",
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

  # Branch 3: Pass-through rows (already-classified lakes and untouched rows)
  # No column changes needed.
  passthru_exprs <- qi(all_cols)

  fmt_select <- function(exprs) paste(exprs, collapse = ",\n      ")

  # ------------------------------------------------------------------
  # Temp table names
  # ------------------------------------------------------------------
  pfx <- paste0("_csl_", result_tbl, "_")
  tmp_laksrc <- paste0(pfx, "laksrc")
  tmp_nlkids <- paste0(pfx, "nlkids")
  tmp_hitraw <- paste0(pfx, "hitraw")
  tmp_hitids <- paste0(pfx, "hitids")
  tmp_batchu <- paste0(pfx, "batchu")
  tmp_iraw <- paste0(pfx, "iraw") # intersection pieces (raw geometry)
  tmp_diff <- paste0(pfx, "diff") # difference pieces
  tmp_passthru <- paste0(pfx, "passthru") # unchanged pass-through rows

  on.exit(
    {
      for (t in c(tmp_laksrc, tmp_nlkids, tmp_hitraw, tmp_hitids, tmp_batchu, tmp_iraw, tmp_diff, tmp_passthru)) {
        try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", t)), silent = TRUE)
      }
    },
    add = TRUE
  )

  # Raise memory_limit for the duration of this function.  By the time
  # correct_small_lakes_duckdb is called the buffer pool is near-full from all
  # prior steps (V_VRI, VRIBEM, etc.).  A higher ceiling
  # lets DuckDB evict clean (checkpointed) V_ table pages on demand when the
  # ST_Intersection / ST_Difference allocations need new buffer frames.
  # Restored to the original value when the function returns.
  if (!is.null(mem_limit)) {
    orig_mem_limit <- tryCatch(
      DBI::dbGetQuery(
        conn,
        "SELECT value FROM duckdb_settings() WHERE name = 'memory_limit'"
      )$value,
      error = function(e) NULL
    )
    if (!is.null(orig_mem_limit)) {
        cur_bytes <- parse_duckdb_bytes(orig_mem_limit)
        tgt_bytes <- parse_duckdb_bytes(mem_limit)
        if (isTRUE(!is.na(cur_bytes) && !is.na(tgt_bytes) && tgt_bytes > cur_bytes)) {
          try(DBI::dbExecute(conn, sprintf("SET memory_limit = '%s';", mem_limit)),
            silent = TRUE
          )
          on.exit(
            try(DBI::dbExecute(conn, sprintf("SET memory_limit = '%s';", orig_mem_limit)),
              silent = TRUE
            ),
            add = TRUE
          )
        }
    }
  }

  # Disable insertion-order preservation for the heavy geo operations to
  # reduce peak memory usage (restored in on.exit below).
    orig_pio <- tryCatch(
      DBI::dbGetQuery(conn, "SELECT current_setting('preserve_insertion_order') AS v")$v,
      error = function(e) NULL
    )
  DBI::dbExecute(conn, "SET preserve_insertion_order=false;")
  on.exit(
      try(DBI::dbExecute(conn,
        sprintf("SET preserve_insertion_order=%s;",
          if (!is.null(orig_pio) && tolower(orig_pio) == "false") "false" else "true"
        )
      ), silent = TRUE),
    add = TRUE
  )

  # ------------------------------------------------------------------
  # 1.  Prepare lakes source and identify candidate source rows.
  #
  # Only source rows that actually intersect a lake are sent through the
  # expensive geometry path.  This avoids both the giant global lake union and
  # the 1.2M-row non-lake staging table.
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  lakes_src <- lakes_tbl
  if (nrow(DBI::dbGetQuery(conn, sprintf(
    "SELECT 1 FROM duckdb_views() WHERE view_name = '%s'", lakes_tbl
  ))) > 0L) {
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s;",
      tmp_laksrc, lakes_tbl
    ))
    try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    lakes_src <- tmp_laksrc
  }

  lakes_idx <- paste0(lakes_src, "_rtree")
  lakes_idx_exists <- nrow(DBI::dbGetQuery(conn, sprintf(
    "SELECT 1 FROM duckdb_indexes() WHERE index_name = '%s'", lakes_idx
  ))) > 0L
  if (!lakes_idx_exists) {
    n_lakes <- tryCatch(
      DBI::dbGetQuery(conn, sprintf("SELECT count(*) AS n FROM %s;", lakes_src))$n,
      error = function(e) 0L
    )
    if (n_lakes > 0L) {
      tryCatch(
        DBI::dbExecute(conn, sprintf(
          "CREATE INDEX %s ON %s USING RTREE (Shape);",
          lakes_idx, lakes_src
        )),
        error = function(e) logger::log_warn(
          "correct_small_lakes_duckdb: RTREE on {lakes_src} failed: {conditionMessage(e)}"
        )
      )
    }
  }

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT row_number() OVER () AS _rn, rowid AS src_rowid
     FROM %s
     WHERE \"BEUMC_S1\" NOT IN ('LS', 'LL', 'OW');",
    tmp_nlkids, vri_bem_tbl
  ))
  n_nonlake <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s;", tmp_nlkids))$n

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s (src_rowid BIGINT);",
    tmp_hitraw
  ))

  candidate_batch_size <- max(as.integer(batch_size) * 20L, 10000L)
  if (n_nonlake > 0L) {
    for (batch_start in seq(1L, n_nonlake, by = candidate_batch_size)) {
      batch_end <- min(batch_start + candidate_batch_size - 1L, n_nonlake)
      DBI::dbExecute(conn, sprintf(
        "INSERT INTO %s
         SELECT ids.src_rowid
         FROM %s ids
         JOIN %s v ON v.rowid = ids.src_rowid
         WHERE ids._rn BETWEEN %d AND %d
           AND EXISTS (
             SELECT 1 FROM %s l WHERE ST_Intersects(v.\"Shape\", l.\"Shape\")
           );",
        tmp_hitraw, tmp_nlkids, vri_bem_tbl, batch_start, batch_end, lakes_src
      ))
      if (batch_end < n_nonlake) {
        try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
      }
    }
  }

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT row_number() OVER () AS _rn, src_rowid
     FROM %s;",
    tmp_hitids, tmp_hitraw
  ))
  n_hits <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s;", tmp_hitids))$n
  logger::log_info(
    "correct_small_lakes_duckdb: step 1 – {n_hits} lake-intersecting rows identified from {n_nonlake} non-lake rows ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  if (n_hits == 0L) {
    logger::log_info("correct_small_lakes_duckdb: no non-lake rows intersect lakes. Skipping.")
    if (!identical(result_tbl, vri_bem_tbl)) {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TEMP TABLE %s AS SELECT * FROM %s;",
        result_tbl, vri_bem_tbl
      ))
    }
    if (!lakes_idx_exists) {
      try(DBI::dbExecute(conn, sprintf("DROP INDEX IF EXISTS %s;", lakes_idx)), silent = TRUE)
    }
    return(invisible(result_tbl))
  }

  # ------------------------------------------------------------------
  # 2.  Intersection and difference pieces -- batched
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]

  iraw_batch_exprs <- sapply(all_cols, function(col) {
    if (col == "Shape") {
      "ST_CollectionExtract(ST_MakeValid(ST_Intersection(c.\"Shape\", u.lake_union)), 3) AS \"Shape\""
    } else {
      sprintf("c.%s", qi(col))
    }
  })

  diff_batch_geom_exprs <- sapply(all_cols, function(col) {
    if (col == "Shape") {
      "ST_CollectionExtract(ST_MakeValid(ST_Difference(c.\"Shape\", u.lake_union)), 3) AS \"Shape\""
    } else {
      sprintf("c.%s", qi(col))
    }
  })

  empty_schema_sql <- sprintf(
    "SELECT %s FROM %s WHERE FALSE;",
    fmt_select(qi(all_cols)), vri_bem_tbl
  )
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TEMP TABLE %s AS %s", tmp_iraw, empty_schema_sql))
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TEMP TABLE %s AS %s", tmp_diff, empty_schema_sql))

  for (batch_start in seq(1L, n_hits, by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1L, n_hits)

    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TEMP TABLE %s AS
       SELECT
         v.rowid AS src_rowid,
         ST_Union_Agg(ST_MakeValid(l.\"Shape\")) AS lake_union
       FROM %s v
       JOIN %s h ON v.rowid = h.src_rowid
       JOIN %s l ON ST_Intersects(v.\"Shape\", l.\"Shape\")
       WHERE h._rn BETWEEN %d AND %d
       GROUP BY v.rowid;",
      tmp_batchu, vri_bem_tbl, tmp_hitids, lakes_src, batch_start, batch_end
    ))

    DBI::dbExecute(conn, sprintf(
      "INSERT INTO %s
       SELECT *
       FROM (
         SELECT %s
         FROM %s c
         JOIN %s u ON c.rowid = u.src_rowid
       ) sub
       WHERE NOT ST_IsEmpty(\"Shape\") AND ST_Area(\"Shape\") > %s;",
      tmp_iraw,
      fmt_select(iraw_batch_exprs),
      vri_bem_tbl, tmp_batchu, tolerance_m2
    ))

    DBI::dbExecute(conn, sprintf(
      "INSERT INTO %s
       SELECT %s
       FROM (
         SELECT %s
         FROM %s c
         JOIN %s u ON c.rowid = u.src_rowid
       ) sub
       WHERE NOT ST_IsEmpty(\"Shape\") AND ST_Area(\"Shape\") > %s;",
      tmp_diff,
      fmt_select(diff_exprs),
      fmt_select(diff_batch_geom_exprs),
      vri_bem_tbl, tmp_batchu, tolerance_m2
    ))

    if (batch_end < n_hits) {
      try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
    }
  }
  logger::log_info(
    "correct_small_lakes_duckdb: step 2 – intersection+difference done ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 3.  Capture unchanged rows before overwriting result_tbl.
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT %s FROM %s v
     WHERE v.\"BEUMC_S1\" IN ('LS', 'LL', 'OW')
        OR NOT EXISTS (
          SELECT 1 FROM %s h WHERE h.src_rowid = v.rowid
        );",
    tmp_passthru,
    fmt_select(passthru_exprs),
    vri_bem_tbl,
    tmp_hitids
  ))

  # ------------------------------------------------------------------
  # 4.  Assemble result: intersection + difference + pass-through
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  try(DBI::dbExecute(conn, sprintf("DROP VIEW IF EXISTS %s;", result_tbl)), silent = TRUE)

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT %s FROM %s
     UNION ALL
     SELECT * FROM %s
     UNION ALL
     SELECT * FROM %s;",
    result_tbl,
    fmt_select(intersect_exprs), tmp_iraw,
    tmp_diff,
    tmp_passthru
  ))
  if (!lakes_idx_exists) {
    try(DBI::dbExecute(conn, sprintf("DROP INDEX IF EXISTS %s;", lakes_idx)), silent = TRUE)
  }
  logger::log_info(
    "correct_small_lakes_duckdb: step 4 – result assembled ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 5.  Fix SDEC / SITE_M3A for all lake rows
  #     Mirrors the final mutate() in correct_small_lakes().
  # ------------------------------------------------------------------
  set_exprs <- "SDEC_1 = 10"
  if ("SDEC_2" %in% non_geom) set_exprs <- paste0(set_exprs, ", SDEC_2 = 0")
  if ("SDEC_3" %in% non_geom) set_exprs <- paste0(set_exprs, ", SDEC_3 = 0")
  if ("SITE_M3A" %in% non_geom) set_exprs <- paste0(set_exprs, ", SITE_M3A = NULL")

  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET %s WHERE \"BEUMC_S1\" IN ('LS', 'LL', 'OW');",
    result_tbl, set_exprs
  ))
  logger::log_info(
    "correct_small_lakes_duckdb: step 5 – SDEC/SITE_M3A updated"
  )

  logger::log_info(
    "correct_small_lakes_duckdb: done (total {round(proc.time()[['elapsed']] - t0, 1)}s)"
  )

  invisible(result_tbl)
}
