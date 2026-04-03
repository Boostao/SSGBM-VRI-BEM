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
        "CREATE OR REPLACE TEMP TABLE %s AS SELECT * FROM %s;",
        result_tbl, vri_bem_tbl
      ))
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

  vribem_non_geom <- vribem_cols[vribem_cols != "Shape"]

  # Explicit SELECT of all vri_bem_tbl non-geom cols qualified with alias v
  orig_non_geom_select <- paste(sprintf("v.%s", qi(vribem_non_geom)), collapse = ",\n      ")

  # ------------------------------------------------------------------
  # Temp table names (prefixed to avoid collisions across calls)
  # ------------------------------------------------------------------
  pfx          <- paste0("_alp_", result_tbl, "_")
  tmp_large    <- paste0(pfx, "large")
  tmp_adj      <- paste0(pfx, "adj")
  tmp_dissolv  <- paste0(pfx, "dissolv")
  tmp_vrixl    <- paste0(pfx, "vrixl")
  tmp_xlunion  <- paste0(pfx, "xlunion")
  tmp_overlay  <- paste0(pfx, "overlay")
  tmp_nonovl   <- paste0(pfx, "nonovl")
  tmp_xlpieces <- paste0(pfx, "xlpieces")
  tmp_bemjoin  <- paste0(pfx, "bemjoin")
  tmp_xlfinal  <- paste0(pfx, "xlfinal")
  tmp_origdiff <- paste0(pfx, "origdiff")

  on.exit({
    for (t in c(tmp_large, tmp_adj, tmp_dissolv, tmp_vrixl, tmp_xlunion,
                tmp_overlay, tmp_nonovl, tmp_xlpieces, tmp_bemjoin,
                tmp_xlfinal, tmp_origdiff)) {
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", t)), silent = TRUE)
    }
  }, add = TRUE)

  # ------------------------------------------------------------------
  # 1.  Extract polygons larger than 3500 ha
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
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
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT DISTINCT v.*
     FROM %s v
     JOIN %s xl ON ST_Touches(v.Shape, xl.Shape);",
    tmp_adj, vri_bem_tbl, tmp_large
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 2 – adjacent polygons found ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 3.  Dissolve combined large + adjacent by VRI identifier attributes
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT
       INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
       SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
       ST_Union_Agg(ST_MakeValid(Shape)) AS Shape
     FROM (
       SELECT INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
              SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, Shape
       FROM %s
       UNION ALL
       SELECT INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
              SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, Shape
       FROM %s
     ) combined
     GROUP BY
       INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5,
       SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE;",
    tmp_dissolv, tmp_large, tmp_adj
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 3 – dissolved ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 4.  Filter dissolved > 3500 ha; extract polygon geometry only
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
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

  if (n_xl == 0L) {
    logger::log_info("amend_large_polygons_duckdb: no XL regions after dissolving. Skipping.")
    if (!identical(result_tbl, vri_bem_tbl)) {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TEMP TABLE %s AS SELECT * FROM %s;",
        result_tbl, vri_bem_tbl
      ))
    }
    return(invisible(result_tbl))
  }

  # ------------------------------------------------------------------
  # 5.  Union of all XL regions – used to erase them from the original table
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT ST_Union_Agg(ST_MakeValid(Shape)) AS xl_union FROM %s;",
    tmp_xlunion, tmp_vrixl
  ))

  # ------------------------------------------------------------------
  # 6.  Overlay pieces: intersect XL with glaciers, lakes, wetlands
  #     Each piece carries updated BCLCS codes and an overlay_beumc label.
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]

  overlay_parts <- list()

  if (tbl_exists(conn, glaciers_tbl)) {
    overlay_parts[["glaciers"]] <- sprintf(
      "SELECT
         xl.INVENTORY_STANDARD_CD,
         'N'  AS BCLCS_LV_1,
         'L'  AS BCLCS_LV_2,
         'A'  AS BCLCS_LV_3,
         'SI' AS BCLCS_LV_4,
         'OT' AS BCLCS_LV_5,
         xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
         'GL' AS overlay_beumc,
         10   AS overlay_dec,
         ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, g.Shape)), 3) AS Shape
       FROM %s xl
       JOIN %s g ON ST_Intersects(xl.Shape, g.Shape)
       WHERE ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, g.Shape)), 3)) > %s",
      tmp_vrixl, glaciers_tbl, tolerance_m2
    )
  }

  if (tbl_exists(conn, lakes_tbl)) {
    overlay_parts[["lakes"]] <- sprintf(
      "SELECT
         xl.INVENTORY_STANDARD_CD,
         'N'  AS BCLCS_LV_1,
         'W'  AS BCLCS_LV_2,
         'W'  AS BCLCS_LV_3,
         NULL AS BCLCS_LV_4,
         'LA' AS BCLCS_LV_5,
         xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
         'LL' AS overlay_beumc,
         10   AS overlay_dec,
         ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, l.Shape)), 3) AS Shape
       FROM %s xl
       JOIN %s l ON ST_Intersects(xl.Shape, l.Shape)
       WHERE ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, l.Shape)), 3)) > %s",
      tmp_vrixl, lakes_tbl, tolerance_m2
    )
  }

  if (tbl_exists(conn, wetlands_tbl)) {
    overlay_parts[["wetlands"]] <- sprintf(
      "SELECT
         xl.INVENTORY_STANDARD_CD,
         'V'  AS BCLCS_LV_1,
         'N'  AS BCLCS_LV_2,
         'W'  AS BCLCS_LV_3,
         NULL AS BCLCS_LV_4,
         NULL AS BCLCS_LV_5,
         xl.SPEC_CD_1, xl.BGC_ZONE, xl.BGC_SUBZON, xl.BGC_VRT, xl.BGC_PHASE,
         'WL' AS overlay_beumc,
         10   AS overlay_dec,
         ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, w.Shape)), 3) AS Shape
       FROM %s xl
       JOIN %s w ON ST_Intersects(xl.Shape, w.Shape)
       WHERE ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Intersection(xl.Shape, w.Shape)), 3)) > %s",
      tmp_vrixl, wetlands_tbl, tolerance_m2
    )
  }

  if (length(overlay_parts) == 0L) {
    # No overlay layers available – create empty placeholder with the right schema
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TEMP TABLE %s AS
       SELECT
         CAST(NULL AS VARCHAR)  AS INVENTORY_STANDARD_CD,
         CAST(NULL AS VARCHAR)  AS BCLCS_LV_1,
         CAST(NULL AS VARCHAR)  AS BCLCS_LV_2,
         CAST(NULL AS VARCHAR)  AS BCLCS_LV_3,
         CAST(NULL AS VARCHAR)  AS BCLCS_LV_4,
         CAST(NULL AS VARCHAR)  AS BCLCS_LV_5,
         CAST(NULL AS VARCHAR)  AS SPEC_CD_1,
         CAST(NULL AS VARCHAR)  AS BGC_ZONE,
         CAST(NULL AS VARCHAR)  AS BGC_SUBZON,
         CAST(NULL AS VARCHAR)  AS BGC_VRT,
         CAST(NULL AS VARCHAR)  AS BGC_PHASE,
         CAST(NULL AS VARCHAR)  AS overlay_beumc,
         CAST(NULL AS INTEGER)  AS overlay_dec,
         CAST(NULL AS GEOMETRY) AS Shape
       WHERE FALSE;",
      tmp_overlay
    ))
  } else {
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TEMP TABLE %s AS\n%s;",
      tmp_overlay,
      paste(overlay_parts, collapse = "\nUNION ALL\n")
    ))
  }

  logger::log_info(
    "amend_large_polygons_duckdb: step 6 – overlay pieces created ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 7.  Non-overlay residual: XL area minus the union of all overlay shapes
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
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
     CROSS JOIN (
       SELECT ST_Union_Agg(ST_MakeValid(Shape)) AS overlay_geom
       FROM %s
     ) ov
     WHERE CASE
       WHEN ov.overlay_geom IS NULL THEN ST_Area(xl.Shape)
       ELSE ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Difference(xl.Shape, ov.overlay_geom)), 3))
     END > %s;",
    tmp_nonovl, tmp_vrixl, tmp_overlay, tolerance_m2
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 7 – non-overlay residuals ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 8.  Combine overlay + residual pieces
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT * FROM %s
     UNION ALL
     SELECT * FROM %s;",
    tmp_xlpieces, tmp_overlay, tmp_nonovl
  ))

  # ------------------------------------------------------------------
  # 9.  Re-join XL pieces with BEM
  #     One row per (XL piece × BEM polygon intersection) — mirrors vribem_view.
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]

  bem_other_select <- paste(sprintf("b.%s", qi(bem_other)), collapse = ",\n         ")

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
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
  logger::log_info(
    "amend_large_polygons_duckdb: step 9 – BEM re-join done ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 10. Apply overlay overrides: BEUMC_S1, BEUMC_S2/S3, SDEC, POLY_COMM
  # ------------------------------------------------------------------
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
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

  # ------------------------------------------------------------------
  # 11. Subtract XL footprint from original vri_bem_tbl rows
  # ------------------------------------------------------------------
  t1 <- proc.time()[["elapsed"]]
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     -- Polygons overlapping XL region: keep only the difference (trim XL area)
     SELECT
       %s,
       ST_CollectionExtract(ST_MakeValid(ST_Difference(v.Shape, u.xl_union)), 3) AS Shape
     FROM %s v
     CROSS JOIN %s u
     WHERE ST_Intersects(v.Shape, u.xl_union)
       AND ST_Area(ST_CollectionExtract(ST_MakeValid(ST_Difference(v.Shape, u.xl_union)), 3)) > %s
     UNION ALL
     -- Polygons not overlapping XL region: pass through unchanged
     SELECT %s, v.Shape
     FROM %s v
     WHERE NOT EXISTS (
       SELECT 1 FROM %s u WHERE ST_Intersects(v.Shape, u.xl_union)
     );",
    tmp_origdiff,
    orig_non_geom_select, vri_bem_tbl, tmp_xlunion, tolerance_m2,
    orig_non_geom_select, vri_bem_tbl, tmp_xlunion
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 11 – originals erased/kept ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  # ------------------------------------------------------------------
  # 12. Align XL-final columns to vri_bem_tbl schema and write result_tbl
  #     Columns present in vri_bem_tbl but absent from tmp_xlfinal are NULL,
  #     except Area_Ha and Shape_Area which are recomputed from the new Shape.
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
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT %s FROM %s
     UNION ALL
     SELECT %s FROM %s;",
    result_tbl,
    paste(qi(vribem_cols), collapse = ", "), tmp_origdiff,
    final_select_xl,                         tmp_xlfinal
  ))
  logger::log_info(
    "amend_large_polygons_duckdb: step 12 – result combined ({round(proc.time()[['elapsed']] - t1, 1)}s)"
  )

  logger::log_info(
    "amend_large_polygons_duckdb: done (total {round(proc.time()[['elapsed']] - t0, 1)}s)"
  )

  invisible(result_tbl)
}
