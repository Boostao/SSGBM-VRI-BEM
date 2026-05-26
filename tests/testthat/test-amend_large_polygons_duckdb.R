library(testthat)
library(duckdb)
library(DBI)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_alp_conn <- function() {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  conn
}

#  Create minimal tables required by amend_large_polygons_duckdb.
#
#  vri_bem_tbl columns (hardcoded by the function's dissolve/overlay SQL):
#    INVENTORY_STANDARD_CD, BCLCS_LV_1-5, SPEC_CD_1,
#    BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE,
#    BEUMC_S1, BEUMC_S2, BEUMC_S3, SDEC_1, SDEC_2, SDEC_3,
#    POLY_COMM, Area_Ha, Shape_Area, Shape
#
#  bem_tbl columns (must include all step-10 EXCLUDE targets):
#    BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, TEIS_ID,
#    BEUMC_S1, BEUMC_S2, BEUMC_S3, SDEC_1, SDEC_2, SDEC_3,
#    POLY_COMM, Shape
#
#  Polygon coordinates use arbitrary metre units.
#  A polygon from (x0,y0)-(x1,y1) has area (x1-x0)*(y1-y0) m².
#    3500 ha = 35,000,000 m²  →  6000×6000 = 36,000,000 m² (large)
#                               1000×1000 =  1,000,000 m² (small)
make_alp_env <- function(conn,
                          vribem_tbl = "VRIBEM_TEST",
                          bem_tbl    = "BEM_TEST",
                          lakes_tbl  = NULL,
                          # Each element: list(x0,y0,x1,y1)
                          vribem_polys = list(
                            list(x0 = 0L,     y0 = 0L,    x1 = 6000L,  y1 = 6000L), # large
                            list(x0 = 20000L, y0 = 0L,    x1 = 21000L, y1 = 1000L)  # small far
                          ),
                          lake_polys = NULL) {

  sq <- function(r) sprintf(
    "ST_GeomFromText('POLYGON ((%d %d, %d %d, %d %d, %d %d, %d %d))')",
    r$x0, r$y0, r$x1, r$y0, r$x1, r$y1, r$x0, r$y1, r$x0, r$y0
  )

  # ---- VRIBEM_TEST -------------------------------------------------------
  vribem_rows_sql <- paste(lapply(vribem_polys, function(r) {
    geom <- sq(r)
    sprintf(
      "SELECT
         CAST('V'   AS VARCHAR)  AS INVENTORY_STANDARD_CD,
         CAST('V'   AS VARCHAR)  AS BCLCS_LV_1,
         CAST('N'   AS VARCHAR)  AS BCLCS_LV_2,
         CAST('B'   AS VARCHAR)  AS BCLCS_LV_3,
         CAST('DC'  AS VARCHAR)  AS BCLCS_LV_4,
         CAST('OT'  AS VARCHAR)  AS BCLCS_LV_5,
         CAST('PL'  AS VARCHAR)  AS SPEC_CD_1,
         CAST('SBS' AS VARCHAR)  AS BGC_ZONE,
         CAST('wk'  AS VARCHAR)  AS BGC_SUBZON,
         CAST('1'   AS VARCHAR)  AS BGC_VRT,
         CAST(NULL  AS VARCHAR)  AS BGC_PHASE,
         CAST('AT'  AS VARCHAR)  AS BEUMC_S1,
         CAST(NULL  AS VARCHAR)  AS BEUMC_S2,
         CAST(NULL  AS VARCHAR)  AS BEUMC_S3,
         CAST(10    AS INTEGER)  AS SDEC_1,
         CAST(0     AS INTEGER)  AS SDEC_2,
         CAST(0     AS INTEGER)  AS SDEC_3,
         CAST(NULL  AS VARCHAR)  AS POLY_COMM,
         ROUND(ST_Area(%s) / 10000, 2) AS Area_Ha,
         ST_Area(%s) AS Shape_Area,
         %s AS Shape",
      geom, geom, geom
    )
  }), collapse = "\nUNION ALL\n")
  DBI::dbExecute(conn,
    sprintf("CREATE OR REPLACE TEMP TABLE %s AS %s", vribem_tbl, vribem_rows_sql))

  # ---- BEM_TEST ----------------------------------------------------------
  # Single BEM polygon covering the full coordinate space so every XL piece
  # gets exactly one BEM intersection in step 9.
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT
       CAST('SBS' AS VARCHAR) AS BGC_ZONE,
       CAST('wk'  AS VARCHAR) AS BGC_SUBZON,
       CAST('1'   AS VARCHAR) AS BGC_VRT,
       CAST(NULL  AS VARCHAR) AS BGC_PHASE,
       CAST(1     AS INTEGER) AS TEIS_ID,
       CAST('AT'  AS VARCHAR) AS BEUMC_S1,
       CAST(NULL  AS VARCHAR) AS BEUMC_S2,
       CAST(NULL  AS VARCHAR) AS BEUMC_S3,
       CAST(10    AS INTEGER) AS SDEC_1,
       CAST(0     AS INTEGER) AS SDEC_2,
       CAST(0     AS INTEGER) AS SDEC_3,
       CAST(NULL  AS VARCHAR) AS POLY_COMM,
       ST_GeomFromText('POLYGON ((0 0, 25000 0, 25000 25000, 0 25000, 0 0))') AS Shape",
    bem_tbl
  ))

  # ---- Optional lake overlay table ----------------------------------------
  if (!is.null(lakes_tbl) && !is.null(lake_polys) && length(lake_polys) > 0L) {
    lake_rows_sql <- paste(lapply(lake_polys, function(r) {
      sprintf("SELECT %s AS Shape", sq(r))
    }), collapse = "\nUNION ALL\n")
    DBI::dbExecute(conn,
      sprintf("CREATE OR REPLACE TEMP TABLE %s AS %s", lakes_tbl, lake_rows_sql))
  }

  invisible(conn)
}

# ---------------------------------------------------------------------------
# Test 1: No large polygons – function exits early
# ---------------------------------------------------------------------------
test_that("amend_large_polygons_duckdb exits early when no polygon > 3500 ha", {
  conn <- make_alp_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Only small polygons (1000×1000 = 100 ha each)
  make_alp_env(conn,
    vribem_polys = list(list(x0=0L,y0=0L,x1=1000L,y1=1000L))
  )

  expect_no_error(
    amend_large_polygons_duckdb(conn,
      vri_bem_tbl  = "VRIBEM_TEST",
      lakes_tbl    = "LAKES_NONE",
      glaciers_tbl = "GLACIERS_NONE",
      wetlands_tbl = "WETLANDS_NONE",
      bem_tbl      = "BEM_TEST",
      result_tbl   = "RESULT_EARLY")
  )

  # result_tbl is a copy of vri_bem_tbl (1 row, unchanged)
  n <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_EARLY")$n
  expect_equal(n, 1L)
})

# ---------------------------------------------------------------------------
# Test 2: Large polygon processed; result_tbl created and preserve_insertion_order restored
# ---------------------------------------------------------------------------
test_that("amend_large_polygons_duckdb processes large polygon and restores preserve_insertion_order", {
  conn <- make_alp_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Default env: one large (6000×6000 = 3600 ha) + one small far polygon
  make_alp_env(conn)

  # Setting should be 'true' by default
  before_val <- DBI::dbGetQuery(
    conn, "SELECT value FROM duckdb_settings() WHERE name = 'preserve_insertion_order'"
  )$value
  expect_equal(before_val, "true")

  expect_no_error(
    amend_large_polygons_duckdb(conn,
      vri_bem_tbl  = "VRIBEM_TEST",
      lakes_tbl    = "LAKES_NONE",
      glaciers_tbl = "GLACIERS_NONE",
      wetlands_tbl = "WETLANDS_NONE",
      bem_tbl      = "BEM_TEST",
      result_tbl   = "RESULT_LARGE")
  )

  # preserve_insertion_order must be restored to true
  after_val <- DBI::dbGetQuery(
    conn, "SELECT value FROM duckdb_settings() WHERE name = 'preserve_insertion_order'"
  )$value
  expect_equal(after_val, "true")

  # result_tbl must exist
  n_result <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_LARGE")$n
  # At minimum: the far-away small polygon (unchanged) + at least one XL piece from BEM join
  expect_gte(n_result, 2L)
})

test_that("amend_large_polygons_duckdb preserves a pre-existing false preserve_insertion_order setting", {
  conn <- make_alp_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_alp_env(conn)
  DBI::dbExecute(conn, "SET preserve_insertion_order = false")

  expect_no_error(
    amend_large_polygons_duckdb(conn,
      vri_bem_tbl  = "VRIBEM_TEST",
      lakes_tbl    = "LAKES_NONE",
      glaciers_tbl = "GLACIERS_NONE",
      wetlands_tbl = "WETLANDS_NONE",
      bem_tbl      = "BEM_TEST",
      result_tbl   = "RESULT_PIO_FALSE")
  )

  after_val <- DBI::dbGetQuery(
    conn, "SELECT value FROM duckdb_settings() WHERE name = 'preserve_insertion_order'"
  )$value
  expect_equal(after_val, "false")
})

# ---------------------------------------------------------------------------
# Test 3: Large polygon with lake overlay – produces LL piece
# ---------------------------------------------------------------------------
test_that("amend_large_polygons_duckdb assigns BEUMC_S1='LL' for lake-overlay pieces", {
  conn <- make_alp_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_alp_env(conn,
    lakes_tbl  = "LAKES_TEST",
    lake_polys = list(list(x0 = 1000L, y0 = 1000L, x1 = 2000L, y1 = 2000L))
  )

  expect_no_error(
    amend_large_polygons_duckdb(conn,
      vri_bem_tbl  = "VRIBEM_TEST",
      lakes_tbl    = "LAKES_TEST",
      glaciers_tbl = "GLACIERS_NONE",
      wetlands_tbl = "WETLANDS_NONE",
      bem_tbl      = "BEM_TEST",
      result_tbl   = "RESULT_LL")
  )

  # At least one row should have BEUMC_S1 = 'LL' (the lake overlay piece)
  n_ll <- DBI::dbGetQuery(
    conn, "SELECT count(*) AS n FROM RESULT_LL WHERE BEUMC_S1 = 'LL'"
  )$n
  expect_gte(n_ll, 1L)

  # The lake piece should have POLY_COMM = 'XL edit'
  n_edit <- DBI::dbGetQuery(
    conn, "SELECT count(*) AS n FROM RESULT_LL WHERE POLY_COMM = 'XL edit'"
  )$n
  expect_gte(n_edit, 1L)
})

test_that("amend_large_polygons_duckdb accepts a lake overlay supplied as a view", {
  conn <- make_alp_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_alp_env(conn,
    lakes_tbl  = "LAKES_TABLE",
    lake_polys = list(list(x0 = 1000L, y0 = 1000L, x1 = 2000L, y1 = 2000L))
  )
  DBI::dbExecute(conn, "CREATE OR REPLACE VIEW LAKES_VIEW AS SELECT * FROM LAKES_TABLE")

  expect_no_error(
    amend_large_polygons_duckdb(conn,
      vri_bem_tbl  = "VRIBEM_TEST",
      lakes_tbl    = "LAKES_VIEW",
      glaciers_tbl = "GLACIERS_NONE",
      wetlands_tbl = "WETLANDS_NONE",
      bem_tbl      = "BEM_TEST",
      result_tbl   = "RESULT_LL_VIEW")
  )

  n_ll <- DBI::dbGetQuery(
    conn, "SELECT count(*) AS n FROM RESULT_LL_VIEW WHERE BEUMC_S1 = 'LL'"
  )$n
  expect_gte(n_ll, 1L)
})

# ---------------------------------------------------------------------------
# Test 4: XL polygon is replaced; far-away polygon is preserved unchanged
# ---------------------------------------------------------------------------
test_that("amend_large_polygons_duckdb removes XL footprint and preserves non-XL rows", {
  conn <- make_alp_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_alp_env(conn)

  amend_large_polygons_duckdb(conn,
    vri_bem_tbl  = "VRIBEM_TEST",
    lakes_tbl    = "LAKES_NONE",
    glaciers_tbl = "GLACIERS_NONE",
    wetlands_tbl = "WETLANDS_NONE",
    bem_tbl      = "BEM_TEST",
    result_tbl   = "RESULT_DIFF")

  # The original large row is removed from tmp_origdiff (difference = empty)
  # and replaced by the BEM-re-joined XL piece.  So result has exactly 2 rows:
  #   1 × small far-away polygon (passed through unchanged)
  #   1 × XL piece from BEM re-join (replaces the original large polygon)
  n_result <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_DIFF")$n
  expect_equal(n_result, 2L)

  # The small far-away polygon must still be present (area ≈ 1,000,000 m²)
  n_small <- DBI::dbGetQuery(
    conn, "SELECT count(*) AS n FROM RESULT_DIFF WHERE Shape_Area BETWEEN 900000 AND 1100000"
  )$n
  expect_equal(n_small, 1L)
})
