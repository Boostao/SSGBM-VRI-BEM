library(testthat)
library(duckdb)
library(DBI)

make_csl_conn <- function() {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  conn
}

make_csl_env <- function(conn,
                         vribem_tbl = "VRIBEM_TEST",
                         lakes_tbl = "LAKES_TEST") {
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT * FROM (
       SELECT
         CAST('AT' AS VARCHAR) AS BEUMC_S1,
         CAST('V' AS VARCHAR) AS BCLCS_LV_1,
         CAST('N' AS VARCHAR) AS BCLCS_LV_2,
         CAST('B' AS VARCHAR) AS BCLCS_LV_3,
         CAST('DC' AS VARCHAR) AS BCLCS_LV_4,
         CAST('OT' AS VARCHAR) AS BCLCS_LV_5,
         CAST('PL' AS VARCHAR) AS SPEC_CD_1,
         CAST(100 AS DOUBLE) AS SPEC_PCT_1,
         CAST(NULL AS VARCHAR) AS lbl_edit,
         CAST(7 AS INTEGER) AS SDEC_1,
         CAST(2 AS INTEGER) AS SDEC_2,
         CAST(1 AS INTEGER) AS SDEC_3,
         CAST('M' AS VARCHAR) AS SITE_M3A,
         ROUND(ST_Area(Shape) / 10000, 2) AS Area_Ha,
         ST_Area(Shape) AS Shape_Area,
         Shape
       FROM (SELECT ST_GeomFromText('POLYGON ((0 0, 1000 0, 1000 1000, 0 1000, 0 0))') AS Shape)

       UNION ALL

       SELECT
         CAST('LL' AS VARCHAR) AS BEUMC_S1,
         CAST('N' AS VARCHAR) AS BCLCS_LV_1,
         CAST('W' AS VARCHAR) AS BCLCS_LV_2,
         CAST(NULL AS VARCHAR) AS BCLCS_LV_3,
         CAST(NULL AS VARCHAR) AS BCLCS_LV_4,
         CAST('LA' AS VARCHAR) AS BCLCS_LV_5,
         CAST(NULL AS VARCHAR) AS SPEC_CD_1,
         CAST(NULL AS DOUBLE) AS SPEC_PCT_1,
         CAST(NULL AS VARCHAR) AS lbl_edit,
         CAST(4 AS INTEGER) AS SDEC_1,
         CAST(3 AS INTEGER) AS SDEC_2,
         CAST(3 AS INTEGER) AS SDEC_3,
         CAST('X' AS VARCHAR) AS SITE_M3A,
         ROUND(ST_Area(Shape) / 10000, 2) AS Area_Ha,
         ST_Area(Shape) AS Shape_Area,
         Shape
       FROM (SELECT ST_GeomFromText('POLYGON ((2000 0, 2600 0, 2600 600, 2000 600, 2000 0))') AS Shape)

       UNION ALL

       SELECT
         CAST('AT' AS VARCHAR) AS BEUMC_S1,
         CAST('V' AS VARCHAR) AS BCLCS_LV_1,
         CAST('N' AS VARCHAR) AS BCLCS_LV_2,
         CAST('B' AS VARCHAR) AS BCLCS_LV_3,
         CAST('DC' AS VARCHAR) AS BCLCS_LV_4,
         CAST('OT' AS VARCHAR) AS BCLCS_LV_5,
         CAST('SX' AS VARCHAR) AS SPEC_CD_1,
         CAST(80 AS DOUBLE) AS SPEC_PCT_1,
         CAST(NULL AS VARCHAR) AS lbl_edit,
         CAST(10 AS INTEGER) AS SDEC_1,
         CAST(0 AS INTEGER) AS SDEC_2,
         CAST(0 AS INTEGER) AS SDEC_3,
         CAST('Z' AS VARCHAR) AS SITE_M3A,
         ROUND(ST_Area(Shape) / 10000, 2) AS Area_Ha,
         ST_Area(Shape) AS Shape_Area,
         Shape
       FROM (SELECT ST_GeomFromText('POLYGON ((5000 0, 6000 0, 6000 1000, 5000 1000, 5000 0))') AS Shape)
     ) src;",
    vribem_tbl
  ))

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT ST_GeomFromText('POLYGON ((0 0, 500 0, 500 500, 0 500, 0 0))') AS Shape;",
    lakes_tbl
  ))

  invisible(conn)
}

test_that("correct_small_lakes_duckdb splits only candidate rows and preserves others", {
  conn <- make_csl_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_csl_env(conn)

  expect_no_error(
    correct_small_lakes_duckdb(conn,
      vri_bem_tbl = "VRIBEM_TEST",
      lakes_tbl = "LAKES_TEST",
      batch_size = 1L,
      result_tbl = "RESULT_CSL")
  )

  n_rows <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_CSL")$n
  expect_equal(n_rows, 4L)

  n_ls <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_CSL WHERE BEUMC_S1 = 'LS'")$n
  expect_equal(n_ls, 1L)

  n_edit <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_CSL WHERE lbl_edit = 'Corrected with FWA Lakes polygons'")$n
  expect_gte(n_edit, 1L)

  n_far <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_CSL WHERE SPEC_CD_1 = 'SX'")$n
  expect_equal(n_far, 1L)

  known_lake <- DBI::dbGetQuery(conn, "SELECT SDEC_1, SDEC_2, SDEC_3, SITE_M3A FROM RESULT_CSL WHERE Shape_Area = 360000")
  expect_equal(known_lake$SDEC_1, 10)
  expect_equal(known_lake$SDEC_2, 0)
  expect_equal(known_lake$SDEC_3, 0)
  expect_true(is.na(known_lake$SITE_M3A))
})

test_that("correct_small_lakes_duckdb accepts a lakes view", {
  conn <- make_csl_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_csl_env(conn, lakes_tbl = "LAKES_TABLE")
  DBI::dbExecute(conn, "CREATE OR REPLACE VIEW LAKES_VIEW AS SELECT * FROM LAKES_TABLE")

  expect_no_error(
    correct_small_lakes_duckdb(conn,
      vri_bem_tbl = "VRIBEM_TEST",
      lakes_tbl = "LAKES_VIEW",
      batch_size = 1L,
      result_tbl = "RESULT_CSL_VIEW")
  )

  n_ls <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM RESULT_CSL_VIEW WHERE BEUMC_S1 = 'LS'")$n
  expect_equal(n_ls, 1L)
})

test_that("correct_small_lakes_duckdb restores preserve_insertion_order", {
  conn <- make_csl_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_csl_env(conn)
  DBI::dbExecute(conn, "SET preserve_insertion_order = false")

  expect_no_error(
    correct_small_lakes_duckdb(conn,
      vri_bem_tbl = "VRIBEM_TEST",
      lakes_tbl = "LAKES_TEST",
      batch_size = 1L,
      result_tbl = "RESULT_CSL_PIO")
  )

  pio <- DBI::dbGetQuery(conn, "SELECT current_setting('preserve_insertion_order') AS v")$v
  expect_equal(tolower(pio), "false")
})