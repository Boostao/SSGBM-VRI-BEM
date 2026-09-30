library(testthat)
library(duckdb)
library(DBI)

make_conn <- function() {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  conn
}

make_vri_tbl <- function(conn,
                         polygons = list(
                           list(feature_id = 1L, x0 = 0, y0 = 0, x1 = 100, y1 = 100)
                         )) {
  rows <- vapply(polygons, function(p) {
    sprintf(
      "SELECT
         %d AS FEATURE_ID,
         'A' AS BCLCS_LV_1,
         'B' AS BCLCS_LV_2,
         'C' AS BCLCS_LV_3,
         'D' AS BCLCS_LV_4,
         'E' AS BCLCS_LV_5,
         'phase' AS VRI_BEC_PHASE,
         'subzon' AS VRI_BEC_SUBZON,
         '1' AS VRI_BEC_VRT,
         'ZONE' AS VRI_BEC_ZONE,
         10 AS CR_CLOSURE,
         20 AS COV_PCT_1,
         CAST(NULL AS VARCHAR) AS HRVSTDT,
         'STD' AS INVENTORY_STANDARD_CD,
         'LAND' AS LAND_CD_1,
         'VEG' AS LBL_VEGCOV,
         50 AS PROJ_AGE_1,
         2020 AS VRI_SURVEY_YEAR,
         'MESIC' AS SOIL_MOISTURE_REGIME_1,
         'RICH' AS SOIL_NUTRIENT_REGIME,
         'FD' AS SPEC_CD_1,
         CAST(NULL AS VARCHAR) AS SPEC_CD_2,
         CAST(NULL AS VARCHAR) AS SPEC_CD_3,
         CAST(NULL AS VARCHAR) AS SPEC_CD_4,
         CAST(NULL AS VARCHAR) AS SPEC_CD_5,
         CAST(NULL AS VARCHAR) AS SPEC_CD_6,
         100 AS SPEC_PCT_1,
         CAST(NULL AS INTEGER) AS SPEC_PCT_2,
         CAST(NULL AS INTEGER) AS SPEC_PCT_3,
         CAST(NULL AS INTEGER) AS SPEC_PCT_4,
         CAST(NULL AS INTEGER) AS SPEC_PCT_5,
         CAST(NULL AS INTEGER) AS SPEC_PCT_6,
         ST_GeomFromText('POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))') AS Shape",
      p$feature_id,
      p$x0, p$y0,
      p$x1, p$y0,
      p$x1, p$y1,
      p$x0, p$y1,
      p$x0, p$y0
    )
  }, character(1L))

  DBI::dbExecute(
    conn,
    sprintf("CREATE OR REPLACE TEMP TABLE V_VRI AS\n%s", paste(rows, collapse = "\nUNION ALL\n"))
  )
}

make_pem_tbl <- function(conn,
                         polygons = list(
                           list(pred_class = "forested", x0 = 0, y0 = 0, x1 = 60, y1 = 100)
                         )) {
  if (length(polygons) == 0L) {
    DBI::dbExecute(conn, "
      CREATE OR REPLACE TEMP TABLE PEM AS
      SELECT
        CAST(NULL AS VARCHAR) AS PEM_PRED_CLASS,
        ST_GeomFromText('POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))') AS Shape
      WHERE 1 = 0
    ")
    return(invisible(conn))
  }

  rows <- vapply(polygons, function(p) {
    sprintf(
      "SELECT
         '%s' AS PEM_PRED_CLASS,
         ST_GeomFromText('POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))') AS Shape",
      p$pred_class,
      p$x0, p$y0,
      p$x1, p$y0,
      p$x1, p$y1,
      p$x0, p$y1,
      p$x0, p$y0
    )
  }, character(1L))

  DBI::dbExecute(
    conn,
    sprintf("CREATE OR REPLACE TEMP TABLE PEM AS\n%s", paste(rows, collapse = "\nUNION ALL\n"))
  )
}

run_merge <- function(conn) {
  merge_vri_pem_duckdb(conn)
  DBI::dbGetQuery(
    conn,
    "SELECT FEATURE_ID, PEM_PRED_CLASS, Shape_Area, VRI_Area FROM VRI_AND_PEM ORDER BY FEATURE_ID, Shape_Area"
  )
}

test_that("partial PEM overlap yields intersection and VRI remainder rows", {
  conn <- make_conn()
  make_vri_tbl(conn)
  make_pem_tbl(conn, polygons = list(
    list(pred_class = "forested", x0 = 0, y0 = 0, x1 = 60, y1 = 100)
  ))

  res <- run_merge(conn)

  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$PEM_PRED_CLASS[1]))
  expect_equal(res$PEM_PRED_CLASS[2], "forested")
  expect_equal(res$Shape_Area, c(4000, 6000), tolerance = 1e-6)
  expect_true(all(res$VRI_Area == 10000))

  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("VRI polygons with no PEM overlap are retained with original geometry", {
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(feature_id = 1L, x0 = 0, y0 = 0, x1 = 100, y1 = 100),
    list(feature_id = 2L, x0 = 200, y0 = 0, x1 = 300, y1 = 100)
  ))
  make_pem_tbl(conn, polygons = list(
    list(pred_class = "forested", x0 = 0, y0 = 0, x1 = 60, y1 = 100)
  ))

  res <- run_merge(conn)
  no_overlap <- res[res$FEATURE_ID == 2L, , drop = FALSE]

  expect_equal(nrow(no_overlap), 1L)
  expect_true(is.na(no_overlap$PEM_PRED_CLASS))
  expect_equal(no_overlap$Shape_Area, 10000, tolerance = 1e-6)
  expect_equal(no_overlap$VRI_Area, 10000, tolerance = 1e-6)

  DBI::dbDisconnect(conn, shutdown = TRUE)
})