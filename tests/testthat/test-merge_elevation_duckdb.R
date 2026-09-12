library(testthat)
library(duckdb)
library(DBI)
library(sf)
library(terra)
library(arrow)
library(data.table)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a tiny in-memory DuckDB connection with the spatial extension loaded
# and a minimal VRI-BEM-like table.
make_test_conn <- function() {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  conn
}

# Build a simple 10x10 elevation raster centred on (0, 0) in a generic
# projected CRS (EPSG:32610 UTM zone 10N) with uniform values so that the
# expected aggregates are deterministic.
# slope_val_deg is converted to radians unless slope_val_rad is provided directly
# (slope_val_rad allows injecting values > pi/2 for testing the >100% slope branch).
make_uniform_raster <- function(elev_val = 1600,
                                slope_val_deg = 30,
                                aspect_val_deg = 90,
                                tri_val = 7,
                                slope_val_rad = NULL) {

  # 10x10 cells, 10 m each, origin at (500000, 5000000)
  r_elev   <- terra::rast(nrows = 10, ncols = 10,
                          xmin = 500000, xmax = 500100,
                          ymin = 5000000, ymax = 5000100,
                          crs = "EPSG:32610")
  terra::values(r_elev) <- elev_val
  names(r_elev) <- "dem"

  slope_rad  <- if (!is.null(slope_val_rad)) slope_val_rad else slope_val_deg * pi / 180
  aspect_rad <- aspect_val_deg * pi / 180

  r_slope  <- terra::rast(r_elev); terra::values(r_slope)  <- slope_rad;  names(r_slope)  <- "slope"
  r_aspect <- terra::rast(r_elev); terra::values(r_aspect) <- aspect_rad; names(r_aspect) <- "aspect"
  r_tri    <- terra::rast(r_elev); terra::values(r_tri)    <- tri_val;    names(r_tri)    <- "TRI"

  list(
    elev    = r_elev,
    terrain = c(r_slope, r_aspect, r_tri)
  )
}

# Build one polygon covering the whole raster extent as a DuckDB table.
# `extra` is a named list of additional columns (character scalar values).
make_test_table <- function(conn,
                            tbl_name = "VRI_BEM_TEST",
                            beumc_s1 = "AT",
                            beumc_s2 = NA_character_,
                            beumc_s3 = NA_character_,
                            bgc_zone = "SBS") {

  # Polygon covering raster extent (EPSG:32610)
  poly_wkt <- "POLYGON ((500000 5000000, 500100 5000000, 500100 5000100, 500000 5000100, 500000 5000000))"

  beumc_s2_sql <- if (is.na(beumc_s2)) "NULL" else sprintf("'%s'", beumc_s2)
  beumc_s3_sql <- if (is.na(beumc_s3)) "NULL" else sprintf("'%s'", beumc_s3)

  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT
       '%s'::VARCHAR        AS BEUMC_S1,
       %s::VARCHAR          AS BEUMC_S2,
       %s::VARCHAR          AS BEUMC_S3,
       '%s'::VARCHAR        AS BGC_ZONE,
       ST_GeomFromText('%s') AS Shape",
    tbl_name,
    beumc_s1, beumc_s2_sql, beumc_s3_sql,
    bgc_zone,
    poly_wkt
  ))

  invisible(conn)
}

# Thin wrapper: run merge_elevation_duckdb and return the result as a data.frame
run_and_fetch <- function(conn, tbl_name, rasters,
                          elevation_threshold = 1500,
                          result_tbl = "VRIBEM_ELEV_RESULT") {
  ssgbm::merge_elevation_duckdb(
    conn               = conn,
    vri_bem_tbl        = tbl_name,
    elev_raster        = rasters$elev,
    elevation_threshold = elevation_threshold,
    terrain_raster     = rasters$terrain,
    result_tbl         = result_tbl
  )
  DBI::dbGetQuery(conn, sprintf("SELECT * EXCLUDE (Shape) FROM %s", result_tbl))
}

make_test_sf <- function(beumc_s1 = "AT",
                         beumc_s2 = NA_character_,
                         beumc_s3 = NA_character_,
                         bgc_zone = "SBS") {
  poly <- sf::st_as_sfc(
    "POLYGON ((500000 5000000, 500100 5000000, 500100 5000100, 500000 5000100, 500000 5000000))",
    crs = 32610
  )

  sf::st_sf(
    BEUMC_S1 = beumc_s1,
    BEUMC_S2 = beumc_s2,
    BEUMC_S3 = beumc_s3,
    BGC_ZONE = bgc_zone,
    geometry = poly
  )
}

# ---------------------------------------------------------------------------
# Tests: ABOVE_ELEV_THOLD
# ---------------------------------------------------------------------------

test_that("ABOVE_ELEV_THOLD is 'Y' when elevation is above threshold", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(elev_val = 1600)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters, elevation_threshold = 1500)

  expect_equal(res$ABOVE_ELEV_THOLD, "Y")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("ABOVE_ELEV_THOLD is 'N' when elevation is below threshold", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(elev_val = 900)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters, elevation_threshold = 1500)

  expect_equal(res$ABOVE_ELEV_THOLD, "N")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("ABOVE_ELEV_THOLD uses custom threshold correctly", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(elev_val = 800)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters, elevation_threshold = 900)

  expect_equal(res$ABOVE_ELEV_THOLD, "N")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("merge_elevation_duckdb aggregates MEAN_TRI", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(tri_val = 12.5)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$MEAN_TRI, 12.5)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("merge_elevation_raster_on_sf aggregates MEAN_TRI", {
  rasters <- make_uniform_raster(tri_val = 12.5)
  vri_bem <- make_test_sf()

  res <- ssgbm::merge_elevation_raster_on_sf(
    elev_raster = rasters$elev,
    vri_bem = vri_bem,
    terrain_raster = rasters$terrain,
    elevation_threshold = 1500
  )

  expect_equal(res$MEAN_TRI, 12.5)
})

# ---------------------------------------------------------------------------
# Tests: SLOPE_MOD – non-CWH/MH zones
# ---------------------------------------------------------------------------

# slope 30 deg -> ~33 % slope; aspect 90 deg (east, cool, 0-134 band) -> 'k'
test_that("SLOPE_MOD 'k': non-CWH, cool aspect, 25-100% slope", {
  conn    <- make_test_conn()
  # slope ~33 % (30 deg), aspect 90 deg (cool side)
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "k")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope 15 deg -> ~17 % slope; aspect 90 deg (cool) -> 'j'
test_that("SLOPE_MOD 'j': non-CWH, cool aspect, 10-25% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 15, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "j")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope_val_rad = 1.6 (> pi/2) gives MEAN_SLOPE ~101.9% – triggers 'q' branch
test_that("SLOPE_MOD 'q': non-CWH, cool aspect, >100% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_rad = 1.6, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "q")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope 30 deg, aspect 180 deg (warm) -> 'w'
test_that("SLOPE_MOD 'w': non-CWH, warm aspect, 25-100% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 180)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "w")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope 15 deg, aspect 180 deg (warm) -> 'j'
test_that("SLOPE_MOD 'j': non-CWH, warm aspect, 10-25% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 15, aspect_val_deg = 180)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "j")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope_val_rad = 1.6, aspect 180 deg (warm) -> 'z'
test_that("SLOPE_MOD 'z': non-CWH, warm aspect, >100% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_rad = 1.6, aspect_val_deg = 180)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "z")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: SLOPE_MOD – CWH zone (higher threshold: 35% instead of 25%)
# ---------------------------------------------------------------------------

# slope 30 deg (~33%), CWH, cool aspect -> 'j' (not 'k', threshold is 35% in CWH)
test_that("SLOPE_MOD 'j': CWH, cool aspect, 10-35% slope (not 'k')", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "CWH")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "j")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope 37 deg (~75%), CWH, cool aspect -> 'k'
test_that("SLOPE_MOD 'k': CWH, cool aspect, 35-100% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 37, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "CWH")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "k")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope 37 deg (~75%), CWH, warm aspect -> 'w'
test_that("SLOPE_MOD 'w': CWH, warm aspect, 35-100% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 37, aspect_val_deg = 180)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "CWH")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "w")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# slope_val_rad = 1.6, MH zone, warm aspect -> 'z'
test_that("SLOPE_MOD 'z': MH zone, warm aspect, >100% slope", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_rad = 1.6, aspect_val_deg = 200)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "MH")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "z")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: no-slope-mod BEUMC codes -> SLOPE_MOD is NA
# ---------------------------------------------------------------------------

test_that("SLOPE_MOD is NA when BEUMC_S1 is a water/no-mod code", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "OW", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_true(is.na(res$SLOPE_MOD))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("SLOPE_MOD is NA when BEUMC_S2 is a no-mod code", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", beumc_s2 = "LL", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_true(is.na(res$SLOPE_MOD))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("SLOPE_MOD is NA when BEUMC_S3 is a no-mod code", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", beumc_s3 = "WL", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_true(is.na(res$SLOPE_MOD))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: result table is written to DuckDB
# ---------------------------------------------------------------------------

test_that("result table exists in DuckDB after function call", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster()
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  out_tbl <- "MY_RESULT_TBL"
  returned_name <- ssgbm::merge_elevation_duckdb(
    conn               = conn,
    vri_bem_tbl        = "VRI_BEM_TEST",
    elev_raster        = rasters$elev,
    elevation_threshold = 1500,
    terrain_raster     = rasters$terrain,
    result_tbl         = out_tbl
  )

  expect_equal(returned_name, out_tbl)
  expect_true(DBI::dbGetQuery(conn, sprintf(
    "SELECT count(*) > 0 AS found FROM duckdb_tables() WHERE table_name = '%s'",
    out_tbl
  ))$found)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("merge_elevation_duckdb rasterizes polygon ids in batches", {
  conn <- make_test_conn()
  rasters <- make_uniform_raster(elev_val = 1600, slope_val_deg = 30, aspect_val_deg = 90)
  old_opt <- options(
    ssgbm.merge_elevation_batch_size = 1L,
    ssgbm.merge_elevation_export_blocks = 2L
  )
  on.exit(options(old_opt), add = TRUE)

  DBI::dbExecute(conn, "
    CREATE OR REPLACE TEMP TABLE VRI_BEM_BATCH_TEST AS
    SELECT * FROM (
      SELECT
        'AT'::VARCHAR AS BEUMC_S1,
        NULL::VARCHAR AS BEUMC_S2,
        NULL::VARCHAR AS BEUMC_S3,
        'SBS'::VARCHAR AS BGC_ZONE,
        ST_GeomFromText('POLYGON ((500000 5000000, 500050 5000000, 500050 5000100, 500000 5000100, 500000 5000000))') AS Shape
      UNION ALL
      SELECT
        'AT'::VARCHAR AS BEUMC_S1,
        NULL::VARCHAR AS BEUMC_S2,
        NULL::VARCHAR AS BEUMC_S3,
        'SBS'::VARCHAR AS BGC_ZONE,
        ST_GeomFromText('POLYGON ((500050 5000000, 500100 5000000, 500100 5000100, 500050 5000100, 500050 5000000))') AS Shape
    ) src
  ")

  res <- run_and_fetch(conn, "VRI_BEM_BATCH_TEST", rasters, result_tbl = "VRI_BEM_BATCH_RESULT")

  expect_equal(nrow(res), 2L)
  expect_equal(res$ELEV, c(1600, 1600), tolerance = 1e-6)
  expect_equal(res$ABOVE_ELEV_THOLD, c("Y", "Y"))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("ELEV, MEAN_SLOPE, MEAN_ASP columns are present in result", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(elev_val = 800, slope_val_deg = 20, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_true(all(c("ELEV", "MEAN_SLOPE", "MEAN_ASP") %in% names(res)))
  expect_equal(res$ELEV, 800, tolerance = 1e-6)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: slope below 10% -> no modifier (SLOPE_MOD NA)
# ---------------------------------------------------------------------------

test_that("SLOPE_MOD is NA when slope is below 10%", {
  conn    <- make_test_conn()
  # slope 5 deg -> ~5.5%, below 10% threshold
  rasters <- make_uniform_raster(slope_val_deg = 5, aspect_val_deg = 90)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_true(is.na(res$SLOPE_MOD))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: 285-359 aspect band (cool, wrap-around end)
# ---------------------------------------------------------------------------

test_that("SLOPE_MOD 'k': aspect in 285-359 range (cool wrap-around)", {
  conn    <- make_test_conn()
  rasters <- make_uniform_raster(slope_val_deg = 30, aspect_val_deg = 320)
  make_test_table(conn, beumc_s1 = "AT", bgc_zone = "SBS")

  res <- run_and_fetch(conn, "VRI_BEM_TEST", rasters)

  expect_equal(res$SLOPE_MOD, "k")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})
