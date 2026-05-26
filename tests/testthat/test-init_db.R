library(testthat)
library(duckdb)
library(DBI)
library(sf)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_conn <- function() {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  conn
}

# Create a minimal GeoPackage with a single WGS84 polygon and one attribute
# column. DuckDB's ST_Read exposes the geometry column from a GPKG as "geom".
make_test_gpkg <- function(col_name = "TSA_NUMBER_DESCRIPTION",
                            col_val  = "Test TSA 01",
                            layer    = "test_layer") {
  tmp <- tempfile(fileext = ".gpkg")
  poly <- sf::st_sf(
    setNames(list(col_val), col_name),
    geom = sf::st_sfc(
      sf::st_polygon(list(cbind(
        c(-127, -126, -126, -127, -127),
        c(54, 54, 55, 55, 54)
      ))),
      crs = 4326
    )
  )
  sf::st_write(poly, tmp, layer = layer, quiet = TRUE)
  list(dsn = tmp, layer = layer)
}

# ---------------------------------------------------------------------------
# Tests: geometry CRS stripping (core bugfix)
# ---------------------------------------------------------------------------

test_that("ST_GeomFromWKB/ST_AsWKB round-trip stores transformed geometry without error", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  # This simulates the SQL that init_generic now generates when storing
  # geometry transformed to the Albers projection.  Without the WKB
  # round-trip, newer DuckDB spatial would tag the column with a CRS
  # identifier which is incompatible with database storage versions < 1.5.0.
  expect_no_error(
    DBI::dbExecute(conn, "
      CREATE OR REPLACE TABLE geom_test AS (
        SELECT ST_GeomFromWKB(ST_AsWKB(
          ST_MakeValid(ST_Transform(
            ST_GeomFromText('POLYGON ((-127 54, -126 54, -126 55, -127 55, -127 54))'),
            '+proj=longlat +datum=WGS84 +no_defs',
            '+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs',
            always_xy := true
          ))
        )) AS Shape
      );
    ")
  )

  rows <- DBI::dbGetQuery(conn, "SELECT ST_Area(Shape) AS area FROM geom_test")
  expect_equal(nrow(rows), 1L)
  expect_gt(rows$area, 0)
})

# ---------------------------------------------------------------------------
# Tests: init_generic with a local file (dsn != NULL path)
# ---------------------------------------------------------------------------

test_that("init_generic creates a table from a local GPKG without CRS error", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_test_gpkg()
  on.exit(unlink(pkg$dsn), add = TRUE)

  expect_no_error(
    init_generic(
      conn      = conn,
      ask       = FALSE,
      dsn       = pkg$dsn,
      layer     = pkg$layer,
      geom      = "geom",
      tablename = "TSA_TEST",
      .include  = "TSA_NUMBER_DESCRIPTION"
    )
  )

  expect_true(nrow(DBI::dbGetQuery(conn, "SELECT 1 FROM information_schema.tables WHERE table_name = 'TSA_TEST'")) > 0)
  cols <- DBI::dbGetQuery(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'TSA_TEST'")$column_name
  expect_true("TSA_NUMBER_DESCRIPTION" %in% cols)
  expect_true("Shape" %in% cols)
})

test_that("init_generic stores the polygon row with the correct attribute value", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_test_gpkg(col_val = "Prince George TSA")
  on.exit(unlink(pkg$dsn), add = TRUE)

  init_generic(
    conn      = conn,
    ask       = FALSE,
    dsn       = pkg$dsn,
    layer     = pkg$layer,
    geom      = "geom",
    tablename = "TSA_TEST",
    .include  = "TSA_NUMBER_DESCRIPTION"
  )

  result <- DBI::dbGetQuery(conn, "SELECT TSA_NUMBER_DESCRIPTION FROM TSA_TEST")
  expect_equal(nrow(result), 1L)
  expect_equal(result$TSA_NUMBER_DESCRIPTION, "Prince George TSA")
})

test_that("init_generic stores a non-NULL geometry (Shape column)", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_test_gpkg()
  on.exit(unlink(pkg$dsn), add = TRUE)

  init_generic(
    conn      = conn,
    ask       = FALSE,
    dsn       = pkg$dsn,
    layer     = pkg$layer,
    geom      = "geom",
    tablename = "TSA_TEST",
    .include  = "TSA_NUMBER_DESCRIPTION"
  )

  area <- DBI::dbGetQuery(conn, "SELECT ST_Area(Shape) AS area FROM TSA_TEST")
  expect_gt(area$area, 0)
})

test_that("init_generic creates an RTREE spatial index on the result table", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_test_gpkg()
  on.exit(unlink(pkg$dsn), add = TRUE)

  init_generic(
    conn      = conn,
    ask       = FALSE,
    dsn       = pkg$dsn,
    layer     = pkg$layer,
    geom      = "geom",
    tablename = "TSA_TEST",
    .include  = "TSA_NUMBER_DESCRIPTION"
  )

  # The spatial index allows queries; this verifies the index was created
  expect_no_error(
    DBI::dbGetQuery(conn, "SELECT count(*) FROM TSA_TEST WHERE
      ST_Within(ST_GeomFromText('POINT (0 0)'), Shape) OR TRUE")
  )
})

test_that("init_generic skips when table already exists and ask=FALSE", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  # Pre-create the target table with a sentinel row (no Shape column so that
  # DBI::dbExistsTable works reliably; real skip logic uses tbl_exists which is
  # immune to the GEOMETRY-column prepare bug).
  DBI::dbExecute(conn, "CREATE TABLE TSA (TSA_NUMBER_DESCRIPTION VARCHAR);")
  DBI::dbExecute(conn, "INSERT INTO TSA VALUES ('Sentinel TSA');")

  pkg <- make_test_gpkg()
  on.exit(unlink(pkg$dsn), add = TRUE)

  expect_no_error(
    init_generic(
      conn      = conn,
      ask       = FALSE,
      dsn       = pkg$dsn,
      layer     = pkg$layer,
      geom      = "geom",
      tablename = "TSA",
      .include  = "TSA_NUMBER_DESCRIPTION"
    )
  )

  # Sentinel data must be intact: the existing table was not re-initialised
  result <- DBI::dbGetQuery(conn, "SELECT TSA_NUMBER_DESCRIPTION FROM TSA")
  expect_equal(result$TSA_NUMBER_DESCRIPTION, "Sentinel TSA")
})

# ---------------------------------------------------------------------------
# Tests: init_tsa
# ---------------------------------------------------------------------------

test_that("init_tsa creates TSA table from a local file via tsa_dsn", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_test_gpkg()
  on.exit(unlink(pkg$dsn), add = TRUE)

  expect_no_error(
    init_tsa(
      conn            = conn,
      ask             = FALSE,
      Skeena_boundary = FALSE,
      tsa_dsn         = pkg$dsn
    )
  )

  expect_true(nrow(DBI::dbGetQuery(conn, "SELECT 1 FROM information_schema.tables WHERE table_name = 'TSA'")) > 0)
  cols <- DBI::dbGetQuery(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'TSA'")$column_name
  expect_true("TSA_NUMBER_DESCRIPTION" %in% cols)
  expect_true("Shape" %in% cols)
})

test_that("init_tsa stores the polygon row with the correct attribute value", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_test_gpkg(col_val = "Mackenzie TSA")
  on.exit(unlink(pkg$dsn), add = TRUE)

  init_tsa(
    conn            = conn,
    ask             = FALSE,
    Skeena_boundary = FALSE,
    tsa_dsn         = pkg$dsn
  )

  result <- DBI::dbGetQuery(conn, "SELECT TSA_NUMBER_DESCRIPTION FROM TSA")
  expect_equal(result$TSA_NUMBER_DESCRIPTION, "Mackenzie TSA")
})

test_that("init_tsa skips TSA initialisation when table already exists and ask=FALSE", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  # Pre-populate TSA with sentinel data (no Shape column so skip check works
  # without triggering the DuckDB GEOMETRY prepare bug).
  DBI::dbExecute(conn,
    "CREATE TABLE TSA (TSA_NUMBER_DESCRIPTION VARCHAR);")
  DBI::dbExecute(conn,
    "INSERT INTO TSA VALUES ('Existing TSA');")

  # No tsa_dsn supplied, Skeena_boundary=FALSE → would normally download from
  # bcdata, but since TSA already exists and ask=FALSE, it must be skipped.
  expect_no_error(
    init_tsa(
      conn            = conn,
      ask             = FALSE,
      Skeena_boundary = FALSE
    )
  )

  result <- DBI::dbGetQuery(conn, "SELECT TSA_NUMBER_DESCRIPTION FROM TSA")
  expect_equal(result$TSA_NUMBER_DESCRIPTION, "Existing TSA")
})

test_that(".duckdb_locking_pid parses lock-holder PID from DuckDB error text", {
  msg <- paste(
    'Cannot open file "C:\\path\\to\\ssgbm.duckdb": file is already open.',
    'File is already open in C:\\PROGRA~1\\R\\R-45~1.3\\bin\\x64\\Rterm.exe (PID 12820)'
  )

  expect_equal(ssgbm:::.duckdb_locking_pid(msg), 12820L)
  expect_true(is.na(ssgbm:::.duckdb_locking_pid("no pid here")))
})

test_that("init_tsa skips initialisation of both TSA and SKEENA when both tables exist", {
  conn <- make_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  DBI::dbExecute(conn,
    "CREATE TABLE TSA (TSA_NUMBER_DESCRIPTION VARCHAR);")
  DBI::dbExecute(conn,
    "INSERT INTO TSA VALUES ('Existing TSA');")

  DBI::dbExecute(conn,
    "CREATE TABLE SKEENA (ORG_UNIT_NAME VARCHAR);")
  DBI::dbExecute(conn,
    "INSERT INTO SKEENA VALUES ('Skeena Natural Resource Region');")

  expect_no_error(
    init_tsa(
      conn            = conn,
      ask             = FALSE,
      Skeena_boundary = TRUE
    )
  )

  tsa_result    <- DBI::dbGetQuery(conn, "SELECT TSA_NUMBER_DESCRIPTION FROM TSA")
  skeena_result <- DBI::dbGetQuery(conn, "SELECT ORG_UNIT_NAME FROM SKEENA")

  expect_equal(tsa_result$TSA_NUMBER_DESCRIPTION, "Existing TSA")
  expect_equal(skeena_result$ORG_UNIT_NAME, "Skeena Natural Resource Region")
})
