library(testthat)
library(duckdb)
library(DBI)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_conn <- function() {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  conn
}

# Create a minimal table with a Shape column holding a polygon at the given
# BC Albers-like offset.  Coordinates are metre-scale to match real data.
make_spatial_tbl <- function(conn, tbl, x0, y0, x1, y1) {
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT ST_GeomFromText('POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))') AS Shape,
            '%s' AS SRC",
    tbl,
    x0, y0, x1, y0, x1, y1, x0, y1, x0, y0,
    tbl
  ))
  invisible(conn)
}

# AOI WKT that overlaps the polygon at (1000, 1000)-(2000, 2000) but NOT (9e5, 9e5)
aoi_inside_wkt <- "POLYGON ((500 500, 2500 500, 2500 2500, 500 2500, 500 500))"
# AOI that does NOT overlap either polygon
aoi_outside_wkt <- "POLYGON ((5000 5000, 6000 5000, 6000 6000, 5000 6000, 5000 5000))"

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("filtered_views returns matching rows for overlapping AOI", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI",  x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)
  make_spatial_tbl(conn, "BEM",  x0 = 1200, y0 = 1200, x1 = 1800, y1 = 1800)

  # Should not error and should find rows
  expect_error(
    filtered_views(conn, aoi_inside_wkt,
                   tables = c("VRI", "BEM"),
                   materialize = TRUE,
                   build_spatial_index = FALSE),
    regexp = NA
  )

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  n_bem <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_BEM")$n

  expect_equal(n_vri, 1L)
  expect_equal(n_bem, 1L)
})

test_that("filtered_views returns 0 rows for non-overlapping AOI", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  expect_error(
    filtered_views(conn, aoi_outside_wkt,
                   tables = c("VRI"),
                   materialize = TRUE,
                   build_spatial_index = FALSE),
    regexp = NA
  )

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  expect_equal(n_vri, 0L)
})

test_that("filtered_views skips missing tables", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  # BEM does not exist — should not error, should still filter VRI
  expect_error(
    filtered_views(conn, aoi_inside_wkt,
                   tables = c("VRI", "BEM"),
                   materialize = TRUE,
                   build_spatial_index = FALSE),
    regexp = NA
  )

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  expect_equal(n_vri, 1L)

  v_bem_exists <- DBI::dbGetQuery(
    conn, "SELECT count(*) AS n FROM information_schema.tables WHERE table_name = 'V_BEM'"
  )$n
  expect_equal(v_bem_exists, 0L)
})

test_that("filtered_views geographic-CRS guard triggers correct error", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  # Geographic WKT (lon/lat around BC)
  geo_wkt <- "POLYGON ((-130 54, -115 54, -115 60, -130 60, -130 54))"

  expect_error(
    filtered_views(conn, geo_wkt, tables = c("VRI")),
    regexp = "lat/lon"
  )
})

test_that("filtered_views with materialize=FALSE creates views", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  expect_error(
    filtered_views(conn, aoi_inside_wkt,
                   tables = c("VRI"),
                   materialize = FALSE),
    regexp = NA
  )

  # V_VRI should be a VIEW, not a TABLE
  v_view <- DBI::dbGetQuery(
    conn, "SELECT count(*) AS n FROM duckdb_views() WHERE view_name = 'V_VRI'"
  )$n
  expect_equal(v_view, 1L)
})

test_that("filtered_views is idempotent (second call re-filters correctly)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  filtered_views(conn, aoi_inside_wkt,  tables = c("VRI"),
                 materialize = TRUE, build_spatial_index = FALSE)
  # Second call with a non-overlapping AOI should overwrite V_VRI to 0 rows
  filtered_views(conn, aoi_outside_wkt, tables = c("VRI"),
                 materialize = TRUE, build_spatial_index = FALSE)

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  expect_equal(n_vri, 0L)
})

# ---------------------------------------------------------------------------
# Persistent-DB round-trip: write → CHECKPOINT → close → reopen → filter
# This reproduces the scenario where source tables were checkpointed in a
# previous session (init_db) and the connection is reopened for filtered_views.
# The && bounding-box operator had a known interaction with DuckDB's CHECKPOINT
# that caused it to return 0 rows on checkpointed persistent tables; this test
# guards against regressions.
# ---------------------------------------------------------------------------

make_persistent_spatial_tbl <- function(dbpath, tbl, x0, y0, x1, y1) {
  cfg <- list(storage_compatibility_version = "v1.5.0")
  conn <- duckdb::dbConnect(duckdb::duckdb(dbdir = dbpath, config = cfg))
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT ST_GeomFromWKB(ST_AsWKB(ST_GeomFromText('POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))')))
       AS Shape, '%s' AS SRC",
    tbl,
    x0, y0, x1, y0, x1, y1, x0, y1, x0, y0,
    tbl
  ))
  DBI::dbExecute(conn, sprintf(
    "CREATE INDEX %s_IDX ON %s USING RTREE (Shape);", tbl, tbl
  ))
  DBI::dbExecute(conn, "CHECKPOINT;")
  duckdb::dbDisconnect(conn, shutdown = TRUE)
  invisible(dbpath)
}

test_that("filtered_views finds rows in checkpointed persistent tables", {
  dbpath <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(dbpath, paste0(dbpath, ".wal"))), add = TRUE)

  # Write two source tables in a fresh persistent DB (mimics init_db)
  make_persistent_spatial_tbl(dbpath, "VRI",
    x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)
  make_persistent_spatial_tbl(dbpath, "BEM",
    x0 = 1200, y0 = 1200, x1 = 1800, y1 = 1800)

  # Reopen the DB (mimics init_conn after init_db)
  cfg <- list(storage_compatibility_version = "v1.5.0")
  conn <- duckdb::dbConnect(duckdb::duckdb(dbdir = dbpath, config = cfg))
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # filtered_views must find rows even though the tables were checkpointed in
  # a previous session.  The polygon at (1000,1000)-(2000,2000) overlaps the
  # AOI at (500,500)-(2500,2500).
  expect_error(
    filtered_views(conn, aoi_inside_wkt,
                   tables = c("VRI", "BEM"),
                   materialize = TRUE,
                   build_spatial_index = FALSE),
    regexp = NA
  )

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  n_bem <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_BEM")$n

  expect_equal(n_vri, 1L)
  expect_equal(n_bem, 1L)
})

test_that("filtered_views returns 0 rows for non-overlapping AOI on persistent tables", {
  dbpath <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(dbpath, paste0(dbpath, ".wal"))), add = TRUE)

  make_persistent_spatial_tbl(dbpath, "VRI",
    x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  cfg <- list(storage_compatibility_version = "v1.5.0")
  conn <- duckdb::dbConnect(duckdb::duckdb(dbdir = dbpath, config = cfg))
  DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  filtered_views(conn, aoi_outside_wkt,
                 tables = c("VRI"),
                 materialize = TRUE,
                 build_spatial_index = FALSE)

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  expect_equal(n_vri, 0L)
})

# ---------------------------------------------------------------------------
# Large-table path: preserve_insertion_order=false is set and restored.
# Override large_table_rows=0 so a single-row table is treated as "large"
# and the memory-saving path is exercised without needing millions of rows.
# ---------------------------------------------------------------------------

test_that("filtered_views with large_table_rows=0 still returns correct row count", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  expect_error(
    filtered_views(conn, aoi_inside_wkt,
                   tables = c("VRI"),
                   materialize = TRUE,
                   build_spatial_index = FALSE,
                   large_table_rows = 0),  # force large-table path
    regexp = NA
  )

  n_vri <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRI")$n
  expect_equal(n_vri, 1L)

  # preserve_insertion_order must be restored to TRUE after filtered_views
  pio <- DBI::dbGetQuery(conn, "SELECT current_setting('preserve_insertion_order') AS v")$v
  expect_true(tolower(pio) == "true")
})

test_that("filtered_views never lowers an existing memory_limit for large tables", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Set a high memory limit on the connection (simulates init_conn memory_limit="14GB")
  DBI::dbExecute(conn, "SET memory_limit = '14GB';")

  make_spatial_tbl(conn, "VRI", x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)

  # vri_mem_limit is lower than the current 14GB limit → must NOT be applied.
  # Record what DuckDB reports before the call and compare the string after.
  # Avoids decimal-vs-binary byte conversion ambiguity (DuckDB accepts '14GB'
  # decimal but reports back in GiB binary, e.g. "13.0 GiB").
  lim_before <- DBI::dbGetQuery(conn, "SELECT value FROM duckdb_settings() WHERE name = 'memory_limit'")$value

  filtered_views(conn, aoi_inside_wkt,
                 tables = c("VRI"),
                 materialize = TRUE,
                 build_spatial_index = FALSE,
                 large_table_rows = 0,       # force large-table path
                 vri_mem_limit = "6GB")      # lower than 14GB → must be ignored

  lim_after <- DBI::dbGetQuery(conn, "SELECT value FROM duckdb_settings() WHERE name = 'memory_limit'")$value
  expect_equal(lim_after, lim_before)
})
