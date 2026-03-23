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

# Create a minimal VRI-BEM table with one attribute column (VRI_ATTR)
# and one polygon covering (x0,y0)-(x1,y1).
make_vri_tbl <- function(conn,
                          tbl = "VRIBEM_TEST",
                          polygons = list(
                            list(x0 = 0,   y0 = 0,   x1 = 100, y1 = 100, attr = "A")
                          )) {
  rows <- vapply(polygons, function(p) {
    sprintf(
      "SELECT CAST('%s' AS VARCHAR) AS VRI_ATTR,
              ST_GeomFromText('POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))') AS Shape",
      p$attr,
      p$x0, p$y0,
      p$x1, p$y0,
      p$x1, p$y1,
      p$x0, p$y1,
      p$x0, p$y0
    )
  }, character(1L))

  union_sql <- paste(rows, collapse = "\nUNION ALL\n")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS\n%s", tbl, union_sql
  ))
  invisible(conn)
}

# Create a CCB table with one attribute column (HARVEST_YEAR) and one polygon.
make_ccb_tbl <- function(conn,
                          tbl = "CCB_TEST",
                          polygons = list(
                            list(x0 = 0, y0 = 0, x1 = 60, y1 = 100, yr = 2020L)
                          )) {
  rows <- vapply(polygons, function(p) {
    sprintf(
      "SELECT CAST(%d AS INTEGER) AS HARVEST_YEAR,
              ST_GeomFromText('POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))') AS Shape",
      p$yr,
      p$x0, p$y0,
      p$x1, p$y0,
      p$x1, p$y1,
      p$x0, p$y1,
      p$x0, p$y0
    )
  }, character(1L))

  union_sql <- paste(rows, collapse = "\nUNION ALL\n")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS\n%s", tbl, union_sql
  ))
  invisible(conn)
}

run_merge <- function(conn,
                       vri_tbl    = "VRIBEM_TEST",
                       ccb_tbl    = "CCB_TEST",
                       tolerance  = 10,
                       result_tbl = "VRIBEM_CCB_RESULT") {
  merge_ccb_duckdb(
    conn         = conn,
    vri_bem_tbl  = vri_tbl,
    ccb_tbl      = ccb_tbl,
    tolerance_m2 = tolerance,
    result_tbl   = result_tbl
  )
  DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s", result_tbl))
}

# ---------------------------------------------------------------------------
# Tests: result table
# ---------------------------------------------------------------------------

test_that("result table is created in the database", {
  conn <- make_conn()
  make_vri_tbl(conn)
  make_ccb_tbl(conn)
  merge_ccb_duckdb(conn, "VRIBEM_TEST", "CCB_TEST", result_tbl = "RES")
  expect_true(DBI::dbExistsTable(conn, "RES"))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("result table contains VRI and CCB attribute columns plus Shape", {
  conn <- make_conn()
  make_vri_tbl(conn)
  make_ccb_tbl(conn)
  merge_ccb_duckdb(conn, "VRIBEM_TEST", "CCB_TEST", result_tbl = "RES")
  cols <- DBI::dbGetQuery(conn, "SELECT * FROM RES LIMIT 0") |> names()
  expect_true("VRI_ATTR"    %in% cols)
  expect_true("HARVEST_YEAR" %in% cols)
  expect_true("Shape"        %in% cols)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("function returns result_tbl name invisibly", {
  conn <- make_conn()
  make_vri_tbl(conn)
  make_ccb_tbl(conn)
  ret <- merge_ccb_duckdb(conn, "VRIBEM_TEST", "CCB_TEST", result_tbl = "RES")
  expect_equal(ret, "RES")
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: partial overlap
# ---------------------------------------------------------------------------

test_that("partial overlap yields intersection piece with CCB attr and difference piece with NULL", {
  # VRI: 100x100.  CCB covers left 60 columns.
  # Expected: 2 rows – intersection (HARVEST_YEAR=2020) + difference (HARVEST_YEAR=NULL)
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 100, y1 = 100, attr = "A")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 60, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn)

  expect_equal(nrow(res), 2L)

  # Row with CCB attribute set is the intersection
  intr <- res[!is.na(res$HARVEST_YEAR), , drop = FALSE]
  diff <- res[ is.na(res$HARVEST_YEAR), , drop = FALSE]

  expect_equal(nrow(intr), 1L)
  expect_equal(nrow(diff), 1L)
  expect_equal(intr$HARVEST_YEAR, 2020L)
  expect_equal(intr$VRI_ATTR, "A")
  expect_equal(diff$VRI_ATTR, "A")

  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("intersection area is approximately the overlap area", {
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 100, y1 = 100, attr = "A")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 60, y1 = 100, yr = 2020L)
  ))
  run_merge(conn)
  areas <- DBI::dbGetQuery(
    conn,
    "SELECT ST_Area(Shape) AS area, HARVEST_YEAR FROM VRIBEM_CCB_RESULT"
  )
  intr_area <- areas$area[!is.na(areas$HARVEST_YEAR)]
  diff_area <- areas$area[ is.na(areas$HARVEST_YEAR)]

  expect_equal(intr_area, 6000, tolerance = 1e-3)
  expect_equal(diff_area, 4000, tolerance = 1e-3)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: no overlap
# ---------------------------------------------------------------------------

test_that("VRI polygons with no CCB overlap pass through with NULL CCB attrs", {
  conn <- make_conn()
  # Two VRI polygons; CCB only touches the first one
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0,   y0 = 0,   x1 = 100, y1 = 100, attr = "A"),
    list(x0 = 500, y0 = 500, x1 = 600, y1 = 600, attr = "B")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 60, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn)

  # Polygon B (no overlap) should appear exactly once with NULL HARVEST_YEAR
  poly_b <- res[res$VRI_ATTR == "B", , drop = FALSE]
  expect_equal(nrow(poly_b), 1L)
  expect_true(is.na(poly_b$HARVEST_YEAR))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("when CCB table is empty all VRI polygons pass through unchanged", {
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0,  y0 = 0,  x1 = 100, y1 = 100, attr = "X"),
    list(x0 = 200, y0 = 0, x1 = 300, y1 = 100, attr = "Y")
  ))
  # Empty CCB table
  DBI::dbExecute(conn, "CREATE OR REPLACE TEMP TABLE CCB_EMPTY AS
    SELECT CAST(NULL AS INTEGER) AS HARVEST_YEAR,
           ST_GeomFromText('POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))') AS Shape
    WHERE 1 = 0")
  res <- run_merge(conn, ccb_tbl = "CCB_EMPTY")

  expect_equal(nrow(res), 2L)
  expect_true(all(is.na(res$HARVEST_YEAR)))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: tolerance filtering
# ---------------------------------------------------------------------------

test_that("fragments at or below tolerance_m2 are dropped", {
  # CCB covers 99.9 x 100 of the VRI polygon, leaving a 0.1 x 100 = 10 unit sliver.
  # With tolerance_m2 = 10 the sliver (area exactly 10) must be dropped
  # (condition is strictly greater-than).
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 100, y1 = 100, attr = "A")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 99.9, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn, tolerance = 10)

  # Difference area = 0.1 * 100 = 10, which is NOT > 10, so it is dropped.
  # Only the intersection piece should remain.
  expect_equal(nrow(res), 1L)
  expect_equal(res$HARVEST_YEAR, 2020L)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("fragments just above tolerance_m2 are kept", {
  # CCB covers 99.8 x 100, leaving a 0.2 x 100 = 20 unit sliver.
  # With tolerance_m2 = 10 the sliver (area 20) is kept.
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 100, y1 = 100, attr = "A")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 99.8, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn, tolerance = 10)

  expect_equal(nrow(res), 2L)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: multiple CCB polygons
# ---------------------------------------------------------------------------

test_that("multiple CCB polygons per VRI produce one row each for intersection", {
  # VRI: 0-100 x 0-100
  # CCB1: 0-40 x 0-100 (HARVEST_YEAR=2018)
  # CCB2: 60-100 x 0-100 (HARVEST_YEAR=2020)
  # Difference: 40-60 x 0-100 (area=2000, NULL HARVEST_YEAR)
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 100, y1 = 100, attr = "A")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0,  y0 = 0, x1 = 40,  y1 = 100, yr = 2018L),
    list(x0 = 60, y0 = 0, x1 = 100, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn)

  # Should be: 2 intersection rows + 1 difference row = 3 total
  expect_equal(nrow(res), 3L)

  intr_rows <- res[!is.na(res$HARVEST_YEAR), , drop = FALSE]
  diff_rows  <- res[ is.na(res$HARVEST_YEAR), , drop = FALSE]

  expect_equal(nrow(intr_rows), 2L)
  expect_equal(nrow(diff_rows), 1L)
  expect_setequal(intr_rows$HARVEST_YEAR, c(2018L, 2020L))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

test_that("multiple VRI polygons handled independently", {
  # VRI poly A: 0-100 x 0-100  overlaps CCB
  # VRI poly B: 200-300 x 0-100  no overlap
  conn <- make_conn()
  make_vri_tbl(conn, polygons = list(
    list(x0 = 0,   y0 = 0, x1 = 100, y1 = 100, attr = "A"),
    list(x0 = 200, y0 = 0, x1 = 300, y1 = 100, attr = "B")
  ))
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 60, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn)

  # poly A → 2 rows (intersection + difference)
  # poly B → 1 row (no overlap)
  expect_equal(nrow(res), 3L)
  expect_equal(sum(res$VRI_ATTR == "A"), 2L)
  expect_equal(sum(res$VRI_ATTR == "B"), 1L)
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: VRI columns with no matching CCB column are preserved
# ---------------------------------------------------------------------------

test_that("VRI columns not in CCB are preserved in all output rows", {
  conn <- make_conn()
  # VRI table with multiple attribute columns
  DBI::dbExecute(conn, "CREATE OR REPLACE TEMP TABLE VRI_MULTI AS
    SELECT
      'A'::VARCHAR AS VRI_ATTR,
      42::INTEGER  AS VRI_NUM,
      ST_GeomFromText('POLYGON ((0 0, 100 0, 100 100, 0 100, 0 0))') AS Shape")
  make_ccb_tbl(conn, polygons = list(
    list(x0 = 0, y0 = 0, x1 = 60, y1 = 100, yr = 2020L)
  ))
  res <- run_merge(conn, vri_tbl = "VRI_MULTI")

  expect_true("VRI_ATTR" %in% names(res))
  expect_true("VRI_NUM"  %in% names(res))
  expect_true(all(res$VRI_ATTR == "A"))
  expect_true(all(res$VRI_NUM  == 42L))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

# ---------------------------------------------------------------------------
# Tests: input validation
# ---------------------------------------------------------------------------

test_that("non-duckdb connection raises error", {
  expect_error(
    merge_ccb_duckdb("not_a_conn"),
    class = "simpleError"
  )
})
