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

# Build a minimal VRI table. `rows` must be a data.frame with column
# `proj_age_1`; optionally `mrsrd_y` when include_mrsrd_y = TRUE.
make_vri_tbl <- function(conn,
                          tbl = "VRIBEM_TEST",
                          rows,
                          include_mrsrd_y = FALSE) {
  if (include_mrsrd_y) {
    row_sqls <- mapply(function(age, mrsrd) {
      sprintf(
        "SELECT CAST(%s AS DOUBLE) AS PROJ_AGE_1, CAST(%s AS DOUBLE) AS MRSRD_Y",
        ifelse(is.na(age),   "NULL", as.character(age)),
        ifelse(is.na(mrsrd), "NULL", as.character(mrsrd))
      )
    }, rows$proj_age_1, rows$mrsrd_y, SIMPLIFY = TRUE)
  } else {
    row_sqls <- vapply(rows$proj_age_1, function(age) {
      sprintf(
        "SELECT CAST(%s AS DOUBLE) AS PROJ_AGE_1",
        ifelse(is.na(age), "NULL", as.character(age))
      )
    }, character(1L))
  }
  union_sql <- paste(row_sqls, collapse = "\nUNION ALL\n")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS\n%s", tbl, union_sql
  ))
  invisible(conn)
}

# Build a minimal CCB table with a HARVEST_START_YEAR_CALENDAR column.
make_ccb_tbl <- function(conn,
                          tbl = "CCB_TEST",
                          harvest_years = 2020L) {
  row_sqls <- vapply(harvest_years, function(yr) {
    sprintf("SELECT CAST(%d AS INTEGER) AS HARVEST_START_YEAR_CALENDAR", yr)
  }, character(1L))
  union_sql <- paste(row_sqls, collapse = "\nUNION ALL\n")
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS\n%s", tbl, union_sql
  ))
  invisible(conn)
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("VRI_AGE_CL_STS and VRI_AGE_CL_STD columns are added", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = 50))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  cols <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRIBEM_TEST LIMIT 0"))
  expect_true("VRI_AGE_CL_STS" %in% cols)
  expect_true("VRI_AGE_CL_STD" %in% cols)
})

test_that("function returns vri_bem_tbl name invisibly", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = 50))
  make_ccb_tbl(conn)

  result <- calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")
  expect_equal(result, "VRIBEM_TEST")
})

test_that("in-place: only two new columns added, no extra tables", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = 50))
  make_ccb_tbl(conn)

  cols_before <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRIBEM_TEST LIMIT 0"))
  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")
  cols_after  <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRIBEM_TEST LIMIT 0"))

  expect_equal(length(cols_after), length(cols_before) + 2L)
  expect_true("VRI_AGE_CL_STS" %in% cols_after)
  expect_true("VRI_AGE_CL_STD" %in% cols_after)
})

test_that("VRI_AGE_CL_STS boundary values are correct", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  ages         <- c(-5, 0, 3, 4, 10, 11, 30, 31, 40, 41, 60, 61, 80, 81, 140, 141, 249, 250)
  expected_sts <- c(-1,  2, 2, 7,  7, 20, 20, 35, 35, 50, 50, 70, 70, 125, 125, 195, 195, 301)

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = ages))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn, "SELECT VRI_AGE_CL_STS FROM VRIBEM_TEST ORDER BY PROJ_AGE_1"
  )
  expect_equal(result$VRI_AGE_CL_STS, as.double(expected_sts))
})

test_that("VRI_AGE_CL_STD boundary values are correct", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  ages         <- c(-1,  0, 15, 16, 30, 31, 50, 51, 80, 81)
  expected_std <- c(-1, 15, 15, 30, 30, 50, 50, 80, 80, 9999)

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = ages))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn, "SELECT VRI_AGE_CL_STD FROM VRIBEM_TEST ORDER BY PROJ_AGE_1"
  )
  expect_equal(result$VRI_AGE_CL_STD, as.double(expected_std))
})

test_that("NULL PROJ_AGE_1 maps to -1 for both classes", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = NA_real_))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn, "SELECT VRI_AGE_CL_STS, VRI_AGE_CL_STD FROM VRIBEM_TEST"
  )
  expect_equal(result$VRI_AGE_CL_STS, -1)
  expect_equal(result$VRI_AGE_CL_STD, -1)
})

test_that("MRSRD_Y overrides PROJ_AGE_1 where not NULL", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Row 1: PROJ_AGE_1=50, MRSRD_Y=2010 → new PROJ_AGE_1 = 2020 - 2010 = 10 → STS=7, STD=15
  # Row 2: PROJ_AGE_1=50, MRSRD_Y=NA   → PROJ_AGE_1 stays 50             → STS=50, STD=80
  make_vri_tbl(
    conn,
    rows = data.frame(proj_age_1 = c(50, 50), mrsrd_y = c(2010, NA_real_)),
    include_mrsrd_y = TRUE
  )
  make_ccb_tbl(conn, harvest_years = 2020L)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn,
    "SELECT PROJ_AGE_1, VRI_AGE_CL_STS, VRI_AGE_CL_STD FROM VRIBEM_TEST ORDER BY PROJ_AGE_1"
  )
  expect_equal(result$PROJ_AGE_1,    c(10, 50))
  expect_equal(result$VRI_AGE_CL_STS, c(7, 50))
  expect_equal(result$VRI_AGE_CL_STD, c(15, 50))
})

test_that("most_recent_harvest_year uses MAX of harvest year column", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # CCB has years 2015 and 2018 → max = 2018
  # MRSRD_Y = 2000 → PROJ_AGE_1 = 2018 - 2000 = 18 → STS=20
  make_vri_tbl(
    conn,
    rows = data.frame(proj_age_1 = 50, mrsrd_y = 2000),
    include_mrsrd_y = TRUE
  )
  make_ccb_tbl(conn, harvest_years = c(2015L, 2018L))

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn, "SELECT PROJ_AGE_1, VRI_AGE_CL_STS FROM VRIBEM_TEST"
  )
  expect_equal(result$PROJ_AGE_1,     18)
  expect_equal(result$VRI_AGE_CL_STS, 20)
})

test_that("custom harvest_year_col parameter is respected", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  DBI::dbExecute(conn, paste0(
    "CREATE OR REPLACE TEMP TABLE CCB_TEST AS ",
    "SELECT 2022 AS HARVEST_MID_YEAR_CALENDAR"
  ))
  # MRSRD_Y = 2000 → PROJ_AGE_1 = 2022 - 2000 = 22 → STS=20
  make_vri_tbl(
    conn,
    rows = data.frame(proj_age_1 = 50, mrsrd_y = 2000),
    include_mrsrd_y = TRUE
  )

  calc_forest_age_class_duckdb(
    conn, "VRIBEM_TEST", "CCB_TEST",
    harvest_year_col = "HARVEST_MID_YEAR_CALENDAR"
  )

  result <- DBI::dbGetQuery(conn, "SELECT PROJ_AGE_1, VRI_AGE_CL_STS FROM VRIBEM_TEST")
  expect_equal(result$PROJ_AGE_1,     22)
  expect_equal(result$VRI_AGE_CL_STS, 20)
})

test_that("no MRSRD_Y column: PROJ_AGE_1 unchanged, classes still computed", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = 50))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn, "SELECT PROJ_AGE_1, VRI_AGE_CL_STS, VRI_AGE_CL_STD FROM VRIBEM_TEST"
  )
  expect_equal(result$PROJ_AGE_1,     50)
  expect_equal(result$VRI_AGE_CL_STS, 50)
  expect_equal(result$VRI_AGE_CL_STD, 50)
})

test_that("ADD COLUMN is idempotent (no error on second call)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, rows = data.frame(proj_age_1 = 50))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")
  expect_no_error(
    calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")
  )
})

test_that("multiple rows processed correctly", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  ages <- c(0, 25, 100, 300)
  make_vri_tbl(conn, rows = data.frame(proj_age_1 = ages))
  make_ccb_tbl(conn)

  calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")

  result <- DBI::dbGetQuery(
    conn,
    "SELECT VRI_AGE_CL_STS, VRI_AGE_CL_STD FROM VRIBEM_TEST ORDER BY PROJ_AGE_1"
  )
  expect_equal(result$VRI_AGE_CL_STS, c(2,   20,  125, 301))
  expect_equal(result$VRI_AGE_CL_STD, c(15,  30, 9999, 9999))
})

test_that("invalid conn raises error", {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  duckdb::dbDisconnect(conn, shutdown = TRUE)
  expect_error(
    calc_forest_age_class_duckdb(conn, "VRIBEM_TEST", "CCB_TEST")
  )
})
