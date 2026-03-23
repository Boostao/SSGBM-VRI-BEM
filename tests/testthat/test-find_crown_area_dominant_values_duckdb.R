library(testthat)
library(duckdb)
library(DBI)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_conn <- function() {
  duckdb::dbConnect(duckdb::duckdb(), ":memory:")
}

# Build a minimal VRI table using a data.frame.
make_vri_tbl <- function(conn, tbl = "VRIBEM_TEST", df) {
  DBI::dbWriteTable(conn, tbl, df, temporary = TRUE, overwrite = TRUE)
  invisible(conn)
}

# Build a single-row data.frame with all required columns.
# Override any column via name = value arguments.
default_row <- function(...) {
  base <- data.frame(
    CROWN_ALL  = "H",
    FORESTED_1 = "Y", STRCT_S1 = "5",
    FORESTED_2 = "Y", STRCT_S2 = "5",
    FORESTED_3 = "Y", STRCT_S3 = "5",
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("CROWN_ALL_1/2/3 columns are added to the table", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  cols <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRIBEM_TEST LIMIT 0"))
  expect_true("CROWN_ALL_1" %in% cols)
  expect_true("CROWN_ALL_2" %in% cols)
  expect_true("CROWN_ALL_3" %in% cols)
})

test_that("function returns vri_bem_tbl invisibly", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  result <- find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")
  expect_equal(result, "VRIBEM_TEST")
})

test_that("CROWN_ALL_i inherits CROWN_ALL when FORESTED='Y' and STRCT starts 4-7", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Test each valid STRCT prefix 4, 5, 6, 7
  for (prefix in c("4", "5", "6", "7")) {
    df <- default_row(CROWN_ALL = "VH",
                      STRCT_S1 = prefix, FORESTED_1 = "Y")
    make_vri_tbl(conn, df = df)
    find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")
    r <- DBI::dbGetQuery(conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST")
    expect_equal(r$CROWN_ALL_1, "VH",
                 info = paste("STRCT_S1 prefix:", prefix))
  }
})

test_that("CROWN_ALL_i is NULL when FORESTED='N'", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(FORESTED_1 = "N", STRCT_S1 = "5",
                    FORESTED_2 = "N", STRCT_S2 = "6",
                    FORESTED_3 = "N", STRCT_S3 = "7")
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(conn, "SELECT CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3 FROM VRIBEM_TEST")
  expect_true(is.na(r$CROWN_ALL_1))
  expect_true(is.na(r$CROWN_ALL_2))
  expect_true(is.na(r$CROWN_ALL_3))
})

test_that("CROWN_ALL_i is NULL when STRCT_Si does NOT start with 4-7", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  for (prefix in c("1", "2", "3", "1a", "2b", "3c")) {
    df <- default_row(FORESTED_1 = "Y", STRCT_S1 = prefix)
    make_vri_tbl(conn, df = df)
    find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")
    r <- DBI::dbGetQuery(conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST")
    expect_true(is.na(r$CROWN_ALL_1),
                info = paste("STRCT_S1 prefix:", prefix))
  }
})

test_that("CROWN_ALL_i is NULL when STRCT_Si is NULL", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(FORESTED_1 = "Y", STRCT_S1 = NA_character_)
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST")
  expect_true(is.na(r$CROWN_ALL_1))
})

test_that("CROWN_ALL_i is NULL when CROWN_ALL itself is NULL", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(CROWN_ALL = NA_character_,
                    FORESTED_1 = "Y", STRCT_S1 = "5")
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST")
  expect_true(is.na(r$CROWN_ALL_1))
})

test_that("each decile is independently controlled", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(
    CROWN_ALL  = "M",
    FORESTED_1 = "Y", STRCT_S1 = "6",   # should be M
    FORESTED_2 = "N", STRCT_S2 = "6",   # should be NA (FORESTED='N')
    FORESTED_3 = "Y", STRCT_S3 = "2"    # should be NA (STRCT < 4)
  )
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(conn,
    "SELECT CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3 FROM VRIBEM_TEST"
  )
  expect_equal(r$CROWN_ALL_1, "M")
  expect_true(is.na(r$CROWN_ALL_2))
  expect_true(is.na(r$CROWN_ALL_3))
})

test_that("multiple rows processed correctly", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    default_row(CROWN_ALL = "H",   FORESTED_1 = "Y", STRCT_S1 = "5"),
    default_row(CROWN_ALL = "VH",  FORESTED_1 = "N", STRCT_S1 = "5"),
    default_row(CROWN_ALL = "VL-L",FORESTED_1 = "Y", STRCT_S1 = "3"),
    default_row(CROWN_ALL = "M",   FORESTED_1 = "Y", STRCT_S1 = "7")
  )
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(
    conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$CROWN_ALL_1, c("H", NA_character_, NA_character_, "M"))
})

test_that("STRCT_S1 = '7a' / '7b' are treated as STRCT >= 4 (starts with '7')", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    default_row(CROWN_ALL = "H", FORESTED_1 = "Y", STRCT_S1 = "7a"),
    default_row(CROWN_ALL = "M", FORESTED_1 = "Y", STRCT_S1 = "7b")
  )
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(
    conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$CROWN_ALL_1, c("H", "M"))
})

test_that("function is idempotent (second call does not error)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")
  expect_no_error(
    find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")
  )
})

test_that("invalid conn raises error", {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  duckdb::dbDisconnect(conn, shutdown = TRUE)
  expect_error(
    find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")
  )
})

test_that("CROWN_ALL_i values match all valid CROWN_ALL categories", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  categories <- c("VL-L", "M", "H", "VH")
  df <- do.call(rbind, lapply(categories, function(cv) {
    default_row(CROWN_ALL = cv, FORESTED_1 = "Y", STRCT_S1 = "5")
  }))
  make_vri_tbl(conn, df = df)
  find_crown_area_dominant_values_duckdb(conn, "VRIBEM_TEST")

  r <- DBI::dbGetQuery(
    conn, "SELECT CROWN_ALL_1 FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$CROWN_ALL_1, categories)
})
