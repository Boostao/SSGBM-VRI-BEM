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

# Write attribute data.frame then attach a 100×100 square as Shape.
# ST_Area of this polygon = 10 000 (m²) in flat space.
make_vri_tbl <- function(conn, tbl = "VRIBEM_TEST", df) {
  DBI::dbWriteTable(conn, "_tmp_rrm_in", df, temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT *, ST_GeomFromText('POLYGON ((0 0, 100 0, 100 100, 0 100, 0 0))') AS Shape
     FROM _tmp_rrm_in",
    tbl
  ))
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS _tmp_rrm_in")
  invisible(conn)
}

# Build a single-row data.frame with all columns required by
# create_RRM_ecosystem_duckdb / _bear. Override any column via name = value.
default_row <- function(...) {
  sts_cols <- do.call(c, lapply(1:3, function(i) {
    setNames(
      as.list(rep("3", 9L)),
      c(sprintf("STS_%d_Age_0_3",    i), sprintf("STS_%d_Age_4_10",   i),
        sprintf("STS_%d_Age_11_30",  i), sprintf("STS_%d_Age_31_40",  i),
        sprintf("STS_%d_Age_41_60",  i), sprintf("STS_%d_Age_61_80",  i),
        sprintf("STS_%d_Age_81_139", i), sprintf("STS_%d_Age_140_249",i),
        sprintf("STS_%d_Age_gt_249", i))
    )
  }))

  stand_cols <- do.call(c, lapply(1:3, function(i) {
    setNames(
      as.list(rep("C", 5L)),
      c(sprintf("STAND_%d_Age_0_15",  i), sprintf("STAND_%d_Age_16_30", i),
        sprintf("STAND_%d_Age_31_50", i), sprintf("STAND_%d_Age_51_80", i),
        sprintf("STAND_%d_Age_gt_80", i))
    )
  }))

  base_list <- c(
    list(
      ECO_SEC = "COAST", BGC_ZONE = "CWH", BGC_SUBZON = "vm",
      BGC_VRT = NA_character_, BGC_PHASE = NA_character_,
      SLOPE_MOD = "0", SITE_M3A = "A", SNOW_CODE = "A",
      ABOVE_ELEV_THOLD = "N",
      SDEC_1 = 5L, BEUMC_S1 = "AT", FORESTED_1 = "Y",
      CROWN_ALL_1 = "M", STRCT_S1 = "4", STAND_A1 = "C",
      SDEC_2 = 5L, BEUMC_S2 = "SBS", FORESTED_2 = "Y",
      CROWN_ALL_2 = "M", STRCT_S2 = "5", STAND_A2 = "C",
      SDEC_3 = 0L, BEUMC_S3 = NA_character_, FORESTED_3 = NA_character_,
      CROWN_ALL_3 = NA_character_, STRCT_S3 = NA_character_, STAND_A3 = NA_character_
    ),
    sts_cols, stand_cols,
    list(stringsAsFactors = FALSE)
  )

  df <- do.call(data.frame, base_list)

  args <- list(...)
  for (nm in names(args)) df[[nm]] <- args[[nm]]
  df
}

# Huckleberry variant of default_row
default_row_huck <- function(...) {
  df <- data.frame(
    ECO_SEC = "COAST", BGC_ZONE = "CWH", BGC_SUBZON = "vm",
    BGC_VRT = NA_character_, BGC_PHASE = NA_character_,
    HUCK_ASP = "N", HUCK_ELEV_Thold = "Y",
    SDEC_1 = 10L, BEUMC_S1 = "AT", FORESTED_1 = "Y",
    CROWN_ALL_1 = "M", STRCT_S1 = "4", STAND_A1 = "C",
    SDEC_2 = 0L,  BEUMC_S2 = NA_character_, FORESTED_2 = NA_character_,
    CROWN_ALL_2 = NA_character_, STRCT_S2 = NA_character_, STAND_A2 = NA_character_,
    SDEC_3 = 0L,  BEUMC_S3 = NA_character_, FORESTED_3 = NA_character_,
    CROWN_ALL_3 = NA_character_, STRCT_S3 = NA_character_, STAND_A3 = NA_character_,
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) df[[nm]] <- args[[nm]]
  df
}

# ---------------------------------------------------------------------------
# Structure tests — create_RRM_ecosystem_duckdb
# ---------------------------------------------------------------------------

test_that("returns a data.frame", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))
  make_vri_tbl(conn, df = default_row())
  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  expect_s3_class(result, "data.frame")
})

test_that("result contains expected columns", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))
  make_vri_tbl(conn, df = default_row())
  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  expected_cols <- c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE",
                     "BEUMC", "SLOPE_MOD", "SITE_M3A", "SNOW_CODE",
                     "ABOVE_ELEV_THOLD", "CROWN_ALL", "STRCT", "STAND",
                     "FORESTED", "Hectares")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("missing column:", col))
  }
})

test_that("returns rows (non-empty result for valid input)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))
  make_vri_tbl(conn, df = default_row())
  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  expect_gt(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Area calculation
# ---------------------------------------------------------------------------

test_that("actual Hectares = ST_Area(Shape) * SDEC / 10 / 10000", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Single decile 1 only: SDEC_1=10, Shape area = 10000 m²
  # Expected actual Hectares = 10000 * 10 / 10 / 10000 = 1.0
  df <- default_row(SDEC_1 = 10L, BEUMC_S1 = "AT", FORESTED_1 = "Y",
                    SDEC_2 = 0L, BEUMC_S2 = NA_character_, FORESTED_2 = NA_character_)
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # The actual row (STRCT from STRCT_S1="4", projection=FALSE) has area 1.0
  actual <- result[result$STRCT == "4" & !is.na(result$STAND), , drop = FALSE]
  expect_equal(sum(actual$Hectares), 1.0, tolerance = 1e-8)
})

test_that("projected rows contribute 0 Hectares", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(SDEC_1 = 10L, SDEC_2 = 0L, BEUMC_S2 = NA_character_,
                    FORESTED_2 = NA_character_)
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # Projected rows: STRCT = "3" (our default STS age-class values)
  projected <- result[result$STRCT == "3", , drop = FALSE]
  expect_true(all(projected$Hectares == 0))
})

test_that("two active deciles produce two separate groups", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(SDEC_1 = 5L, BEUMC_S1 = "AT",
                    SDEC_2 = 5L, BEUMC_S2 = "SBS",
                    SDEC_3 = 0L, BEUMC_S3 = NA_character_, FORESTED_3 = NA_character_)
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  actual  <- result[result$STRCT == "4" | result$STRCT == "5", , drop = FALSE]
  beumc_actual <- unique(actual[actual$Hectares > 0, "BEUMC"])
  expect_true("AT"  %in% beumc_actual)
  expect_true("SBS" %in% beumc_actual)
})

test_that("Hectares for SDEC_1=5 on 10000 m² polygon is 0.5", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(SDEC_1 = 5L, SDEC_2 = 0L, BEUMC_S2 = NA_character_,
                    FORESTED_2 = NA_character_, STRCT_S1 = "6")
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # actual row: STRCT="6", STAND="C" (STAND_A1), Hectares=0.5
  actual <- result[!is.na(result$STAND) & result$STRCT == "6" & result$Hectares > 0, ]
  expect_equal(sum(actual$Hectares), 0.5, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# STAND correction (projected rows)
# ---------------------------------------------------------------------------

test_that("STAND correction: projected B/C/M with STRCT not in 4-7 → STAND NULL", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Set projected age-class STAND col to "B" and STRCT to "3" (not in 4-7)
  # The 3-year projection for decile 1 uses STS_1_Age_0_3 as STRCT and
  # STAND_1_Age_0_15 as STAND. We set STRCT="3", STAND="B".
  df <- default_row(
    SDEC_1 = 10L, SDEC_2 = 0L, BEUMC_S2 = NA_character_, FORESTED_2 = NA_character_,
    STS_1_Age_0_3   = "3",    # STRCT col for the 3-yr projection
    STAND_1_Age_0_15 = "B"   # STAND col for the 3-yr projection → should be corrected
  )
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # Any row with STRCT="3" and Hectares=0 should have STAND = NA (corrected)
  proj_rows <- result[result$STRCT == "3" & result$Hectares == 0, ]
  stand_vals <- unique(proj_rows$STAND)
  expect_true(all(is.na(stand_vals)),
              info = paste("Expected all NA, got:", paste(stand_vals, collapse=",")))
})

test_that("STAND correction: projected B with STRCT in 4-7 is NOT set to NULL", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # STS_1_Age_0_3="5" (in 4-7), STAND_1_Age_0_15="B" → STAND should stay "B"
  df <- default_row(
    SDEC_1 = 10L, SDEC_2 = 0L, BEUMC_S2 = NA_character_, FORESTED_2 = NA_character_,
    STS_1_Age_0_3    = "5",
    STAND_1_Age_0_15 = "B"
  )
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # Rows with STRCT="5" and STAND="B" should exist
  expect_true(any(result$STRCT == "5" & !is.na(result$STAND) & result$STAND == "B"))
})

test_that("STAND correction: actual rows (Hectares > 0) are unaffected", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(
    SDEC_1 = 10L, SDEC_2 = 0L, BEUMC_S2 = NA_character_, FORESTED_2 = NA_character_,
    STRCT_S1 = "3", STAND_A1 = "B"   # actual row: STRCT="3", STAND="B"
  )
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # Actual row: STRCT="3", Hectares=1.0, STAND should be "B" (not corrected)
  actual <- result[result$STRCT == "3" & result$Hectares > 0, ]
  expect_true(nrow(actual) > 0L)
  expect_true(all(actual$STAND == "B"))
})

# ---------------------------------------------------------------------------
# FORESTED / BEUMC NULL filtering
# ---------------------------------------------------------------------------

test_that("rows with NA BEUMC are excluded from output", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(SDEC_1 = 10L,
                    SDEC_2 = 5L, BEUMC_S2 = NA_character_, FORESTED_2 = "Y")
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # Only decile 1 (BEUMC_S1="AT") should appear
  expect_false("SBS" %in% result$BEUMC)
})

test_that("rows with NA FORESTED are excluded from output", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(SDEC_1 = 10L,
                    SDEC_2 = 5L, BEUMC_S2 = "SBS", FORESTED_2 = NA_character_)
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  expect_false("SBS" %in% result$BEUMC)
})

# ---------------------------------------------------------------------------
# create_RRM_ecosystem_moose_duckdb — delegates
# ---------------------------------------------------------------------------

test_that("moose variant returns same result as base", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  base  <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  moose <- create_RRM_ecosystem_moose_duckdb(conn, "VRIBEM_TEST")

  # Sort both by every column to make the comparison order-independent
  sort_df <- function(df) df[do.call(order, lapply(names(df), function(c) df[[c]])), ]
  expect_equal(sort_df(base), sort_df(moose), ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# create_RRM_ecosystem_bear_duckdb
# ---------------------------------------------------------------------------

test_that("bear variant returns data.frame with Salmon column", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row(Salmon = "N")
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_bear_duckdb(conn, "VRIBEM_TEST")
  expect_s3_class(result, "data.frame")
  expect_true("Salmon" %in% names(result))
})

test_that("bear variant groups by Salmon — two Salmon levels → separate rows", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    transform(default_row(SDEC_2 = 0L, BEUMC_S2 = NA_character_,
                          FORESTED_2 = NA_character_, Salmon = "Y")),
    transform(default_row(SDEC_2 = 0L, BEUMC_S2 = NA_character_,
                          FORESTED_2 = NA_character_, Salmon = "N"))
  )
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_bear_duckdb(conn, "VRIBEM_TEST")
  # Both Salmon levels should appear
  expect_true("Y" %in% result$Salmon)
  expect_true("N" %in% result$Salmon)
})

test_that("bear variant has correct columns (including Salmon, no extra)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(Salmon = "N"))
  result <- create_RRM_ecosystem_bear_duckdb(conn, "VRIBEM_TEST")

  expected <- c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE",
                "BEUMC", "SLOPE_MOD", "SITE_M3A", "Salmon", "SNOW_CODE",
                "ABOVE_ELEV_THOLD", "CROWN_ALL", "STRCT", "STAND",
                "FORESTED", "Hectares")
  for (col in expected) {
    expect_true(col %in% names(result), info = paste("missing:", col))
  }
})

# ---------------------------------------------------------------------------
# create_RRM_ecosystem_huckleberry_duckdb
# ---------------------------------------------------------------------------

test_that("huckleberry variant returns data.frame", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row_huck())
  result <- create_RRM_ecosystem_huckleberry_duckdb(conn, "VRIBEM_TEST")
  expect_s3_class(result, "data.frame")
})

test_that("huckleberry result has expected columns", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row_huck())
  result <- create_RRM_ecosystem_huckleberry_duckdb(conn, "VRIBEM_TEST")

  expected <- c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE",
                "HUCK_ASP", "HUCK_ELEV_Thold", "CROWN_ALL", "STRCT", "STAND",
                "FORESTED", "Hectares")
  for (col in expected) {
    expect_true(col %in% names(result), info = paste("missing:", col))
  }
  expect_false("BEUMC" %in% names(result))
  expect_false("SLOPE_MOD" %in% names(result))
  expect_false("SNOW_CODE" %in% names(result))
})

test_that("huckleberry area calculation: SDEC_1=10, Shape=10000 m² → 1.0 ha", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- default_row_huck(SDEC_1 = 10L)
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_huckleberry_duckdb(conn, "VRIBEM_TEST")
  expect_equal(sum(result$Hectares), 1.0, tolerance = 1e-8)
})

test_that("huckleberry groups by HUCK_ASP — two aspects produce separate rows", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(default_row_huck(HUCK_ASP = "N"), default_row_huck(HUCK_ASP = "S"))
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_huckleberry_duckdb(conn, "VRIBEM_TEST")
  expect_true("N" %in% result$HUCK_ASP)
  expect_true("S" %in% result$HUCK_ASP)
})

test_that("huckleberry produces no projected rows (only actual)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row_huck(SDEC_1 = 10L))
  result <- create_RRM_ecosystem_huckleberry_duckdb(conn, "VRIBEM_TEST")
  # All rows have Hectares > 0 (no projection-only 0-area rows)
  expect_true(all(result$Hectares >= 0))
  expect_true(any(result$Hectares > 0))
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# Multiple VRI rows aggregate correctly
# ---------------------------------------------------------------------------

test_that("two VRI polygons with same ecosystem key are summed", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    default_row(SDEC_1 = 10L, SDEC_2 = 0L, BEUMC_S2 = NA_character_,
                FORESTED_2 = NA_character_, STRCT_S1 = "5", STAND_A1 = "C"),
    default_row(SDEC_1 = 10L, SDEC_2 = 0L, BEUMC_S2 = NA_character_,
                FORESTED_2 = NA_character_, STRCT_S1 = "5", STAND_A1 = "C")
  )
  make_vri_tbl(conn, df = df)

  result <- create_RRM_ecosystem_duckdb(conn, "VRIBEM_TEST")
  # Both polygons identical → same group → Hectares = 2 × (1.0) = 2.0
  actual <- result[result$STRCT == "5" & !is.na(result$STAND) &
                     result$Hectares > 0, ]
  expect_equal(sum(actual$Hectares), 2.0, tolerance = 1e-8)
})
