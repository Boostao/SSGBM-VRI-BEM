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

# Write attribute data.frame to a temp table, then attach a unit square
# geometry for each row (0,0)-(100,100).
make_vri_tbl <- function(conn, tbl = "VRIBEM_TEST", df) {
  DBI::dbWriteTable(conn, "_tmp_hem_attrs", df,
                    temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(conn, sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     SELECT *, ST_GeomFromText(
       'POLYGON ((0 0, 100 0, 100 100, 0 100, 0 0))'
     ) AS Shape
     FROM _tmp_hem_attrs",
    tbl
  ))
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS _tmp_hem_attrs")
  invisible(conn)
}

# Fire table: empty (no rows) by default, or a polygon (x0,y0)-(x1,y1).
make_fire_tbl <- function(conn, tbl = "FIRE_TEST",
                           x0 = NULL, y0 = NULL, x1 = NULL, y1 = NULL) {
  if (is.null(x0)) {
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TEMP TABLE %s AS
       SELECT ST_GeomFromText('POLYGON ((0 0,1 0,1 1,0 1,0 0))') AS Shape
       WHERE FALSE",
      tbl
    ))
  } else {
    DBI::dbExecute(conn, sprintf(
      "CREATE OR REPLACE TEMP TABLE %s AS
       SELECT ST_GeomFromText(
         'POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))'
       ) AS Shape",
      tbl, x0, y0, x1, y0, x1, y1, x0, y1, x0, y0
    ))
  }
  invisible(conn)
}

# Single-row data.frame with sensible defaults; override via name=value.
default_row <- function(...) {
  base <- data.frame(
    BCLCS_LV_1 = "V", BCLCS_LV_2 = "T", BCLCS_LV_3 = "F",
    BCLCS_LV_4 = "TC", BCLCS_LV_5 = "ME",
    SPEC_CD_1  = "FD",  SPEC_PCT_1 = 100,
    SPEC_CD_2  = NA_character_,  SPEC_PCT_2 = NA_real_,
    PROJ_AGE_1 = 50,   ELEV       = 1000, MEAN_SLOPE  = 30,
    LBL_VEGCOV = "TC", CR_CLOSURE = 50,
    HARVEST_YEAR    = 1990L,
    VRI_AGE_CL_STS  = 50,
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

# Convenience: run function with empty fire table and fixed year.
run_hem <- function(conn, tbl = "VRIBEM_TEST", fire_tbl = "FIRE_TEST",
                    year = 2025L, ...) {
  calc_hem_fields_duckdb(conn, tbl,
                         fire_tbl         = fire_tbl,
                         current_year     = year, ...)
}

get1 <- function(conn, col, tbl = "VRIBEM_TEST") {
  DBI::dbGetQuery(conn, sprintf("SELECT %s FROM %s", col, tbl))[[1L]]
}

# ---------------------------------------------------------------------------
# Structural tests
# ---------------------------------------------------------------------------

test_that("all 24 output columns are added to the table", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)
  run_hem(conn)

  cols <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRIBEM_TEST LIMIT 0"))
  expected <- c(
    "fire_pct",
    "Static_Wetland_ST", "Static_Wetland_SL", "Static_Wetland_HE",
    "Static_Wetland_Shrub_Riparian", "Static_Upland", "Static_Willow",
    "Static_Sb_Bog", "Static_Riparian", "Waterbody",
    "Elev_Threshold", "Slope_Limit", "W_Site_Conditions_Met",
    "Age_Class8_9", "Static_Persist_Decid", "W_Shelter_1",
    "Dynamic_WFD_4to10", "Dynamic_WFD_11to30", "Security_1",
    "Static_Brush", "Static_WFD_All", "Dynamic_WFD_All",
    "Dynamic_L", "Dynamic_F"
  )
  for (col in expected) {
    expect_true(col %in% cols, info = paste("missing column:", col))
  }
})

test_that("function returns vri_bem_tbl invisibly", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)

  result <- run_hem(conn)
  expect_equal(result, "VRIBEM_TEST")
})

test_that("function is idempotent — running twice does not error", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)

  run_hem(conn)
  expect_no_error(run_hem(conn))

  r <- DBI::dbGetQuery(conn, "SELECT Static_Wetland_ST FROM VRIBEM_TEST")
  expect_equal(r$Static_Wetland_ST[[1L]], 0L)
})

# ---------------------------------------------------------------------------
# Static_Wetland_ST / SL / HE
# ---------------------------------------------------------------------------

test_that("Static_Wetland_ST = 1 for V/N/W/ST/OP", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  dr <- default_row(BCLCS_LV_1 = "V", BCLCS_LV_2 = "N", BCLCS_LV_3 = "W",
                    BCLCS_LV_4 = "ST", BCLCS_LV_5 = "OP")
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Wetland_ST"), 1L)
  expect_equal(get1(conn, "Static_Wetland_SL"), 0L)
  expect_equal(get1(conn, "Static_Wetland_HE"), 0L)
})

test_that("Static_Wetland_SL = 1 for V/N/W/SL/ST", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  dr <- default_row(BCLCS_LV_1 = "V", BCLCS_LV_2 = "N", BCLCS_LV_3 = "W",
                    BCLCS_LV_4 = "SL", BCLCS_LV_5 = "ST")
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Wetland_SL"), 1L)
})

test_that("Static_Wetland_HE = 1 for HE/OP and for BY/CL", {
  for (pair in list(c("HE", "OP"), c("BY", "CL"))) {
    conn <- make_conn()
    dr <- default_row(BCLCS_LV_1 = "V", BCLCS_LV_2 = "N", BCLCS_LV_3 = "W",
                      BCLCS_LV_4 = pair[[1]], BCLCS_LV_5 = pair[[2]])
    make_vri_tbl(conn, df = dr)
    make_fire_tbl(conn)
    run_hem(conn)

    result <- get1(conn, "Static_Wetland_HE")
    duckdb::dbDisconnect(conn, shutdown = TRUE)

    expect_equal(result, 1L,
                 info = paste("LV_4/LV_5:", pair[[1]], pair[[2]]))
  }
})

test_that("Static_Wetland_Shrub_Riparian = 1 when any wetland flag is 1", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # ST → Shrub_Riparian = 1
  dr <- default_row(BCLCS_LV_1 = "V", BCLCS_LV_2 = "N", BCLCS_LV_3 = "W",
                    BCLCS_LV_4 = "ST", BCLCS_LV_5 = "OP")
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Wetland_Shrub_Riparian"), 1L)
})

test_that("Static_Wetland_Shrub_Riparian = 0 when all wetland flags are 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Wetland_Shrub_Riparian"), 0L)
})

# ---------------------------------------------------------------------------
# Static_Upland
# ---------------------------------------------------------------------------

test_that("Static_Upland = 1 for V/N/U/ST/OP", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  dr <- default_row(BCLCS_LV_1 = "V", BCLCS_LV_2 = "N", BCLCS_LV_3 = "U",
                    BCLCS_LV_4 = "ST", BCLCS_LV_5 = "OP")
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Upland"), 1L)
})

test_that("Static_Upland = 1 for V/T/U/TC/SP", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  dr <- default_row(BCLCS_LV_2 = "T", BCLCS_LV_3 = "U",
                    BCLCS_LV_4 = "TC", BCLCS_LV_5 = "SP")
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Upland"), 1L)
})

# ---------------------------------------------------------------------------
# Static_Willow
# ---------------------------------------------------------------------------

test_that("Static_Willow = 1 when SPEC_CD_1 in ('W','WS')", {
  for (sp in c("W", "WS")) {
    conn <- make_conn()
    make_vri_tbl(conn, df = default_row(SPEC_CD_1 = sp))
    make_fire_tbl(conn)
    run_hem(conn)
    result <- get1(conn, "Static_Willow")
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    expect_equal(result, 1L, info = sp)
  }
})

test_that("Static_Willow = 1 when SPEC_CD_2 is 'WS'", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(SPEC_CD_1 = "FD", SPEC_CD_2 = "WS"))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Willow"), 1L)
})

# ---------------------------------------------------------------------------
# Static_Sb_Bog
# ---------------------------------------------------------------------------

test_that("Static_Sb_Bog = 1 when SPEC_CD_1='SB' and SPEC_PCT_1 > 89", {
  # boundary: 90 → 1, 89 → 0
  for (pct in list(list(90, 1L), list(89, 0L))) {
    conn <- make_conn()
    make_vri_tbl(conn, df = default_row(SPEC_CD_1 = "SB", SPEC_PCT_1 = pct[[1]]))
    make_fire_tbl(conn)
    run_hem(conn)
    result <- get1(conn, "Static_Sb_Bog")
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    expect_equal(result, pct[[2]],
                 info = paste("SPEC_PCT_1 =", pct[[1]]))
  }
})

# ---------------------------------------------------------------------------
# Static_Riparian
# ---------------------------------------------------------------------------

test_that("Static_Riparian = 1 for known riparian LBL_VEGCOV values", {
  for (lv in c("ri,sl", "st,ri", "ri,st,hg,by")) {
    conn <- make_conn()
    make_vri_tbl(conn, df = default_row(LBL_VEGCOV = lv))
    make_fire_tbl(conn)
    run_hem(conn)
    result <- get1(conn, "Static_Riparian")
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    expect_equal(result, 1L, info = lv)
  }
})

test_that("Static_Riparian = 0 for non-riparian LBL_VEGCOV", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(LBL_VEGCOV = "TC"))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Riparian"), 0L)
})

# ---------------------------------------------------------------------------
# Waterbody, Elev_Threshold, Slope_Limit
# ---------------------------------------------------------------------------

test_that("Waterbody = 0 when BCLCS_LV_5 = 'LA', else 1", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = rbind(default_row(BCLCS_LV_5 = "LA"),
                                default_row(BCLCS_LV_5 = "ME")))
  make_fire_tbl(conn)
  run_hem(conn)

  r <- DBI::dbGetQuery(conn, "SELECT Waterbody FROM VRIBEM_TEST ORDER BY BCLCS_LV_5")
  expect_equal(r$Waterbody, c(0L, 1L))
})

test_that("Elev_Threshold boundary: <1501 → 1, >=1501 → 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = rbind(default_row(ELEV = 1500),
                                default_row(ELEV = 1501)))
  make_fire_tbl(conn)
  run_hem(conn)

  r <- DBI::dbGetQuery(conn, "SELECT Elev_Threshold FROM VRIBEM_TEST ORDER BY ELEV")
  expect_equal(r$Elev_Threshold, c(1L, 0L))
})

test_that("Slope_Limit boundary: <81 → 1, >=81 → 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = rbind(default_row(MEAN_SLOPE = 80),
                                default_row(MEAN_SLOPE = 81)))
  make_fire_tbl(conn)
  run_hem(conn)

  r <- DBI::dbGetQuery(conn, "SELECT Slope_Limit FROM VRIBEM_TEST ORDER BY MEAN_SLOPE")
  expect_equal(r$Slope_Limit, c(1L, 0L))
})

# ---------------------------------------------------------------------------
# Age_Class8_9
# ---------------------------------------------------------------------------

test_that("Age_Class8_9 boundary: >140 → 1, <=140 → 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = rbind(default_row(PROJ_AGE_1 = 140),
                                default_row(PROJ_AGE_1 = 141)))
  make_fire_tbl(conn)
  run_hem(conn)

  r <- DBI::dbGetQuery(conn, "SELECT Age_Class8_9 FROM VRIBEM_TEST ORDER BY PROJ_AGE_1")
  expect_equal(r$Age_Class8_9, c(0L, 1L))
})

# ---------------------------------------------------------------------------
# Static_Persist_Decid
# ---------------------------------------------------------------------------

test_that("Static_Persist_Decid = 1 via SPEC_CD_1 (decid + age>60 + pct>60)", {
  # Boundary: PROJ_AGE_1 = 61 → 1;  60 → 0
  for (case in list(list(age = 61L, pct = 70, exp = 1L),
                    list(age = 60L, pct = 70, exp = 0L))) {
    conn <- make_conn()
    make_vri_tbl(conn, df = default_row(SPEC_CD_1 = "AC",
                                        SPEC_PCT_1 = case$pct,
                                        PROJ_AGE_1 = case$age))
    make_fire_tbl(conn)
    run_hem(conn)
    result <- get1(conn, "Static_Persist_Decid")
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    expect_equal(result, case$exp,
                 info = paste("PROJ_AGE_1 =", case$age))
  }
})

test_that("Static_Persist_Decid = 1 via SPEC_CD_2", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  dr <- default_row(SPEC_CD_1 = "FD", SPEC_PCT_1 = 30,
                    SPEC_CD_2 = "AT", SPEC_PCT_2 = 70,
                    PROJ_AGE_1 = 70)
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Persist_Decid"), 1L)
})

# ---------------------------------------------------------------------------
# W_Site_Conditions_Met
# ---------------------------------------------------------------------------

test_that("W_Site_Conditions_Met = 1 when Waterbody=1, Elev<=1500, Slope<=80", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # default: LV_5='ME' (not LA), ELEV=1000, MEAN_SLOPE=30
  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "W_Site_Conditions_Met"), 1L)
})

test_that("W_Site_Conditions_Met = 0 when BCLCS_LV_5 = 'LA' (Waterbody=0)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(BCLCS_LV_5 = "LA"))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "W_Site_Conditions_Met"), 0L)
})

# ---------------------------------------------------------------------------
# W_Shelter_1
# ---------------------------------------------------------------------------

test_that("W_Shelter_1 = 1 when all conditions satisfied and Persist_Decid=0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # BA in shelter_sp; not in decid_sp; PROJ_AGE_1=150>60 but 'BA' not decid
  dr <- default_row(SPEC_CD_1 = "BA", SPEC_PCT_1 = 50, PROJ_AGE_1 = 150,
                    ELEV = 1000, MEAN_SLOPE = 30, CR_CLOSURE = 40)
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "W_Shelter_1"), 1L)
})

test_that("W_Shelter_1 = 0 when Static_Persist_Decid = 1 (via SPEC_CD_2)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # BA in shelter_sp; SPEC_CD_2='AC' (decid) at 70% with age=150 → Persist_Decid=1
  dr <- default_row(SPEC_CD_1 = "BA", SPEC_PCT_1 = 50, PROJ_AGE_1 = 150,
                    ELEV = 1000, MEAN_SLOPE = 30, CR_CLOSURE = 40,
                    SPEC_CD_2 = "AC", SPEC_PCT_2 = 70)
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_Persist_Decid"), 1L)
  expect_equal(get1(conn, "W_Shelter_1"), 0L)
})

test_that("W_Shelter_1 = 0 when PROJ_AGE_1 <= 120", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  dr <- default_row(SPEC_CD_1 = "BA", SPEC_PCT_1 = 50, PROJ_AGE_1 = 120,
                    CR_CLOSURE = 40)
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "W_Shelter_1"), 0L)
})

# ---------------------------------------------------------------------------
# Dynamic_WFD_4to10 / Dynamic_WFD_11to30 / Security_1
# ---------------------------------------------------------------------------

test_that("Dynamic_WFD_4to10 = 1 when VRI_AGE_CL_STS=7, Elev<=1500, Slope<=80", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 7,
                                       ELEV = 1000, MEAN_SLOPE = 30))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_WFD_4to10"),  1L)
  expect_equal(get1(conn, "Dynamic_WFD_11to30"), 0L)
})

test_that("Dynamic_WFD_11to30 = 1 when VRI_AGE_CL_STS=20, Elev<=1500, Slope<=80", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 20,
                                       ELEV = 1000, MEAN_SLOPE = 30))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_WFD_4to10"),  0L)
  expect_equal(get1(conn, "Dynamic_WFD_11to30"), 1L)
})

test_that("Security_1 boundary: PROJ_AGE_1 > 40 → 1, <= 40 → 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = rbind(default_row(PROJ_AGE_1 = 40),
                                default_row(PROJ_AGE_1 = 41)))
  make_fire_tbl(conn)
  run_hem(conn)

  r <- DBI::dbGetQuery(conn, "SELECT Security_1 FROM VRIBEM_TEST ORDER BY PROJ_AGE_1")
  expect_equal(r$Security_1, c(0L, 1L))
})

test_that("Security_1 = 0 when ELEV >= 1501 even if PROJ_AGE_1 > 40", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(PROJ_AGE_1 = 100, ELEV = 1501))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Security_1"), 0L)
})

# ---------------------------------------------------------------------------
# Static_Brush and Static_WFD_All
# ---------------------------------------------------------------------------

test_that("Static_Brush = 1 for brush LBL_VEGCOV values", {
  for (lv in c("NC", "NCBR", "NP", "NPBR", "NPBU", "NSR")) {
    conn <- make_conn()
    make_vri_tbl(conn, df = default_row(LBL_VEGCOV = lv))
    make_fire_tbl(conn)
    run_hem(conn)
    result <- get1(conn, "Static_Brush")
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    expect_equal(result, 1L, info = lv)
  }
})

test_that("Static_WFD_All = 1 when Static_Brush=1 and Elev+Slope within limits", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(LBL_VEGCOV = "NC",
                                       ELEV = 1000, MEAN_SLOPE = 30))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_WFD_All"), 1L)
})

test_that("Static_WFD_All = 1 when Static_Persist_Decid=1 and Elev+Slope OK", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # AC+70%+age 70 → Persist_Decid=1
  dr <- default_row(SPEC_CD_1 = "AC", SPEC_PCT_1 = 70, PROJ_AGE_1 = 70,
                    ELEV = 1000, MEAN_SLOPE = 30)
  make_vri_tbl(conn, df = dr)
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_WFD_All"), 1L)
})

test_that("Static_WFD_All = 0 when none of the static flags are set", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Static_WFD_All"), 0L)
})

# ---------------------------------------------------------------------------
# Dynamic_WFD_All
# ---------------------------------------------------------------------------

test_that("Dynamic_WFD_All = 1 when Dynamic_WFD_4to10=1", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 7))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_WFD_All"), 1L)
})

test_that("Dynamic_WFD_All = 1 when Dynamic_WFD_11to30=1", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 20))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_WFD_All"), 1L)
})

test_that("Dynamic_WFD_All = 0 when neither age class matches", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 50))
  make_fire_tbl(conn)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_WFD_All"), 0L)
})

# ---------------------------------------------------------------------------
# Dynamic_L
# ---------------------------------------------------------------------------

test_that("Dynamic_L = 1 when harvest within 31 years", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # current_year=2025, HARVEST_YEAR=2000 → 25 years ≤ 31 → 1
  make_vri_tbl(conn, df = default_row(HARVEST_YEAR = 2000L))
  make_fire_tbl(conn)
  run_hem(conn, year = 2025L)

  expect_equal(get1(conn, "Dynamic_L"), 1L)
})

test_that("Dynamic_L = 0 when harvest older than 31 years", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # current_year=2025, HARVEST_YEAR=1990 → 35 years > 31 → 0
  make_vri_tbl(conn, df = default_row(HARVEST_YEAR = 1990L))
  make_fire_tbl(conn)
  run_hem(conn, year = 2025L)

  expect_equal(get1(conn, "Dynamic_L"), 0L)
})

test_that("Dynamic_L = 0 and warning issued when harvest_year_col absent", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Build table WITHOUT HARVEST_YEAR column
  df_no_harvest <- default_row()
  df_no_harvest$HARVEST_YEAR <- NULL
  make_vri_tbl(conn, df = df_no_harvest)
  make_fire_tbl(conn)

  expect_warning(
    calc_hem_fields_duckdb(conn, "VRIBEM_TEST",
                           fire_tbl     = "FIRE_TEST",
                           current_year = 2025L),
    "'HARVEST_YEAR'"
  )
  expect_equal(get1(conn, "Dynamic_L"), 0L)
})

# ---------------------------------------------------------------------------
# fire_pct and Dynamic_F (geometry-dependent)
# ---------------------------------------------------------------------------

test_that("fire_pct = 0 when no fire polygon overlaps VRI", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn)  # empty fire table
  run_hem(conn)

  expect_equal(get1(conn, "fire_pct"), 0)
})

test_that("fire_pct ≈ 70 when fire covers 70% of VRI polygon", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # VRI = (0,0)-(100,100),  fire = (0,0)-(70,100)
  # intersection area = 7000, VRI area = 10000 → pct = 70
  make_vri_tbl(conn, df = default_row())
  make_fire_tbl(conn, x0 = 0, y0 = 0, x1 = 70, y1 = 100)
  run_hem(conn)

  expect_equal(get1(conn, "fire_pct"), 70, tolerance = 1e-4)
})

test_that("Dynamic_F = 1 when fire_pct > 50 and Dynamic_WFD_All = 1", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # age_cl = 7 → Dynamic_WFD_4to10=1 → Dynamic_WFD_All=1
  # fire covers 70% → Dynamic_F = 1
  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 7))
  make_fire_tbl(conn, x0 = 0, y0 = 0, x1 = 70, y1 = 100)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_WFD_All"), 1L)
  expect_equal(get1(conn, "Dynamic_F"), 1L)
})

test_that("Dynamic_F = 0 when fire_pct <= 50", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Fire covers only 40% — below threshold
  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 7))
  make_fire_tbl(conn, x0 = 0, y0 = 0, x1 = 40, y1 = 100)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_F"), 0L)
})

test_that("Dynamic_F = 0 when fire_pct > 50 but Dynamic_WFD_All = 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # age_cl = 50 → Dynamic_WFD_All=0; fire=70% but Dynamic_F stays 0
  make_vri_tbl(conn, df = default_row(VRI_AGE_CL_STS = 50))
  make_fire_tbl(conn, x0 = 0, y0 = 0, x1 = 70, y1 = 100)
  run_hem(conn)

  expect_equal(get1(conn, "Dynamic_F"), 0L)
})

# ---------------------------------------------------------------------------
# Custom parameters
# ---------------------------------------------------------------------------

test_that("age_cl_sts_col parameter supports alternate column names", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Rename VRI_AGE_CL_STS → AGE_CL_STS to test the parameter
  df <- default_row(VRI_AGE_CL_STS = 7)
  names(df)[names(df) == "VRI_AGE_CL_STS"] <- "AGE_CL_STS"
  make_vri_tbl(conn, df = df)
  make_fire_tbl(conn)

  calc_hem_fields_duckdb(conn, "VRIBEM_TEST",
                         fire_tbl       = "FIRE_TEST",
                         age_cl_sts_col = "AGE_CL_STS",
                         current_year   = 2025L)

  expect_equal(get1(conn, "Dynamic_WFD_4to10"), 1L)
})
