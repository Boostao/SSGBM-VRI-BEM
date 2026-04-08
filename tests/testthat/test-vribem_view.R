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

# Populate the minimal tables required by vribem_view:
#   VRI, BEM, V_VRI, V_BEM, V_RIVERS
# All tables use BC Albers-like metre coordinates.
# `bem_rows` is a list of lists, each with:
#   teis_id, bgc_zone, bgc_subzon, bgc_vrt, bgc_phase, sdec_1, beumc_s1,
#   x0, y0, x1, y1
make_vribem_env <- function(conn,
                             vri_rows = list(
                               list(feature_id = 1L,
                                    x0 = 900, y0 = 900, x1 = 2100, y1 = 2100)
                             ),
                             bem_rows = list(
                               list(teis_id = 101L, bgc_zone = "SBS",
                                    bgc_subzon = "wk", bgc_vrt = 1L,
                                    bgc_phase  = NA_character_,
                                    sdec_1 = 10L, beumc_s1 = "AT",
                                    x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)
                             )) {

  # ---- VRI ----------------------------------------------------------------
  vri_sql <- paste(
    vapply(vri_rows, function(r) {
      sprintf(
        "SELECT CAST(%d AS INTEGER) AS FEATURE_ID,
                CAST(NULL AS VARCHAR)  AS BCLCS_LEVEL_1,
                CAST(NULL AS VARCHAR)  AS BCLCS_LEVEL_2,
                CAST(NULL AS VARCHAR)  AS BCLCS_LEVEL_3,
                CAST(NULL AS VARCHAR)  AS BCLCS_LEVEL_4,
                CAST(NULL AS VARCHAR)  AS BCLCS_LEVEL_5,
                CAST(NULL AS VARCHAR)  AS VRI_BEC_ZONE,
                CAST(NULL AS VARCHAR)  AS VRI_BEC_SUBZON,
                CAST(NULL AS VARCHAR)  AS VRI_BEC_VRT,
                CAST(NULL AS VARCHAR)  AS VRI_BEC_PHASE,
                CAST(NULL AS DOUBLE)   AS PROJ_AGE_1,
                CAST(NULL AS VARCHAR)  AS SPECIES_CD_1,
                CAST(NULL AS DOUBLE)   AS SPECIES_PCT_1,
                CAST(NULL AS VARCHAR)  AS SPECIES_CD_2,
                CAST(NULL AS DOUBLE)   AS SPECIES_PCT_2,
                CAST(NULL AS VARCHAR)  AS SPECIES_CD_3,
                CAST(NULL AS DOUBLE)   AS SPECIES_PCT_3,
                CAST(NULL AS VARCHAR)  AS SPECIES_CD_4,
                CAST(NULL AS DOUBLE)   AS SPECIES_PCT_4,
                CAST(NULL AS VARCHAR)  AS SPECIES_CD_5,
                CAST(NULL AS DOUBLE)   AS SPECIES_PCT_5,
                CAST(NULL AS VARCHAR)  AS SPECIES_CD_6,
                CAST(NULL AS DOUBLE)   AS SPECIES_PCT_6,
                CAST(NULL AS DOUBLE)   AS CROWN_CLOSURE,
                CAST(NULL AS VARCHAR)  AS LAND_COVER_CLASS_CD_1,
                CAST(NULL AS DOUBLE)   AS EST_COVERAGE_PCT_1,
                CAST(NULL AS VARCHAR)  AS INVENTORY_STANDARD_CD,
                CAST(NULL AS VARCHAR)  AS LINE_5_VEGETATION_COVER,
                CAST(NULL AS DATE)     AS HARVEST_DATE,
                CAST(NULL AS INTEGER)  AS REFERENCE_YEAR,
                CAST(NULL AS VARCHAR)  AS SOIL_MOISTURE_REGIME_1,
                CAST(NULL AS VARCHAR)  AS SOIL_NUTRIENT_REGIME,
                CAST(NULL AS VARCHAR)  AS BEC_ZONE,
                CAST(NULL AS VARCHAR)  AS BEC_SUBZONE,
                CAST(NULL AS VARCHAR)  AS BEC_VARIANT,
                CAST(NULL AS VARCHAR)  AS BEC_PHASE,
                ST_GeomFromText('POLYGON ((%d %d, %d %d, %d %d, %d %d, %d %d))') AS Shape",
        r$feature_id,
        r$x0, r$y0, r$x1, r$y0, r$x1, r$y1, r$x0, r$y1, r$x0, r$y0
      )
    }, character(1L)),
    collapse = "\nUNION ALL\n"
  )
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TABLE VRI AS %s", vri_sql))
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TABLE V_VRI AS SELECT * FROM VRI"))

  # ---- BEM ----------------------------------------------------------------
  bem_sql <- paste(
    vapply(bem_rows, function(r) {
      sprintf(
        "SELECT CAST(%d AS INTEGER)          AS TEIS_ID,
                CAST('%s' AS VARCHAR)         AS BGC_ZONE,
                CAST('%s' AS VARCHAR)         AS BGC_SUBZON,
                CAST(%d AS INTEGER)           AS BGC_VRT,
                CAST(%s AS VARCHAR)           AS BGC_PHASE,
                CAST(%d AS INTEGER)           AS SDEC_1,
                CAST('%s' AS VARCHAR)         AS BEUMC_S1,
                CAST(NULL AS VARCHAR)         AS BEUMC_S2,
                CAST(NULL AS VARCHAR)         AS BEUMC_S3,
                CAST(NULL AS INTEGER)         AS SDEC_2,
                CAST(NULL AS INTEGER)         AS SDEC_3,
                CAST(NULL AS VARCHAR)         AS FORESTED_1,
                CAST(NULL AS VARCHAR)         AS FORESTED_2,
                CAST(NULL AS VARCHAR)         AS FORESTED_3,
                CAST(NULL AS VARCHAR)         AS STRCT_S1,
                CAST(NULL AS VARCHAR)         AS STRCT_S2,
                CAST(NULL AS VARCHAR)         AS STRCT_S3,
                CAST(NULL AS VARCHAR)         AS STAND_A1,
                CAST(NULL AS VARCHAR)         AS STAND_A2,
                CAST(NULL AS VARCHAR)         AS STAND_A3,
                ST_GeomFromText('POLYGON ((%d %d, %d %d, %d %d, %d %d, %d %d))') AS Shape",
        r$teis_id,
        r$bgc_zone, r$bgc_subzon, r$bgc_vrt,
        if (is.na(r$bgc_phase)) "NULL" else sprintf("'%s'", r$bgc_phase),
        r$sdec_1, r$beumc_s1,
        r$x0, r$y0, r$x1, r$y0, r$x1, r$y1, r$x0, r$y1, r$x0, r$y0
      )
    }, character(1L)),
    collapse = "\nUNION ALL\n"
  )
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TABLE BEM AS %s", bem_sql))
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE TABLE V_BEM AS SELECT * FROM BEM"))

  # ---- V_RIVERS (empty – not needed for polygon tests) --------------------
  DBI::dbExecute(conn, "
    CREATE OR REPLACE TABLE V_RIVERS AS
    SELECT ST_GeomFromText('POINT (0 0)') AS Shape
    WHERE 1 = 0
  ")

  invisible(conn)
}

# ---------------------------------------------------------------------------
# Tests: vribem_view with no duplicates
# ---------------------------------------------------------------------------

test_that("vribem_view creates V_VRIBEM with expected rows for overlapping polygons", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vribem_env(conn)

  expect_no_error(vribem_view(conn, validate_intersect = FALSE))

  rows <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRIBEM")$n
  expect_equal(rows, 1L)
})

# ---------------------------------------------------------------------------
# Tests: duplicate TEIS_ID in V_BEM (materialized table path)
# ---------------------------------------------------------------------------

test_that("vribem_view deduplicates V_BEM (table) and emits a warning", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Two BEM rows with the same TEIS_ID but different (non-overlapping) shapes
  make_vribem_env(conn, bem_rows = list(
    list(teis_id = 101L, bgc_zone = "SBS", bgc_subzon = "wk", bgc_vrt = 1L,
         bgc_phase = NA_character_, sdec_1 = 10L, beumc_s1 = "AT",
         x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000),
    list(teis_id = 101L, bgc_zone = "SBS", bgc_subzon = "wk", bgc_vrt = 1L,
         bgc_phase = NA_character_, sdec_1 = 10L, beumc_s1 = "AT",
         x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)   # exact duplicate
  ))

  # Should warn about duplicates (native warning(), not logger message)
  expect_warning(
    vribem_view(conn, validate_intersect = FALSE),
    regexp = "duplicate TEIS_ID"
  )

  # After dedup V_BEM must have only 1 row
  n_v_bem <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_BEM")$n
  expect_equal(n_v_bem, 1L)

  # The join should still produce a result (VRI ∩ BEM overlap)
  n_vribem <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRIBEM")$n
  expect_equal(n_vribem, 1L)
})

test_that("vribem_view deduplicates V_BEM (view path) and emits a warning", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vribem_env(conn, bem_rows = list(
    list(teis_id = 202L, bgc_zone = "SBS", bgc_subzon = "wk", bgc_vrt = 1L,
         bgc_phase = NA_character_, sdec_1 = 10L, beumc_s1 = "AT",
         x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000),
    list(teis_id = 202L, bgc_zone = "SBS", bgc_subzon = "wk", bgc_vrt = 1L,
         bgc_phase = NA_character_, sdec_1 = 10L, beumc_s1 = "AT",
         x0 = 1000, y0 = 1000, x1 = 2000, y1 = 2000)
  ))

  # Replace V_BEM TABLE with a VIEW to exercise the view-dedup path.
  # DROP the TABLE first so the VIEW doesn't shadow it — duckdb_views() must be
  # the only reference to V_BEM, otherwise the DELETE path would kick in via
  # the stale duckdb_tables() entry.
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS V_BEM")
  DBI::dbExecute(conn, "CREATE VIEW V_BEM AS SELECT * FROM BEM")

  expect_warning(
    vribem_view(conn, validate_intersect = FALSE),
    regexp = "duplicate TEIS_ID"
  )

  n_v_bem <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_BEM")$n
  expect_equal(n_v_bem, 1L)

  n_vribem <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM V_VRIBEM")$n
  expect_equal(n_vribem, 1L)
})

# ---------------------------------------------------------------------------
# Tests: init_bem deduplication
# ---------------------------------------------------------------------------

# Helper: write an in-memory BEM table with optional duplicate TEIS_IDs,
# then export it to a temporary GeoPackage for init_bem to read.
make_bem_gpkg <- function(teis_ids, crs = 3005) {
  tmp <- tempfile(fileext = ".gpkg")
  rows <- lapply(seq_along(teis_ids), function(i) {
    sf::st_sf(
      TEIS_ID  = teis_ids[[i]],
      BGC_ZONE = "SBS",
      BGC_SUBZON = "wk",
      BGC_VRT  = 1L,
      BGC_PHASE = NA_character_,
      SDEC_1   = 10L,
      BEUMC_S1 = "AT",
      geometry = sf::st_sfc(
        sf::st_polygon(list(cbind(
          c(1000, 2000, 2000, 1000, 1000),
          c(1000, 1000, 2000, 2000, 1000)
        ) + i * 10)),
        crs = crs
      )
    )
  })
  sfc <- do.call(rbind, rows)
  sf::st_write(sfc, tmp, layer = "BEM_TEST", quiet = TRUE, delete_dsn = TRUE,
               layer_options = "GEOMETRY_NAME=geometry")
  list(dsn = tmp, layer = "BEM_TEST")
}

test_that("init_bem removes duplicate TEIS_IDs at load time and warns", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_bem_gpkg(teis_ids = c(1L, 1L, 2L))  # TEIS_ID 1 appears twice
  on.exit(unlink(pkg$dsn), add = TRUE)

  expect_warning(
    init_bem(conn, ask = FALSE, dsn = pkg$dsn, layer = pkg$layer, geom = "geometry"),
    regexp = "duplicate TEIS_ID"
  )

  n <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM BEM")$n
  expect_equal(n, 2L)  # 3 input rows, 1 duplicate removed → 2 unique TEIS_IDs

  n_dup <- DBI::dbGetQuery(
    conn,
    "SELECT count(*) AS n FROM (SELECT TEIS_ID FROM BEM GROUP BY TEIS_ID HAVING count(*) > 1)"
  )$n
  expect_equal(n_dup, 0L)
})

test_that("init_bem with no duplicates does not warn and keeps all rows", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  pkg <- make_bem_gpkg(teis_ids = c(10L, 20L, 30L))  # all unique
  on.exit(unlink(pkg$dsn), add = TRUE)

  expect_no_warning(
    init_bem(conn, ask = FALSE, dsn = pkg$dsn, layer = pkg$layer, geom = "geometry")
  )

  n <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM BEM")$n
  expect_equal(n, 3L)
})
