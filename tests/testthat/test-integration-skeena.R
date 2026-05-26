library(testthat)
library(duckdb)
library(DBI)

# ---------------------------------------------------------------------------
# Integration test: full Skeena workflow up to amend_large_polygons_duckdb
#
# Requires:
#   - D:/Boostao/SSGBM-data/Skeena Region Boundary/Skeena_region.{shp,dbf,...}
#   - ssgbm.duckdb already initialised with VRI, BEM, LAKES, GLACIERS, WETLANDS
#
# Skipped automatically when data files are not present (CI / other machines).
# ---------------------------------------------------------------------------

skeena_dir <- "D:/Boostao/SSGBM-data/Skeena Region Boundary"
db_path    <- tools::R_user_dir("SSGBM.VRI.BEM", which = "cache")
db_file    <- file.path(db_path, "ssgbm.duckdb")

test_that("amend_large_polygons_duckdb completes without OOM on full Skeena data", {
  skip_if(!dir.exists(skeena_dir),
          "Skeena Region Boundary data not found – skipping integration test")
  skip_if(!file.exists(db_file),
          "ssgbm.duckdb not initialized – skipping integration test")

  # 20 GB gives ~6 GiB headroom above the static buffer footprint created by
  # materialising V_VRIBEM (1.4 M rows) + 5 RTREE indexes during
  # amend_large_polygons_duckdb.  14 GB leads to OOM at the per-rowid
  # ST_Intersection step when the buffer pool is fully occupied.
  conn <- try(
    init_conn(temp_dir = "./duckdb_tmp", memory_limit = "14GB", threads = 1L),
    silent = TRUE
  )
  skip_if(inherits(conn, "try-error"),
          "Could not open ssgbm.duckdb (may be locked by another session)")
  on.exit({
    try(DBI::dbExecute(conn, "DROP TABLE IF EXISTS VRIBEM"),  silent = TRUE)
    duckdb::dbDisconnect(conn, shutdown = TRUE)
  }, add = TRUE)

  aoi_wkt <- sf::st_read(skeena_dir, layer = "Skeena_region", quiet = TRUE)$geometry |>
    sf::st_transform(3005) |>
    sf::st_union() |>
    sf::st_as_text()

  filtered_views(conn, aoi_wkt, build_spatial_index = FALSE)

  vribem_view(conn, validate_intersect = FALSE)

  expect_no_error(
    amend_large_polygons_duckdb(conn,
                                vri_bem_tbl  = "V_VRIBEM",
                                lakes_tbl    = "V_LAKES",
                                glaciers_tbl = "V_GLACIERS",
                                wetlands_tbl = "V_WETLANDS",
                                bem_tbl      = "V_BEM",
                                result_tbl   = "VRIBEM")
  )

  n <- DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM VRIBEM")$n
  expect_gt(n, 0L)
})
