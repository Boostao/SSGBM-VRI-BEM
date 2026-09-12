# Hybrid DuckDB / terra pipeline
#
# Architecture
# ─────────────────────────────────────────────────────────────────────────────
# DuckDB phase  — all polygon-level, elevation-free BEM corrections:
#   D01  init_conn + filtered_views
#   D02  vribem_view → materialise as VRIBEM table
#          (amend_large_polygons_duckdb skipped — per-cell raster corrections
#           handle glacier/lake/wetland attribution, making the polygon split
#           redundant)
#   D03  vribem_corrections_view                        (VRI rules + INTERSECTS_RIVER)
#   D04  vri_bem_wetlands_corrections_view(skip_riparian = TRUE)
#          ↳ riparian MEAN_SLOPE adjust deferred to raster phase
#   D05  merge_geometry_duckdb (CCB)  +  merge_fire_perimeters_duckdb
#   D06  export VRIBEM_FDL to GeoPackage
#
# Rasterize phase — convert corrected VRIBEM + ancillary layers to rasters:
#   VRI + BEM fields  → from corrected VRIBEM GeoPackage (single source)
#   wetlands          → still needed for per-cell wl_pct correction (T08)
#   rivers            → raster adjacency (SITE_M3A = "a") applied per-cell (T05)
#   elevation         → DEM for terrain derivation
#   NOTE: lakes rasterized separately — terra_rrm_correct_small_lakes handles
#         them per-cell (avoids the large-polygon attribution problem)
#         ccb   = NULL (HARVESTYR merged into VRIBEM via merge_geometry_duckdb,
#                       rasterized as a BEM numeric attribute instead)
#
# Terra phase   — per-cell, elevation-dependent corrections only:
#   T03   read_input_stack  (from corrected rasters)
#   T03b  compute_terrain_layers  (MEAN_SLOPE, ABOVE_ELEV_THOLD from DEM)
#   T05   apply_river_adjacency_stage     (per-cell, rivers raster)
#   T06   correct_small_lakes             (per-cell, lakes raster)
#   T08   correct_bem_from_wetlands       (per-cell wl_pct)
#   T08b  correct_bem_from_wetlands_riparian_stage  (per-cell MEAN_SLOPE < 10)
#   T09   apply_rules                     (per-cell ABOVE_ELEV_THOLD)
#   T10   calc_forest_age_class
#   T11   merge_unique_ecosystem_fields
#   T12   find_crown_area_dominant_values
#   T13   export moose ecosystem CSV
#   T14   export bear ecosystem CSV
#
# Steps skipped vs. terra_rrm_run_step_by_step.R:
#   [04] terra_rrm_correct_bem_from_vri          ← done in DuckDB (D03)
#   [07] terra_rrm_correct_bem_from_wetlands (polygon-level BEU swap)
#          ← done in DuckDB (D05); only per-cell wl_pct pass remains (T08)
#
# Steps kept in terra vs. pure-DuckDB SkWERM_run_example_duckdb.R:
#   [06] terra_rrm_correct_small_lakes           ← raster phase (per-cell)
#          correct_small_lakes_duckdb would cut polygons at vector level,
#          reintroducing the large-polygon attribution problem we avoided by
#          skipping amend_large_polygons_duckdb.
# ─────────────────────────────────────────────────────────────────────────────

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Please install 'devtools' to run this script.", call. = FALSE)
}

devtools::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
})


# User inputs ---------------------------------------------------------------

elevation_raster <- file.path("..", "SSGBM-VRI-BEM-data", "dem.tif")
data_folder      <- file.path("..", "SSGBM-VRI-BEM-data")

# DuckDB settings
duckdb_memory_limit <- "14GB"
duckdb_threads      <- 1L

# Elevation threshold for ABOVE_ELEV_THOLD (metres)
elevation_threshold <- 1400L

# AOI as WKT (Albers BC / EPSG:3005).  Replace with your study area.
aoi_wkt_full <- "MULTIPOLYGON (((1065018 932215.1, 941827.7 932215.1, 941827.7 1016988, 1065018 1016988, 1065018 932215.1)))"
# Small AOI for quick end-to-end testing (~2 km x 2 km).
aoi_wkt <- "POLYGON ((1000000 960000, 1002000 960000, 1002000 962000, 1000000 962000, 1000000 960000))"

# Path to BEM geodatabase (required for init_db)
bem_gdb <- file.path(data_folder, "Skeena_BEM.gdb")
bem_layer <- "Skeena_BEM_v1"

# BEU rules Excel file
rules_xl <- file.path(data_folder, "Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx")


# Fixed paths ---------------------------------------------------------------

data_folder      <- normalizePath(data_folder, winslash = "/", mustWork = TRUE)
vector_dir       <- file.path(data_folder, "vectors")
raster_dir       <- file.path(data_folder, "rasters")
output_dir       <- file.path(data_folder, "outputs")
duckdb_temp_dir  <- file.path(data_folder, "duckdb_tmp")

dir.create(vector_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(raster_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(duckdb_temp_dir, recursive = TRUE, showWarnings = FALSE)

elevation_raster <- normalizePath(elevation_raster, winslash = "/", mustWork = TRUE)

stopifnot(file.exists(rules_xl))


# Download vector inputs ----------------------------------------------------

message("\n[01] Download vector inputs")

download_vri(
  path = file.path(vector_dir, "vri.gpkg"),
  overwrite = TRUE
)

download_bem(
  path      = file.path(vector_dir, "bem.gpkg"),
  dsn       = bem_gdb,
  layer     = bem_layer,
  overwrite = TRUE
)

download_wetlands(
  path      = file.path(vector_dir, "wetlands.gpkg"),
  layer     = "FWA_WETLANDS_POLY",
  overwrite = TRUE
)

download_rivers(
  path      = file.path(vector_dir, "rivers.gpkg"),
  layer     = "FWA_RIVERS_POLY",
  overwrite = TRUE
)

download_lakes(
  path      = file.path(vector_dir, "lakes.gpkg"),
  layer     = "FWA_LAKES_POLY",
  overwrite = TRUE
)

download_ccb(
  path      = file.path(vector_dir, "ccb.gpkg"),
  layer     = "Cut_Block_all_BC",
  overwrite = TRUE
)


# D01 — Initialise DuckDB ---------------------------------------------------

message("\n[D01] Initialise DuckDB connection and load spatial data")

# Run init_db once per study area to seed the persistent DuckDB tables.
# Skip (or comment out) on subsequent runs when the DB already exists.
init_db(
  bem_dsn = bem_gdb
)

conn <- init_conn(
  temp_dir     = duckdb_temp_dir,
  memory_limit = duckdb_memory_limit,
  threads      = duckdb_threads
)

filtered_views(conn, wkt_filter = aoi_wkt, build_spatial_index = FALSE)


# D02 — Build VRIBEM view --------------------------------------------------
# amend_large_polygons_duckdb is intentionally omitted here.
# That function dissolves polygons >3500 ha and re-splits them by
# glacier/lake/wetland boundaries to fix homogeneous BEM attribution over
# heterogeneous terrain.  In the raster pipeline this problem does not arise:
#   - Glacier pixels are corrected per-cell by terra_rrm_correct_bem_from_vri
#     (BCLCS_LV_5 = "GL" rule).
#   - Lake pixels are corrected per-cell by terra_rrm_correct_small_lakes
#     using a dedicated lakes raster layer (T06).
#   - Wetland pixels are corrected per-cell by the wl_pct raster overlay (T08).
# The polygon split is only necessary when producing a corrected vector output.

message("\n[D02] Build VRIBEM view")

vribem_view(conn, validate_intersect = FALSE)

DBI::dbExecute(conn, "CREATE OR REPLACE TABLE VRIBEM AS SELECT * FROM V_VRIBEM;")


# D03 — VRI corrections (includes river adjacency via INTERSECTS_RIVER) ----

message("\n[D03] Apply VRI/BEM corrections")

duckdb::duckdb_read_csv(
  conn, "beu_bec_corr",
  "inst/csv/Allowed_BEC_BEUs_NE_ALL.csv",
  temporary = TRUE
)

vribem_corrections_view(
  conn,
  vri_bem_tbl          = "VRIBEM",
  beu_bec              = "beu_bec_corr",
  result_tbl           = "VRIBEM",
  skip_river_adjacency = TRUE
  # River adjacency deferred to terra T05 — SITE_M3A = "a" set only for pixels
  # physically covered by river pixels, not for the whole intersecting polygon.
)


# D05 — Wetlands (polygon-level BEU swap; riparian deferred to raster) -----

message("\n[D05] Wetland BEU corrections (skip_riparian = TRUE)")

duckdb::duckdb_read_csv(
  conn, "beu_wetland_updates",
  "inst/csv/beu_wetland_updates.csv",
  temporary = TRUE
)

vri_bem_wetlands_corrections_view(
  conn,
  vri_bem            = "VRIBEM",
  beu_wetland_updates = "beu_wetland_updates",
  skip_riparian      = TRUE
  # Riparian adjustment (MEAN_SLOPE < 10) deferred to terra step T08b where
  # per-cell DEM-derived slope is used instead of a polygon average.
)


# D05 — Disturbance layers (CCB + fire) ------------------------------------

message("\n[D05] Merge CCB and fire disturbance")

merge_geometry_duckdb(
  conn       = conn,
  x_tbl      = "VRIBEM",
  y_tbl      = "V_CCB",
  tolerance_m2 = 10,
  result_tbl = "VRIBEM_FDL"
)
# HARVESTYR is now a column in VRIBEM_FDL; it will be rasterized as a BEM
# numeric attribute in the rasterize phase instead of via a separate CCB raster.

# Temporary test fallback: if V_FIRE is missing FIRE_YEAR, mock it as 2010
# so merge_fire_perimeters_duckdb can run on environments with geometry-only
# fire views.
fire_tbl_for_merge <- "V_FIRE"
if (tbl_exists(conn, "V_FIRE")) {
  has_fire_year <- DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n
     FROM information_schema.columns
     WHERE upper(table_name) = 'V_FIRE'
       AND upper(column_name) = 'FIRE_YEAR';"
  )$n > 0

  if (!has_fire_year) {
    message("[D05] V_FIRE has no FIRE_YEAR; using mock FIRE_YEAR = 2010 for testing")
    DBI::dbExecute(
      conn,
      "CREATE OR REPLACE TEMP VIEW V_FIRE_MOCK AS
       SELECT Shape, CAST(2010 AS INTEGER) AS FIRE_YEAR
       FROM V_FIRE;"
    )
    fire_tbl_for_merge <- "V_FIRE_MOCK"
  }
}

merge_fire_perimeters_duckdb(
  conn,
  vri_bem_tbl = "VRIBEM_FDL",
  fire_tbl    = fire_tbl_for_merge
)
# most_recent_fire / percent_burned are added in-place.
DBI::dbGetQuery(conn, "SELECT * from VRIBEM_FDL") |> names()
# D06 — Export corrected VRIBEM to GeoPackage for rasterization ------------

message("\n[D06] Export corrected VRIBEM to GeoPackage")

vribem_gpkg <- file.path(vector_dir, "vribem_corrected.gpkg")

vribem_wkb <- DBI::dbGetQuery(
  conn,
  "SELECT * EXCLUDE Shape, ST_AsWKB(Shape) AS wkb FROM VRIBEM_FDL"
)
vribem_sf <- sf::st_sf(
  vribem_wkb[, setdiff(names(vribem_wkb), "wkb")],
  geometry = sf::st_as_sfc(vribem_wkb$wkb)
)

# Temporary test fallback: ensure HARVESTYR exists for rasterization inputs.
if (!"HARVESTYR" %in% names(vribem_sf)) {
  message("[D06] HARVESTYR missing; mocking HARVESTYR = 2012 for testing")
  vribem_sf$HARVESTYR <- 2012L
}

sf::st_write(vribem_sf, dsn = vribem_gpkg, layer = "VRIBEM", append = FALSE)
rm(vribem_wkb, vribem_sf)

DBI::dbDisconnect(conn, shutdown = TRUE)
message("DuckDB connection closed.")


# Rasterize -----------------------------------------------------------------

message("\n[02] Rasterize inputs")

# VRI and BEM fields are merged into one layer — single rasterization pass.
vribem_dsn   <- file.path(raster_dir, "vribem.tif")
wetlands_dsn <- file.path(raster_dir, "wetlands.tif")
rivers_dsn   <- file.path(raster_dir, "rivers.tif")
lakes_dsn    <- file.path(raster_dir, "lakes.tif")
# ccb not rasterized — HARVESTYR merged into VRIBEM via merge_geometry_duckdb (D06)

rasterize_vribem_materialized(
  src_datasource = vribem_gpkg,
  layer          = "VRIBEM",
  dst_filename   = vribem_dsn,
  reference      = elevation_raster,
  aoi            = aoi_wkt,
  dynamic_factor_conv = TRUE,
  dynamic_factor_strategy = "append",
  output_raster  = FALSE,
  verbose        = TRUE
)

rasterize_wetlands_materialized(
  src_datasource = file.path(vector_dir, "wetlands.gpkg"),
  layer          = "WETLANDS",
  dst_filename   = wetlands_dsn,
  reference      = elevation_raster,
  aoi            = aoi_wkt,
  dynamic_factor_conv = TRUE,
  dynamic_factor_strategy = "append",
  output_raster  = FALSE,
  verbose        = TRUE
)

rasterize_rivers_materialized(
  src_datasource = file.path(vector_dir, "rivers.gpkg"),
  layer          = "RIVERS",
  dst_filename   = rivers_dsn,
  reference      = elevation_raster,
  aoi            = aoi_wkt,
  dynamic_factor_conv = TRUE,
  dynamic_factor_strategy = "append",
  output_raster  = FALSE,
  verbose        = TRUE
)
# The rivers layer is consumed by .terra_rrm_apply_river_adjacency_stage at T05.

rasterize_lakes_materialized(
  src_datasource = file.path(vector_dir, "lakes.gpkg"),
  layer          = "LAKES",
  dst_filename   = lakes_dsn,
  reference      = elevation_raster,
  aoi            = aoi_wkt,
  dynamic_factor_conv = TRUE,
  dynamic_factor_strategy = "append",
  output_raster  = FALSE,
  verbose        = TRUE
)


# Read support tables -------------------------------------------------------

buc                 <- fread(file.path("inst", "csv", "beu_wetland_updates.csv"))
unique_ecosystem_dt <- read_unique_ecosystem_dt(file.path("inst", "csv", "Skeena_VRIBEM_LUT.csv"))


# T03 — Read aligned raster stack -------------------------------------------

message("\n[T03] Read aligned raster stack")

step_stack <- .terra_rrm_read_input_stack_from_vribem(
  vribem_dsn    = vribem_dsn,
  rivers_dsn    = rivers_dsn,
  wetlands_dsn  = wetlands_dsn,
  lakes_dsn     = lakes_dsn,
  elevation_dsn = elevation_raster,
  aoi           = aoi_wkt,
  filename      = file.path(output_dir, "T03_input_stack.tif"),
  overwrite     = TRUE
)


# T03b — Per-cell terrain layers (MEAN_SLOPE, ABOVE_ELEV_THOLD) -----------

message("\n[T03b] Compute per-cell terrain layers")

step_stack <- terra_rrm_compute_terrain_layers(
  x                   = step_stack,
  elevation_threshold = elevation_threshold,
  filename            = file.path(output_dir, "T03b_terrain_layers.tif"),
  overwrite           = TRUE
)
# Returns a zero-copy virtual multi-source SpatRaster:
#   c(T03_input_stack.tif,  T03b_terrain_layers.tif)


# T05 — Per-cell river adjacency (SITE_M3A = "a") -------------------------
# Skipped in DuckDB phase: INTERSECTS_RIVER flags the whole polygon if any
# part touches a river, so a large polygon spanning non-riparian terrain would
# incorrectly get SITE_M3A = "a" everywhere.  The raster stage sets it only
# for pixels physically covered by river pixels.

message("\n[T05] Apply river adjacency (per-cell)")

step_rivers <- .terra_rrm_apply_river_adjacency_stage(
  x = step_stack
  # No filename: 1 ifel operation, chains lazily into T06 which writes.
)


# T06 — Per-cell lake corrections ------------------------------------------
# Skipped in DuckDB phase: correct_small_lakes_duckdb cuts polygons at vector
# level, which reintroduces the large-polygon attribution problem.  Doing it
# here on the lakes raster means every pixel is evaluated independently.

message("\n[T06] Correct small lakes (per-cell)")

step_lakes <- terra_rrm_correct_small_lakes(
  x         = step_rivers,
  lake_layer = "lakes",
  filename  = file.path(output_dir, "T06_lakes.tif"),
  overwrite = TRUE
)


# T08 — Per-cell wetland corrections (wl_pct) ------------------------------
# Checkpoint: the wetlands function accumulates ~40 ifel chains internally.
# Writing here prevents an excessively deep lazy DAG feeding into T09.

message("\n[T08] Correct BEM from wetlands (per-cell wl_pct)")

step_wetlands <- terra_rrm_correct_bem_from_wetlands(
  x        = step_lakes,
  buc      = buc,
  filename = file.path(output_dir, "T08_wetland_corrections.tif"),
  overwrite = TRUE
)


# T08b — Riparian adjustment (per-cell MEAN_SLOPE < 10) -------------------
# Deferred from DuckDB (D05 used skip_riparian = TRUE) so that DEM-derived
# per-cell slope is used instead of a polygon-averaged value.

message("\n[T08b] Riparian wetland corrections (per-cell MEAN_SLOPE)")

step_riparian <- terra_rrm_correct_bem_from_wetlands_riparian_stage(
  x = step_wetlands
  # No filename: shallow DAG (~8 ifel ops), chains lazily into T09 which writes.
)


# T09 — BEU rules (per-cell ABOVE_ELEV_THOLD) ------------------------------
# Checkpoint: rules loop builds 1 ifel per rule row; can be hundreds deep.
# Writing here resets the DAG before T10–T12.

message("\n[T09] Apply BEU rules")

step_rules <- terra_rrm_apply_rules(
  x        = step_riparian,
  rules_dt = rules_xl,
  filename = file.path(output_dir, "T09_rules.tif"),
  overwrite = TRUE
)


# T09b — Final ecosystem keys -----------------------------------------------
# BEUMC fields can change in T06/T08/T08b/T09, so the merge keys used by T11
# are generated here from the final post-correction raster state rather than
# from the original vector export.

message("\n[T09b] Build final ecosystem keys")

step_keys <- terra_rrm_add_ecosystem_keys(
  x = step_rules,
  filename = file.path(output_dir, "T09b_ecosystem_keys.tif"),
  overwrite = TRUE
)


# T10 — Forest age class ----------------------------------------------------

message("\n[T10] Calculate forest age class")

step_forest_age <- terra_rrm_calc_forest_age_class(
  x                        = step_keys,
  most_recent_harvest_year = as.integer(format(Sys.Date(), "%Y"))
  # No filename: shallow DAG, chains lazily into T11 which writes.
)


# T11 — Unique ecosystem fields ---------------------------------------------
# Checkpoint: loops over 3 components × N LUT rows, building a very deep ifel
# DAG.  Writing here avoids evaluating T10+T11 lazily on top of the T09 tif.
# The LUT is reduced before matching so we only keep the codes present in the
# current raster stack plus deduplicated ecosystem rows; this avoids evaluating
# irrelevant lookup combinations on every cell.

message("\n[T11] Merge unique ecosystem fields")

reduced_unique_ecosystem_dt <- terra_rrm_reduce_unique_ecosystem_dt(
  unique_ecosystem_dt,
  available_codes = .terra_rrm_available_ecosystem_codes(step_forest_age)
)

step_unique_eco <- terra_rrm_merge_unique_ecosystem_fields(
  x                  = step_forest_age,
  unique_ecosystem_dt = reduced_unique_ecosystem_dt,
  filename           = file.path(output_dir, "T11_unique_ecosystem.tif"),
  overwrite          = TRUE
)


# T12 — Crown area dominant values -----------------------------------------
# No intermediate write: shallow DAG, final output written directly.

message("\n[T12] Find crown area dominant values")

step_crown <- terra_rrm_find_crown_area_dominant_values(
  x        = step_unique_eco,
  filename = file.path(output_dir, "T12_crown.tif"),
  overwrite = TRUE
)




# T13 — Moose ecosystem export ----------------------------------------------

message("\n[T13] Export moose ecosystem")

moose_export_dt <- terra_rrm_create_RRM_ecosystem_moose(step_crown)
fwrite(moose_export_dt, file.path(output_dir, "T13_moose_export.csv"))


# T14 — Bear ecosystem export -----------------------------------------------

message("\n[T14] Export grizzly bear ecosystem")

bear_export_dt <- terra_rrm_create_RRM_ecosystem_bear(step_crown)
fwrite(bear_export_dt, file.path(output_dir, "T14_bear_export.csv"))

message("\nHybrid pipeline complete.")
