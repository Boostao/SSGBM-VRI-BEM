# Run this script from the package root.

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
data_folder <- file.path("..", "SSGBM-VRI-BEM-data")

# Raster-stage adjacency buffers, expressed in raster cells.
# Set to 0L to keep the exact feature footprint only.
river_buffer_cells <- 2L
lake_buffer_cells  <- 2L


# Fixed paths ---------------------------------------------------------------

data_folder <- normalizePath(data_folder, winslash = "/", mustWork = TRUE)
vector_dir <- file.path(data_folder, "vectors")
raster_dir <- file.path(data_folder, "rasters")
output_dir <- file.path(data_folder, "outputs")

dir.create(vector_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

elevation_raster <- normalizePath(elevation_raster, winslash = "/", mustWork = TRUE)
reference_resolution_m <- max(terra::res(terra::rast(elevation_raster)))
river_buffer_m <- river_buffer_cells * reference_resolution_m
lake_buffer_m  <- lake_buffer_cells * reference_resolution_m

stopifnot(file.exists(file.path(data_folder, "Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx")))

# Download ------------------------------------------------------------------

message("\n[01] Download vector inputs")

download_vri(
  path = file.path(vector_dir, "vri.gpkg"),
  overwrite = TRUE
)

download_bem(
  path = file.path(vector_dir, "bem.gpkg"),
  dsn = file.path(data_folder, "BEM_VRI"),
  layer = "BEM",
  overwrite = TRUE
)

download_wetlands(
  path = file.path(vector_dir, "wetlands.gpkg"),
  layer = "FWA_WETLANDS_POLY",
  overwrite = TRUE
)

download_rivers(
  path = file.path(vector_dir, "rivers.gpkg"),
  layer = "FWA_RIVERS_POLY",
  overwrite = TRUE
)

download_lakes(
  path = file.path(vector_dir, "lakes.gpkg"),
  layer = "FWA_LAKES_POLY",
  overwrite = TRUE
)

download_ccb(
  path = file.path(vector_dir, "ccb.gpkg"),
  layer = "Cut_Block_all_BC",
  overwrite = TRUE
)


# Rasterize -----------------------------------------------------------------

message("\n[02] Rasterize vector inputs")

vri_dsn <- file.path(raster_dir, "vri.tif")
bem_dsn <- file.path(raster_dir, "bem.tif")
wetlands_dsn <- file.path(raster_dir, "wetlands.tif")
rivers_dsn <- file.path(raster_dir, "rivers.tif")
lakes_dsn <- file.path(raster_dir, "lakes.tif")
ccb_dsn <- file.path(raster_dir, "ccb.tif")

rasterize_vri_materialized(
  src_datasource = file.path(vector_dir, "vri.gpkg"),
  layer = "VRI",
  dst_filename = vri_dsn,
  reference = elevation_raster,
  output_raster = FALSE,
  verbose = TRUE
)


rasterize_bem_materialized(
  src_datasource = "/Users/nicolas/Documents/GitHub/SSGBM-VRI-BEM-data/Skeena_BEM.gdb",
  layer = "Skeena_BEM_v1",
  dst_filename = bem_dsn,
  reference = elevation_raster,
  output_raster = FALSE,
  verbose = TRUE
)

rasterize_wetlands_materialized(
  src_datasource = file.path(vector_dir, "wetlands.gpkg"),
  layer = "WETLANDS",
  dst_filename = wetlands_dsn,
  reference = elevation_raster,
  output_raster = FALSE,
  verbose = TRUE
)

rasterize_rivers_materialized(
  src_datasource = file.path(vector_dir, "rivers.gpkg"),
  layer = "RIVERS",
  dst_filename = rivers_dsn,
  reference = elevation_raster,
  output_raster = FALSE,
  verbose = TRUE
)

rasterize_sf_gdal_materialized(
  src_datasource = file.path(vector_dir, "lakes.gpkg"),
  layer = "LAKES",
  dst_filename = lakes_dsn,
  reference = elevation_raster,
  burn = "lakes",
  output_raster = FALSE,
  verbose = TRUE
)

rasterize_ccb_materialized(
  src_datasource = file.path(vector_dir, "ccb.gpkg"),
  layer = "CCB",
  dst_filename = ccb_dsn,
  reference = elevation_raster,
  output_raster = FALSE,
  verbose = TRUE
)


# Read support tables -------------------------------------------------------

buc <- fread(file.path("inst", "csv", "beu_wetland_updates.csv"))
beu_bec <- fread(file.path("inst", "csv", "Allowed_BEC_BEUs_NE_ALL.csv"))
unique_ecosystem_dt <- read_unique_ecosystem_dt(file.path("inst", "csv", "Skeena_VRIBEM_LUT.csv"))


# Run terra pipeline --------------------------------------------------------

# [03] Read aligned raster stack — write directly to disk
message("\n[03] Read aligned raster stack")
step_01_input_stack <- .terra_rrm_read_input_stack(
  vri_dsn = vri_dsn,
  bem_dsn = bem_dsn,
  rivers_dsn = rivers_dsn,
  wetlands_dsn = wetlands_dsn,
  lakes_dsn = lakes_dsn,
  ccb_dsn = ccb_dsn,
  elevation_dsn = elevation_raster,
  filename = file.path(output_dir, "01_input_stack.tif"),
  overwrite = TRUE
)

# [03b] Derive per-cell terrain layers — writes only MEAN_SLOPE + ABOVE_ELEV_THOLD
# (2 small layers); result is a zero-copy virtual stack pointing to both files.
message("\n[03b] Compute per-cell terrain layers (MEAN_SLOPE, ABOVE_ELEV_THOLD)")
step_01b_terrain <- terra_rrm_compute_terrain_layers(
  x = step_01_input_stack,
  elevation_threshold = 1400,   # adjust to match your study area threshold
  filename = file.path(output_dir, "01b_terrain_layers.tif"),
  overwrite = TRUE
)

message("\n[04] Correct BEM from VRI")
step_02_vri <- terra_rrm_correct_bem_from_vri(
  x = step_01b_terrain,
  clear_site_ma = TRUE,
  use_ifelse = TRUE,
  beu_bec = beu_bec,
  filename = file.path(output_dir, "02_vri_corrections.tif"),
  overwrite = TRUE
)

message("\n[05] Apply river adjacency")
step_03_river <- .terra_rrm_apply_river_adjacency_stage(
  x = step_02_vri,
  buffer_m = river_buffer_m
)
# river adjacency only sets 1 value on 1 layer — chain into next write

message("\n[06] Correct small lakes")
step_04_small_lakes <- terra_rrm_correct_small_lakes(
  x = step_03_river,
  lake_layer = "lakes",
  buffer_m = lake_buffer_m,
  filename = file.path(output_dir, "03_river_and_lakes.tif"),
  overwrite = TRUE
)

message("\n[07] Correct BEM from wetlands")
step_05_wetlands <- terra_rrm_correct_bem_from_wetlands(
  x = step_04_small_lakes,
  buc = buc,
  filename = file.path(output_dir, "04_wetland_corrections.tif"),
  overwrite = TRUE
)

message("\n[08] Apply riparian wetland corrections")
step_06_riparian <- terra_rrm_correct_bem_from_wetlands_riparian_stage(
  x = step_05_wetlands,
  filename = file.path(output_dir, "05_riparian_wetlands.tif"),
  overwrite = TRUE
)

message("\n[09] Apply rules")
step_07_rules <- terra_rrm_apply_rules(
  x = step_06_riparian,
  rules_dt = file.path(data_folder, "Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx"),
  filename = file.path(output_dir, "06_rules.tif"),
  overwrite = TRUE
)

message("\n[09b] Build final ecosystem keys")
step_07b_keys <- terra_rrm_add_ecosystem_keys(
  x = step_07_rules,
  filename = file.path(output_dir, "06b_ecosystem_keys.tif"),
  overwrite = TRUE
)

message("\n[10] Calculate forest age")
step_08_forest_age <- terra_rrm_calc_forest_age_class(
  x = step_07b_keys,
  most_recent_harvest_year = as.integer(format(Sys.Date(), "%Y")),
  filename = file.path(output_dir, "07_forest_age.tif"),
  overwrite = TRUE
)

message("\n[11] Merge unique ecosystem fields")
step_09_unique_ecosystem <- terra_rrm_merge_unique_ecosystem_fields(
  x = step_08_forest_age,
  unique_ecosystem_dt = unique_ecosystem_dt,
  filename = file.path(output_dir, "08_unique_ecosystem.tif"),
  overwrite = TRUE
)

message("\n[12] Find crown area dominant values")
step_10_crown <- terra_rrm_find_crown_area_dominant_values(
  x = step_09_unique_ecosystem,
  filename = file.path(output_dir, "09_crown.tif"),
  overwrite = TRUE
)


# Export --------------------------------------------------------------------

message("\n[13] Export moose ecosystem")
moose_export_dt <- terra_rrm_create_RRM_ecosystem_moose(step_10_crown)
fwrite(moose_export_dt, file.path(output_dir, "11_moose_export.csv"))

message("\n[14] Export bear ecosystem")
bear_export_dt <- terra_rrm_create_RRM_ecosystem_bear(step_10_crown)
fwrite(bear_export_dt, file.path(output_dir, "12_bear_export.csv"))
