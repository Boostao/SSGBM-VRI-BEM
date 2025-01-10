# library(terra)
# dem <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
# terra::inMemory(dem)
# terrain1 <- terra::terrain(dem, v = c("slope", "aspect"), unit = "radians")
# terra::inMemory(dem)
# terra::inMemory(terrain1)
# terrain2 <- terra::terrain(dem, v = c("slope", "aspect"), unit = "radians", filename = "terrain.tif")
# terra::inMemory(terrain2)
#
# # r <- terra::rast(crs = "EPSG:4326")
# # terra::ext(r) <- c(-139.5, -113.5, 48, 60);
# # terra::project(r, "EPSG:3005")
#
# mask <- terra::rast(
#   crs = "EPSG:3005",
#   resolution = units::as_units(100, "m"),
#   extent = terra::ext(-4140.005, 1932528, 330400.3, 1740337),
#   vals = 1
# )
# mask <- terra::mask(mask, bcmaps::bc_bound() |> terra::vect(), updatevalue = 0)
# terra::plet(mask)
#
#
#
# vri_vars <- c("BCLCS_LEVEL_1","BCLCS_LEVEL_2","BCLCS_LEVEL_3","BCLCS_LEVEL_4","BCLCS_LEVEL_5",
#               "SPECIES_CD_1","SPECIES_CD_2","SPECIES_CD_3","SPECIES_CD_4","SPECIES_CD_5","SPECIES_CD_6",
#               "SPECIES_PCT_1","SPECIES_PCT_2","SPECIES_PCT_3","SPECIES_PCT_4","SPECIES_PCT_5","SPECIES_PCT_6",
#               "CROWN_CLOSURE","LAND_COVER_CLASS_CD_1","EST_COVERAGE_PCT_1","LINE_5_VEGETATION_COVER",
#               "HARVEST_DATE","PROJ_AGE_1","SOIL_MOISTURE_REGIME_1","SOIL_NUTRIENT_REGIME","INVENTORY_STANDARD_CD",
#               "BEC_ZONE_CODE","BEC_SUBZONE","BEC_VARIANT","BEC_PHASE","REFERENCE_YEAR")
#
# for (f in vri_vars) {
#   terra::rasterize(terra::query(vri, vars = f, where = "%s IS NOT NULL"), y, field = f, filename = "../SSGBM-VRI-BEM-data/VRI_tif/%s.tif" |> sprintf(f))
# }
#
# a <- terra::query(vri, vars = f, where = "%s IS NOT NULL" |> sprintf(f))
# bcmaps::bc_bound() |> terra::vect() |> terra::plet()
