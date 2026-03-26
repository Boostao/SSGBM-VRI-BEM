library(ssgbm)
library(terra)

elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
terra::terrain(elev_rast, v = c("slope", "aspect"), unit = "radians", filename = "../slope_aspect.tif", overwrite = T)
slope_asp <- terra::rast("../slope_aspect.tif ")
terra::add(elev_rast) <- slope_asp
names(elev_rast) <- c("ELEV", "MEAN_SLOPE", "MEAN_ASP")
terra::writeRaster(elev_rast, "../elev_test.tif", overwrite = T)

rasterize_bem(src_datasource = "../SSGBM-VRI-BEM-data/BEM_VRI", dst_filename = "../SSGBM-VRI-BEM-data/bem_test.tif", reference = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif", output_raster = F, layer = "BEM")
rasterize_vri(src_datasource = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY", dst_filename = "../SSGBM-VRI-BEM-data/vri_test.tif", reference = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif", output_raster = F, layer = "VEG_R1_PLY_polygon")
rasterize_wetlands(src_datasource = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", dst_filename = "../SSGBM-VRI-BEM-data/wetlands_test.tif", reference = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif", output_raster = F, layer = "FWA_WETLANDS_POLY")
rasterize_rivers(src_datasource = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", dst_filename = "../SSGBM-VRI-BEM-data/rivers_test.tif", reference = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif", output_raster = F, layer = "FWA_RIVERS_POLY")
rasterize_ccb(src_datasource = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", dst_filename = "../SSGBM-VRI-BEM-data/ccb_test.tif", reference = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif", output_raster = F, layer = "CNS_CUT_BL_polygon")

aoi_filter <- "POLYGON ((1023955 988730.2, 1065018 988730.2, 1065018 1016988, 1023955 1016988, 1023955 988730.2))"

test_out <- create_RRM_ecosystem_from_rasters(vri_dsn = "../SSGBM-VRI-BEM-data/vri_test.tif",
                                  bem_dsn = "../SSGBM-VRI-BEM-data/bem_test.tif",
                                  wetlands_dsn = "../SSGBM-VRI-BEM-data/wetlands_test.tif",
                                  rivers_dsn = "../SSGBM-VRI-BEM-data/rivers_test.tif",
                                  ccb_dsn = "../SSGBM-VRI-BEM-data/ccb_test.tif",
                                  elevation_dsn = "../elev_test.tif",
                                  most_recent_harvest_year = 2020,
                                  beu_bec_csv = "inst/csv/Allowed_BEC_BEUs_NE_ALL.csv",
                                  beu_wetland_update_csv = "inst/csv/beu_wetland_updates.csv",
                                  unique_ecosystem = "inst/csv/Skeena_VRIBEM_LUT.csv",
                                  elevation_threshold = 1400,
                                  wkt_filter = aoi_filter,
                                  n_iterations = 1)

test_out[, rrm := sample(1:3, .N , replace = T)]

ref_rast <- rast("../elev_test.tif")

rrm_rast <- writeRaster(raster(nrows = nrow(ref_rast), ncols = ncol(ref_rast), xmn = ext(ref_rast)[1], xmx = ext(ref_rast)[2], ymn = ext(ref_rast)[3], ymx = ext(ref_rast)[4], crs = crs(ref_rast, proj = T), resolution = res(ref_rast), vals = 0),"../rrm_test.tif", overwrite = T)

write_dt <- test_out[area_sum > 0 , .(cell = list(unlist(cell))), keyby = rrm]

for (i in 1:nrow(write_dt)) {
  rrm_rast <- update(rrm_rast, rep(write_dt$rrm[i], length(unlist(write_dt$cell[i]))), cell = unlist(write_dt$cell[i]))
}
plot(rrm_rast)