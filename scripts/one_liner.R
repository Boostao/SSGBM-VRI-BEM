devtools::load_all()

aoi_filter <- "POLYGON ((1023955 988730.2, 1065018 988730.2, 1065018 1016988, 1023955 1016988, 1023955 988730.2))"

unique_eco <-  create_unique_ecosystem_from_scratch(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
                                     vri_dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
                                     bem_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
                                     wkt_filter = aoi_filter,
                                     n_iterations = 1,
                                     use_ifelse = FALSE)

unique_eco_2 <- create_unique_ecosystem_from_scratch(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
                                                                    vri_dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
                                                                    bem_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
                                                                    wkt_filter = aoi_filter,
                                                                    n_iterations = 1)


rrm_output <-  create_RRM_ecosystem_from_scratch(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
                                                 vri_dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
                                                 bem_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
                                                 elevation_dsn = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif",
                                                 most_recent_harvest_year = 2020,
                                                 wkt_filter = aoi_filter,
                                                 n_iterations = 1)


vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
elevation <- terra::rast( "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")

vri_rast <- rasterize(vect(vri))

extract(vect(vri), 3)

raster

attributes(elevation)
terra::
help(rast)


f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)
vect_vri <- vect(vri)
r <- rast(vect_vri, ncols=ncol(elevation), nrows= nrow(elevation), nlyrs = ncol(vri))
value
x <- rasterize(vect_vri, r, c("FEATURE_ID"))

x <- rasterize(v, r, "NAME_2")

elevation <- terra::rast( "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")
v_bem <- vect(bem)
r_bem <- rast(v_bem, ncols = ncol(elevation), nrows = nrow(elevation))
raster_bem <- rasterize(v_bem, r_bem)
which_col_to_merge <- which(!sapply(bem , function(x) "sfc" %in% class(x)))
for (variable in names(bem)[which_col_to_merge]) {
  add(raster_bem) <- rasterize(v_bem, r_bem, variable)
}

writeRaster(raster_bem, "../bem.tif")


rast_bem <- rast("../bem.tif")
vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
vri_grid <- st_make_grid(vri, n = c(3,3))
x_bem_vri <- setDT(extract(rast_bem[[1]], vect(vri_grid[1])))



vri[, ]



library(terra)

elevation <- terra::rast( "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")

# create vri raster
vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
vri_var <- c("BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5", "LAND_CD_1",  "COV_PCT_1",  "LBL_VEGCOV", "SPEC_CD_1",  "SPEC_PCT_1")
v_vri <- vect(vri)
r_vri <- rast(v_vri, ncols = ncol(elevation), nrows = nrow(elevation), resolution = c(25,25))
raster_vri <- rasterize(v_vri, r_vri)
for (variable in vri_var) {
  add(raster_vri) <- rasterize(v_vri, r_vri, variable)
}

writeRaster(raster_vri, "../vri.tif", overwrite = T)

rm(list = ls())
gc()

# create bem raster
elevation <- terra::rast( "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")
v_bem <- vect(bem)
r_bem <- rast(v_bem, ncols = ncol(elevation), nrows = nrow(elevation), resolution = c(25,25))
raster_bem <- rasterize(v_bem, r_bem)
which_col_to_merge <- c("TEIS_ID","SDEC_1",
                   "BEUMC_S1",
                   "REALM_1",
                   "GROUP_1",
                   "CLASS_1",
                   "KIND_1",
                   "SITE_S1",
                   "SITEAM_S1A",
                   "SITEAM_S1B",
                   "SITEAM_S1C",
                   "SITEAM_S1D",
                   "SITEMC_S1",
                   "SITE_M1A",
                   "SITE_M1B",
                   "STRCT_S1",
                   "STRCT_M1",
                   "STAND_A1",
                   "SERAL_1",
                   "TREE_C1",
                   "SHRUB_C1",
                   "DISTCLS_1",
                   "DISTSCLS_1",
                   "DISSSCLS_1",
                   "SECL_1",
                   "SESUBCL_1",
                   "COND_1",
                   "VIAB_1",
                   "SDEC_2",
                   "BEUMC_S2",
                   "REALM_2",
                   "GROUP_2",
                   "CLASS_2",
                   "KIND_2",
                   "SITE_S2",
                   "SITEAM_S2A",
                   "SITEAM_S2B",
                   "SITEAM_S2C",
                   "SITEAM_S2D",
                   "SITEMC_S2",
                   "SITE_M2A",
                   "SITE_M2B",
                   "STRCT_S2",
                   "STRCT_M2",
                   "STAND_A2",
                   "SERAL_2",
                   "TREE_C2",
                   "SHRUB_C2",
                   "DISTCLS_2",
                   "DISTSCLS_2",
                   "DISSSCLS_2",
                   "SECL_2",
                   "SESUBCL_2",
                   "COND_2",
                   "VIAB_2",
                   "SDEC_3",
                   "BEUMC_S3",
                   "REALM_3",
                   "GROUP_3",
                   "CLASS_3",
                   "KIND_3",
                   "SITE_S3",
                   "SITEAM_S3A",
                   "SITEAM_S3B",
                   "SITEAM_S3C",
                   "SITEAM_S3D",
                   "SITEMC_S3",
                   "SITE_M3A",
                   "SITE_M3B",
                   "STRCT_S3",
                   "STRCT_M3",
                   "STAND_A3",
                   "SERAL_3",
                   "TREE_C3",
                   "SHRUB_C3",
                   "DISTCLS_3",
                   "DISTSCLS_3",
                   "DISSSCLS_3",
                   "SECL_3",
                   "SESUBCL_3",
                   "COND_3",
                   "VIAB_3",
                   "SLOPE_MOD",
                   "FORESTED_1",
                   "FORESTED_2",
                   "FORESTED_3",
                   "AGE_CL_STS",
                   "Area_Ha",
                   "BGC_ZONE",
                   "BGC_SUBZON")

for (variable in which_col_to_merge) {
  add(raster_bem) <- rasterize(v_bem, r_bem, variable)
}

writeRaster(raster_bem, "../bem.tif", overwrite = T)

library(terra)
vri <- read_vri("../vri.tif", wkt_filter = "POLYGON ((941827.7 932215.1, 982891.1 932215.1, 982891.1 960472.6, 941827.7 960472.6, 941827.7 932215.1))")
bem <- read_bem("../bem.tif", wkt_filter = "POLYGON ((941827.7 932215.1, 982891.1 932215.1, 982891.1 960472.6, 941827.7 960472.6, 941827.7 932215.1))")

v_rast <- rast("../vri.tif")
b_rast <- rast("../bem.tif")
elev_rast <- rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
terrain_raster <- terrain(elev_rast, v = c("slope", "aspect"), unit = "radians")
add(v_rast) <- elev_rast
add(v_rast) <- terrain_raster
rivers <- read_rivers("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")
wetlands <- read_wetlands("../SSGBM-VRI-BEM-data/CodeWithUs.gdb", wkt_filter = "POLYGON ((941827.7 932215.1, 982891.1 932215.1, 982891.1 960472.6, 941827.7 960472.6, 941827.7 932215.1))")

vri_bem <- setDT(extract(v_rast, vect("POLYGON ((941827.7 932215.1, 982891.1 932215.1, 982891.1 960472.6, 941827.7 960472.6, 941827.7 932215.1))"), cells = TRUE, xy = TRUE))

# merge rivers
river_intersect <- cells(v_rast, vect(rivers))[, 2]
set(vri_bem, i = match(vri_bem$cell, river_intersect), j = "ind_river", value = "a")


# merge wetland
wet_intersect <- cells(v_rast, vect(wetlands$Shape))[, 2]
set(vri_bem, i = match(vri_bem$cell, wet_intersect), j = "wl_pct", value = !is.na(match(vri_bem$cell, wet_intersect)))

# merge ccb


vri_bem <- merge_bem_on_vri.data.table(vri, bem)


cells(b_rast$TEIS_ID, 15729)


# comput slope and aspect
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
terrain(elev_rast, v = c("slope", "aspect"), unit = "radians", filename = "../slope_aspect.tif")
slope_asp <- rast("../slope_aspect.tif ")
add(elev_rast) <- slope_asp
writeRaster(elev_rast, "../elev.tif", overwrite = T)


# create raster
library(gdalUtils)
library(terra)
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp")
extent <- ext(elev_rast)
gdal_rasterize(src_datasource = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
               a = "OPEN_SRC",
               dst_filename = "../SSGBM-VRI-BEM-data/temp_vri_OPEN_SRC.tif",
               a_srs =  crs(elev_rast, proj = T),
               te = c(extent[1], extent[3], extent[2], extent[4]),
               tr = res(elev_rast))

gdal_rasterize(src_datasource = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
               a = "GEOMETRY_Area",
               sql = "SELECT  GEOMETRY_Area FROM FWA_WETLANDS_POLY",
               dst_filename = "../SSGBM-VRI-BEM-data/wetlands.tif",
               a_srs =  crs(elev_rast, proj = T),
               te = c(extent[1], extent[3], extent[2], extent[4]),
               tr = res(elev_rast))
w_r <- rast( "../SSGBM-VRI-BEM-data/wetlands.tif")
plot(w_r)
"../SSGBM-VRI-BEM-data/CodeWithUs.gdb"
FWA_WETLANDS_POLY
GEOMETRY_Area
wet <- read_wetlands("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")
gdalUtils::gdal_rasterize("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
               sql = "SELECT CAST(HARVEST_YEAR, AS numeric(10,3)) as TEST FROM VEG_R1_PLY_polygon",
               a = "TEST",
               dst_filename = "../SSGBM-VRI-BEM-data/temp_vri_HARVEST_DATE_2.tif",
               a_srs =  crs(elev_rast, proj = T),
               te = c(extent[1], extent[3], extent[2], extent[4]),
               tr = res(elev_rast))

gdal_rasterize("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp",
               burn = 1,
               where = '"OPEN_IND" = \'Y\'',
               dst_filename = "../SSGBM-VRI-BEM-data/temp_vri_OPEN_IND.tif",
               a_srs =  crs(elev_rast, proj = T),
               te = c(extent[1], extent[3], extent[2], extent[4]),
               tr = res(elev_rast),
               init  = 2)

'SELECT geometry julianday(HARVEST_DATE) - julianday(\'1970-01-01\')   as attr FROM VEG_R1_PLY_polygon'

gdal_rasterize("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp",
               burn = 2,
               where = '"OPEN_IND" = \'N\'',
               dst_filename = "../SSGBM-VRI-BEM-data/temp_vri_OPEN_IND.tif")

vri_r1 <- rast("../SSGBM-VRI-BEM-data/temp_vri_POLY_AREA.tif")
vri_r2 <- rast("../SSGBM-VRI-BEM-data/temp_vri_OPEN_SRC.tif")
vri_r3 <- rast("../SSGBM-VRI-BEM-data/temp_vri_BCLCS_LV_1.tif")
vri_r4 <- rast("../SSGBM-VRI-BEM-data/temp_vri_OPEN_IND.tif")
vri_r4 <- rast("../SSGBM-VRI-BEM-data/temp_vri_HARVEST_DATE_2.tif")
values(vri_r4)
plot(vri_r4)
  add(vri_r1) <- vri_r2
writeRaster(vri_r1 , "../SSGBM-VRI-BEM-data/temp_vri.tif")
vri_r <- rast("../SSGBM-VRI-BEM-data/temp_vri.tif")
plot(vri_r)
plot(test)
vri <- read_sf("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp")


gdal_rasterize("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp",
   a = "POLY_AREA",
dst_filename = "../SSGBM-VRI-BEM-data/temp_vri_POLY_AREA.tif",
a_srs =  crs(elev_rast, proj = T),
te = c(extent[1], extent[3], extent[2], extent[4]),
tr = res(elev_rast))
