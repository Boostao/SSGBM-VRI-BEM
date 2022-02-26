## The frew lines in comment below are equivalent to `devtools::load_all()` went working in an RStudio Project.
## They could also be replaced by `library(SSGBM.VRI.BEM)` is this package is installed.

# library(data.table)
# library(sf)
# library(terra)
# library(units)
# invisible(lapply(list.files("R", full.names = TRUE), source))

devtools::load_all()
library(data.table)

# Read data ----
use_bcdata <- FALSE # Set to TRUE if you what to use the VRI, rivers and wetlands from the BC data catalog also consider updating the aoi_wkt

if (use_bcdata){
  # Define an AOI:
  aoi_wkt <- "POLYGON ((1023955 988730.2, 1065018 988730.2, 1065018 1016988, 1023955 1016988, 1023955 988730.2))"
  vri <- read_vri(wkt_filter = aoi_wkt)
  rivers <- read_rivers(wkt_filter = aoi_wkt)
  wetlands <- read_wetlands(wkt_filter = aoi_wkt)
} else {
  vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
  rivers <- read_rivers("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")
  wetlands <- read_wetlands("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")

}

bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")



# 1a ----
vri_bem <- merge_bem_on_vri(vri = vri,
                            bem = bem,
                            return_intersection_dt = TRUE)

vri_bem_intersection_dt <- vri_bem$intersection_dt
vri_bem <- vri_bem$vri

# filter out vri that have no overlapping bem (usually you should make sure the BEM covers all VRI)
vri_bem <- vri_bem[which(!is.na(vri_bem$TEIS_ID)),]

# 1b ----
beu_bec_csv <- fread("csv/Allowed_BEC_BEUs_NE_ALL.csv")
vri_bem <- update_bem_from_vri(vri_bem = vri_bem,
                               rivers = rivers,
                               beu_bec = beu_bec_csv,
                               clear_site_ma = TRUE,
                               use_ifelse = TRUE)

#1c ----
beu_wetland_update_csv <- fread("csv/beu_wetland_updates.csv")
vri_bem <- update_bem_from_wetlands(vri_bem = vri_bem,
                                    wetlands = wetlands,
                                    buc = beu_wetland_update_csv)

#2 ----
unique_eco <- create_unique_ecosystem_dt(vri_bem = vri_bem)

fwrite(unique_eco, file = "../unique_ecosystem.csv")


#3abc ----
vri_bem <- merge_elevation_raster_on_sf(elev_raster = elev_rast,
                                        vri_bem = vri_bem,
                                        elevation_threshold = 1400)

# merge cutblock
ccb <- read_ccb("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")

vri_bem <- merge_ccb_on_vri(vri_bem = vri_bem,
                            ccb = ccb)

#4 ----
vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                 most_recent_harvest_year = 2020)


#4b /4d2 ----
unique_eco_example <- read_unique_ecosystem_dt("csv/Skeena_VRIBEM_LUT.csv")
vri_bem <- merge_unique_ecosystem_fields(vri_bem = vri_bem,
                                         unique_ecosystem_dt = unique_eco_example)

#4d3 ----
vri_bem <- find_crown_area_dominant_values(vri = vri_bem,
                                           bem = bem,
                                           intersection_dt = vri_bem_intersection_dt)

#5 ----
export_dt <- create_RRM_ecosystem(vri_bem = vri_bem)
fwrite(export_dt, file = "../RRM_input_table.csv")
