devtools::load_all()

duckdb_run_moose <- function(aoi_wkt = "POLYGON ((1000000 960000, 1002000 960000, 1002000 962000, 1000000 962000, 1000000 960000))") {
# Initialize database connection and load data into duckdb. 
# init_db(bem_dsn = "D:/Boostao/SSGBM-data/Skeena_BEM.gdb",
#         pem_dsn = "D:/Boostao/SSGBM-data/PEM/PEM_Mar2026.gpkg")

conn <- init_conn(temp_dir = "./duckdb_tmp", 
                  memory_limit = "14GB", threads = 1L)

#aoi_wkt <- get_aoi_wkt_from_tsa(conn, aoi_name = "Pacific")
#aoi_wkt <- "MULTIPOLYGON (((1065018 932215.1, 941827.7 932215.1, 941827.7 1016988, 1065018 1016988, 1065018 932215.1)))"
#aoi_wkt <- sf::st_read("D:/Boostao/SSGBM-data/Skeena Region Boundary", layer = "Skeena_region")$geometry |> sf::st_transform(3005) |> sf::st_union() |> wk::as_wkt() |> paste0()
#aoi_wkt <- "POLYGON ((1000000 960000, 1002000 960000, 1002000 962000, 1000000 962000, 1000000 960000))"


filtered_views(conn, aoi_wkt, build_spatial_index = FALSE)


# 1a ----
vribem_view(conn, validate_intersect = FALSE)

# Amend large polygons (>3500 ha): dissolve, split by glaciers/lakes/wetlands, re-join BEM

amend_large_polygons_duckdb(conn,
                            vri_bem_tbl  = "V_VRIBEM",
                            lakes_tbl    = "V_LAKES",
                            glaciers_tbl = "V_GLACIERS",
                            wetlands_tbl = "V_WETLANDS",
                            bem_tbl      = "V_BEM",
                            result_tbl   = "VRIBEM")

# 1b ----
duckdb::duckdb_read_csv(conn, "beu_bec_corr",  "inst/csv/Allowed_BEC_BEUs_NE_ALL.csv", temporary = TRUE) #TODO update tu use system.file on package csv

vribem_corrections_view(conn, vri_bem_tbl = "VRIBEM", beu_bec = "beu_bec_corr")

#1c ----
#Lakes and wetlands
duckdb_tables(conn)


# Spatially cut non-lake VRI polygons by FWA Lakes; updates VRIBEM in-place.
correct_small_lakes_duckdb(conn,
                           vri_bem_tbl = "VRIBEM",
                           lakes_tbl   = "V_LAKES",
                           batch_size  = 500L)     # ← tune this down if you run into memory issues; the default of 500 is faster but uses more RAM
                           

#wetlands
duckdb::duckdb_read_csv(conn, "beu_wetland_updates",  "inst/csv/beu_wetland_updates.csv", temporary = TRUE)
#vri_bem_wetlands_corrections_view(conn, vri_bem = "VRIBEM", beu_wetland_updates = "beu_wetland_updates")

#3a ----
#Moved earlier in the process (need accurate SLOPE_MOD for update_beu_from_rules_dt)
elev_rast <- terra::rast(file.path("..", "SSGBM-VRI-BEM-data", "dem.tif"))

merge_elevation_duckdb(conn = conn,
                       vri_bem_tbl = "VRIBEM",
                       elev_raster = elev_rast,
                       elevation_threshold = 1400)

#1d ----
import_rules_to_duckdb(conn,
  rules_xl = rules_xl <- file.path(data_folder, "Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx"),
  tbl_name = "beu_update_rules")

# TODO Maybe create another table instead of updating wetland_corrections table.... 
vribem_beu_rules_update(conn,
  vri_bem = "VRIBEM",
  rules_tbl = "beu_update_rules")

#2 ----
unique_eco <- create_unique_ecosystem_dt(conn = conn, vri_bem =  "VRIBEM")

fwrite(unique_eco, file = "../unique_ecosystem.csv")


#3bc ---- disturbances
#TODO use duckdb for disturbance layer too
# merge Forest Disturbance Layer
#fdl <- st_read("Forest_Disturbance")

#vri_bem <- merge_geometry(vri_bem, fdl, tolerance = units::as_units(10, "m2"))

# We will merge CCB instead of FDL since FDL is not available to us.
merge_geometry_duckdb(conn = conn,
  x_tbl = "VRIBEM",
  y_tbl = "V_CCB",  #TODO Create table V_FDL or a mock in duckdb
  tolerance_m2 = 10,
  result_tbl = "VRIBEM_FDL")

# Merge fire perimeters: adds percent_burned and most_recent_fire in-place to VRIBEM.
#merge_fire_perimeters_duckdb(conn,
 #                            vri_bem_tbl = "VRIBEM_FDL",
  #                           fire_tbl    = "V_FIRE")


#4 ----
calc_forest_age_class_duckdb(conn = conn, 
                             vri_bem_tbl = "VRIBEM_FDL")

#4b /4d2/d3 ----

unique_eco_example <- read_unique_ecosystem_dt("inst/csv/Skeena_VRIBEM_LUT.csv")

merge_unique_ecosystem_fields_duckdb(conn,
                                     vri_bem_tbl = "VRIBEM_FDL",
                                     unique_ecosystem_dt = unique_eco_example)

find_crown_area_dominant_values_duckdb(conn, vri_bem_tbl = "VRIBEM_FDL")

#5 ----
#######################
#Create RRM ecosystem and assign values for moose
#######################
moose_export_dt <- create_RRM_ecosystem_moose_duckdb(conn, vri_bem_tbl = "VRIBEM_FDL")
return(moose_export_dt)
}

#duckdb_run_moose()
