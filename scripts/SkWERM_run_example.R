aoi_name <- "Pacific"

#Set area boundary
aoi_wkt <- read_tsa(tsa_name = paste0(aoi_name," TSA")) %>% wk::as_wkt(.)

# read vri and bem layers
vri <- read_vri(wkt_filter = aoi_wkt)
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")

# 1a ----
vri_bem <- merge_bem_on_vri(vri = vri,
                            bem = bem,
                            return_intersection_dt = TRUE)

#Break up very large polygons into smaller polygons (using lakes, glaciers, and wetlands)
lakes <- read_lakes(wkt_filter = aoi_wkt)
wetlands <- read_wetlands(wkt_filter = aoi_wkt)
glaciers <- read_glaciers(wkt_filter = aoi_wkt)

vri_bem <- amend_large_polygons(vri_bem = vri_bem, bem = bem, lakes = lakes, glaciers = glaciers, wetlands = wetlands)

# 1b ----
beu_bec_csv <- fread(system.file("csv/Allowed_BEC_BEUs_NE_ALL.csv", package = "SSGBM.VRI.BEM")) # 
rivers <- read_rivers(wkt_filter = aoi_wkt)

vri_bem <- update_bem_from_vri(vri_bem = vri_bem,
                               rivers = rivers,
                               beu_bec = beu_bec_csv,
                               clear_site_ma = TRUE,
                               use_ifelse = TRUE)

#1c ----
#Lakes and wetlands

#Capture smaller lakes
vri_bem <- correct_small_lakes(vri_bem = vri_bem,lakes = lakes)

#wetlands
beu_wetland_update_csv <- fread(system.file("csv/beu_wetland_updates.csv", package = "SSGBM.VRI.BEM")) # 
vri_bem <- update_bem_from_wetlands(vri_bem = vri_bem,
                                    wetlands = wetlands,
                                    buc = beu_wetland_update_csv)

#3a ----
#Moved earlier in the process (need accurate SLOPE_MOD for update_beu_from_rules_dt)
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")

vri_bem <- merge_elevation_raster_on_sf(elev_raster = elev_rast,
                                        vri_bem = vri_bem,
                                        elevation_threshold = 1500)

#1d ----
vri_bem <- update_beu_from_rules_dt(vri_bem = vri_bem,
                                    rules_dt = "../SSGBM-VRI-BEM-data/Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx")

#2 ----
unique_eco <- create_unique_ecosystem_dt(vri_bem = vri_bem)

fwrite(unique_eco, file = "../unique_ecosystem.csv")

#3bc ---- disturbances

# merge Forest Disturbance Layer
fdl <- st_read("Forest_Disturbance")

vri_bem <- merge_geometry(vri_bem, fdl, tolerance = units::as_units(10, "m2"))

#merge fire perimeters 
fire <- read_fire(wkt_filter = aoi_wkt)
vri_bem <- merge_fire_perimeters(vri_bem=vri_bem,fire_perimeter= fire)

#4 ----
vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                 most_recent_harvest_year = as.integer(format(Sys.Date(),"%Y")))

#4b /4d2/d3 ----
vri_bem <- merge_unique_ecosystem_fields(vri_bem = vri_bem,
                                         unique_ecosystem_dt = unique_eco) %>%
find_crown_area_dominant_values(vri = .)

#5 ----
#######################
#Create RRM ecosystem and assign values for moose
#######################
moose_export_dt <- create_RRM_ecosystem_moose(vri_bem = vri_bem)

RSI_BGC_BEU_moose <- unique(moose_export_dt[,list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)])[order(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)]
data.table::setkey(RSI_BGC_BEU_moose, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)

template_dir_moose <- "../SkWERM/MOOSE/RRM/"
rsi_source_moose <- "../SkWERM/MOOSE/RRM/RSI_SOURCE of Ratings for Initial Attributes Tab in Models_MALAN_Skeena_15Jan2025.xlsx"
templates_moose <- list.files(template_dir_moose, "template.xlsx", full.names = TRUE)

rsi_rating_moose <- readxl::read_xlsx(rsi_source_moose, grep("rating", readxl::excel_sheets(path = rsi_source_moose), value = TRUE, ignore.case = TRUE)[1])
data.table::setDT(rsi_rating_moose)
data.table::setkey(rsi_rating_moose, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_Mapcode)

moose_no_match <- RSI_BGC_BEU_moose[!rsi_rating_moose][,data.table::key(RSI_BGC_BEU_moose), with = FALSE]
if (nrow(moose_no_match)) {
  logger::log_warn("No matching RSI ratings found for the following RRM ecosystem values.")
  print(moose_no_match)
}

moose_init <- mapply(
  function(x,y) {
    res <- rsi_rating_moose[RSI_BGC_BEU_moose]
    res <- res[, list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEU_Mapcode, Iav_strct_d = res[[y]], RSI = res[[x]])]
    return(res)
  },
  names(rsi_rating_moose) |> grep("_RSI$", x = _, value = TRUE),
  names(rsi_rating_moose) |> grep("_Optimal Structural Stage$", x = _, value = TRUE),
  SIMPLIFY = FALSE
)

names(moose_init) <- names(rsi_rating_moose) |> grep("_RSI$", x = _, value = TRUE) |> tolower() |> gsub("_rsi", "", x = _) |> trimws()  |> gsub("\\s+", "_", x = _) |> paste("moose", ... = _, sep = "_")

logger::log_threshold("INFO")
MALAN_GFD_6C <- rrm_calc_ratings(moose_export_dt, templates_moose[1], moose_init$moose_growing_season_forage)
MALAN_WFD_6C <- rrm_calc_ratings(moose_export_dt, templates_moose[2], moose_init$moose_winter_forage)
MALAN_WST_6C <- rrm_calc_ratings(moose_export_dt, templates_moose[3], moose_init$moose_winter_shelter)
rrm_missing_lines(MALAN_WFD_6C)
rrm_missing_lines(MALAN_GFD_6C)
rrm_missing_lines(MALAN_WST_6C)

data.table::set(moose_export_dt, j = "MALAN_WFD_6C", value = MALAN_WFD_6C[[1]]$RATING)
data.table::set(moose_export_dt, j = "MALAN_WFD_RSI", value = MALAN_WFD_6C[[1]]$VALUE)
data.table::set(moose_export_dt, j = "MALAN_GFD_6C", value = MALAN_GFD_6C[[1]]$RATING)
data.table::set(moose_export_dt, j = "MALAN_GFD_RSI", value = MALAN_GFD_6C[[1]]$VALUE)
data.table::set(moose_export_dt, j = "MALAN_WST_6C", value = MALAN_WST_6C[[1]]$RATING)
data.table::set(moose_export_dt, j = "MALAN_WST_RSI", value = MALAN_WST_6C[[1]]$VALUE)

###
#Final step: assign WHR ratings back to spatial map
###
Moose_SkWERM <- merge_rrm_on_vri(vri_bem=vri_bem, rrm_dt=moose_export_dt, animal="moose")

#Check for mismatches
check <- filter(Moose_SkWERM,rrm_merge_ind == "FALSE") %>%
  dplyr::select(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S1, BEUMC_S2, BEUMC_S3, SLOPE_MOD, SITE_M3A, SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3, STRCT_S1, STAND_A1)

#Mismatches should only be for missing LUT ecosystems. If there are more than that, double check process
if(nrow(check)>0){
  print("RRM output was not fully merged with spatial map. Check to see which variables did not match up. Mismatching values are only expected for missing LUT fields.")
}

#######################
#Create RRM ecosystem and assign values for grizzly
#######################
bear_export_dt <- create_RRM_ecosystem_bear(vri_bem = vri_bem)

RSI_BGC_BEU_bear <- unique(bear_export_dt[,list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)])[order(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)]
data.table::setkey(RSI_BGC_BEU_bear, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)

template_dir <- "../SkWERM/GRIZZLY_BEAR/RRM/"
rsi_source <- "../SkWERM/GRIZZLY_BEAR/RRM/GM_31-March-2023_GB_BEUMC_RRM_Ratings_Skeena_Region.xlsx"

templates <- list.files(template_dir, "template.xlsx", full.names = TRUE)

rsi_rating_bear <- readxl::read_xlsx(rsi_source, grep("rating", readxl::excel_sheets(path = rsi_source), value = TRUE, ignore.case = TRUE)[1])
data.table::setDT(rsi_rating_bear)
data.table::setkey(rsi_rating_bear, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_Mapcode)

grizzly_no_match <- RSI_BGC_BEU_bear[!rsi_rating_bear][,data.table::key(RSI_BGC_BEU_bear), with = FALSE]
if (nrow(grizzly_no_match)) {
  logger::log_warn("No matching RSI ratings found for the following RRM ecosystem values.")
  print(grizzly_no_match)
}
grizzly_init <- mapply(
  function(x,y,z) {
    res <- rsi_rating_bear[RSI_BGC_BEU_bear]
    #make sure it's Iav_slope_mod, not Iav_site_m3a
    res <- res[, list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEU_Mapcode, Iav_strct_d = res[[y]], Iav_slope_mod = res[[z]], RSI = res[[x]])]
    return(res)
  },
  names(rsi_rating_bear) |> grep("^Grizzly_RSI", x = _, value = TRUE),
  names(rsi_rating_bear) |> grep("^Grizzly_IAV_Structural", x = _, value = TRUE),
  names(rsi_rating_bear) |> grep("^Grizzly_IAV_Site Modifier", x = _, value = TRUE), 
  SIMPLIFY = FALSE
)
names(grizzly_init) <- names(rsi_rating_bear) |> grep("^Grizzly_RSI_", x = _, value = TRUE) |> tolower() |> gsub("^grizzly_rsi_", "", x = _) |> trimws() |> gsub("\\s+", "_", x = _) |> paste("grizzly", ... = _, sep = "_")

logger::log_threshold("INFO")
MURAR_FFD_6C  <- rrm_calc_ratings(bear_export_dt, templates[1], grizzly_init$grizzly_fall_forage)
MURAR_HI_6C   <- rrm_calc_ratings(bear_export_dt, templates[2], grizzly_init$grizzly_hibernation)
MURAR_PEFD_6C <- rrm_calc_ratings(bear_export_dt, templates[3], grizzly_init$grizzly_early_spring_forage)
MURAR_PLFD_6C <- rrm_calc_ratings(bear_export_dt, templates[4], grizzly_init$grizzly_late_spring_forage)
MURAR_SFD_6C  <- rrm_calc_ratings(bear_export_dt, templates[5], grizzly_init$grizzly_summer_forage)

data.table::set(bear_export_dt, j = "MURAR_PEFD_6C", value = MURAR_PEFD_6C[[1]]$RATING)
data.table::set(bear_export_dt, j = "MURAR_PEFD_RSI", value = MURAR_PEFD_6C[[1]]$VALUE)
data.table::set(bear_export_dt, j = "MURAR_PLFD_6C", value = MURAR_PLFD_6C[[1]]$RATING)
data.table::set(bear_export_dt, j = "MURAR_PLFD_RSI", value = MURAR_PLFD_6C[[1]]$VALUE)
data.table::set(bear_export_dt, j = "MURAR_SFD_6C", value = MURAR_SFD_6C[[1]]$RATING)
data.table::set(bear_export_dt, j = "MURAR_SFD_RSI", value = MURAR_SFD_6C[[1]]$VALUE)
data.table::set(bear_export_dt, j = "MURAR_FFD_6C", value = MURAR_FFD_6C[[1]]$RATING)
data.table::set(bear_export_dt, j = "MURAR_FFD_RSI", value = MURAR_FFD_6C[[1]]$VALUE)
data.table::set(bear_export_dt, j = "MURAR_HI_6C", value = MURAR_HI_6C[[1]]$RATING)
data.table::set(bear_export_dt, j = "MURAR_HI_RSI", value = MURAR_HI_6C[[1]]$VALUE)

#grizzly bear
SkWERM_VRI_BEM_WHR <- merge_rrm_on_vri(vri_bem=Moose_SkWERM, rrm_dt=bear_export_dt, animal="bear")

check <- filter(SkWERM_VRI_BEM_WHR,rrm_merge_ind == "FALSE") %>%
  dplyr::select(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S1, BEUMC_S2, BEUMC_S3, SLOPE_MOD, SITE_M3A, SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3, STRCT_S1, STAND_A1)

if(nrow(check)>0){
  print("RRM output was not fully merged with spatial map. Check to see which variables did not match up. Mismatching values are only expected for missing LUT fields.")
}

SkWERM_final <- clean_vri_bem_output(vri_bem = SkWERM_VRI_BEM_WHR)