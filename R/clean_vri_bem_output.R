#'Clean up the VRI-BEM-WHR output
#'This function cleans up the final VRI-BEM-WHR output (removes extraneous
#'fields), merges polygons smaller than `min_area_m2` into the most similar
#'nearby polygon, and adds the unique ID (PolyID) and ECO_TYPE.
#'
#' @param vri_bem VRI-BEM feature class
#' @param min_area_m2 Minimum polygon area threshold in square metres.
#'
#' @return "cleaned" vri_bem
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @export
#'

#' @noRd
merge_small_polygon_slivers <- function(vri_bem,
                                        min_area_m2 = 1000,
                                        similarity_fields = c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC_S1")) {

  if (!inherits(vri_bem, "sf") || nrow(vri_bem) < 2L) {
    return(vri_bem)
  }

  min_area <- units::set_units(min_area_m2, "m^2")
  working <- sf::st_make_valid(vri_bem)
  similarity_fields <- intersect(similarity_fields, names(working))

  same_value <- function(x, y) {
    (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && as.character(x) == as.character(y))
  }

  repeat {
    areas <- sf::st_area(working)
    sliver_idx <- which(areas < min_area)

    if (!length(sliver_idx) || nrow(working) < 2L) {
      break
    }

    current_idx <- sliver_idx[which.min(as.numeric(areas[sliver_idx]))]
    candidate_idx <- setdiff(seq_len(nrow(working)), current_idx)

    if (!length(candidate_idx)) {
      break
    }

    touching_idx <- candidate_idx[
      sf::st_touches(
        working[current_idx, , drop = FALSE],
        working[candidate_idx, , drop = FALSE],
        sparse = TRUE
      )[[1]]
    ]

    if (!length(touching_idx)) {
      intersecting_idx <- candidate_idx[
        sf::st_intersects(
          working[current_idx, , drop = FALSE],
          working[candidate_idx, , drop = FALSE],
          sparse = TRUE
        )[[1]]
      ]

      if (length(intersecting_idx)) {
        touching_idx <- intersecting_idx
      }
    }

    if (length(touching_idx)) {
      candidate_idx <- touching_idx
    }

    similarity <- vapply(candidate_idx, function(idx) {
      sum(vapply(similarity_fields, function(field) {
        same_value(working[[field]][current_idx], working[[field]][idx])
      }, logical(1)))
    }, integer(1))

    distances <- as.numeric(sf::st_distance(
      working[current_idx, , drop = FALSE],
      working[candidate_idx, , drop = FALSE]
    ))
    candidate_areas <- as.numeric(areas[candidate_idx])
    target_idx <- candidate_idx[order(-similarity, distances, -candidate_areas, candidate_idx)][1L]

    target_row <- working[target_idx, , drop = FALSE]
    sf::st_geometry(target_row) <- sf::st_make_valid(sf::st_union(
      sf::st_geometry(target_row),
      sf::st_geometry(working[current_idx, , drop = FALSE])
    ))

    keep_idx <- setdiff(seq_len(nrow(working)), c(current_idx, target_idx))
    working <- rbind(working[keep_idx, , drop = FALSE], target_row)
  }

  working
}

clean_vri_bem_output <- function(vri_bem, min_area_m2 = 1000) {

  if (FALSE) {
    ABOVE_ELEV_THOLD<-AGE_CL_STD<-AGE_CL_STS<-BCLCS_LV_1<-BCLCS_LV_5<-BEU_BEC<-BGC_PHASE<-BGC_ZONE<-
      COV_PCT_1<-CR_CLOSURE<-CROWN_ALL_1<-CROWN_ALL_3<-DOM_TREE<-
      dupID<-ECO_SEC<-ELEV<-FEATURE_ID<-finalarea<-FORESTED_1<-FORESTED_3<-HARVEST_YEAR<-KIND_1<-
      KIND_2<-KIND_3<-LAND_CD_1<-LBL_VEGCOV<-m<-MALAN_WFD_6C_SU_1<-MALAN_WST_6C_CAP_WA<-MEAN_ASP<-
      MEAN_SLOPE<-MEAN_TRI<-MURAR_HI_6C_CAP_WA<-MURAR_PEFD_6C_SU_1<-POLY_COMM<-PolyID<-PROJ_AGE_1<-
      Salmon<-SDEC_1<-SDEC_2<-SDEC_3<-Shape<-Shape_Area<-SITE_M3A<-SLOPE_MOD<-
      SOIL_MOISTURE_REGIME_1<-SOIL_NUTRIENT_REGIME<-SPEC_CD_1<-SPEC_CD_2<-SPEC_CD_3<-SPEC_CD_4<-
      SPEC_CD_5<-SPEC_CD_6<-SPEC_PCT_1<-SPEC_PCT_2<-SPEC_PCT_3<-SPEC_PCT_4<-SPEC_PCT_5<-
      SPEC_PCT_6<-STAND_A1<-STAND_A2<-STAND_A3<-STAND_CLIMAX_1<-STAND_CLIMAX_2<-STAND_CLIMAX_3<-
      STRCT_mod<-STRCT_S1<-STRCT_S2<-STRCT_S3<-STS_CLIMAX_1<-STS_CLIMAX_2<-STS_CLIMAX_3<-TEIS_ID<-
      VEG_CONSOLIDATED_CUT_BLOCK_ID<-VRI_AGE_CL_STD<-VRI_AGE_CL_STS<-
      VRI_SURVEY_YEAR<-BEUMC_S1<-BGC_label<-NULL
  }

  # Merge small polygons into the best nearby ecosystem match before final cleanup.
  vri_bem <-  vri_bem |>
    merge_small_polygon_slivers(min_area_m2 = min_area_m2) |>
    dplyr::mutate(finalarea = sf::st_area(Shape)) |>
    dplyr::filter(finalarea > units::set_units(0, "m^2")) |>
    mutate(Shape_Area = as.numeric(finalarea)) |>
    dplyr::select(-(finalarea))

  #set as data table for quicker processing
  vri_bem_dt <- as.data.table(vri_bem)

  if (!"aoi_name" %in% names(vri_bem_dt)) {
    vri_bem_dt[, aoi_name := "AOI"]
  }

  #Keep only necessary variables
  vri_bem_dt <- vri_bem_dt |>

    dplyr::select(aoi_name, FEATURE_ID, BCLCS_LV_1:BCLCS_LV_5,LAND_CD_1,COV_PCT_1,LBL_VEGCOV, SOIL_MOISTURE_REGIME_1, SOIL_NUTRIENT_REGIME, SITE_POSITION_MESO, CR_CLOSURE, SPEC_CD_1, SPEC_PCT_1, SPEC_CD_2, SPEC_PCT_2, SPEC_CD_3, SPEC_PCT_3, SPEC_CD_4, SPEC_PCT_4, SPEC_CD_5, SPEC_PCT_5, SPEC_CD_6, SPEC_PCT_6,PROJ_AGE_1, POLY_COMM, TEIS_ID,SITE_INDEX,EST_SITE_INDEX,ECO_SEC,BGC_ZONE, BGC_SUBZON, BGC_VRT,BGC_PHASE,SDEC_1,BEUMC_S1,REALM_1,GROUP_1,CLASS_1,KIND_1,STRCT_S1,STAND_A1,SDEC_2,BEUMC_S2,REALM_2,GROUP_2,CLASS_2,KIND_2,STRCT_S2,STAND_A2,SDEC_3,BEUMC_S3,REALM_3,GROUP_3,CLASS_3,KIND_3,SITE_M3A,STRCT_S3,STAND_A3,AGE_CL_STS,AGE_CL_STD,FORESTED_1:FORESTED_3,ABOVE_ELEV_THOLD,VRI_SURVEY_YEAR, VRI_AGE_CL_STS,VRI_AGE_CL_STD,CROWN_ALL_1:CROWN_ALL_3,Salmon,SLOPE_MOD,ELEV,MEAN_SLOPE,MEAN_TRI,MEAN_ASP,STS_CLIMAX_1:STAND_CLIMAX_1,STS_CLIMAX_2:STAND_CLIMAX_2,STS_CLIMAX_3:STAND_CLIMAX_3,DSTRB_HIST,MRSRD_Y,MRSRD_A,MRSRD_D,MRSRD_S,SIFA,most_recent_fire, percent_burned,dplyr::matches("_(6C|RSI)_(SU|CAP)_(1|2|3|HV|WA)$"),lbl_edit, Lbl_edit_wl,Shape_Area,rrm_merge_ind,Shape)|> dplyr::select(-(rrm_merge_ind))

  #Create unique ID
  vri_bem_dt[,PolyID := paste0(gsub("^(.).*(.)$","\\1\\2",ifelse(is.na(aoi_name) | aoi_name == "", "AOI", aoi_name)),"_",FEATURE_ID,"_",TEIS_ID,sep = "")]
  vri_bem_dt[, aoi_name := NULL]

  #Find and correct duplicates
  setDT(vri_bem_dt)[, dupID := rowid(PolyID)]

  #update polyid with duplicate values
  vri_bem_dt[,PolyID := paste(PolyID,"_",dupID,sep = "")][, dupID := NULL]

  #Concatenate BGC vars into "BGC_label" and combine with BEU for "BEU_BEC"
  vri_bem_dt <- vri_bem_dt |>
    tidyr::unite("BGC_label", BGC_ZONE:BGC_PHASE, na.rm = TRUE, remove = FALSE, sep = "") |>
    dplyr::mutate(BEU_BEC = paste0(BEUMC_S1,"_",BGC_label))

  #Establish dominant tree for SPEC_CD_1 (may need to revisit these definitions and update missing tree codes)
  #unique(vri_bem_dt[,SPEC_CD_1])
  vri_bem_dt <- vri_bem_dt |>
    mutate(DOM_TREE = case_when(
      SPEC_CD_1 %in% c("S","SE","SB","SXS","SW","SX","SS") ~ "Spruce",
      SPEC_CD_1 %in% c("BL","BA","B","BG") ~ "Fir",
      SPEC_CD_1 %in% c("P","PLI","PL","PLC") ~ "Pine",
      SPEC_CD_1 == "PA" ~ "Whitebark",
      SPEC_CD_1 %in% c("AT","ACT","AC") ~ "Populus",
      SPEC_CD_1 %in% c("HW","HM","H") ~ "Hemlock",
      SPEC_CD_1 %in% c("E","EP","EA") ~ "Birch",
      SPEC_CD_1 %in% c("F","FD","FDI") ~ "Douglas-fir",
      SPEC_CD_1 %in% c("CW","C") ~ "Cedar",
      SPEC_CD_1 %in% c("DR","DG","DM") ~ "Alder",
      SPEC_CD_1 %in% c("YC","Y") ~ "Cypress",
      SPEC_CD_1 %in% c("LT","LA","LW","L") ~ "Larch",
      SPEC_CD_1 %in% c("M","MB") ~ "Maple",
      .default = SPEC_CD_1)) |>
    mutate(STRCT_mod = case_when(
      STRCT_S1 %in% c("6","7a") ~ "7",
      .default = STRCT_S1)) #I kept it the same as the site selection script, but why change 6 to 7?

  vri_bem_dt <- vri_bem_dt |> mutate(ECO_TYPE = paste0(BEU_BEC,"_",STAND_A1,"_",STRCT_mod,"_",CROWN_ALL_1,"_",DOM_TREE))

  vri_bem <- st_as_sf(vri_bem_dt)

  return(vri_bem)
}

#'Simplify the VRI_BEM_WHR output
#'Further reduce layers present in VRI-BEM for sharing and distributing, based on similar reductions for site selection
#'
#'Consider retaining BEUMC_S2, STRCT_S2, BEUMC_S3, etc?
#'
#' @param vri_bem VRI-BEM feature class
#'
#' @return simplified vri_bem
#' @export
#'

simplify_vri_bem_output <- function(vri_bem) {

  if (FALSE) {
    BCLCS_LV_1<-BCLCS_LV_5<-BEU_BEC<-BEUMC_S1<-BGC_PHASE<-BGC_SUBZON<-BGC_VRT<-BGC_ZONE<-
      CROWN_ALL_1<-DOM_TREE<-ECO_TYPE<-ELEV<-MALAN_GFD_6C_SU_WA<-
      MALAN_WFD_6C_SU_WA<-MALAN_WST_6C_SU_WA<-MEAN_ASP<-MEAN_SLOPE<-MEAN_TRI<-MURAR_FFD_6C_SU_WA<-
      MURAR_HI_6C_SU_WA<-MURAR_PEFD_6C_SU_WA<-MURAR_PLFD_6C_SU_WA<-MURAR_SFD_6C_SU_WA<-PolyID<-
      PROJ_AGE_1<-Salmon<-Shape<-SITE_M3A<-SOIL_MOISTURE_REGIME_1<-SOIL_NUTRIENT_REGIME<-SPEC_CD_1<-
      SPEC_CD_2<-SPEC_CD_3<-SPEC_CD_4<-SPEC_CD_5<-SPEC_CD_6<-SPEC_PCT_1<-SPEC_PCT_2<-SPEC_PCT_3<-
      SPEC_PCT_4<-SPEC_PCT_5<-SPEC_PCT_6<-STAND_A1<-STRCT_mod<-TEIS_ID<-
      MALAN_WFD_6C_CAP_WA<-MALAN_GFD_6C_CAP_WA<-MALAN_WST_6C_CAP_WA<-MURAR_PEFD_6C_CAP_WA<-
      MURAR_HI_6C_CAP_WA<-MURAR_FFD_6C_CAP_WA<-MURAR_SFD_6C_CAP_WA<-MURAR_PLFD_6C_CAP_WA<-NULL
  }

  if("DOM_TREE" %in% colnames(vri_bem)){
    vri_bem <- vri_bem |> dplyr::select(PolyID, TEIS_ID, ECO_TYPE, BCLCS_LV_1:BCLCS_LV_5, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BGC_label, BEUMC_S1, SDEC_1, ELEV, MEAN_ASP, MEAN_SLOPE, MEAN_TRI, SITE_M3A, STAND_A1, STRCT_S1, PROJ_AGE_1, CROWN_ALL_1, DOM_TREE, SPEC_CD_1, SPEC_PCT_1, SPEC_CD_2, SPEC_PCT_2, SPEC_CD_3, SPEC_PCT_3, SPEC_CD_4, SPEC_PCT_4, SPEC_CD_5, SPEC_PCT_5, SPEC_CD_6, SPEC_PCT_6, Salmon, SOIL_MOISTURE_REGIME_1, SOIL_NUTRIENT_REGIME,SITE_POSITION_MESO, DSTRB_HIST, MRSRD_Y, most_recent_fire, percent_burned, dplyr::matches("_(6C|RSI)_(SU|CAP)_WA$"), Shape)}

  if(!"DOM_TREE" %in% colnames(vri_bem)){
    vri_bem <- clean_vri_bem_output(vri_bem) |>
      dplyr::select(PolyID, TEIS_ID, ECO_TYPE, BCLCS_LV_1:BCLCS_LV_5, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BGC_label, BEUMC_S1, SDEC_1, ELEV, MEAN_ASP, MEAN_SLOPE, MEAN_TRI, SITE_M3A, STAND_A1, STRCT_S1, PROJ_AGE_1, CROWN_ALL_1, DOM_TREE, SPEC_CD_1, SPEC_PCT_1, SPEC_CD_2, SPEC_PCT_2, SPEC_CD_3, SPEC_PCT_3, SPEC_CD_4, SPEC_PCT_4, SPEC_CD_5, SPEC_PCT_5, SPEC_CD_6, SPEC_PCT_6, Salmon, SOIL_MOISTURE_REGIME_1, SOIL_NUTRIENT_REGIME, SITE_POSITION_MESO, DSTRB_HIST, MRSRD_Y, dplyr::matches("_(6C|RSI)_(SU|CAP)_WA$"), Shape)}

    return(vri_bem)
}

