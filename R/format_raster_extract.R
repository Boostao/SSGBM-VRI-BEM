#' Format the extraction of a raster
#'
#'
#' @param input_dt data.table extracted raster converted to data.table
#' @param character_attributes character vector that represent the attributes of type character that need to be transformed
#' @param date_attributes character vector that represent the attributes of type date that need to be transformed
#' @param factor_conv_list list of data.table that contains corresponding factor values for each variable that is not numeric
#' @return data.table
#' @import data.table
#' @export
format_raster_extract <- function(input_dt, character_attributes = NULL, date_attributes = NULL, factor_conv_list) {
  attributes_names <- names(input_dt)
  if (!is.null(character_attributes)) {
    for (char_att in character_attributes) {
      matching_att <- grep(paste0("^",char_att) , attributes_names, value = TRUE)
      if (length(matching_att) > 1) {
        set(input_dt, j = char_att, value = rowSums(input_dt[, matching_att, with = F]))
        set(input_dt, j = matching_att, value = NULL)
      }
      set(input_dt, j = char_att, value = factor_conv_list[[char_att]][["value"]][match(input_dt[[char_att]],
                                                                                        factor_conv_list[[char_att]][["factor"]])])
    }
  }

  if (!is.null(date_attributes)) {
    for (date_att in date_attributes) {
      set(input_dt, j = date_attributes, value = as.character(input_dt[[date_att]]))
    }
  }

  return(input_dt)

}


#' Format the extraction of a the BEM raster
#'
#'
#' @inheritParams format_raster_extract
#' @return data.table
#' @import data.table
#' @export
format_bem_raster_extract <- function(input_dt, character_attributes = c("BGC_ZONE","BGC_SUBZON", "BGC_PHASE", "SLOPE_MOD", "ECO_SEC",  "BEUMC_S1",   "REALM_1",    "GROUP_1",    "CLASS_1",    "KIND_1",     "SITE_S1",    "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1",  "SITE_M1A",
                                                                         "SITE_M1B" ,  "STRCT_S1",   "STRCT_M1",  "STAND_A1",   "SERAL_1",    "DISTCLS_1",  "DISTSCLS_1", "DISSSCLS_1", "SECL_1",     "SESUBCL_1",  "COND_1",     "VIAB_1",     "FORESTED_1", "TREE_C1", "SHRUB_C1", "BEUMC_S2",   "REALM_2",
                                                                         "GROUP_2",    "CLASS_2",    "KIND_2",     "SITE_S2",    "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C", "SITEAM_S2D", "SITEMC_S2",  "SITE_M2A",   "SITE_M2B",   "STRCT_S2",   "STRCT_M2",   "STAND_A2",   "SERAL_2",
                                                                         "DISTCLS_2",  "DISTSCLS_2", "DISSSCLS_2", "SECL_2",     "SESUBCL_2",  "COND_2",     "VIAB_2",     "FORESTED_2","TREE_C2", "SHRUB_C2", "BEUMC_S3",   "REALM_3",    "GROUP_3",   "CLASS_3",    "KIND_3",     "SITE_S3" ,   "SITEAM_S3A",
                                                                         "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3",  "SITE_M3A",   "SITE_M3B",   "STRCT_S3",   "STRCT_M3",   "STAND_A3",   "SERAL_3",    "DISTCLS_3",  "DISTSCLS_3", "DISSSCLS_3", "SECL_3",     "SESUBCL_3",
                                                                         "COND_3", "VIAB_3", "FORESTED_3", "TREE_C3", "SHRUB_C3"),
                                  date_attributes = "HRVSTDT") {

 format_raster_extract(input_dt = input_dt,
                       character_attributes = character_attributes,
                       date_attributes = date_attributes,
                       factor_conv_list = raster_conv$bem)

}

#' Format the extraction of a the VRI raster
#'
#'
#' @inheritParams format_raster_extract
#' @return data.table
#' @import data.table
#' @export
format_vri_raster_extract <- function(input_dt, character_attributes = c(paste0("BCLCS_LV_", 1:5), paste0("SPEC_CD_", 1:6), "LAND_CD_1", "LBL_VEGCOV"),
                                      date_attributes = NULL) {

  format_raster_extract(input_dt = input_dt,
                        character_attributes = character_attributes,
                        date_attributes = date_attributes,
                        factor_conv_list = raster_conv$bem)

}
