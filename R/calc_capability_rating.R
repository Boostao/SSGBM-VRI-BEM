#' calc_capability_rating
#'
#' calculate capability rating which is the best suitability rating among ecosystem regardless of structural stage
#'
#' @param rrm_dt data.table object that contains the rrm
#' @param animal character, "bear" or "moose"
#' @return data.table rrm with new column capability_rating
#' @import data.table
#' @export
calc_capability_rating <- function(rrm_dt, animal) {

  if(animal %in% c("bear","moose")){
    if (FALSE) {
      .<-Eco_sec<-Bgc_zone<-Bgc_subzon<-Bgc_vrt<-Bgc_phase<- Beumc<-Slope_mod<-Site_m3a<-
      Salmon<-Snow_code<-Above_Elev_Thold<-Crown_all<-NULL
    }
  }

  if(animal == "huckleberry"){
    if (FALSE) {
      .<-Eco_sec<-Bgc_zone<-Bgc_subzon<-Bgc_vrt<-Bgc_phase<-Huck_asp<-HUCK_ELEV_Thold<-Crown_All<-NULL
    }
  }

  data.table::setDT(rrm_dt)
  rating_variables <- grep("_6C$", names(rrm_dt), value = TRUE)
  rsi_variables <- sub("_6C$", "_RSI", rating_variables)
  cap_rating_variables <- paste0(rating_variables, "_CAP")
  cap_rsi_variables <- paste0(rsi_variables, "_CAP")

  has_rsi_variables <- rsi_variables %in% names(rrm_dt)
  if (any(has_rsi_variables) && !all(has_rsi_variables)) {
    stop("rrm_dt must contain matching '*_RSI' columns for each '*_6C' rating column", call. = FALSE)
  }

  group_cols <- switch(
    animal,
    bear = c("Eco_sec", "Bgc_zone", "Bgc_subzon", "Bgc_vrt", "Bgc_phase", "Beumc", "Slope_mod", "Site_m3a", "Salmon", "Snow_code", "Above_Elev_Thold", "Crown_all"),
    moose = c("Eco_sec", "Bgc_zone", "Bgc_subzon", "Bgc_vrt", "Bgc_phase", "Beumc", "Slope_mod", "Site_m3a", "Snow_code", "Above_Elev_Thold", "Crown_all"),
    huckleberry = c("Eco_sec", "Bgc_zone", "Bgc_subzon", "Bgc_vrt", "Bgc_phase", "Huck_asp", "HUCK_ELEV_Thold", "Crown_All")
  )

  rrm_dt[, (cap_rating_variables) := lapply(.SD, .rrm_capability_rating_summary), by = group_cols, .SDcols = rating_variables]

  if (all(has_rsi_variables)) {
    rrm_dt[, (cap_rsi_variables) := lapply(.SD, .rrm_capability_rsi_summary), by = group_cols, .SDcols = rsi_variables]
  }

  return(rrm_dt)
}
