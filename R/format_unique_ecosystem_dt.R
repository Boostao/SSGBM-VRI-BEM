#' Format unique ecosystem data
#'
#' Format the unique ecosystem data, to make sure that any conversion that happened when reading the csv did not affect the expected class of each fields
#'
#' @param unique_ecosystem_dt data.table object that represent the unique ecosystems
#' @return data.table
#' @import data.table
#' @export
format_unique_ecosystem_dt <- function(unique_ecosystem_dt) {
  #default to NAs for missing data
  #unique_ecosystem_dt[BGC_VRT == "" | BGC_VRT == 0, BGC_VRT := NA_character_] TMP FIX
  #unique_ecosystem_dt[ , BGC_VRT := as.character(BGC_VRT)] TMP FIX
  data.table::set(unique_ecosystem_dt, j = "BGC_VRT", value = as.character(unique_ecosystem_dt[["BGC_VRT"]]))

  bgc_vrt_na_idx <- which(unique_ecosystem_dt[["BGC_VRT"]] == "0")
  if (length(bgc_vrt_na_idx) > 0L) {
    data.table::set(unique_ecosystem_dt, i = bgc_vrt_na_idx, j = "BGC_VRT", value = NA_character_)
  }

  bgc_phase_na_idx <- which(unique_ecosystem_dt[["BGC_PHASE"]] == "")
  if (length(bgc_phase_na_idx) > 0L) {
    data.table::set(unique_ecosystem_dt, i = bgc_phase_na_idx, j = "BGC_PHASE", value = NA_character_)
  }

  unique_ecosystem_dt
}
