#' merge_rrm_on_vri
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param animal character, "bear" or "moose"
#' @param return_sf logical, if TRUE  return sf object , if FALSE return data.table object and update by reference
#' @return vri-bem object with new columns for rating
#' @import data.table
#' @export
.rrm_value_to_rating <- function(x) {
  cut(
    signif(x, digits = 3),
    breaks = c(-Inf, 0, 0.05, 0.25, 0.5, 0.75, Inf),
    labels = c(6L, 5L, 4L, 3L, 2L, 1L),
    right = TRUE
  ) |>
    as.character() |>
    as.integer()
}


.rrm_weighted_average_rsi <- function(vri_bem, value_cols) {
  weights <- as.matrix(vri_bem[, c("SDEC_1", "SDEC_2", "SDEC_3"), with = FALSE])
  values <- as.matrix(vri_bem[, value_cols, with = FALSE])

  storage.mode(weights) <- "double"
  storage.mode(values) <- "double"

  valid <- !is.na(values)
  numer <- rowSums(values * weights, na.rm = TRUE)
  denom <- rowSums(weights * valid)
  result <- numer / denom
  result[is.na(values[, 1]) | denom == 0] <- NA_real_

  result
}


.rrm_capability_rating_summary <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}


.rrm_capability_rsi_summary <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}


.rrm_suitability_special_rows <- function(vri_bem, decile) {
  forested_col <- sprintf("FORESTED_%d", decile)
  strct_col <- sprintf("STRCT_S%d", decile)
  climax_col <- sprintf("STS_CLIMAX_%d", decile)

  which(
    (vri_bem[[forested_col]] == "Y" &
       vri_bem[[strct_col]] == "7a" &
       vri_bem[["VRI_AGE_CL_STS"]] == -1) |
      (vri_bem[[forested_col]] == "N" &
         is.na(vri_bem[[strct_col]]) &
         vri_bem[["ABOVE_ELEV_THOLD"]] == "N" &
         !is.na(vri_bem[[climax_col]]))
  )
}


merge_rrm_on_vri <- function(vri_bem, rrm_dt, animal, return_sf = TRUE) {

  if (FALSE) {
    Hectares_1<-Hectares_2<-Hectares_3<-rrm_merge_ind<-SDEC_1<-SDEC_2<-SDEC_3<-NULL
  }

  vri_bem <- vri_bem |> mutate(SDEC_1 = ifelse(is.na(SDEC_1), 0, SDEC_1),
                               SDEC_2 = ifelse(is.na(SDEC_2), 0, SDEC_2),
                               SDEC_3 = ifelse(is.na(SDEC_3), 0, SDEC_3)) #replace all NAs in SDEC_1, 2, 3 with 0

  setDT(vri_bem)

  # calc capability rating and format rrm table
  rrm_dt <- format_rrm_dt(rrm_dt = rrm_dt, animal = animal) #updated to format in a species-specific way
  rrm_dt <- calc_capability_rating(rrm_dt = rrm_dt, animal = animal)


  rating_variables <- grep("_6C$", names(rrm_dt), value = TRUE)
  rsi_variables <- sub("_6C$", "_RSI", rating_variables)
  cap_rating_variables <- paste0(rating_variables, "_CAP")
  cap_rsi_variables <- paste0(rsi_variables, "_CAP")

  has_rsi_variables <- rsi_variables %in% names(rrm_dt)
  if (any(has_rsi_variables) && !all(has_rsi_variables)) {
    stop("rrm_dt must contain matching '*_RSI' columns for each '*_6C' rating column", call. = FALSE)
  }
  if (!all(has_rsi_variables)) {
    stop("rrm_dt must contain matching '*_RSI' columns for each '*_6C' rating column", call. = FALSE)
  }

  variables_merge_expr <- paste0(
    "list(",
    paste(c(rating_variables, rsi_variables, cap_rating_variables, cap_rsi_variables, "Hectares"), collapse = ","),
    ")"
  )

  first_decile_variables <- c(
    paste0(rating_variables, "_SU_1"),
    paste0(rsi_variables, "_SU_1"),
    paste0(cap_rating_variables, "_1"),
    paste0(cap_rsi_variables, "_1"),
    "Hectares_1"
  )
  second_decile_variables <- c(
    paste0(rating_variables, "_SU_2"),
    paste0(rsi_variables, "_SU_2"),
    paste0(cap_rating_variables, "_2"),
    paste0(cap_rsi_variables, "_2"),
    "Hectares_2"
  )
  third_decile_variables <- c(
    paste0(rating_variables, "_SU_3"),
    paste0(rsi_variables, "_SU_3"),
    paste0(cap_rating_variables, "_3"),
    paste0(cap_rsi_variables, "_3"),
    "Hectares_3"
  )

  if (animal == "bear") {
    merge_rating_bear(vri_bem = vri_bem,
                      rrm_dt = rrm_dt,
                      rating_variables = list(first_decile_variables,
                                              second_decile_variables,
                                              third_decile_variables),
                      rating_variables_expr = variables_merge_expr)
  }

  if (animal == "moose") {
    merge_rating_moose(vri_bem = vri_bem,
                      rrm_dt = rrm_dt,
                      rating_variables = list(first_decile_variables,
                                              second_decile_variables,
                                              third_decile_variables),
                      rating_variables_expr = variables_merge_expr)
  }

  if (animal == "huckleberry"){
    merge_rating_huckleberry(vri_bem = vri_bem,
                       rrm_dt = rrm_dt,
                       rating_variables = list(first_decile_variables,
                                               second_decile_variables,
                                               third_decile_variables),
                       rating_variables_expr = variables_merge_expr)
  }

  vri_bem[ , rrm_merge_ind := !is.na(fcoalesce(Hectares_1, Hectares_2, Hectares_3))]

  suit_special_rows <- lapply(1:3, function(decile) .rrm_suitability_special_rows(vri_bem, decile))

  for (idx in seq_along(rating_variables)) {
    rating_variable <- rating_variables[[idx]]
    rsi_variable <- rsi_variables[[idx]]
    cap_variable <- cap_rating_variables[[idx]]
    cap_rsi_variable <- cap_rsi_variables[[idx]]

    suit_vars <- paste0(rating_variable, "_SU_", 1:3)
    suit_rsi_vars <- paste0(rsi_variable, "_SU_", 1:3)
    high_value_suit_var <- paste0(rating_variable, "_SU_HV")
    weighted_average_suit_var <- paste0(rating_variable, "_SU_WA")
    weighted_average_suit_rsi_var <- paste0(rsi_variable, "_SU_WA")

    cap_vars <- paste0(cap_variable, "_", 1:3)
    cap_rsi_vars <- paste0(cap_rsi_variable, "_", 1:3)
    high_value_cap_var <- paste0(cap_variable, "_HV")
    weighted_average_cap_var <- paste0(cap_variable, "_WA")
    weighted_average_cap_rsi_var <- paste0(cap_rsi_variable, "_WA")

    for (decile in 1:3) {
      if (length(suit_special_rows[[decile]]) > 0L) {
        set(
          vri_bem,
          i = suit_special_rows[[decile]],
          j = c(suit_vars[[decile]], suit_rsi_vars[[decile]]),
          value = NA
        )
      }

      suit_invalid_rows <- which(vri_bem[[suit_vars[[decile]]]] > 6)
      if (length(suit_invalid_rows) > 0L) {
        set(vri_bem, i = suit_invalid_rows, j = c(suit_vars[[decile]], suit_rsi_vars[[decile]]), value = NA)
      }

      cap_invalid_rows <- which(vri_bem[[cap_vars[[decile]]]] > 6)
      if (length(cap_invalid_rows) > 0L) {
        set(vri_bem, i = cap_invalid_rows, j = c(cap_vars[[decile]], cap_rsi_vars[[decile]]), value = NA)
      }
    }

    suit_hv <- do.call(pmin, lapply(suit_vars, function(col) fcoalesce(vri_bem[[col]], 9)))
    suit_hv[suit_hv > 8] <- NA_real_
    set(vri_bem, j = high_value_suit_var, value = suit_hv)

    suit_wa_rsi <- .rrm_weighted_average_rsi(vri_bem, suit_rsi_vars)
    set(vri_bem, j = weighted_average_suit_rsi_var, value = suit_wa_rsi)
    set(vri_bem, j = weighted_average_suit_var, value = .rrm_value_to_rating(suit_wa_rsi))

    cap_hv <- do.call(pmin, lapply(cap_vars, function(col) fcoalesce(vri_bem[[col]], 9)))
    cap_hv[cap_hv > 8] <- NA_real_
    set(vri_bem, j = high_value_cap_var, value = cap_hv)

    cap_wa_rsi <- .rrm_weighted_average_rsi(vri_bem, cap_rsi_vars)
    set(vri_bem, j = weighted_average_cap_rsi_var, value = cap_wa_rsi)
    set(vri_bem, j = weighted_average_cap_var, value = .rrm_value_to_rating(cap_wa_rsi))
  }

  # rating for decile that are 0 should be NA
  set(vri_bem, i = which(vri_bem$SDEC_1 == 0), j = first_decile_variables, value = NA)
  set(vri_bem, i = which(vri_bem$SDEC_2 == 0), j = second_decile_variables, value = NA)
  set(vri_bem, i = which(vri_bem$SDEC_3 == 0), j = third_decile_variables, value = NA)

  set(vri_bem, j = c("Hectares_1", "Hectares_2", "Hectares_3"), value = NULL)

  if (return_sf) {
    return(st_as_sf(vri_bem))
  } else {
    return(vri_bem)
  }

}



#' merge_rating_bear
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param rating_variables list, vector for each rating variables to create
#' @param rating_variables_expr parse_expr , expression for variable to merge in data.table
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rating_bear <- function(vri_bem, rrm_dt, rating_variables, rating_variables_expr) {
  # merge on decile 1 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, Salmon = Salmon, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_1 = Crown_all, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c(", paste0("'", paste(rating_variables[[1]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  # merge on decile 2 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, Salmon = Salmon, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_2 = Crown_all, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c(", paste0("'", paste(rating_variables[[2]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))



  # merge on decile 3 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, Salmon = Salmon, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_3 = Crown_all, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c(", paste0("'", paste(rating_variables[[3]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))


  return(vri_bem)
}


#' merge_rating_moose
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param rating_variables list, vector for each rating variables to create
#' @param rating_variables_expr parse_expr , expression for variable to merge in data.table
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rating_moose <- function(vri_bem, rrm_dt, rating_variables, rating_variables_expr) {
  # merge on decile 1 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_1 = Crown_all, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c(", paste0("'", paste(rating_variables[[1]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  # merge on decile 2 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_2 = Crown_all, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c(", paste0("'", paste(rating_variables[[2]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))



  # merge on decile 3 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_3 = Crown_all, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c(", paste0("'", paste(rating_variables[[3]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  return(vri_bem)
}

#' merge_rating_huckleberry
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param rating_variables list, vector for each rating variables to create
#' @param rating_variables_expr parse_expr , expression for variable to merge in data.table
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rating_huckleberry <- function(vri_bem, rrm_dt, rating_variables, rating_variables_expr) {
  # merge on decile 1 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,BGC_PHASE = Bgc_phase, HUCK_ASP = Huck_asp,HUCK_ELEV_Thold = HUCK_ELEV_Thold, CROWN_ALL_1 = Crown_All, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c(", paste0("'", paste(rating_variables[[1]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  # merge on decile 2 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,BGC_PHASE = Bgc_phase, HUCK_ASP = Huck_asp,HUCK_ELEV_Thold = HUCK_ELEV_Thold,
  CROWN_ALL_2 = Crown_All, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c(", paste0("'", paste(rating_variables[[2]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  # merge on decile 3 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon,
  BGC_VRT = Bgc_vrt,BGC_PHASE = Bgc_phase, HUCK_ASP = Huck_asp,HUCK_ELEV_Thold = HUCK_ELEV_Thold,
  CROWN_ALL_3 = Crown_All, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c(", paste0("'", paste(rating_variables[[3]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  return(vri_bem)
}
