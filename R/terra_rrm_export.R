#' Terra-native RRM export helpers for the raster-only path

.terra_rrm_decode_layer_values <- function(x, layer_name) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(layer_name %in% names(x))

  values <- terra::values(x[[layer_name]], mat = FALSE)
  categories <- terra::cats(x[[layer_name]])[[1]]

  if (is.null(categories) || ncol(categories) < 2L) {
    if (identical(layer_name, "SLOPE_MOD")) {
      categories <- data.frame(
        value = c(1L, 2L, 3L, 4L, 5L),
        label = c("j", "k", "q", "w", "z"),
        stringsAsFactors = FALSE
      )
    }

    raster_conv <- .terra_rrm_get_raster_conv()
    lookup_source <- if (layer_name %in% names(raster_conv$bem)) raster_conv$bem[[layer_name]] else if (layer_name %in% names(raster_conv$vri)) raster_conv$vri[[layer_name]] else NULL
    if ((is.null(categories) || ncol(categories) < 2L) && !is.null(lookup_source) && nrow(lookup_source) > 0L) {
      categories <- data.frame(value = lookup_source[["factor"]], label = lookup_source[["value"]], stringsAsFactors = FALSE)
    }
  }

  if (!is.null(categories) && ncol(categories) >= 2L) {
    return(unname(categories[[2]][match(values, categories[[1]])]))
  }

  values
}


.terra_rrm_required_export_layers <- function(kind = c("moose", "bear")) {
  kind <- match.arg(kind)

  base_layers <- c(
    "ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SLOPE_MOD", "SITE_M3A",
    "SNOW_CODE", "ABOVE_ELEV_THOLD",
    paste0("SDEC_", 1:3), paste0("BEUMC_S", 1:3), paste0("FORESTED_", 1:3),
    paste0("CROWN_ALL_", 1:3), paste0("STRCT_S", 1:3), paste0("STAND_A", 1:3),
    c(
      paste0("STS_1_Age_", c("0_3", "4_10", "11_30", "31_40", "41_60", "61_80", "81_139", "140_249", "gt_249")),
      paste0("STS_2_Age_", c("0_3", "4_10", "11_30", "31_40", "41_60", "61_80", "81_139", "140_249", "gt_249")),
      paste0("STS_3_Age_", c("0_3", "4_10", "11_30", "31_40", "41_60", "61_80", "81_139", "140_249", "gt_249"))
    ),
    c(
      paste0("STAND_1_Age_", c("0_15", "16_30", "31_50", "51_80", "gt_80")),
      paste0("STAND_2_Age_", c("0_15", "16_30", "31_50", "51_80", "gt_80")),
      paste0("STAND_3_Age_", c("0_15", "16_30", "31_50", "51_80", "gt_80"))
    )
  )

  if (identical(kind, "bear")) {
    base_layers <- c(base_layers, "Salmon")
  }

  unique(base_layers)
}


.terra_rrm_export_values_table <- function(x, layer_names) {
  decoded <- lapply(layer_names, function(layer_name) .terra_rrm_decode_layer_values(x, layer_name))
  names(decoded) <- layer_names
  decoded$cell <- seq_len(terra::ncell(x))
  data.table::as.data.table(decoded)
}


.terra_rrm_projection_pairs <- function(component) {
  list(
    list(strct = sprintf("STS_%d_Age_0_3", component), stand = sprintf("STAND_%d_Age_0_15", component)),
    list(strct = sprintf("STS_%d_Age_4_10", component), stand = sprintf("STAND_%d_Age_0_15", component)),
    list(strct = sprintf("STS_%d_Age_11_30", component), stand = sprintf("STAND_%d_Age_0_15", component)),
    list(strct = sprintf("STS_%d_Age_11_30", component), stand = sprintf("STAND_%d_Age_16_30", component)),
    list(strct = sprintf("STS_%d_Age_31_40", component), stand = sprintf("STAND_%d_Age_31_50", component)),
    list(strct = sprintf("STS_%d_Age_41_60", component), stand = sprintf("STAND_%d_Age_31_50", component)),
    list(strct = sprintf("STS_%d_Age_41_60", component), stand = sprintf("STAND_%d_Age_51_80", component)),
    list(strct = sprintf("STS_%d_Age_61_80", component), stand = sprintf("STAND_%d_Age_51_80", component)),
    list(strct = sprintf("STS_%d_Age_81_139", component), stand = sprintf("STAND_%d_Age_gt_80", component)),
    list(strct = sprintf("STS_%d_Age_140_249", component), stand = sprintf("STAND_%d_Age_gt_80", component)),
    list(strct = sprintf("STS_%d_Age_gt_249", component), stand = sprintf("STAND_%d_Age_gt_80", component))
  )
}


.terra_rrm_component_export_dt <- function(values_dt, component, cell_area_ha, include_salmon = FALSE) {
  beumc_col <- sprintf("BEUMC_S%d", component)
  forested_col <- sprintf("FORESTED_%d", component)
  crown_col <- sprintf("CROWN_ALL_%d", component)
  strct_col <- sprintf("STRCT_S%d", component)
  stand_col <- sprintf("STAND_A%d", component)
  sdec_col <- sprintf("SDEC_%d", component)

  fixed_cols <- c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SLOPE_MOD", "SITE_M3A", if (include_salmon) "Salmon", "SNOW_CODE", "ABOVE_ELEV_THOLD")

  actual <- values_dt[
    get(sdec_col) > 0 & !is.na(get(beumc_col)) & !is.na(get(forested_col)),
    c(
      mget(fixed_cols),
      list(
        BEUMC = get(beumc_col),
        CROWN_ALL = get(crown_col),
        STRCT = get(strct_col),
        STAND = get(stand_col),
        FORESTED = get(forested_col),
        area_sum = cell_area_ha * get(sdec_col) / 10,
        projection = FALSE
      )
    )
  ]

  projected <- lapply(.terra_rrm_projection_pairs(component), function(pair) {
    values_dt[
      get(sdec_col) > 0 & !is.na(get(beumc_col)) & !is.na(get(forested_col)),
      c(
        mget(fixed_cols),
        list(
          BEUMC = get(beumc_col),
          CROWN_ALL = get(crown_col),
          STRCT = get(pair$strct),
          STAND = get(pair$stand),
          FORESTED = get(forested_col),
          area_sum = 0,
          projection = TRUE
        )
      )
    ]
  })

  data.table::rbindlist(c(list(actual), projected), use.names = TRUE, fill = TRUE)
}


.terra_rrm_finalize_rrm_export <- function(summary_dt, include_salmon = FALSE) {
  stand_reset_idx <- which(
    summary_dt[["area_sum"]] == 0 &
      summary_dt[["STAND"]] %in% c("B", "C", "M") &
      !(summary_dt[["STRCT"]] %in% c("4", "5", "6", "7"))
  )

  if (length(stand_reset_idx) > 0L) {
    data.table::set(summary_dt, i = stand_reset_idx, j = "STAND", value = NA_character_)
  }

  by_cols <- c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEUMC", "SLOPE_MOD", "SITE_M3A", if (include_salmon) "Salmon", "SNOW_CODE", "ABOVE_ELEV_THOLD", "CROWN_ALL", "STRCT", "STAND", "FORESTED")
  summary_dt[, list(Hectares = sum(get("area_sum"), na.rm = TRUE)), by = by_cols]
}


#' Terra-native RRM ecosystem summary for moose
#'
#' Creates the moose-style RRM ecosystem export from the raster-only terra path.
#' The function decodes categorical rasters to their label values, expands the
#' 11 projected age combinations for each decile, applies the same projected
#' stand cleanup used by the existing workflow, and returns grouped hectares.
#'
#' @param x A `terra::SpatRaster` containing the aligned raster-only pipeline
#'   outputs.
#' @return A `data.table` with the same grouped columns as
#'   `create_RRM_ecosystem()`.
#' @export
terra_rrm_create_RRM_ecosystem_moose <- function(x) {
  stopifnot(inherits(x, "SpatRaster"))

  required_layers <- .terra_rrm_required_export_layers("moose")
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(sprintf("Missing required layers for terra_rrm_create_RRM_ecosystem_moose: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
  }

  values_dt <- .terra_rrm_export_values_table(x, required_layers)
  cell_area_ha <- prod(terra::res(x)) / 10000
  summary_dt <- data.table::rbindlist(lapply(1:3, function(component) .terra_rrm_component_export_dt(values_dt, component, cell_area_ha, include_salmon = FALSE)), use.names = TRUE, fill = TRUE)
  .terra_rrm_finalize_rrm_export(summary_dt, include_salmon = FALSE)
}


#' Terra-native RRM ecosystem summary for bear
#'
#' Equivalent to `terra_rrm_create_RRM_ecosystem_moose()` but includes the
#' `Salmon` grouping key required by the bear export workflow.
#'
#' @param x A `terra::SpatRaster` containing the aligned raster-only pipeline
#'   outputs, including a `Salmon` layer.
#' @return A `data.table` with the bear-style grouped columns.
#' @export
terra_rrm_create_RRM_ecosystem_bear <- function(x) {
  stopifnot(inherits(x, "SpatRaster"))

  required_layers <- .terra_rrm_required_export_layers("bear")
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(sprintf("Missing required layers for terra_rrm_create_RRM_ecosystem_bear: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
  }

  values_dt <- .terra_rrm_export_values_table(x, required_layers)
  cell_area_ha <- prod(terra::res(x)) / 10000
  summary_dt <- data.table::rbindlist(lapply(1:3, function(component) .terra_rrm_component_export_dt(values_dt, component, cell_area_ha, include_salmon = TRUE)), use.names = TRUE, fill = TRUE)
  .terra_rrm_finalize_rrm_export(summary_dt, include_salmon = TRUE)
}