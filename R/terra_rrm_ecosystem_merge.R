#' Terra-native unique ecosystem merge stage for the raster-only RRM path

.terra_rrm_blank_like <- function(template, layer_name) {
  stopifnot(inherits(template, "SpatRaster"))

  blank <- template[[1]]
  terra::values(blank) <- NA_real_
  names(blank) <- layer_name
  blank
}


.terra_rrm_constant_like <- function(template, layer_name, value) {
  constant <- .terra_rrm_blank_like(template, layer_name)
  terra::values(constant) <- value
  constant
}


.terra_rrm_add_missing_layers <- function(x, layer_names) {
  stopifnot(inherits(x, "SpatRaster"))

  missing_layers <- setdiff(layer_names, names(x))
  if (length(missing_layers) == 0L) {
    return(x)
  }

  result <- x
  raster_conv <- .terra_rrm_get_raster_conv()
  for (layer_name in missing_layers) {
    blank <- .terra_rrm_blank_like(result[[1]], layer_name)

    if (layer_name %in% names(raster_conv$bem)) {
      levels(blank) <- data.frame(
        value = raster_conv$bem[[layer_name]][["factor"]],
        label = raster_conv$bem[[layer_name]][["value"]],
        stringsAsFactors = FALSE
      )
    } else if (layer_name %in% names(raster_conv$vri)) {
      levels(blank) <- data.frame(
        value = raster_conv$vri[[layer_name]][["factor"]],
        label = raster_conv$vri[[layer_name]][["value"]],
        stringsAsFactors = FALSE
      )
    }

    result <- c(result, blank)
  }
  result
}


.terra_rrm_apply_raster_values <- function(x, mask, values) {
  stopifnot(inherits(x, "SpatRaster"), inherits(mask, "SpatRaster"), inherits(values, "SpatRaster"))

  categories <- terra::cats(x)[[1]]
  result <- terra::ifel(mask[[1]] == 1, values[[1]], x[[1]])
  names(result) <- names(x)
  if (!is.null(categories) && ncol(categories) >= 2L) {
    levels(result) <- categories
  }
  result
}


.terra_rrm_replace_layer <- function(x, layer_name, values) {
  stopifnot(inherits(x, "SpatRaster"), inherits(values, "SpatRaster"))
  stopifnot(layer_name %in% names(x))

  categories <- terra::cats(x[[layer_name]])[[1]]
  names(values) <- layer_name
  x[[layer_name]] <- values
  if (!is.null(categories) && ncol(categories) >= 2L) {
    levels(x[[layer_name]]) <- categories
  }
  x
}


.terra_rrm_match_exact_value <- function(x, layer_name, value, na_matches_na = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(layer_name %in% names(x))

  if (is.na(value) || !nzchar(trimws(as.character(value)))) {
    if (!na_matches_na) {
      return(.terra_rrm_non_missing_mask(x))
    }
    return(.terra_rrm_optional_na_mask(x, layer_name))
  }

  .terra_rrm_rule_value_mask(x, layer_name, as.character(value))
}


.terra_rrm_row_lookup_raster <- function(age_raster, target_layer, row, columns) {
  stopifnot(inherits(age_raster, "SpatRaster"), inherits(target_layer, "SpatRaster"))

  resolved <- vapply(
    columns,
    function(column_name) .terra_rrm_resolve_output_value(target_layer, names(target_layer), row[[column_name]]),
    numeric(1L)
  )

  result <- suppressWarnings(
    terra::app(
      age_raster,
      fun = function(values) {
        out <- rep(NA_real_, length(values))
        valid <- !is.na(values)

        if (length(columns) == 5L) {
          out[valid & values <= 15] <- resolved[1]
          out[valid & values > 15 & values <= 30] <- resolved[2]
          out[valid & values > 30 & values <= 50] <- resolved[3]
          out[valid & values > 50 & values <= 80] <- resolved[4]
          out[valid & values > 80] <- resolved[5]
        } else {
          out[valid & values <= 3] <- resolved[1]
          out[valid & values > 3 & values <= 10] <- resolved[2]
          out[valid & values > 10 & values <= 30] <- resolved[3]
          out[valid & values > 30 & values <= 40] <- resolved[4]
          out[valid & values > 40 & values <= 60] <- resolved[5]
          out[valid & values > 60 & values <= 80] <- resolved[6]
          out[valid & values > 80 & values <= 139] <- resolved[7]
          out[valid & values > 139 & values <= 249] <- resolved[8]
          out[valid & values > 249] <- resolved[9]
        }

        out
      }
    )
  )

  names(result) <- names(target_layer)
  result
}


.terra_rrm_match_label_prefix <- function(x, layer_name, pattern) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(layer_name %in% names(x))

  categories <- terra::cats(x[[layer_name]])[[1]]
  if (is.null(categories) || ncol(categories) < 2L) {
    raster_conv <- .terra_rrm_get_raster_conv()
    lookup_source <- if (layer_name %in% names(raster_conv$bem)) raster_conv$bem[[layer_name]] else if (layer_name %in% names(raster_conv$vri)) raster_conv$vri[[layer_name]] else NULL
    if (!is.null(lookup_source) && nrow(lookup_source) > 0L) {
      categories <- data.frame(value = lookup_source[["factor"]], label = lookup_source[["value"]], stringsAsFactors = FALSE)
    }
  }

  if (is.null(categories) || ncol(categories) < 2L) {
    stop(sprintf("Layer '%s' requires categories for prefix matching.", layer_name), call. = FALSE)
  }

  matched_codes <- categories[[1]][grepl(pattern, categories[[2]])]
  .terra_rrm_match_codes(x[[layer_name]], matched_codes)
}


.terra_rrm_translate_layer_codes <- function(source_layer, target_layer) {
  stopifnot(inherits(source_layer, "SpatRaster"), inherits(target_layer, "SpatRaster"))

  source_categories <- terra::cats(source_layer)[[1]]
  target_categories <- terra::cats(target_layer)[[1]]

  if (is.null(source_categories) || ncol(source_categories) < 2L || is.null(target_categories) || ncol(target_categories) < 2L) {
    return(source_layer)
  }

  target_codes <- target_categories[[1]][match(source_categories[[2]], target_categories[[2]])]
  translated <- suppressWarnings(
    terra::app(
      source_layer,
      fun = function(values) {
        matched_idx <- match(values, source_categories[[1]])
        out <- target_codes[matched_idx]
        out[is.na(matched_idx)] <- NA
        out
      }
    )
  )

  names(translated) <- names(target_layer)
  translated
}


.terra_rrm_compute_std_vri <- function(x) {
  species_list <- c(
    "D", "DR", "DG", "DM", "U", "UP", "A", "AC", "ACB", "ACT", "AX", "AT",
    "R", "RA", "E", "EA", "EXP", "EP", "EW", "G", "GP", "M", "MB", "MV",
    "Q", "QG", "XH", "V", "VB", "VP", "W", "WS", "WA", "WB", "WD", "WP", "WT", "ZH"
  )

  pct_layers <- paste0("SPEC_PCT_", 1:6)
  cd_layers <- paste0("SPEC_CD_", 1:6)
  if (!all(c(pct_layers, cd_layers) %in% names(x))) {
    return(x)
  }

  total_pct <- x[[pct_layers[1]]] * 0
  names(total_pct) <- "total_pct"

  for (idx in seq_along(cd_layers)) {
    species_mask <- .terra_rrm_rule_value_mask(x, cd_layers[idx], paste(species_list, collapse = ","))
    contribution <- terra::ifel(species_mask[[1]] == 1, x[[pct_layers[idx]]], 0)
    contribution <- terra::ifel(is.na(contribution), 0, contribution)
    total_pct <- total_pct + contribution
    names(total_pct) <- "total_pct"
  }

  x[["STD_VRI"]] <- .terra_rrm_apply_raster_values(
    x[["STD_VRI"]],
    .terra_rrm_non_missing_mask(x),
    terra::ifel(
      total_pct < 25,
      .terra_rrm_constant_like(x[["STD_VRI"]], "STD_VRI", .terra_rrm_resolve_output_value(x, "STD_VRI", "C")),
      terra::ifel(
        total_pct < 75,
        .terra_rrm_constant_like(x[["STD_VRI"]], "STD_VRI", .terra_rrm_resolve_output_value(x, "STD_VRI", "M")),
        .terra_rrm_constant_like(x[["STD_VRI"]], "STD_VRI", .terra_rrm_resolve_output_value(x, "STD_VRI", "B"))
      )
    )
  )

  x
}


.terra_rrm_compute_crown_all <- function(x) {
  if (!"CR_CLOSURE" %in% names(x)) {
    return(x)
  }

  crown_template <- .terra_rrm_blank_like(x[["CROWN_ALL"]], "CROWN_ALL")
  vl_code <- .terra_rrm_resolve_output_value(x, "CROWN_ALL", "VL-L")
  m_code <- .terra_rrm_resolve_output_value(x, "CROWN_ALL", "M")
  h_code <- .terra_rrm_resolve_output_value(x, "CROWN_ALL", "H")
  vh_code <- .terra_rrm_resolve_output_value(x, "CROWN_ALL", "VH")

  crown_values <- terra::ifel(
    x[["CR_CLOSURE"]] <= 25,
    .terra_rrm_constant_like(crown_template, "CROWN_ALL", vl_code),
    terra::ifel(
      x[["CR_CLOSURE"]] <= 40,
      .terra_rrm_constant_like(crown_template, "CROWN_ALL", m_code),
      terra::ifel(
        x[["CR_CLOSURE"]] <= 60,
        .terra_rrm_constant_like(crown_template, "CROWN_ALL", h_code),
        terra::ifel(x[["CR_CLOSURE"]] > 60, .terra_rrm_constant_like(crown_template, "CROWN_ALL", vh_code), NA)
      )
    )
  )
  names(crown_values) <- "CROWN_ALL"
  x <- .terra_rrm_replace_layer(x, "CROWN_ALL", crown_values)
  x
}


#' Terra-native unique ecosystem field merge
#'
#' Recreates the main lookup and age-derivation behavior of
#' `merge_unique_ecosystem_fields()` on an aligned raster stack. This stage
#' merges ecosystem lookup attributes, computes `STD_VRI` and `CROWN_ALL`, and
#' derives `STRCT_S1-3` and `STAND_A1-3` from the unique ecosystem table and
#' age-class layers.
#'
#' @param x A `terra::SpatRaster` containing aligned BEM/VRI layers.
#' @param unique_ecosystem_dt A data frame with the same columns expected by the
#'   polygon workflow.
#' @param filename Optional filename for writing the updated stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with unique ecosystem fields merged.
#' @export
terra_rrm_merge_unique_ecosystem_fields <- function(x,
                                                    unique_ecosystem_dt,
                                                    filename = NULL,
                                                    overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(unique_ecosystem_dt))

  required_layers <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD")
  required_cols <- c(
    "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC", "REALM", "GROUP", "CLASS", "KIND",
    "Snow_Code", "Forested (Y/N)", "Strct_Climax", "Stand_Climax",
    "Stand_Age_0-15", "Stand_Age_16-30", "Stand_Age_31-50", "Stand_Age_51-80", "Stand_Age_80+",
    "Struct_Age_0-3", "Struct_Age_4-10", "Struct_Age_11-30", "Struct_Age_31-40", "Struct_Age_41-60",
    "Struct_Age_61-80", "Struct_Age_81-139", "Struct_Age_140-249", "Struct_Age_250+"
  )

  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(sprintf("Missing required layers for terra_rrm_merge_unique_ecosystem_fields: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
  }

  missing_cols <- setdiff(required_cols, names(unique_ecosystem_dt))
  if (length(missing_cols) > 0L) {
    stop(sprintf("unique_ecosystem_dt is missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  result <- .terra_rrm_add_missing_layers(
    x,
    c(
      "STD_VRI", "CROWN_ALL", "SNOW_CODE",
      paste0(rep(c("REALM_", "GROUP_", "CLASS_", "KIND_", "FORESTED_", "STS_CLIMAX_", "STAND_CLIMAX_", "STRCT_S", "STAND_A"), each = 3), rep(1:3, times = 9))
    )
  )

  result <- .terra_rrm_compute_std_vri(result)
  result <- .terra_rrm_compute_crown_all(result)

  parkland_mask <- {
    categories <- terra::cats(result[["BGC_SUBZON"]])[[1]]
    matched_codes <- if (!is.null(categories) && ncol(categories) >= 2L) categories[[1]][grepl("p$", categories[[2]])] else numeric(0)
    .terra_rrm_match_codes(result[["BGC_SUBZON"]], matched_codes)
  }

  stand_age_layers <- lapply(1:3, function(component) {
    layer_name <- sprintf("STAND_A%d", component)
    if (!layer_name %in% names(result)) {
      return(NULL)
    }
    .terra_rrm_blank_like(result[[layer_name]], sprintf("STAND_AGE_%d", component))
  })
  sts_age_layers <- lapply(1:3, function(component) {
    layer_name <- sprintf("STRCT_S%d", component)
    if (!layer_name %in% names(result)) {
      return(NULL)
    }
    .terra_rrm_blank_like(result[[layer_name]], sprintf("STS_AGE_%d", component))
  })

  unique_ecosystem_dt <- as.data.frame(unique_ecosystem_dt, stringsAsFactors = FALSE)

  for (component in 1:3) {
    beumc_layer <- sprintf("BEUMC_S%d", component)
    if (!beumc_layer %in% names(result)) {
      next
    }

    for (row_idx in seq_len(nrow(unique_ecosystem_dt))) {
      row <- unique_ecosystem_dt[row_idx, , drop = FALSE]
      mask <- .terra_rrm_non_missing_mask(result)
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_match_exact_value(result, "BGC_ZONE", row$BGC_ZONE[[1]]))
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_match_exact_value(result, "BGC_SUBZON", row$BGC_SUBZON[[1]]))
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_match_exact_value(result, "BGC_VRT", row$BGC_VRT[[1]], na_matches_na = TRUE))
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_match_exact_value(result, "BGC_PHASE", row$BGC_PHASE[[1]], na_matches_na = TRUE))
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_match_exact_value(result, beumc_layer, row$BEU_MC[[1]]))

      direct_values <- c(
        REALM = row$REALM[[1]],
        GROUP = row$GROUP[[1]],
        CLASS = row$CLASS[[1]],
        KIND = row$KIND[[1]],
        FORESTED = row[["Forested (Y/N)"]][[1]],
        STS_CLIMAX = row$Strct_Climax[[1]],
        STAND_CLIMAX = row$Stand_Climax[[1]]
      )

      for (field_name in names(direct_values)) {
        target_layer <- sprintf("%s_%d", field_name, component)
        if (!target_layer %in% names(result)) {
          next
        }
        value <- direct_values[[field_name]]
        if (is.na(value) || !nzchar(trimws(as.character(value)))) {
          next
        }
        result <- .terra_rrm_apply_constant(result, target_layer, mask, .terra_rrm_resolve_output_value(result, target_layer, value))
      }

      if (component == 1L && !is.na(row$Snow_Code[[1]]) && nzchar(trimws(as.character(row$Snow_Code[[1]])))) {
        result <- .terra_rrm_apply_constant(result, "SNOW_CODE", mask, .terra_rrm_resolve_output_value(result, "SNOW_CODE", row$Snow_Code[[1]]))
      }

      stand_age_layers[[component]] <- .terra_rrm_apply_raster_values(
        stand_age_layers[[component]],
        mask,
        .terra_rrm_row_lookup_raster(
          result[["VRI_AGE_CL_STD"]],
          result[[sprintf("STAND_A%d", component)]],
          row,
          c("Stand_Age_0-15", "Stand_Age_16-30", "Stand_Age_31-50", "Stand_Age_51-80", "Stand_Age_80+")
        )
      )

      sts_age_layers[[component]] <- .terra_rrm_apply_raster_values(
        sts_age_layers[[component]],
        mask,
        .terra_rrm_row_lookup_raster(
          result[["VRI_AGE_CL_STS"]],
          result[[sprintf("STRCT_S%d", component)]],
          row,
          c("Struct_Age_0-3", "Struct_Age_4-10", "Struct_Age_11-30", "Struct_Age_31-40", "Struct_Age_41-60", "Struct_Age_61-80", "Struct_Age_81-139", "Struct_Age_140-249", "Struct_Age_250+")
        )
      )
    }

    nonforest_mask <- .terra_rrm_rule_value_mask(result, sprintf("FORESTED_%d", component), "N")
    nonforest_or_parkland <- terra::ifel(nonforest_mask[[1]] == 1 | parkland_mask[[1]] == 1, 1, 0)

    strct_values <- terra::ifel(
      result[["VRI_AGE_CL_STS"]] > 0,
      sts_age_layers[[component]],
      terra::ifel(
        nonforest_or_parkland == 1,
        .terra_rrm_translate_layer_codes(result[[sprintf("STS_CLIMAX_%d", component)]], result[[sprintf("STRCT_S%d", component)]]),
        NA
      )
    )
    result <- .terra_rrm_replace_layer(result, sprintf("STRCT_S%d", component), strct_values)

    if (all(c("BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4") %in% names(result))) {
      shrub_mask <- .terra_rrm_non_missing_mask(result)
      shrub_mask <- .terra_rrm_rule_mask(shrub_mask, .terra_rrm_match_exact_value(result, beumc_layer, "WL"))
      shrub_mask <- .terra_rrm_rule_mask(shrub_mask, terra::ifel(.terra_rrm_rule_value_mask(result, "BCLCS_LV_2", "W")[[1]] == 0, 1, NA))
      shrub_mask <- .terra_rrm_rule_mask(shrub_mask, .terra_rrm_match_exact_value(result, "BCLCS_LV_3", "W"))
      shrub_mask <- .terra_rrm_rule_mask(shrub_mask, .terra_rrm_rule_value_mask(result, "BCLCS_LV_4", "HE,HF,HG"))
      result <- .terra_rrm_apply_constant(result, sprintf("STRCT_S%d", component), shrub_mask, .terra_rrm_resolve_output_value(result, sprintf("STRCT_S%d", component), "2"))
    }

    high_mask <- .terra_rrm_match_label_prefix(result, sprintf("STRCT_S%d", component), "^[4567]")
    low_mask <- terra::ifel(
      is.na(result[[sprintf("STRCT_S%d", component)]]),
      1,
      .terra_rrm_match_label_prefix(result, sprintf("STRCT_S%d", component), "^[123]")
    )

    stand_values <- terra::ifel(
      high_mask[[1]] == 1,
      terra::ifel(
        !is.na(result[["STD_VRI"]]),
        .terra_rrm_translate_layer_codes(result[["STD_VRI"]], result[[sprintf("STAND_A%d", component)]]),
        terra::ifel(
          result[["VRI_AGE_CL_STD"]] > 0,
          stand_age_layers[[component]],
          terra::ifel(
            nonforest_or_parkland == 1,
            .terra_rrm_translate_layer_codes(result[[sprintf("STAND_CLIMAX_%d", component)]], result[[sprintf("STAND_A%d", component)]]),
            NA
          )
        )
      ),
      terra::ifel(
        low_mask[[1]] == 1,
        terra::ifel(
          result[["VRI_AGE_CL_STD"]] > 0,
          stand_age_layers[[component]],
          terra::ifel(
            nonforest_or_parkland == 1,
            .terra_rrm_translate_layer_codes(result[[sprintf("STAND_CLIMAX_%d", component)]], result[[sprintf("STAND_A%d", component)]]),
            NA
          )
        ),
        NA
      )
    )
    result <- .terra_rrm_replace_layer(result, sprintf("STAND_A%d", component), stand_values)
  }

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}