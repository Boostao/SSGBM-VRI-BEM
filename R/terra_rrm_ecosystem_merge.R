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


.terra_rrm_derived_layer_levels <- function(x, layer_name) {
  stopifnot(inherits(x, "SpatRaster"))

  if (identical(layer_name, "STD_VRI")) {
    return(data.frame(value = 1:3, label = c("C", "M", "B"), stringsAsFactors = FALSE))
  }

  if (identical(layer_name, "CROWN_ALL")) {
    return(data.frame(value = 1:4, label = c("VL-L", "M", "H", "VH"), stringsAsFactors = FALSE))
  }

  if (grepl("^STAND_CLIMAX_[1-3]$", layer_name)) {
    component <- sub("^STAND_CLIMAX_", "", layer_name)
    source_layer <- paste0("STAND_A", component)
    if (source_layer %in% names(x)) {
      categories <- terra::cats(x[[source_layer]])[[1]]
      if (!is.null(categories) && ncol(categories) >= 2L) {
        return(data.frame(value = categories[[1]], label = categories[[2]], stringsAsFactors = FALSE))
      }
    }
  }

  if (grepl("^STS_CLIMAX_[1-3]$", layer_name)) {
    component <- sub("^STS_CLIMAX_", "", layer_name)
    source_layer <- paste0("STRCT_S", component)
    if (source_layer %in% names(x)) {
      categories <- terra::cats(x[[source_layer]])[[1]]
      if (!is.null(categories) && ncol(categories) >= 2L) {
        return(data.frame(value = categories[[1]], label = categories[[2]], stringsAsFactors = FALSE))
      }
    }
  }

  NULL
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
    derived_levels <- NULL

    if (layer_name %in% names(raster_conv$bem)) {
      levels(blank) <- data.frame(
        value = raster_conv$bem[[layer_name]][["factor"]],
        label = raster_conv$bem[[layer_name]][["value"]],
        stringsAsFactors = FALSE
      )
      names(blank) <- layer_name
    } else if (layer_name %in% names(raster_conv$vri)) {
      levels(blank) <- data.frame(
        value = raster_conv$vri[[layer_name]][["factor"]],
        label = raster_conv$vri[[layer_name]][["value"]],
        stringsAsFactors = FALSE
      )
      names(blank) <- layer_name
    } else {
      derived_levels <- .terra_rrm_derived_layer_levels(result, layer_name)
      if (!is.null(derived_levels)) {
        levels(blank) <- derived_levels
        names(blank) <- layer_name
      }
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


.terra_rrm_append_layer_labels <- function(x, layer_name, labels) {
  stopifnot(inherits(x, "SpatRaster"))

  if (!layer_name %in% names(x)) {
    return(x)
  }

  labels <- unique(trimws(as.character(labels)))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0L) {
    return(x)
  }

  categories <- terra::cats(x[[layer_name]])[[1]]
  if (is.null(categories) || ncol(categories) < 2L) {
    updated <- data.frame(
      value = seq_along(labels),
      label = labels,
      stringsAsFactors = FALSE
    )
    levels(x[[layer_name]]) <- updated
    names(x[[layer_name]]) <- layer_name
    return(x)
  }

  existing_labels <- as.character(categories[[2]])
  missing_labels <- setdiff(labels, existing_labels)
  if (length(missing_labels) == 0L) {
    return(x)
  }

  existing_codes <- suppressWarnings(as.integer(categories[[1]]))
  next_code <- if (length(existing_codes) == 0L || all(is.na(existing_codes))) {
    1L
  } else {
    max(existing_codes, na.rm = TRUE) + 1L
  }

  updated <- rbind(
    data.frame(value = as.integer(categories[[1]]), label = as.character(categories[[2]]), stringsAsFactors = FALSE),
    data.frame(
      value = seq.int(next_code, next_code + length(missing_labels) - 1L),
      label = missing_labels,
      stringsAsFactors = FALSE
    )
  )

  levels(x[[layer_name]]) <- updated
  names(x[[layer_name]]) <- layer_name
  x
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


.terra_rrm_row_pattern_key <- function(row, columns) {
  values <- vapply(
    columns,
    function(column_name) {
      value <- row[[column_name]]
      if (is.na(value) || !nzchar(trimws(as.character(value)))) {
        return("<NA>")
      }
      trimws(as.character(value))
    },
    character(1L)
  )
  paste(values, collapse = "\r")
}

.terra_rrm_available_ecosystem_codes <- function(x) {
  stopifnot(inherits(x, "SpatRaster"))

  key_layers <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE")
  beu_layers <- intersect(grep("^BEUMC_S[1-3]$", names(x), value = TRUE), names(x))
  if (length(beu_layers) == 0L || !all(key_layers %in% names(x))) {
    return(NULL)
  }

  clean_value <- function(value) {
    if (is.factor(value)) {
      value <- as.character(value)
    }
    vapply(value, function(item) {
      item <- trimws(as.character(item))
      if (is.na(item) || !nzchar(item) || identical(item, "0")) return(NA_character_)
      item
    }, character(1L))
  }

  rows <- lapply(beu_layers, function(beu_layer) {
    selected <- c(key_layers, beu_layer)
    df <- terra::as.data.frame(x[[selected]], xy = FALSE, cells = FALSE, na.rm = FALSE)
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }

    names(df)[names(df) == beu_layer] <- "BEU_MC"
    df <- df[, c(key_layers, "BEU_MC"), drop = FALSE]
    for (idx in seq_along(df)) {
      df[[idx]] <- clean_value(df[[idx]])
    }
    df <- df[!is.na(df$BEU_MC), , drop = FALSE]
    if (nrow(df) == 0L) {
      return(NULL)
    }
    df
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(NULL)
  }

  unique(do.call(rbind, rows))
}

terra_rrm_build_ecosystem_key <- function(x,
                                         key_columns = c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC")) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0L))
  }

  if (inherits(x, "data.frame") || inherits(x, "data.table")) {
    dt <- as.data.frame(x, stringsAsFactors = FALSE)
  } else {
    dt <- as.data.frame(x, stringsAsFactors = FALSE)
  }

  key_columns <- intersect(key_columns, names(dt))
  if (length(key_columns) == 0L) {
    return(character(nrow(dt)))
  }

  vapply(seq_len(nrow(dt)), function(row_idx) {
    values <- dt[row_idx, key_columns, drop = FALSE]
    paste(vapply(values, function(value) {
      if (length(value) == 0L || is.null(value) || is.na(value) || !nzchar(trimws(as.character(value)))) {
        return("<NA>")
      }
      trimws(as.character(value))
    }, character(1L)), collapse = "::")
  }, character(1L))
}


.terra_rrm_build_component_ecosystem_key_layer <- function(x, component) {
  stopifnot(inherits(x, "SpatRaster"))

  beumc_layer <- sprintf("BEUMC_S%d", component)
  key_layer <- sprintf("ecosystem_key_s%d", component)
  source_layers <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", beumc_layer)

  if (length(setdiff(source_layers, names(x))) > 0L) {
    stop(
      sprintf(
        "Missing required layers for ecosystem key component %d: %s",
        component,
        paste(setdiff(source_layers, names(x)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  source_values <- as.data.frame(terra::values(x[[source_layers]]), stringsAsFactors = FALSE)
  if (nrow(source_values) == 0L) {
    empty <- .terra_rrm_blank_like(x[[beumc_layer]], key_layer)
    names(empty) <- key_layer
    return(empty)
  }

  decode_layer <- function(layer_name) {
    values <- source_values[[layer_name]]
    labels <- .terra_rrm_lookup_factor_label(x[[layer_name]], values)
    has_labels <- !is.na(labels) & nzchar(trimws(labels))
    out <- ifelse(has_labels, labels, as.character(values))
    out[is.na(values)] <- NA_character_
    out
  }

  key_dt <- data.frame(
    BGC_ZONE = decode_layer("BGC_ZONE"),
    BGC_SUBZON = decode_layer("BGC_SUBZON"),
    BGC_VRT = decode_layer("BGC_VRT"),
    BGC_PHASE = decode_layer("BGC_PHASE"),
    BEU_MC = decode_layer(beumc_layer),
    stringsAsFactors = FALSE
  )

  beu_present <- !is.na(key_dt$BEU_MC) & nzchar(trimws(key_dt$BEU_MC)) & trimws(key_dt$BEU_MC) != "0"
  key_labels <- rep(NA_character_, nrow(key_dt))
  if (any(beu_present)) {
    key_labels[beu_present] <- terra_rrm_build_ecosystem_key(key_dt[beu_present, , drop = FALSE])
  }

  distinct_labels <- unique(key_labels[!is.na(key_labels)])
  key_codes <- match(key_labels, distinct_labels)

  key_raster <- .terra_rrm_blank_like(x[[beumc_layer]], key_layer)
  terra::values(key_raster) <- key_codes
  if (length(distinct_labels) > 0L) {
    levels(key_raster) <- data.frame(
      value = seq_along(distinct_labels),
      label = distinct_labels,
      stringsAsFactors = FALSE
    )
  }
  names(key_raster) <- key_layer
  key_raster
}


.terra_rrm_component_key_categories <- function(x, component) {
  stopifnot(inherits(x, "SpatRaster"))

  key_layer <- sprintf("ecosystem_key_s%d", component)
  if (!key_layer %in% names(x)) {
    stop(sprintf("Missing ecosystem key layer '%s'.", key_layer), call. = FALSE)
  }

  categories <- terra::cats(x[[key_layer]])[[1]]
  if (is.null(categories) || ncol(categories) < 2L) {
    stop(sprintf("Ecosystem key layer '%s' requires category labels.", key_layer), call. = FALSE)
  }

  data.frame(
    key_code = as.numeric(categories[[1]]),
    ecosystem_key = as.character(categories[[2]]),
    stringsAsFactors = FALSE
  )
}


terra_rrm_add_ecosystem_keys <- function(x,
                                         filename = NULL,
                                         overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))

  required_layers <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE")
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(
      sprintf(
        "Missing required layers for terra_rrm_add_ecosystem_keys: %s",
        paste(missing_layers, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  result <- x
  for (component in 1:3) {
    beumc_layer <- sprintf("BEUMC_S%d", component)
    if (!beumc_layer %in% names(result)) {
      next
    }

    key_layer <- sprintf("ecosystem_key_s%d", component)
    key_raster <- .terra_rrm_build_component_ecosystem_key_layer(result, component)
    if (key_layer %in% names(result)) {
      result[[key_layer]] <- key_raster
    } else {
      result <- c(result, key_raster)
    }
  }

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


.terra_rrm_component_lookup_lut <- function(x, unique_ecosystem_dt, component) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(unique_ecosystem_dt))

  key_categories <- .terra_rrm_component_key_categories(x, component)
  lut <- as.data.frame(unique_ecosystem_dt, stringsAsFactors = FALSE)
  if (!"ecosystem_key" %in% names(lut)) {
    lut$ecosystem_key <- terra_rrm_build_ecosystem_key(lut)
  }

  lut$key_code <- key_categories$key_code[match(lut$ecosystem_key, key_categories$ecosystem_key)]
  lut <- lut[!is.na(lut$key_code), , drop = FALSE]
  if (nrow(lut) == 0L) {
    return(lut)
  }

  lut[!duplicated(lut$key_code), , drop = FALSE]
}


.terra_rrm_lookup_by_key <- function(key_raster, target_layer, lookup_keys, lookup_values) {
  stopifnot(inherits(key_raster, "SpatRaster"), inherits(target_layer, "SpatRaster"))

  if (length(lookup_keys) == 0L) {
    blank <- .terra_rrm_blank_like(target_layer, names(target_layer))
    names(blank) <- names(target_layer)
    return(blank)
  }

  result <- suppressWarnings(
    terra::app(
      key_raster,
      fun = function(values) {
        matched_idx <- match(values, lookup_keys)
        out <- lookup_values[matched_idx]
        out[is.na(matched_idx)] <- NA
        out
      }
    )
  )

  names(result) <- names(target_layer)
  result
}


.terra_rrm_lookup_by_key_and_age <- function(key_raster, age_raster, target_layer, lookup_table, multiplier = 100000) {
  stopifnot(inherits(key_raster, "SpatRaster"), inherits(age_raster, "SpatRaster"), inherits(target_layer, "SpatRaster"))

  if (nrow(lookup_table) == 0L) {
    blank <- .terra_rrm_blank_like(target_layer, names(target_layer))
    names(blank) <- names(target_layer)
    return(blank)
  }

  result <- suppressWarnings(
    terra::app(
      c(key_raster, age_raster),
      fun = function(values) {
        key_values <- values[, 1]
        age_values <- values[, 2]
        composite_key <- key_values * multiplier + age_values
        matched_idx <- match(composite_key, lookup_table$composite_key)
        out <- lookup_table$target_code[matched_idx]
        out[is.na(matched_idx)] <- NA
        out[is.na(key_values) | is.na(age_values)] <- NA
        out
      }
    )
  )

  names(result) <- names(target_layer)
  result
}


.terra_rrm_resolve_lookup_codes <- function(x, target_layer_name, values) {
  stopifnot(inherits(x, "SpatRaster"))

  vapply(values, function(value) {
    .terra_rrm_resolve_output_value(x, target_layer_name, value)
  }, numeric(1L))
}


.terra_rrm_build_age_lookup_table <- function(x, component_lut, component, target_prefix, source_columns, age_values) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(component_lut))

  if (nrow(component_lut) == 0L) {
    return(data.frame(composite_key = numeric(0L), target_code = numeric(0L)))
  }

  target_layer_name <- sprintf("%s%d", target_prefix, component)
  resolved <- lapply(source_columns, function(column_name) {
    .terra_rrm_resolve_lookup_codes(x, target_layer_name, component_lut[[column_name]])
  })

  composite_keys <- unlist(lapply(seq_along(source_columns), function(idx) {
    component_lut$key_code * 100000 + age_values[[idx]]
  }), use.names = FALSE)
  target_codes <- unlist(resolved, use.names = FALSE)

  data.frame(
    composite_key = composite_keys,
    target_code = target_codes,
    stringsAsFactors = FALSE
  )
}


.terra_rrm_component_projection_specs <- function(component) {
  list(
    sts = data.frame(
      target = sprintf(
        "STS_%d_Age_%s",
        component,
        c("0_3", "4_10", "11_30", "31_40", "41_60", "61_80", "81_139", "140_249", "gt_249")
      ),
      source = c(
        "Struct_Age_0-3", "Struct_Age_4-10", "Struct_Age_11-30", "Struct_Age_31-40", "Struct_Age_41-60",
        "Struct_Age_61-80", "Struct_Age_81-139", "Struct_Age_140-249", "Struct_Age_250+"
      ),
      stringsAsFactors = FALSE
    ),
    stand = data.frame(
      target = sprintf("STAND_%d_Age_%s", component, c("0_15", "16_30", "31_50", "51_80", "gt_80")),
      source = c("Stand_Age_0-15", "Stand_Age_16-30", "Stand_Age_31-50", "Stand_Age_51-80", "Stand_Age_80+"),
      stringsAsFactors = FALSE
    )
  )
}


.terra_rrm_apply_projection_fields <- function(x, unique_ecosystem_dt, component) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(unique_ecosystem_dt))

  key_layer <- sprintf("ecosystem_key_s%d", component)
  strct_template <- sprintf("STRCT_S%d", component)
  stand_template <- sprintf("STAND_A%d", component)
  if (!all(c(key_layer, strct_template, stand_template) %in% names(x))) {
    return(x)
  }

  specs <- .terra_rrm_component_projection_specs(component)
  result <- x

  # Always materialize projection layers so downstream export contracts hold,
  # even if no LUT rows match this component for the current AOI.
  for (layer_name in c(specs$sts$target, specs$stand$target)) {
    if (!layer_name %in% names(result)) {
      template_layer <- if (grepl("^STS_", layer_name)) strct_template else stand_template
      blank <- .terra_rrm_blank_like(result[[template_layer]], layer_name)
      result <- c(result, blank)
    }
  }

  component_lut <- .terra_rrm_component_lookup_lut(result, unique_ecosystem_dt, component)
  if (nrow(component_lut) == 0L) {
    return(result)
  }

  apply_group <- function(result, spec_dt, template_layer) {
    for (row_idx in seq_len(nrow(spec_dt))) {
      target_layer <- spec_dt$target[[row_idx]]
      source_col <- spec_dt$source[[row_idx]]
      if (!source_col %in% names(component_lut)) {
        next
      }

      result <- .terra_rrm_append_layer_labels(result, target_layer, component_lut[[source_col]])

      resolved_codes <- .terra_rrm_resolve_lookup_codes(result, target_layer, component_lut[[source_col]])
      looked_up <- .terra_rrm_lookup_by_key(
        result[[key_layer]],
        result[[target_layer]],
        component_lut$key_code,
        resolved_codes
      )
      result <- .terra_rrm_replace_layer(result, target_layer, looked_up)
    }

    result
  }

  result <- apply_group(result, specs$sts, strct_template)
  result <- apply_group(result, specs$stand, stand_template)
  result
}


.terra_rrm_apply_static_ecosystem_fields <- function(x, unique_ecosystem_dt) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(unique_ecosystem_dt))

  result <- x
  static_map <- c(
    REALM = "REALM",
    GROUP = "GROUP",
    CLASS = "CLASS",
    KIND = "KIND",
    FORESTED = "Forested (Y/N)",
    STS_CLIMAX = "Strct_Climax",
    STAND_CLIMAX = "Stand_Climax"
  )

  for (component in 1:3) {
    key_layer <- sprintf("ecosystem_key_s%d", component)
    if (!key_layer %in% names(result)) {
      next
    }

    component_lut <- .terra_rrm_component_lookup_lut(result, unique_ecosystem_dt, component)
    if (nrow(component_lut) == 0L) {
      next
    }

    for (field_name in names(static_map)) {
      target_layer <- sprintf("%s_%d", field_name, component)
      if (!target_layer %in% names(result)) {
        next
      }

      resolved_codes <- .terra_rrm_resolve_lookup_codes(result, target_layer, component_lut[[static_map[[field_name]]]])
      looked_up <- .terra_rrm_lookup_by_key(result[[key_layer]], result[[target_layer]], component_lut$key_code, resolved_codes)
      result <- .terra_rrm_replace_layer(result, target_layer, looked_up)
    }

    if (component == 1L && "SNOW_CODE" %in% names(result)) {
      resolved_codes <- .terra_rrm_resolve_lookup_codes(result, "SNOW_CODE", component_lut[["Snow_Code"]])
      looked_up <- .terra_rrm_lookup_by_key(result[[key_layer]], result[["SNOW_CODE"]], component_lut$key_code, resolved_codes)
      result <- .terra_rrm_replace_layer(result, "SNOW_CODE", looked_up)
    }
  }

  result
}

terra_rrm_reduce_unique_ecosystem_dt <- function(unique_ecosystem_dt,
                                                available_codes = NULL) {
  stopifnot(is.data.frame(unique_ecosystem_dt))

  eco <- as.data.frame(unique_ecosystem_dt, stringsAsFactors = FALSE)
  if (nrow(eco) == 0L) {
    eco$ecosystem_key <- character(0)
    return(eco)
  }

  if (!is.null(available_codes)) {
    available_codes <- as.data.frame(available_codes, stringsAsFactors = FALSE)
    if (nrow(available_codes) > 0L) {
      available_codes$BEU_MC <- vapply(
        available_codes[[if ("BEU_MC" %in% names(available_codes)) "BEU_MC" else "BEUMC_S1"]],
        function(value) {
          value <- trimws(as.character(value))
          if (is.na(value) || !nzchar(value) || identical(value, "0")) return(NA_character_)
          value
        },
        character(1L)
      )

      keep <- vapply(seq_len(nrow(eco)), function(row_idx) {
        row <- eco[row_idx, , drop = FALSE]
        row$BGC_ZONE <- if (is.null(row$BGC_ZONE[[1]])) NA_character_ else trimws(as.character(row$BGC_ZONE[[1]]))
        row$BGC_SUBZON <- if (is.null(row$BGC_SUBZON[[1]])) NA_character_ else trimws(as.character(row$BGC_SUBZON[[1]]))
        row$BGC_VRT <- if (is.null(row$BGC_VRT[[1]])) NA_character_ else trimws(as.character(row$BGC_VRT[[1]]))
        row$BGC_PHASE <- if (is.null(row$BGC_PHASE[[1]])) NA_character_ else trimws(as.character(row$BGC_PHASE[[1]]))
        row$BEU_MC <- if (is.null(row$BEU_MC[[1]])) NA_character_ else trimws(as.character(row$BEU_MC[[1]]))
        matches <- which(
          (as.character(available_codes$BGC_ZONE) == row$BGC_ZONE) |
            (is.na(available_codes$BGC_ZONE) & is.na(row$BGC_ZONE)),
          arr.ind = FALSE
        )
        if (length(matches) == 0L) {
          return(FALSE)
        }

        row_match <- vapply(matches, function(match_idx) {
          current <- available_codes[match_idx, , drop = FALSE]
          same_zone <- identical(as.character(current$BGC_ZONE[[1]]), row$BGC_ZONE) || (is.na(current$BGC_ZONE[[1]]) && is.na(row$BGC_ZONE))
          same_subzon <- identical(as.character(current$BGC_SUBZON[[1]]), row$BGC_SUBZON) || (is.na(current$BGC_SUBZON[[1]]) && is.na(row$BGC_SUBZON))
          same_vrt <- identical(as.character(current$BGC_VRT[[1]]), row$BGC_VRT) || (is.na(current$BGC_VRT[[1]]) && is.na(row$BGC_VRT))
          same_phase <- identical(as.character(current$BGC_PHASE[[1]]), row$BGC_PHASE) || (is.na(current$BGC_PHASE[[1]]) && is.na(row$BGC_PHASE))
          same_beu <- identical(as.character(current$BEU_MC[[1]]), row$BEU_MC) || (is.na(current$BEU_MC[[1]]) && is.na(row$BEU_MC))
          isTRUE(same_zone && same_subzon && same_vrt && same_phase && same_beu)
        }, logical(1L))

        any(row_match)
      }, logical(1L))

      eco <- eco[keep, , drop = FALSE]
    }
  }

  key_cols <- c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC")
  key_cols <- intersect(key_cols, names(eco))
  eco$ecosystem_key <- apply(eco[, key_cols, drop = FALSE], 1, function(row_values) {
    paste(vapply(row_values, function(value) {
      if (length(value) == 0L || is.null(value) || is.na(value) || !nzchar(trimws(as.character(value)))) {
        return("<NA>")
      }
      trimws(as.character(value))
    }, character(1L)), collapse = "::")
  })

  eco <- eco[!duplicated(eco$ecosystem_key), , drop = FALSE]
  eco
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

  unique_ecosystem_dt <- terra_rrm_reduce_unique_ecosystem_dt(
    unique_ecosystem_dt,
    available_codes = .terra_rrm_available_ecosystem_codes(x)
  )

  component_key_layers <- vapply(
    intersect(sprintf("BEUMC_S%d", 1:3), names(x)),
    function(layer_name) sprintf("ecosystem_key_s%s", sub("BEUMC_S", "", layer_name)),
    character(1L)
  )
  required_layers <- c(
    "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD",
    component_key_layers
  )
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

  unique_ecosystem_dt <- as.data.frame(unique_ecosystem_dt, stringsAsFactors = FALSE)
  if (!"ecosystem_key" %in% names(unique_ecosystem_dt)) {
    unique_ecosystem_dt$ecosystem_key <- terra_rrm_build_ecosystem_key(unique_ecosystem_dt)
  }

  result <- .terra_rrm_add_missing_layers(
    x,
    c(
      "STD_VRI", "CROWN_ALL", "SNOW_CODE",
      paste0(rep(c("REALM_", "GROUP_", "CLASS_", "KIND_", "FORESTED_", "STS_CLIMAX_", "STAND_CLIMAX_", "STRCT_S", "STAND_A"), each = 3), rep(1:3, times = 9))
    )
  )

  lut_strct_labels <- unique_ecosystem_dt[["Strct_Climax"]]
  lut_stand_labels <- unique_ecosystem_dt[["Stand_Climax"]]
  lut_snow_labels <- unique_ecosystem_dt[["Snow_Code"]]

  for (component in 1:3) {
    result <- .terra_rrm_append_layer_labels(result, sprintf("STS_CLIMAX_%d", component), lut_strct_labels)
    result <- .terra_rrm_append_layer_labels(result, sprintf("STRCT_S%d", component), lut_strct_labels)
    result <- .terra_rrm_append_layer_labels(result, sprintf("STAND_CLIMAX_%d", component), lut_stand_labels)
    result <- .terra_rrm_append_layer_labels(result, sprintf("STAND_A%d", component), lut_stand_labels)
  }
  result <- .terra_rrm_append_layer_labels(result, "SNOW_CODE", lut_snow_labels)

  result <- .terra_rrm_compute_std_vri(result)
  result <- .terra_rrm_compute_crown_all(result)
  result <- .terra_rrm_apply_static_ecosystem_fields(result, unique_ecosystem_dt)

  for (component in 1:3) {
    result <- .terra_rrm_apply_projection_fields(result, unique_ecosystem_dt, component)
  }

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

  for (component in 1:3) {
    beumc_layer <- sprintf("BEUMC_S%d", component)
    if (!beumc_layer %in% names(result)) {
      next
    }
    component_key_layer <- sprintf("ecosystem_key_s%d", component)
    component_lut <- .terra_rrm_component_lookup_lut(result, unique_ecosystem_dt, component)

    stand_age_columns <- c("Stand_Age_0-15", "Stand_Age_16-30", "Stand_Age_31-50", "Stand_Age_51-80", "Stand_Age_80+")
    sts_age_columns <- c("Struct_Age_0-3", "Struct_Age_4-10", "Struct_Age_11-30", "Struct_Age_31-40", "Struct_Age_41-60", "Struct_Age_61-80", "Struct_Age_81-139", "Struct_Age_140-249", "Struct_Age_250+")
    stand_age_lookup <- .terra_rrm_build_age_lookup_table(
      result,
      component_lut,
      component,
      target_prefix = "STAND_A",
      source_columns = stand_age_columns,
      age_values = c(15, 30, 50, 80, 9999)
    )
    sts_age_lookup <- .terra_rrm_build_age_lookup_table(
      result,
      component_lut,
      component,
      target_prefix = "STRCT_S",
      source_columns = sts_age_columns,
      age_values = c(2, 7, 20, 35, 50, 70, 125, 195, 301)
    )

    stand_age_layers[[component]] <- .terra_rrm_lookup_by_key_and_age(
      result[[component_key_layer]],
      result[["VRI_AGE_CL_STD"]],
      result[[sprintf("STAND_A%d", component)]],
      stand_age_lookup
    )

    sts_age_layers[[component]] <- .terra_rrm_lookup_by_key_and_age(
      result[[component_key_layer]],
      result[["VRI_AGE_CL_STS"]],
      result[[sprintf("STRCT_S%d", component)]],
      sts_age_lookup
    )

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