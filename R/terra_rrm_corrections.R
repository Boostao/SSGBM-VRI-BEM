#' Terra correction helpers for the new raster-only RRM path

.terra_rrm_get_raster_conv <- function() {
  if (exists("raster_conv", inherits = TRUE)) {
    return(get("raster_conv", inherits = TRUE))
  }

  search_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  sysdata_path <- NULL

  repeat {
    candidate <- file.path(search_dir, "R", "sysdata.rda")
    if (file.exists(candidate)) {
      sysdata_path <- candidate
      break
    }

    parent_dir <- dirname(search_dir)
    if (identical(parent_dir, search_dir)) {
      break
    }
    search_dir <- parent_dir
  }

  if (is.null(sysdata_path)) {
    stop("Could not find internal raster_conv lookup data.", call. = FALSE)
  }

  lookup_env <- new.env(parent = emptyenv())
  load(sysdata_path, envir = lookup_env)
  lookup_env$raster_conv
}

.terra_rrm_layer_codes <- function(x, layer_name, labels, factor_conv_list = NULL, strict = TRUE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(layer_name %in% names(x))

  categories <- terra::cats(x[[layer_name]])[[1]]
  matched <- rep(NA, length(labels))

  if (!is.null(categories) && ncol(categories) >= 2L) {
    matched <- categories[[1]][match(labels, categories[[2]])]
  }

  if (!is.null(factor_conv_list) && layer_name %in% names(factor_conv_list)) {
    lookup_dt <- factor_conv_list[[layer_name]]
    missing_idx <- which(is.na(matched) & !is.na(labels))
    if (length(missing_idx) > 0L) {
      matched[missing_idx] <- lookup_dt[["factor"]][match(labels[missing_idx], lookup_dt[["value"]])]
    }
    if (strict && !all(is.na(labels) | !is.na(matched))) {
      missing_labels <- labels[is.na(matched) & !is.na(labels)]
      stop(
        sprintf(
          "Could not find lookup codes for layer '%s': %s",
          layer_name,
          paste(unique(missing_labels), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(unname(matched))
  }

  stop(sprintf("Layer '%s' has no categories and no lookup table was provided.", layer_name), call. = FALSE)
}


.terra_rrm_local_layer_codes <- function(x, layer_name, labels) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(layer_name %in% names(x))

  categories <- terra::cats(x[[layer_name]])[[1]]
  if (is.null(categories) || ncol(categories) < 2L) {
    return(rep(NA, length(labels)))
  }

  unname(categories[[1]][match(labels, categories[[2]])])
}


.terra_rrm_non_missing_mask <- function(x) {
  terra::ifel(!is.na(x[[1]]), 1, NA)
}


.terra_rrm_optional_na_mask <- function(x, layer_name) {
  template <- .terra_rrm_non_missing_mask(x)
  if (!layer_name %in% names(x)) {
    return(template)
  }
  terra::ifel(is.na(template), NA, terra::ifel(is.na(x[[layer_name]]), 1, 0))
}


.terra_rrm_apply_constant <- function(x, layer_name, mask, value) {
  if (!layer_name %in% names(x)) {
    return(x)
  }

  replacement <- terra::ifel(mask[[1]], value, x[[layer_name]])
  names(replacement) <- layer_name
  x[[layer_name]] <- replacement
  x
}


.terra_rrm_apply_values <- function(x, layer_name, mask, values) {
  if (!layer_name %in% names(x)) {
    return(x)
  }

  replacement <- terra::ifel(mask[[1]] == 1, values, x[[layer_name]])
  names(replacement) <- layer_name
  x[[layer_name]] <- replacement
  x
}


.terra_rrm_apply_component_identity <- function(x, component, mask, realm_code = NA, group_code = NA, kind_code = NA) {
  stopifnot(component %in% c(1, 2, 3))

  result <- x
  result <- .terra_rrm_apply_constant(result, sprintf("REALM_%s", component), mask, realm_code)
  result <- .terra_rrm_apply_constant(result, sprintf("GROUP_%s", component), mask, group_code)
  result <- .terra_rrm_apply_constant(result, sprintf("KIND_%s", component), mask, kind_code)
  result
}


.terra_rrm_compare_numeric_rule <- function(layer, rule_value) {
  numeric_values <- trimws(strsplit(rule_value, ",", fixed = TRUE)[[1]])
  numeric_values <- suppressWarnings(as.numeric(numeric_values))
  if (any(is.na(numeric_values))) {
    return(NULL)
  }

  suppressWarnings(
    terra::app(
      layer,
      fun = function(cell_values) {
        ifelse(is.na(cell_values), NA, ifelse(cell_values %in% numeric_values, 1, 0))
      }
    )
  )
}


.terra_rrm_rule_value_mask <- function(x, layer_name, rule_value) {
  base_mask <- .terra_rrm_non_missing_mask(x)
  if (is.na(rule_value) || !nzchar(trimws(as.character(rule_value)))) {
    return(base_mask)
  }

  rule_value <- trimws(as.character(rule_value))
  layer <- x[[layer_name]]
  categories <- terra::cats(layer)[[1]]

  if (identical(layer_name, "SLOPE_MOD") && identical(rule_value, "BLANK")) {
    blank_codes <- .terra_rrm_local_layer_codes(x, layer_name, c("k", "q", "j", "w", "z"))
    matched_mask <- .terra_rrm_match_codes(layer, blank_codes)
    return(.terra_rrm_rule_mask(base_mask, terra::ifel(matched_mask == 0, 1, NA)))
  }

  if (grepl("^CONTAINS", rule_value)) {
    pattern <- trimws(sub("^CONTAINS", "", rule_value))
    if (is.null(categories) || ncol(categories) < 2L) {
      stop(sprintf("CONTAINS rules require categorical layer '%s'.", layer_name), call. = FALSE)
    }
    matched_codes <- categories[[1]][grepl(pattern, categories[[2]], fixed = TRUE)]
    return(.terra_rrm_match_codes(layer, matched_codes))
  }

  if (grepl("^DOES NOT CONTAIN", rule_value)) {
    pattern <- trimws(sub("^DOES NOT CONTAIN", "", rule_value))
    if (is.null(categories) || ncol(categories) < 2L) {
      stop(sprintf("DOES NOT CONTAIN rules require categorical layer '%s'.", layer_name), call. = FALSE)
    }
    matched_codes <- categories[[1]][grepl(pattern, categories[[2]], fixed = TRUE)]
    matched_mask <- .terra_rrm_match_codes(layer, matched_codes)
    return(.terra_rrm_rule_mask(base_mask, terra::ifel(matched_mask == 0, 1, NA)))
  }

  values <- trimws(strsplit(rule_value, ",", fixed = TRUE)[[1]])
  if (!is.null(categories) && ncol(categories) >= 2L) {
    matched_codes <- categories[[1]][match(values, categories[[2]])]
    matched_mask <- .terra_rrm_match_codes(layer, matched_codes)
    if (all(is.na(matched_codes))) {
      numeric_mask <- .terra_rrm_compare_numeric_rule(layer, rule_value)
      if (!is.null(numeric_mask)) {
        return(numeric_mask)
      }
    }
    return(matched_mask)
  }

  raster_conv <- .terra_rrm_get_raster_conv()
  lookup_source <- if (layer_name %in% names(raster_conv$bem)) raster_conv$bem else if (layer_name %in% names(raster_conv$vri)) raster_conv$vri else NULL
  if (!is.null(lookup_source)) {
    matched_codes <- .terra_rrm_layer_codes(x, layer_name, values, lookup_source, strict = FALSE)
    if (any(!is.na(matched_codes))) {
      return(.terra_rrm_match_codes(layer, matched_codes))
    }
  }

  numeric_mask <- .terra_rrm_compare_numeric_rule(layer, rule_value)
  if (!is.null(numeric_mask)) {
    return(numeric_mask)
  }

  .terra_rrm_match_codes(layer, numeric(0))
}


.terra_rrm_sum_species_pct <- function(x, tree_list, species_layers, pct_layers) {
  stopifnot(length(species_layers) == length(pct_layers))

  species_labels <- trimws(strsplit(gsub(" ", "", tree_list, fixed = TRUE), ",", fixed = TRUE)[[1]])
  species_labels <- species_labels[nzchar(species_labels)]
  if (length(species_labels) == 0L) {
    return(terra::ifel(!is.na(x[[1]]), 0, NA))
  }

  total <- terra::ifel(!is.na(x[[1]]), 0, NA)
  for (idx in seq_along(species_layers)) {
    species_layer <- species_layers[[idx]]
    pct_layer <- pct_layers[[idx]]
    if (!species_layer %in% names(x) || !pct_layer %in% names(x)) {
      next
    }

    species_codes <- .terra_rrm_layer_codes(x, species_layer, species_labels, .terra_rrm_get_raster_conv()$vri, strict = FALSE)
    species_mask <- .terra_rrm_match_codes(x[[species_layer]], species_codes)
    contribution <- terra::ifel(
      species_mask == 1,
      terra::ifel(is.na(x[[pct_layer]]), 0, x[[pct_layer]]),
      0
    )
    total <- total + contribution
  }

  names(total) <- names(x[[1]])
  total
}


.terra_rrm_tree_rule_mask <- function(x, tree_list_rule, tree_pct_rule, species_layers, pct_layers) {
  base_mask <- .terra_rrm_non_missing_mask(x)
  if (is.na(tree_list_rule) || !nzchar(trimws(as.character(tree_list_rule)))) {
    return(base_mask)
  }

  tree_list_rule <- trimws(as.character(tree_list_rule))
  if (grepl("<|>", tree_list_rule)) {
    comparison <- regmatches(tree_list_rule, regexpr("<|>", tree_list_rule))
    groups <- strsplit(tree_list_rule, "<|>")[[1]]
    lhs <- .terra_rrm_sum_species_pct(x, trimws(groups[[1]]), species_layers, pct_layers)
    rhs <- .terra_rrm_sum_species_pct(x, trimws(groups[[2]]), species_layers, pct_layers)
    comparison_mask <- if (identical(comparison, "<")) {
      terra::ifel(lhs < rhs, 1, 0)
    } else {
      terra::ifel(lhs > rhs, 1, 0)
    }
    return(.terra_rrm_rule_mask(base_mask, comparison_mask))
  }

  if (is.na(tree_pct_rule) || !nzchar(trimws(as.character(tree_pct_rule)))) {
    return(base_mask)
  }

  sum_pct <- .terra_rrm_sum_species_pct(x, tree_list_rule, species_layers, pct_layers)
  range_bounds <- strsplit(trimws(as.character(tree_pct_rule)), "-", fixed = TRUE)[[1]]
  if (length(range_bounds) != 2L) {
    stop(sprintf("Unsupported tree percentage range '%s'.", tree_pct_rule), call. = FALSE)
  }

  lower <- suppressWarnings(as.numeric(range_bounds[[1]]))
  upper <- suppressWarnings(as.numeric(range_bounds[[2]]))
  if (is.na(lower) || is.na(upper)) {
    stop(sprintf("Unsupported tree percentage range '%s'.", tree_pct_rule), call. = FALSE)
  }

  .terra_rrm_rule_mask(base_mask, terra::ifel(sum_pct >= lower & sum_pct <= upper, 1, 0))
}


.terra_rrm_lookup_factor_label <- function(layer, values) {
  categories <- terra::cats(layer)[[1]]
  if (is.null(categories) || ncol(categories) < 2L) {
    return(rep(NA_character_, length(values)))
  }
  categories[[2]][match(values, categories[[1]])]
}


.terra_rrm_apply_beu_bec_corrections <- function(x, beu_bec) {
  if (is.null(beu_bec) || !is.data.frame(beu_bec)) {
    return(x)
  }
  required_cols <- c("BGC_Subzone", "BEU", "Script_Rule", "Change_to_BEU")
  if (length(setdiff(required_cols, names(beu_bec))) > 0L) {
    return(x)
  }
  required_layers <- c("BGC_ZONE", "BGC_SUBZON", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3")
  if (length(setdiff(required_layers, names(x))) > 0L) {
    return(x)
  }

  valid_lookup <- beu_bec[beu_bec$Script_Rule == "Error" & !is.na(beu_bec$Change_to_BEU), , drop = FALSE]
  if (nrow(valid_lookup) == 0L) {
    return(x)
  }

  lookup_keys <- paste0(valid_lookup$BGC_Subzone, "::", valid_lookup$BEU)
  lookup_values <- trimws(as.character(valid_lookup$Change_to_BEU))

  result <- x
  zone_labels <- .terra_rrm_lookup_factor_label(result[["BGC_ZONE"]], terra::values(result[["BGC_ZONE"]])[, 1])
  subzon_labels <- .terra_rrm_lookup_factor_label(result[["BGC_SUBZON"]], terra::values(result[["BGC_SUBZON"]])[, 1])

  for (component in 1:3) {
    layer_name <- sprintf("BEUMC_S%s", component)
    component_values <- terra::values(result[[layer_name]])[, 1]
    component_labels <- .terra_rrm_lookup_factor_label(result[[layer_name]], component_values)
    merge_keys <- paste0(zone_labels, subzon_labels, "::", component_labels)
    matched_change <- lookup_values[match(merge_keys, lookup_keys)]
    change_codes <- .terra_rrm_layer_codes(result, layer_name, matched_change, .terra_rrm_get_raster_conv()$bem, strict = FALSE)
    changed_mask <- !is.na(change_codes)
    if (any(changed_mask)) {
      updated_values <- component_values
      updated_values[changed_mask] <- change_codes[changed_mask]
      terra::values(result[[layer_name]]) <- updated_values
    }
  }

  result
}


.terra_rrm_merge_duplicate_beumc_after_rules <- function(x) {
  result <- x

  duplicate_12 <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(!is.na(result[["BEUMC_S1"]]) & result[["BEUMC_S1"]] == result[["BEUMC_S2"]], 1, NA)
  )
  result[["SDEC_1"]] <- terra::ifel(duplicate_12[[1]] == 1, result[["SDEC_1"]] + result[["SDEC_2"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_2"]] <- terra::ifel(duplicate_12[[1]] == 1, result[["SDEC_3"]], result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(duplicate_12[[1]] == 1, 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, duplicate_12, list(c(2, 3), c(3, NA)))

  duplicate_13 <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(!is.na(result[["BEUMC_S1"]]) & result[["BEUMC_S1"]] == result[["BEUMC_S3"]], 1, NA)
  )
  result[["SDEC_1"]] <- terra::ifel(duplicate_13[[1]] == 1, result[["SDEC_1"]] + result[["SDEC_3"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_3"]] <- terra::ifel(duplicate_13[[1]] == 1, 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, duplicate_13, list(c(3, NA)))

  result
}


.terra_rrm_expand_presence_mask <- function(layer, buffer_m = 0) {
  stopifnot(inherits(layer, "SpatRaster"), terra::nlyr(layer) == 1L)
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || is.na(buffer_m) || buffer_m < 0) {
    stop("buffer_m must be a single non-missing numeric value >= 0.", call. = FALSE)
  }

  presence_mask <- terra::ifel(!is.na(layer) & layer > 0, 1, NA)
  if (buffer_m == 0) {
    return(presence_mask)
  }

  distance_to_presence <- terra::distance(presence_mask)
  terra::ifel(!is.na(distance_to_presence) & distance_to_presence <= buffer_m, 1, NA)
}


.terra_rrm_apply_river_adjacency_stage <- function(x, rivers_layer = "rivers",
                                                    buffer_m = 0,
                                                    filename = NULL, overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  if (!rivers_layer %in% names(x) || !"SITE_M3A" %in% names(x)) {
    return(x)
  }

  site_a_code <- .terra_rrm_layer_codes(x, "SITE_M3A", "a", .terra_rrm_get_raster_conv()$bem, strict = FALSE)[[1]]
  if (is.na(site_a_code)) {
    return(x)
  }

  river_mask <- .terra_rrm_expand_presence_mask(x[[rivers_layer]], buffer_m = buffer_m)

  result <- .terra_rrm_apply_constant(x, "SITE_M3A", river_mask, site_a_code)

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


.terra_rrm_resolve_output_value <- function(x, layer_name, output_value) {
  if (is.na(output_value) || !nzchar(trimws(as.character(output_value)))) {
    return(NA)
  }

  output_value <- trimws(as.character(output_value))
  categories <- terra::cats(x[[layer_name]])[[1]]
  if (!is.null(categories) && ncol(categories) >= 2L) {
    matched <- categories[[1]][match(output_value, categories[[2]])]
    if (!is.na(matched)) {
      return(unname(matched))
    }
  }

  numeric_value <- suppressWarnings(as.numeric(output_value))
  if (!is.na(numeric_value)) {
    return(numeric_value)
  }

  raster_conv <- .terra_rrm_get_raster_conv()
  lookup_source <- if (layer_name %in% names(raster_conv$bem)) raster_conv$bem else if (layer_name %in% names(raster_conv$vri)) raster_conv$vri else NULL
  if (!is.null(lookup_source)) {
    return(.terra_rrm_layer_codes(x, layer_name, output_value, lookup_source, strict = FALSE)[[1]])
  }

  stop(sprintf("Could not resolve output value '%s' for layer '%s'.", output_value, layer_name), call. = FALSE)
}


#' Terra-native rules-table BEU updates
#'
#' Applies the rules-table workflow as ordered raster masks. This stage supports
#' equality, comma-list membership, `CONTAINS` / `DOES NOT CONTAIN`, the
#' `SLOPE_MOD = BLANK` special case, and tree-rule columns using the same
#' species-percent aggregation logic as the legacy workflow.
#'
#' @param x A `terra::SpatRaster` with aligned VRI/BEM layers.
#' @param rules_dt A rules data frame or an Excel file path.
#' @param filename Optional filename for writing the corrected stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with the supported rules-table updates applied.
terra_rrm_apply_rules <- function(x,
                                  rules_dt,
                                  filename = NULL,
                                  overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))

  if (inherits(rules_dt, "character") && file.exists(rules_dt)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package 'readxl' is required to read rules Excel files. Please install it or pass an in-memory rules table instead.",
        call. = FALSE
      )
    }

    sheet_name <- readxl::excel_sheets(rules_dt)
    sheet_name <- grep("^comb.+script", sheet_name, ignore.case = TRUE, value = TRUE)[1]
    rules_dt <- readxl::read_excel(rules_dt, sheet = sheet_name)
  }

  stopifnot(is.data.frame(rules_dt))
  rules_dt <- as.data.frame(rules_dt, stringsAsFactors = FALSE)
  rule_names <- names(rules_dt)

  input_col <- which(rule_names == "INPUTS")
  output_col <- which(rule_names == "OUTPUTS")
  if (length(input_col) != 1L || length(output_col) != 1L) {
    stop("Rules table must contain exactly one 'INPUTS' column and one 'OUTPUTS' column.", call. = FALSE)
  }

  tree_rule_cd_columns <- grep("^TREE_RL_SP_CD_[0-9]+$", rule_names)
  tree_rule_pct_columns <- grep("^TREE_RL_SP_PCT_[0-9]+$", rule_names)
  tree_rule_columns <- c(tree_rule_cd_columns, tree_rule_pct_columns)

  input_layers <- setdiff(rule_names[(input_col + 1):(output_col - 1)], rule_names[tree_rule_columns])
  missing_layers <- setdiff(input_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(sprintf("Missing required layers for terra_rrm_apply_rules: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
  }

  result <- x
  output_layers <- rule_names[(output_col + 1):length(rule_names)]
  output_layers <- output_layers[nzchar(output_layers)]
  species_layers <- grep("^SPEC_CD_[0-9]+$", names(result), value = TRUE)
  pct_layers <- grep("^SPEC_PCT_[0-9]+$", names(result), value = TRUE)

  for (rule_idx in seq_len(nrow(rules_dt))) {
    row_values <- rules_dt[rule_idx, , drop = FALSE]

    mask <- .terra_rrm_non_missing_mask(result)
    for (layer_name in input_layers) {
      rule_value <- row_values[[layer_name]][[1]]
      if (is.na(rule_value) || !nzchar(trimws(as.character(rule_value)))) {
        next
      }
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_rule_value_mask(result, layer_name, rule_value))
    }

    if (length(tree_rule_cd_columns) > 0L) {
      for (tree_idx in seq_along(tree_rule_cd_columns)) {
        cd_name <- rule_names[[tree_rule_cd_columns[[tree_idx]]]]
        pct_name <- if (tree_idx <= length(tree_rule_pct_columns)) rule_names[[tree_rule_pct_columns[[tree_idx]]]] else NULL
        pct_value <- if (is.null(pct_name)) NA else row_values[[pct_name]][[1]]
        tree_rule_value <- row_values[[cd_name]][[1]]
        if (!is.na(tree_rule_value) && nzchar(trimws(as.character(tree_rule_value)))) {
          tree_mask <- .terra_rrm_tree_rule_mask(result, tree_rule_value, pct_value, species_layers, pct_layers)
          mask <- .terra_rrm_rule_mask(mask, tree_mask)
        }
      }
    }

    assigned_layers <- character(0L)
    for (layer_name in output_layers) {
      output_value <- row_values[[layer_name]][[1]]
      if (is.na(output_value) || !nzchar(trimws(as.character(output_value)))) {
        next
      }

      target_layer <- if (identical(layer_name, "BEUMC")) "BEUMC_S1" else layer_name
      resolved_value <- .terra_rrm_resolve_output_value(result, target_layer, output_value)
      result <- .terra_rrm_apply_constant(result, target_layer, mask, resolved_value)
      assigned_layers <- c(assigned_layers, target_layer)
    }

    if (length(assigned_layers) == 0L) {
      next
    }
  }

  if (all(c("BEUMC_S1", "BEUMC_S2", "SDEC_1", "SDEC_2", "SDEC_3") %in% names(result))) {
    result <- .terra_rrm_merge_duplicate_beumc_after_rules(result)
  }

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


.terra_rrm_lookup_numeric_raster <- function(template, key_raster, lookup_table, key_col, value_col, default_values = NULL) {
  stopifnot(inherits(template, "SpatRaster"))
  stopifnot(inherits(key_raster, "SpatRaster"))

  if (nrow(lookup_table) == 0L) {
    if (is.null(default_values)) {
      return(terra::ifel(!is.na(template[[1]]), NA, NA))
    }
    return(default_values)
  }

  lookup_keys <- lookup_table[[key_col]]
  lookup_values <- lookup_table[[value_col]]

  mapped <- suppressWarnings(
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

  if (!is.null(default_values)) {
    mapped <- terra::ifel(is.na(mapped), default_values, mapped)
  }

  names(mapped) <- names(key_raster)
  mapped
}


.terra_rrm_current_wetland_zone <- function(x, wl_code) {
  base_mask <- .terra_rrm_non_missing_mask(x)

  terra::ifel(
    is.na(base_mask),
    NA,
    terra::ifel(
      x[["BEUMC_S1"]] == wl_code,
      1,
      terra::ifel(
        x[["BEUMC_S2"]] == wl_code & x[["SDEC_2"]] > 0,
        2,
        terra::ifel(x[["BEUMC_S3"]] == wl_code & x[["SDEC_3"]] > 0, 3, 0)
      )
    )
  )
}


.terra_rrm_select_wetland_code <- function(x, curr_beu_code, buc) {
  stopifnot(is.data.frame(buc))

  lookup_cols <- c("Code_Orig", "Code_WL0", "Code_WL1", "Code_WL2", "Code_WL3", "Code_WL4", "Code_WL5", "Code_WL6", "Code_WL7", "Code_WL8", "Code_WL10")
  missing_cols <- setdiff(lookup_cols, names(buc))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Wetland lookup table is missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  result <- terra::ifel(!is.na(curr_beu_code), NA, NA)

  thresholds <- list(
    list(min = 8, max = 14, col = "Code_WL1"),
    list(min = 14, max = 25, col = "Code_WL2"),
    list(min = 25, max = 35, col = "Code_WL3"),
    list(min = 35, max = 45, col = "Code_WL4"),
    list(min = 45, max = 55, col = "Code_WL5"),
    list(min = 55, max = 65, col = "Code_WL6"),
    list(min = 65, max = 75, col = "Code_WL7"),
    list(min = 75, max = 80, col = "Code_WL8")
  )

  for (threshold in thresholds) {
    selected <- .terra_rrm_lookup_numeric_raster(curr_beu_code, curr_beu_code, buc, "Code_Orig", threshold$col, NA)
    result <- terra::ifel(x[["wl_pct"]] >= threshold$min & x[["wl_pct"]] < threshold$max, selected, result)
  }

  selected_wl10 <- .terra_rrm_lookup_numeric_raster(curr_beu_code, curr_beu_code, buc, "Code_Orig", "Code_WL10", NA)
  result <- terra::ifel(x[["wl_pct"]] >= 80, selected_wl10, result)
  names(result) <- names(curr_beu_code)
  result
}


#' Terra-native wetland-driven BEM corrections
#'
#' Applies the raster wetland correction workflow on an aligned raster stack:
#' remove remnant third-component `WL`, compute BEU wetland codes from `wl_pct`,
#' apply the wetland component transitions, and merge generic `WL` with more
#' specific wetland mapcodes where both are present.
#'
#' @param x A `terra::SpatRaster` containing aligned BEM layers and `wl_pct`.
#' @param buc A data frame with `Code_Orig` and `Code_WL*` columns.
#' @param filename Optional filename for writing the corrected stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with the first wetland-correction stage applied.
terra_rrm_correct_bem_from_wetlands <- function(x,
                                                buc,
                                                filename = NULL,
                                                overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(buc))

  required_layers <- c(
    "wl_pct", "SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3"
  )
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(
      sprintf("Missing required layers for terra_rrm_correct_bem_from_wetlands: %s", paste(missing_layers, collapse = ", ")),
      call. = FALSE
    )
  }

  result <- x
  raster_conv <- .terra_rrm_get_raster_conv()
  wl_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "WL", raster_conv$bem, strict = FALSE)[[1]]
  if (is.na(wl_code)) {
    if (is.null(filename)) {
      return(result)
    }
    terra::writeRaster(result, filename = filename, overwrite = overwrite)
    return(terra::rast(filename))
  }

  # Remove WL in component 3 before deriving BEU wetland codes.
  wl3_mask <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(result[["SDEC_3"]] > 0, 1, NA),
    terra::ifel(result[["BEUMC_S3"]] == wl_code, 1, NA)
  )
  result[["SDEC_1"]] <- terra::ifel(wl3_mask[[1]] == 1, result[["SDEC_1"]] + result[["SDEC_3"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_3"]] <- terra::ifel(wl3_mask[[1]] == 1, 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, wl3_mask, list(c(3, NA)))

  curr_wl_zone <- .terra_rrm_current_wetland_zone(result, wl_code)
  curr_beu_code <- terra::app(
    c(result[["SDEC_1"]], result[["SDEC_2"]], result[["SDEC_3"]], curr_wl_zone),
    fun = function(values) {
      ifelse(is.na(values[, 1]), NA, values[, 1] * 1000 + values[, 2] * 100 + values[, 3] * 10 + values[, 4])
    }
  )
  names(curr_beu_code) <- "curr_beu_code"

  protected_codes <- .terra_rrm_layer_codes(
    result,
    "BEUMC_S1",
    c("BB", "CB", "CR", "ER", "PB", "PR", "RR", "RS", "SK", "SR", "TF", "WG", "WR", "YB", "YS", "BG", "ES", "FE", "ME", "MR", "SC", "SH", "ST", "SW", "WL", "FS", "IM", "IN", "LL", "LS", "RE", "SP", "OW"),
    raster_conv$bem,
    strict = FALSE
  )
  eligible_mask <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(.terra_rrm_match_codes(result[["BEUMC_S1"]], protected_codes) == 0 | result[["SDEC_1"]] != 10, 1, NA),
    terra::ifel(result[["wl_pct"]] >= 8, 1, NA)
  )

  new_beu_code <- .terra_rrm_select_wetland_code(result, curr_beu_code, buc)
  new_beu_code <- terra::ifel(eligible_mask[[1]] == 1, new_beu_code, NA)
  new_wl_zone <- terra::ifel(is.na(new_beu_code), NA, new_beu_code %% 10)
  names(new_wl_zone) <- "new_wl_zone"

  result[["SDEC_1"]] <- terra::ifel(is.na(new_beu_code) | curr_beu_code == new_beu_code, result[["SDEC_1"]], floor(new_beu_code / 1000))
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_2"]] <- terra::ifel(is.na(new_beu_code) | curr_beu_code == new_beu_code, result[["SDEC_2"]], floor((new_beu_code %% 1000) / 100))
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(is.na(new_beu_code) | curr_beu_code == new_beu_code, result[["SDEC_3"]], floor((new_beu_code %% 100) / 10))
  names(result[["SDEC_3"]]) <- "SDEC_3"

  wetland_feature_codes <- .terra_rrm_layer_codes(
    result,
    "BEUMC_S1",
    c("BB", "CB", "CR", "ER", "PB", "PR", "RR", "RS", "SK", "SR", "TF", "WG", "WR", "YB", "YS", "BG", "ES", "FE", "ME", "MR", "SC", "SH", "ST", "SW"),
    raster_conv$bem,
    strict = FALSE
  )
  has_specific_wetland <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(.terra_rrm_match_codes(result[["BEUMC_S1"]], wetland_feature_codes) == 1 |
      .terra_rrm_match_codes(result[["BEUMC_S2"]], wetland_feature_codes) == 1, 1, NA)
  )
  realm_w <- .terra_rrm_layer_codes(result, "REALM_1", "W", raster_conv$bem, strict = FALSE)[[1]]
  group_w <- .terra_rrm_layer_codes(result, "GROUP_1", "W", raster_conv$bem, strict = FALSE)[[1]]
  kind_u <- .terra_rrm_layer_codes(result, "KIND_1", "U", raster_conv$bem, strict = FALSE)[[1]]

  mask_0_to_1 <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(curr_wl_zone == 0, 1, NA),
    terra::ifel(new_wl_zone == 1, 1, NA),
    terra::ifel(has_specific_wetland == 0, 1, NA)
  )
  result <- .terra_rrm_shift_component_fields(result, mask_0_to_1, list(c(2, 1), c(3, 2)))
  result <- .terra_rrm_apply_constant(result, "BEUMC_S1", mask_0_to_1, wl_code)
  result <- .terra_rrm_apply_component_identity(result, 1, mask_0_to_1, realm_w, group_w, kind_u)

  mask_0_to_2 <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(curr_wl_zone == 0, 1, NA),
    terra::ifel(new_wl_zone == 2, 1, NA)
  )
  mask_0_to_2_move3 <- .terra_rrm_rule_mask(
    mask_0_to_2,
    terra::ifel(result[["SDEC_3"]] > 0, 1, NA),
    terra::ifel(.terra_rrm_match_codes(result[["BEUMC_S2"]], wetland_feature_codes) == 0, 1, NA)
  )
  result <- .terra_rrm_shift_component_fields(result, mask_0_to_2_move3, list(c(3, 2)))
  result <- .terra_rrm_apply_constant(result, "BEUMC_S2", mask_0_to_2, wl_code)
  result <- .terra_rrm_apply_component_identity(result, 2, mask_0_to_2, realm_w, group_w, kind_u)
  result <- .terra_rrm_apply_constant(result, "BEUMC_S3", .terra_rrm_rule_mask(mask_0_to_2, terra::ifel(result[["SDEC_3"]] == 0, 1, NA)), NA)

  mask_1_to_0 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 1, 1, NA), terra::ifel(new_wl_zone == 0, 1, NA))
  result <- .terra_rrm_shift_component_fields(result, mask_1_to_0, list(c(1, 2), c(2, 3), c(3, NA)))

  mask_2_to_0 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 2, 1, NA), terra::ifel(new_wl_zone == 0, 1, NA))
  result <- .terra_rrm_shift_component_fields(result, mask_2_to_0, list(c(2, 3), c(3, NA)))

  mask_3_to_0 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 3, 1, NA), terra::ifel(new_wl_zone == 0, 1, NA))
  result <- .terra_rrm_shift_component_fields(result, mask_3_to_0, list(c(3, NA)))

  mask_0_to_3 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 0, 1, NA), terra::ifel(new_wl_zone == 3, 1, NA))
  result <- .terra_rrm_shift_component_fields(result, mask_0_to_3, list(c(3, NA)))
  result <- .terra_rrm_apply_constant(result, "BEUMC_S3", mask_0_to_3, wl_code)
  result <- .terra_rrm_apply_component_identity(result, 3, mask_0_to_3, realm_w, group_w, kind_u)

  mask_2_to_1 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 2, 1, NA), terra::ifel(new_wl_zone == 1, 1, NA))
  mask_1_to_2 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 1, 1, NA), terra::ifel(new_wl_zone == 2, 1, NA), terra::ifel(!is.na(result[["BEUMC_S2"]]), 1, NA))
  result <- .terra_rrm_shift_component_fields(result, terra::ifel(mask_2_to_1[[1]] == 1 | mask_1_to_2[[1]] == 1, 1, NA), list(c(1, 2), c(2, 1)))

  mask_1_to_3 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 1, 1, NA), terra::ifel(new_wl_zone == 3, 1, NA))
  mask_3_to_1 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 3, 1, NA), terra::ifel(new_wl_zone == 1, 1, NA))
  result <- .terra_rrm_shift_component_fields(result, terra::ifel(mask_1_to_3[[1]] == 1 | mask_3_to_1[[1]] == 1, 1, NA), list(c(1, 3), c(3, 1)))

  mask_2_to_3 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 2, 1, NA), terra::ifel(new_wl_zone == 3, 1, NA), terra::ifel(!is.na(result[["BEUMC_S3"]]), 1, NA))
  mask_3_to_2 <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(curr_wl_zone == 3, 1, NA), terra::ifel(new_wl_zone == 2, 1, NA))
  result <- .terra_rrm_shift_component_fields(result, terra::ifel(mask_2_to_3[[1]] == 1 | mask_3_to_2[[1]] == 1, 1, NA), list(c(2, 3), c(3, 2)))

  # Combine generic WL into a more specific wetland code when both are present.
  specific_wetland_s1 <- .terra_rrm_match_codes(result[["BEUMC_S1"]], wetland_feature_codes)
  specific_wetland_s2 <- .terra_rrm_match_codes(result[["BEUMC_S2"]], wetland_feature_codes)
  mask_s1_wl_s2_specific <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(result[["BEUMC_S1"]] == wl_code, 1, NA), terra::ifel(specific_wetland_s2 == 1, 1, NA))
  result[["SDEC_1"]] <- terra::ifel(mask_s1_wl_s2_specific[[1]] == 1, result[["SDEC_1"]] + result[["SDEC_2"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_2"]] <- terra::ifel(mask_s1_wl_s2_specific[[1]] == 1, result[["SDEC_3"]], result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(mask_s1_wl_s2_specific[[1]] == 1, 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, mask_s1_wl_s2_specific, list(c(1, 2), c(2, 3), c(3, NA)))

  mask_s2_wl_s1_specific <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(result[["BEUMC_S2"]] == wl_code, 1, NA), terra::ifel(specific_wetland_s1 == 1, 1, NA))
  result[["SDEC_1"]] <- terra::ifel(mask_s2_wl_s1_specific[[1]] == 1, result[["SDEC_1"]] + result[["SDEC_2"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_2"]] <- terra::ifel(mask_s2_wl_s1_specific[[1]] == 1, result[["SDEC_3"]], result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(mask_s2_wl_s1_specific[[1]] == 1, 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, mask_s2_wl_s1_specific, list(c(2, 3), c(3, NA)))

  mask_s3_wl_specific <- .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(result[["BEUMC_S3"]] == wl_code, 1, NA), terra::ifel(specific_wetland_s1 == 1 | (specific_wetland_s2 == 1 & specific_wetland_s1 == 0), 1, NA))
  result[["SDEC_1"]] <- terra::ifel(mask_s3_wl_specific[[1]] == 1 & specific_wetland_s1 == 1, result[["SDEC_1"]] + result[["SDEC_3"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_2"]] <- terra::ifel(mask_s3_wl_specific[[1]] == 1 & specific_wetland_s1 == 0 & specific_wetland_s2 == 1, result[["SDEC_2"]] + result[["SDEC_3"]], result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(mask_s3_wl_specific[[1]] == 1, 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, mask_s3_wl_specific, list(c(3, NA)))

  result <- .terra_rrm_apply_constant(result, "BEUMC_S2", .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(result[["SDEC_2"]] == 0, 1, NA)), NA)
  result <- .terra_rrm_apply_constant(result, "BEUMC_S3", .terra_rrm_rule_mask(.terra_rrm_non_missing_mask(result), terra::ifel(result[["SDEC_3"]] == 0, 1, NA)), NA)

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


#' Terra-native riparian wetland adjustment stage
#'
#' Applies the riparian reassignment that follows the wetland corrections in the
#' legacy workflow. Cells with `SITE_M3A = 'a'`, slope below 10, a vegetated
#' primary component, and a supported `BGC_ZONE` are reassigned to the mapped
#' riparian BEUMC and reduced to a single 10-decile component.
#'
#' @param x A `terra::SpatRaster` with aligned BEM layers.
#' @param filename Optional filename for writing the corrected stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with riparian mapcode corrections applied.
terra_rrm_correct_bem_from_wetlands_riparian_stage <- function(x,
                                                               filename = NULL,
                                                               overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))

  required_layers <- c("SITE_M3A", "MEAN_SLOPE", "BGC_ZONE", "BEUMC_S1", "SDEC_1", "SDEC_2", "SDEC_3")
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(
      sprintf("Missing required layers for terra_rrm_correct_bem_from_wetlands_riparian_stage: %s", paste(missing_layers, collapse = ", ")),
      call. = FALSE
    )
  }

  result <- x
  raster_conv <- .terra_rrm_get_raster_conv()

  non_veg_codes <- .terra_rrm_local_layer_codes(
    result,
    "BEUMC_S1",
    c("RI", "WL", "BB", "UR", "OW", "LS", "LL", "RE", "CL", "GB", "GL", "GP", "MI", "RO", "TA", "TC", "TR", "UV", "BG", "CB", "FE", "MR", "PB", "RS", "SH", "SK", "SW", "WG", "TF", "YB", "YS", "AU", "AV", "ES", "IM", "ME", "OV", "RM", "SC", "SM", "ST")
  )

  riparian_mapcode_dt <- data.frame(
    bgc_zone = c("CDF", "BWBS", "SWB", "ESSF", "ICH", "CWH", "SBPS", "SBS"),
    beumc_s1 = c("CR", "PR", "PR", "ER", "RR", "SR", "WR", "WR")
  )

  bgc_zone_codes <- .terra_rrm_local_layer_codes(result, "BGC_ZONE", riparian_mapcode_dt$bgc_zone)
  target_beumc_codes <- .terra_rrm_local_layer_codes(result, "BEUMC_S1", riparian_mapcode_dt$beumc_s1)
  riparian_lookup <- data.frame(zone_code = bgc_zone_codes, target_code = target_beumc_codes)
  riparian_lookup <- riparian_lookup[!is.na(riparian_lookup$zone_code) & !is.na(riparian_lookup$target_code), , drop = FALSE]

  site_a_code <- .terra_rrm_layer_codes(result, "SITE_M3A", "a", raster_conv$bem, strict = FALSE)[[1]]
  target_beumc_raster <- .terra_rrm_lookup_numeric_raster(result[["BGC_ZONE"]], result[["BGC_ZONE"]], riparian_lookup, "zone_code", "target_code")

  riparian_mask <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(result[["SITE_M3A"]] == site_a_code, 1, NA),
    terra::ifel(result[["MEAN_SLOPE"]] < 10, 1, NA),
    terra::ifel(.terra_rrm_match_codes(result[["BEUMC_S1"]], non_veg_codes) == 0, 1, NA),
    terra::ifel(!is.na(target_beumc_raster), 1, NA)
  )

  result <- .terra_rrm_apply_values(result, "BEUMC_S1", riparian_mask, target_beumc_raster)
  result <- .terra_rrm_apply_constant(result, "SDEC_1", riparian_mask, 10)
  result <- .terra_rrm_apply_constant(result, "SDEC_2", riparian_mask, 0)
  result <- .terra_rrm_apply_constant(result, "SDEC_3", riparian_mask, 0)
  result <- .terra_rrm_shift_component_fields(result, riparian_mask, list(c(2, NA), c(3, NA)))

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


.terra_rrm_match_codes <- function(layer, codes) {
  valid_codes <- unique(stats::na.omit(codes))
  if (length(valid_codes) == 0L) {
    return(terra::ifel(!is.na(layer), 0, NA))
  }

  matched <- suppressWarnings(
    terra::app(
      layer,
      fun = function(values) {
        ifelse(is.na(values), NA, ifelse(values %in% valid_codes, 1, 0))
      }
    )
  )

  names(matched) <- names(layer)
  matched
}


.terra_rrm_blank_eco_fields <- function(x, mask) {
  groups <- .terra_rrm_component_field_groups()
  char_fields <- c(
    groups$character_1,
    sub("1", "2", groups$character_1),
    sub("1", "3", groups$character_1)
  )
  char_fields <- setdiff(char_fields, c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3"))

  int_fields <- c(
    groups$integer_1,
    sub("1", "2", groups$integer_1),
    sub("1", "3", groups$integer_1)
  )

  result <- x
  for (layer_name in c(char_fields, int_fields)) {
    if (!layer_name %in% names(result)) {
      next
    }
    replacement <- terra::ifel(mask[[1]], NA, result[[layer_name]])
    names(replacement) <- layer_name
    result[[layer_name]] <- replacement
  }

  result <- .terra_rrm_apply_constant(result, "SDEC_2", mask, 0)
  result <- .terra_rrm_apply_constant(result, "SDEC_3", mask, 0)
  result
}


.terra_rrm_rule_mask <- function(base_mask, ...) {
  masks <- list(...)
  combined <- base_mask
  if (length(masks) == 0L) {
    return(combined)
  }

  for (mask in masks) {
    normalized_mask <- terra::ifel(is.na(base_mask), NA, terra::ifel(is.na(mask), 0, mask))
    combined <- combined * normalized_mask
  }

  terra::ifel(is.na(base_mask), NA, terra::ifel(combined == 1, 1, 0))
}


.terra_rrm_apply_primary_beumc_rule <- function(x,
                                                row_updated,
                                                blank_eco,
                                                mask,
                                                beumc_code,
                                                blank_components = TRUE,
                                                set_sdec1 = 10,
                                                set_sdec2 = NULL,
                                                set_sdec3 = NULL) {
  result <- x
  result <- .terra_rrm_apply_constant(result, "SDEC_1", mask, set_sdec1)
  result <- .terra_rrm_apply_constant(result, "BEUMC_S1", mask, beumc_code)

  if (!is.null(set_sdec2)) {
    result <- .terra_rrm_apply_constant(result, "SDEC_2", mask, set_sdec2)
  }
  if (!is.null(set_sdec3)) {
    result <- .terra_rrm_apply_constant(result, "SDEC_3", mask, set_sdec3)
  }

  row_updated <- terra::ifel(mask[[1]], 1, row_updated)
  if (blank_components) {
    blank_eco <- terra::ifel(mask[[1]], 1, blank_eco)
  }

  list(x = result, row_updated = row_updated, blank_eco = blank_eco)
}


.terra_rrm_combine_duplicate_beumc <- function(x, use_ifelse = TRUE) {
  smpl_type_na <- .terra_rrm_optional_na_mask(x, "SMPL_TYPE")
  duplicate_mask <- .terra_rrm_rule_mask(
    smpl_type_na,
    terra::ifel(!is.na(x[["BEUMC_S1"]]) & x[["BEUMC_S1"]] == x[["BEUMC_S2"]], 1, NA)
  )

  result <- x
  result[["SDEC_1"]] <- terra::ifel(duplicate_mask[[1]], x[["SDEC_1"]] + x[["SDEC_2"]], x[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result[["SDEC_2"]] <- terra::ifel(duplicate_mask[[1]], x[["SDEC_3"]], x[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(duplicate_mask[[1]], 0, x[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, duplicate_mask, list(c(2, 3), c(3, NA)))

  row_updated <- terra::ifel(is.na(duplicate_mask), NA, terra::ifel(duplicate_mask[[1]] == 1, as.integer(use_ifelse), 0))
  blank_eco <- terra::ifel(!is.na(x[[1]]), 0, NA)

  list(x = result, row_updated = row_updated, blank_eco = blank_eco)
}


.terra_rrm_remove_inadequate_wetlands <- function(x, row_updated) {
  raster_conv <- .terra_rrm_get_raster_conv()
  treed_codes <- .terra_rrm_layer_codes(x, "BCLCS_LV_4", c("TB", "TC", "TM"), raster_conv$vri)
  wl_codes <- .terra_rrm_layer_codes(x, "BEUMC_S1", c("WL"), raster_conv$bem)
  wl_code <- wl_codes[[1]]
  smpl_type_na <- .terra_rrm_optional_na_mask(x, "SMPL_TYPE")
  not_updated <- terra::ifel(is.na(row_updated), NA, terra::ifel(row_updated == 0, 1, 0))
  treed_mask <- .terra_rrm_match_codes(x[["BCLCS_LV_4"]], treed_codes)

  result <- x

  mask_wl3 <- .terra_rrm_rule_mask(
    smpl_type_na,
    treed_mask,
    terra::ifel(x[["BEUMC_S3"]] == wl_code, 1, NA),
    not_updated
  )
  result[["SDEC_2"]] <- terra::ifel(mask_wl3[[1]], result[["SDEC_2"]] + result[["SDEC_3"]], result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(mask_wl3[[1]], 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  result <- .terra_rrm_shift_component_fields(result, mask_wl3, list(c(3, NA)))
  row_updated <- terra::ifel(mask_wl3[[1]], 1, row_updated)

  not_updated <- terra::ifel(is.na(row_updated), NA, terra::ifel(row_updated == 0, 1, 0))
  mask_wl2_from3 <- .terra_rrm_rule_mask(
    smpl_type_na,
    treed_mask,
    terra::ifel(result[["BEUMC_S2"]] == wl_code, 1, NA),
    terra::ifel(result[["SDEC_3"]] > 0, 1, NA),
    not_updated
  )
  result <- .terra_rrm_shift_component_fields(result, mask_wl2_from3, list(c(2, 3), c(3, NA)))
  result[["SDEC_2"]] <- terra::ifel(mask_wl2_from3[[1]], result[["SDEC_2"]] + result[["SDEC_3"]], result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  result[["SDEC_3"]] <- terra::ifel(mask_wl2_from3[[1]], 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  row_updated <- terra::ifel(mask_wl2_from3[[1]], 1, row_updated)

  not_updated <- terra::ifel(is.na(row_updated), NA, terra::ifel(row_updated == 0, 1, 0))
  mask_wl2_to1 <- .terra_rrm_rule_mask(
    smpl_type_na,
    treed_mask,
    terra::ifel(result[["BEUMC_S2"]] == wl_code, 1, NA),
    terra::ifel(is.na(result[["SDEC_3"]]) | result[["SDEC_3"]] == 0, 1, NA),
    not_updated
  )
  result[["SDEC_1"]] <- terra::ifel(mask_wl2_to1[[1]], result[["SDEC_1"]] + result[["SDEC_2"]], result[["SDEC_1"]])
  names(result[["SDEC_1"]]) <- "SDEC_1"
  result <- .terra_rrm_shift_component_fields(result, mask_wl2_to1, list(c(2, NA)))
  result[["SDEC_2"]] <- terra::ifel(mask_wl2_to1[[1]], 0, result[["SDEC_2"]])
  names(result[["SDEC_2"]]) <- "SDEC_2"
  row_updated <- terra::ifel(mask_wl2_to1[[1]], 1, row_updated)

  not_updated <- terra::ifel(is.na(row_updated), NA, terra::ifel(row_updated == 0, 1, 0))
  mask_wl1_from2 <- .terra_rrm_rule_mask(
    smpl_type_na,
    treed_mask,
    terra::ifel(result[["BEUMC_S1"]] == wl_code, 1, NA),
    terra::ifel(result[["SDEC_2"]] > 0, 1, NA),
    not_updated
  )
  result <- .terra_rrm_shift_component_fields(result, mask_wl1_from2, list(c(1, 2), c(2, 3), c(3, NA)))
  result[["SDEC_3"]] <- terra::ifel(mask_wl1_from2[[1]], 0, result[["SDEC_3"]])
  names(result[["SDEC_3"]]) <- "SDEC_3"
  row_updated <- terra::ifel(mask_wl1_from2[[1]], 1, row_updated)

  list(x = result, row_updated = row_updated)
}


#' Terra-native small-lake correction stage for the raster-only RRM path
#'
#' Uses a rasterized lake-presence layer already aligned with the BEM/VRI stack.
#' Connected lake cells are grouped into patches on disk, their area is
#' measured from the raster geometry, and non-lake cells intersecting those
#' patches are recoded to `OW`, `LS`, or `LL` using the same 10 ha and 60 ha
#' thresholds intended by the polygon workflow.
#'
#' @param x A `terra::SpatRaster` containing aligned VRI/BEM layers and a lake
#'   presence layer.
#' @param lake_layer Name of the raster layer indicating lake presence.
#' @param buffer_m Optional non-negative buffer distance in raster map units.
#'   When `> 0`, classified lake patches are expanded before their recoded
#'   values are applied to the raster stack.
#' @param directions Connectivity passed to [terra::patches()].
#' @param filename Optional filename for writing the corrected stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with lake patches corrected in place.
terra_rrm_correct_small_lakes <- function(x,
                                          lake_layer = "lakes",
                                          buffer_m = 0,
                                          directions = 8,
                                          filename = NULL,
                                          overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))

  required_layers <- c(
    "BEUMC_S1", "SDEC_1", "SDEC_2", "SDEC_3",
    "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_5",
    lake_layer
  )
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(
      sprintf("Missing required layers for terra_rrm_correct_small_lakes: %s", paste(missing_layers, collapse = ", ")),
      call. = FALSE
    )
  }

  raster_conv <- .terra_rrm_get_raster_conv()
  result <- x

  lake_codes <- .terra_rrm_layer_codes(result, "BEUMC_S1", c("OW", "LS", "LL"), raster_conv$bem, strict = FALSE)
  lake_class_mask <- .terra_rrm_match_codes(result[["BEUMC_S1"]], lake_codes)
  lake_mask_seed <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(result[[lake_layer]] > 0, 1, NA),
    terra::ifel(lake_class_mask == 0, 1, NA)
  )

  lake_patches <- terra::patches(terra::ifel(lake_mask_seed[[1]] == 1, 1, NA), directions = directions)
  patch_freq <- terra::freq(lake_patches)

  if (!is.null(patch_freq) && nrow(patch_freq) > 0L) {
    cell_area_ha <- prod(terra::res(result[[1]])) / 10000
    patch_area_ha <- patch_freq$count * cell_area_ha

    ow_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "OW", raster_conv$bem, strict = FALSE)[[1]]
    ls_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "LS", raster_conv$bem, strict = FALSE)[[1]]
    ll_code <- .terra_rrm_layer_codes(result, "BEUMC_S1", "LL", raster_conv$bem, strict = FALSE)[[1]]

    patch_codes <- ifelse(
      patch_area_ha <= 10,
      ow_code,
      ifelse(patch_area_ha <= 60, ls_code, ll_code)
    )

    patch_classes <- terra::subst(lake_patches, from = patch_freq$value, to = patch_codes, others = NA)

    buffered_patch_classes <- patch_classes
    if (buffer_m > 0) {
      for (class_code in c(ow_code, ls_code, ll_code)) {
        class_seed <- terra::ifel(patch_classes == class_code, 1, NA)
        class_mask <- .terra_rrm_expand_presence_mask(class_seed, buffer_m = buffer_m)
        buffered_patch_classes <- terra::ifel(class_mask[[1]] == 1, class_code, buffered_patch_classes)
      }
    }

    lake_mask <- terra::ifel(!is.na(buffered_patch_classes), 1, NA)

    result <- .terra_rrm_apply_values(result, "BEUMC_S1", lake_mask, buffered_patch_classes)
    result <- .terra_rrm_apply_constant(result, "SDEC_1", lake_mask, 10)
    result <- .terra_rrm_apply_constant(result, "SDEC_2", lake_mask, 0)
    result <- .terra_rrm_apply_constant(result, "SDEC_3", lake_mask, 0)
    result <- .terra_rrm_apply_constant(result, "BEUMC_S2", lake_mask, NA)
    result <- .terra_rrm_apply_constant(result, "BEUMC_S3", lake_mask, NA)
    result <- .terra_rrm_apply_constant(result, "SITE_M3A", lake_mask, NA)

    lv1_n <- .terra_rrm_layer_codes(result, "BCLCS_LV_1", "N", raster_conv$vri, strict = FALSE)[[1]]
    lv2_w <- .terra_rrm_layer_codes(result, "BCLCS_LV_2", "W", raster_conv$vri, strict = FALSE)[[1]]
    lv5_la <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", "LA", raster_conv$vri, strict = FALSE)[[1]]
    result <- .terra_rrm_apply_constant(result, "BCLCS_LV_1", lake_mask, lv1_n)
    result <- .terra_rrm_apply_constant(result, "BCLCS_LV_2", lake_mask, lv2_w)
    result <- .terra_rrm_apply_constant(result, "BCLCS_LV_3", lake_mask, NA)
    result <- .terra_rrm_apply_constant(result, "BCLCS_LV_4", lake_mask, NA)
    result <- .terra_rrm_apply_constant(result, "BCLCS_LV_5", lake_mask, lv5_la)

    spec_cd_layers <- grep("^SPEC_CD_", names(result), value = TRUE)
    spec_pct_layers <- grep("^SPEC_PCT_", names(result), value = TRUE)
    for (layer_name in c(spec_cd_layers, spec_pct_layers)) {
      result <- .terra_rrm_apply_constant(result, layer_name, lake_mask, NA)
    }
  }

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


# ---------------------------------------------------------------------------
# Block-native helpers for terra_rrm_correct_bem_from_vri
#
# These functions operate on a plain numeric matrix `m` (rows = raster cells,
# cols = layers) as returned by terra::readValues(..., mat = TRUE).
# They never build lazy terra DAGs and never touch disk.
#
# `idx`   : named integer vector produced by setNames(seq_len(nlyr(x)), names(x))
# `codes` : named list of pre-computed integer codes from raster_conv
# ---------------------------------------------------------------------------

# Test whether cell values belong to a set of codes (NA stays NA).
.blk_match <- function(col, codes) {
  valid <- unique(stats::na.omit(codes))
  if (length(valid) == 0L) {
    return(ifelse(is.na(col), NA_integer_, 0L))
  }
  ifelse(is.na(col), NA_integer_, ifelse(col %in% valid, 1L, 0L))
}

# Intersect two 0/1/NA masks: NA propagates, 0 wins.
.blk_and <- function(a, b) {
  ifelse(is.na(a) | is.na(b), NA_integer_, ifelse(a == 1L & b == 1L, 1L, 0L))
}

# Shift component fields in a block matrix according to shift_pattern.
# shift_pattern is a list of c(to_component, from_component) pairs where
# from_component may be NA (meaning: blank the target).
# `layer_lookup` is the result of .terra_rrm_component_layer_lookup().
.blk_shift_component_fields <- function(m, mask_vec, shift_pattern, layer_lookup, idx) {
  for (pattern in shift_pattern) {
    to_component   <- as.character(pattern[[1]])
    from_component <- pattern[[2]]

    if (is.na(to_component) || !to_component %in% names(layer_lookup)) {
      next
    }

    target_fields <- layer_lookup[[to_component]]
    source_fields <- if (is.na(from_component)) {
      rep(NA_character_, length(target_fields))
    } else {
      layer_lookup[[as.character(from_component)]]
    }

    for (i in seq_along(target_fields)) {
      tgt <- target_fields[[i]]
      src <- source_fields[[i]]
      if (!tgt %in% names(idx)) next

      if (is.na(src) || !src %in% names(idx)) {
        m[mask_vec, idx[[tgt]]] <- NA_real_
      } else {
        m[mask_vec, idx[[tgt]]] <- ifelse(
          is.na(m[mask_vec, idx[[src]]]),
          NA_real_,
          m[mask_vec, idx[[src]]]
        )
      }
    }
  }
  m
}

# Blank eco fields for cells where blank_vec == 1.
# Mirrors .terra_rrm_blank_eco_fields: sets component 1/2/3 eco fields to NA
# (excluding BEUMC_S1/2/3), then sets SDEC_2 and SDEC_3 to 0.
.blk_blank_eco_fields <- function(m, blank_vec, field_groups, idx) {
  mask_vec <- !is.na(blank_vec) & blank_vec == 1L

  char_fields <- c(
    field_groups$character_1,
    sub("1", "2", field_groups$character_1),
    sub("1", "3", field_groups$character_1)
  )
  char_fields <- setdiff(char_fields, c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3"))

  int_fields <- c(
    field_groups$integer_1,
    sub("1", "2", field_groups$integer_1),
    sub("1", "3", field_groups$integer_1)
  )

  for (fld in c(char_fields, int_fields)) {
    if (fld %in% names(idx)) {
      m[mask_vec, idx[[fld]]] <- NA_real_
    }
  }

  if ("SDEC_2" %in% names(idx)) m[mask_vec, idx[["SDEC_2"]]] <- 0
  if ("SDEC_3" %in% names(idx)) m[mask_vec, idx[["SDEC_3"]]] <- 0
  m
}

# Apply a primary BEUMC rule to a block matrix.
# Returns updated list(m, row_updated, blank_eco).
.blk_apply_primary_beumc_rule <- function(m, row_updated, blank_eco, mask_vec,
                                          beumc_code, idx,
                                          blank_components = TRUE,
                                          set_sdec1 = 10L,
                                          set_sdec2 = NULL,
                                          set_sdec3 = NULL) {
  if ("SDEC_1" %in% names(idx))  m[mask_vec, idx[["SDEC_1"]]]  <- set_sdec1
  if ("BEUMC_S1" %in% names(idx)) m[mask_vec, idx[["BEUMC_S1"]]] <- beumc_code
  if (!is.null(set_sdec2) && "SDEC_2" %in% names(idx)) m[mask_vec, idx[["SDEC_2"]]] <- set_sdec2
  if (!is.null(set_sdec3) && "SDEC_3" %in% names(idx)) m[mask_vec, idx[["SDEC_3"]]] <- set_sdec3

  row_updated[mask_vec] <- 1L
  if (blank_components) {
    blank_eco[mask_vec] <- 1L
  }

  list(m = m, row_updated = row_updated, blank_eco = blank_eco)
}

# Combine duplicate BEUMC components (S1 == S2) in a block matrix.
# Returns list(m, row_updated, blank_eco).
.blk_combine_duplicate_beumc <- function(m, idx, layer_lookup, use_ifelse) {
  n <- nrow(m)
  row_updated <- integer(n)
  blank_eco   <- integer(n)

  # NA stays NA for row_updated (cells with no data at all)
  has_data <- !is.na(m[, idx[[names(idx)[1L]]]])
  row_updated[!has_data] <- NA_integer_

  if (!"BEUMC_S1" %in% names(idx) || !"BEUMC_S2" %in% names(idx)) {
    return(list(m = m, row_updated = row_updated, blank_eco = blank_eco))
  }

  s1 <- m[, idx[["BEUMC_S1"]]]
  s2 <- m[, idx[["BEUMC_S2"]]]

  # duplicate mask: S1 == S2 and neither is NA, and cell has data
  dup <- has_data & !is.na(s1) & !is.na(s2) & s1 == s2

  # SMPL_TYPE gate (if present): only process cells where SMPL_TYPE is NA
  if ("SMPL_TYPE" %in% names(idx)) {
    smpl <- m[, idx[["SMPL_TYPE"]]]
    dup <- dup & is.na(smpl)
  }

  if (any(dup)) {
    sdec1 <- if ("SDEC_1" %in% names(idx)) m[, idx[["SDEC_1"]]] else rep(0, n)
    sdec2 <- if ("SDEC_2" %in% names(idx)) m[, idx[["SDEC_2"]]] else rep(0, n)
    sdec3 <- if ("SDEC_3" %in% names(idx)) m[, idx[["SDEC_3"]]] else rep(0, n)

    if ("SDEC_1" %in% names(idx)) m[dup, idx[["SDEC_1"]]] <- sdec1[dup] + sdec2[dup]
    if ("SDEC_2" %in% names(idx)) m[dup, idx[["SDEC_2"]]] <- sdec3[dup]
    if ("SDEC_3" %in% names(idx)) m[dup, idx[["SDEC_3"]]] <- 0

    m <- .blk_shift_component_fields(m, dup, list(c(2, 3), c(3, NA)), layer_lookup, idx)
    row_updated[dup] <- if (isTRUE(use_ifelse)) 1L else 0L
  }

  list(m = m, row_updated = row_updated, blank_eco = blank_eco)
}

# Remove inadequate wetland components in treed units.
# Returns list(m, row_updated).
.blk_remove_inadequate_wetlands <- function(m, row_updated, idx, layer_lookup, codes) {
  has_data <- !is.na(row_updated)
  not_updated <- has_data & !is.na(row_updated) & row_updated == 0L

  treed  <- .blk_match(m[, idx[["BCLCS_LV_4"]]], codes$treed_codes)
  wl_s3  <- !is.na(m[, idx[["BEUMC_S3"]]]) & m[, idx[["BEUMC_S3"]]] == codes$wl_code

  # Gate on SMPL_TYPE if present
  smpl_ok <- if ("SMPL_TYPE" %in% names(idx)) is.na(m[, idx[["SMPL_TYPE"]]]) else rep(TRUE, nrow(m))

  # --- pass 1: WL in S3, treed → blank S3 ---
  mask_wl3 <- smpl_ok & !is.na(treed) & treed == 1L & wl_s3 & not_updated

  if (any(mask_wl3)) {
    sdec2 <- m[, idx[["SDEC_2"]]]; sdec3 <- m[, idx[["SDEC_3"]]]
    m[mask_wl3, idx[["SDEC_2"]]] <- sdec2[mask_wl3] + sdec3[mask_wl3]
    m[mask_wl3, idx[["SDEC_3"]]] <- 0
    m <- .blk_shift_component_fields(m, mask_wl3, list(c(3, NA)), layer_lookup, idx)
    row_updated[mask_wl3] <- 1L
  }

  not_updated <- has_data & !is.na(row_updated) & row_updated == 0L
  wl_s2 <- !is.na(m[, idx[["BEUMC_S2"]]]) & m[, idx[["BEUMC_S2"]]] == codes$wl_code

  # --- pass 2: WL in S2 with non-empty S3, treed → shift S2←S3 ---
  mask_wl2_from3 <- smpl_ok & !is.na(treed) & treed == 1L & wl_s2 &
    !is.na(m[, idx[["SDEC_3"]]]) & m[, idx[["SDEC_3"]]] > 0 & not_updated

  if (any(mask_wl2_from3)) {
    m <- .blk_shift_component_fields(m, mask_wl2_from3, list(c(2, 3), c(3, NA)), layer_lookup, idx)
    sdec2 <- m[, idx[["SDEC_2"]]]; sdec3 <- m[, idx[["SDEC_3"]]]
    m[mask_wl2_from3, idx[["SDEC_2"]]] <- sdec2[mask_wl2_from3] + sdec3[mask_wl2_from3]
    m[mask_wl2_from3, idx[["SDEC_3"]]] <- 0
    row_updated[mask_wl2_from3] <- 1L
  }

  not_updated <- has_data & !is.na(row_updated) & row_updated == 0L
  wl_s2 <- !is.na(m[, idx[["BEUMC_S2"]]]) & m[, idx[["BEUMC_S2"]]] == codes$wl_code

  # --- pass 3: WL in S2 with empty S3, treed → absorb into S1 ---
  mask_wl2_to1 <- smpl_ok & !is.na(treed) & treed == 1L & wl_s2 &
    (is.na(m[, idx[["SDEC_3"]]]) | m[, idx[["SDEC_3"]]] == 0) & not_updated

  if (any(mask_wl2_to1)) {
    sdec1 <- m[, idx[["SDEC_1"]]]; sdec2 <- m[, idx[["SDEC_2"]]]
    m[mask_wl2_to1, idx[["SDEC_1"]]] <- sdec1[mask_wl2_to1] + sdec2[mask_wl2_to1]
    m <- .blk_shift_component_fields(m, mask_wl2_to1, list(c(2, NA)), layer_lookup, idx)
    m[mask_wl2_to1, idx[["SDEC_2"]]] <- 0
    row_updated[mask_wl2_to1] <- 1L
  }

  not_updated <- has_data & !is.na(row_updated) & row_updated == 0L
  wl_s1 <- !is.na(m[, idx[["BEUMC_S1"]]]) & m[, idx[["BEUMC_S1"]]] == codes$wl_code

  # --- pass 4: WL in S1 with non-empty S2, treed → shift S1←S2←S3 ---
  mask_wl1_from2 <- smpl_ok & !is.na(treed) & treed == 1L & wl_s1 &
    !is.na(m[, idx[["SDEC_2"]]]) & m[, idx[["SDEC_2"]]] > 0 & not_updated

  if (any(mask_wl1_from2)) {
    m <- .blk_shift_component_fields(m, mask_wl1_from2, list(c(1, 2), c(2, 3), c(3, NA)), layer_lookup, idx)
    m[mask_wl1_from2, idx[["SDEC_3"]]] <- 0
    row_updated[mask_wl1_from2] <- 1L
  }

  list(m = m, row_updated = row_updated)
}

# Apply all VRI correction rules to one block matrix.
# All parameters are scalars / integer vectors pre-computed outside the block loop.
.blk_apply_vri_rules <- function(m, idx, codes, layer_lookup, field_groups,
                                 clear_site_ma, use_ifelse) {

  # --- initial field clears ---
  if (clear_site_ma) {
    has_data <- !is.na(m[, idx[[names(idx)[1L]]]])
    if ("SITE_M1A" %in% names(idx)) m[has_data, idx[["SITE_M1A"]]] <- NA_real_
    if ("SITE_M2A" %in% names(idx)) m[has_data, idx[["SITE_M2A"]]] <- NA_real_
  }
  if ("SITE_M3A" %in% names(idx)) {
    has_data <- !is.na(m[, idx[[names(idx)[1L]]]])
    m[has_data, idx[["SITE_M3A"]]] <- NA_real_
  }

  # --- combine duplicate BEUMC components ---
  dup_state   <- .blk_combine_duplicate_beumc(m, idx, layer_lookup, use_ifelse)
  m           <- dup_state$m
  row_updated <- dup_state$row_updated
  blank_eco   <- dup_state$blank_eco

  # --- remove inadequate wetland components ---
  wl_state    <- .blk_remove_inadequate_wetlands(m, row_updated, idx, layer_lookup, codes)
  m           <- wl_state$m
  row_updated <- wl_state$row_updated

  # Helper: build the base mask for rule application
  # TRUE where cell has data, SMPL_TYPE is NA (or absent), and not yet updated
  smpl_ok     <- if ("SMPL_TYPE" %in% names(idx)) is.na(m[, idx[["SMPL_TYPE"]]]) else rep(TRUE, nrow(m))
  base_mask   <- function() smpl_ok & !is.na(row_updated) & row_updated == 0L

  # Helper: fire one primary BEUMC rule
  apply_rule  <- function(mask_vec, beumc_code, blank_components = TRUE,
                           set_sdec1 = 10L, set_sdec2 = NULL, set_sdec3 = NULL) {
    if (is.na(beumc_code) || !any(mask_vec)) return(invisible(NULL))
    res <- .blk_apply_primary_beumc_rule(
      m, row_updated, blank_eco, mask_vec, beumc_code, idx,
      blank_components = blank_components,
      set_sdec1 = set_sdec1, set_sdec2 = set_sdec2, set_sdec3 = set_sdec3
    )
    m           <<- res$m
    row_updated <<- res$row_updated
    blank_eco   <<- res$blank_eco
  }

  lv1  <- m[, idx[["BCLCS_LV_1"]]]
  lv2  <- m[, idx[["BCLCS_LV_2"]]]
  lv3  <- m[, idx[["BCLCS_LV_3"]]]
  lv5  <- m[, idx[["BCLCS_LV_5"]]]
  area <- m[, idx[["Area_Ha"]]]

  # lake / reservoir / river rules
  apply_rule(base_mask() & !is.na(lv1) & lv1 == codes$lv1_n & !is.na(lv5) & lv5 == codes$lv5_la & !is.na(area) & area <= 2,    codes$beu_ow)
  apply_rule(base_mask() & !is.na(lv1) & lv1 == codes$lv1_n & !is.na(lv5) & lv5 == codes$lv5_la & !is.na(area) & area > 2 & area <= 60, codes$beu_ls)
  apply_rule(base_mask() & !is.na(lv1) & lv1 == codes$lv1_n & !is.na(lv5) & lv5 == codes$lv5_la & !is.na(area) & area > 60,   codes$beu_ll)
  apply_rule(base_mask() & !is.na(lv1) & lv1 == codes$lv1_n & !is.na(lv5) & lv5 == codes$lv5_re,                              codes$beu_re)
  apply_rule(base_mask() & !is.na(lv1) & lv1 == codes$lv1_n & .blk_match(lv5, codes$lv5_ri_rs) == 1L,                         codes$beu_ri)
  apply_rule(base_mask() & !is.na(lv1) & lv1 == codes$lv1_v & !is.na(lv2) & lv2 == codes$lv2_n &
               !is.na(lv3) & lv3 == codes$lv3_w & !is.na(m[, idx[["AGE_CL_STS"]]]) & m[, idx[["AGE_CL_STS"]]] == -1,         codes$beu_wl)

  # Black spruce
  apply_rule(base_mask() & !is.na(m[, idx[["SPEC_CD_1"]]]) & m[, idx[["SPEC_CD_1"]]] == codes$spec_sb &
               !is.na(m[, idx[["SPEC_PCT_1"]]]) & m[, idx[["SPEC_PCT_1"]]] >= 90,                                              codes$beu_bb)

  # Urban / recreation
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_ap,                                                                  codes$beu_ur)

  # Burned: set SDEC_1=10 and DISTCLS_1=F code without blanking eco, no BEUMC change via apply_rule
  mask_bu <- base_mask() & !is.na(lv5) & lv5 == codes$lv5_bu
  if (any(mask_bu)) {
    if ("SDEC_1" %in% names(idx))    m[mask_bu, idx[["SDEC_1"]]]    <- 10
    if ("DISTCLS_1" %in% names(idx)) m[mask_bu, idx[["DISTCLS_1"]]] <- codes$distcls_f
    row_updated[mask_bu] <- 1L
  }

  # Remaining map-code rules
  apply_rule(base_mask() & .blk_match(m[, idx[["SLOPE_MOD"]]], codes$slope_mod_qz) == 1L,                     codes$beu_cl)
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_gb,                                                 codes$beu_gb)
  apply_rule(base_mask() & .blk_match(lv5, codes$lv5_gl_pn) == 1L,                                            codes$beu_gl)
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_gp,                                                 codes$beu_gp)
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_ll,                                                 codes$beu_ll)
  apply_rule(base_mask() & .blk_match(lv5, codes$lv5_mi) == 1L,                                               codes$beu_mi)
  apply_rule(base_mask() & .blk_match(lv5, codes$lv5_ro) == 1L,                                               codes$beu_ro)
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_ta,                                                 codes$beu_ta)
  apply_rule(base_mask() & .blk_match(lv5, codes$lv5_tc) == 1L,                                               codes$beu_tc)
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_tr,                                                 codes$beu_tr)
  apply_rule(base_mask() & .blk_match(lv5, codes$lv5_uv) == 1L,                                               codes$beu_uv)
  apply_rule(base_mask() & .blk_match(m[, idx[["LAND_CD_1"]]], codes$land_uv) == 1L &
               !is.na(m[, idx[["COV_PCT_1"]]]) & m[, idx[["COV_PCT_1"]]] >= 95,                               codes$beu_uv)
  apply_rule(base_mask() & !is.na(lv5) & lv5 == codes$lv5_ur,                                                 codes$beu_ur)

  # Riparian TC component (treed with rz veg cover)
  mask_tc2 <- base_mask() & !is.na(lv2) & lv2 == codes$lv2_t &
    !is.na(m[, idx[["SDEC_1"]]]) & m[, idx[["SDEC_1"]]] == 10 &
    .blk_match(m[, idx[["LBL_VEGCOV"]]], codes$lbl_rz) == 1L
  if (any(mask_tc2)) {
    if ("SDEC_1"   %in% names(idx)) m[mask_tc2, idx[["SDEC_1"]]]   <- 8
    if ("SDEC_2"   %in% names(idx)) m[mask_tc2, idx[["SDEC_2"]]]   <- 2
    if ("BEUMC_S2" %in% names(idx)) m[mask_tc2, idx[["BEUMC_S2"]]] <- codes$beu_tc
    row_updated[mask_tc2] <- 1L
  }

  # Stand composition updates
  if ("STAND_A1" %in% names(idx)) {
    spec1 <- m[, idx[["SPEC_CD_1"]]]
    pct1  <- m[, idx[["SPEC_PCT_1"]]]

    # deciduous ≥75 % → B
    mask_stand_b <- smpl_ok & .blk_match(spec1, codes$deciduous_codes) == 1L &
      !is.na(pct1) & pct1 >= 75 & .blk_match(m[, idx[["STAND_A1"]]], codes$stand_cm) == 1L
    if (any(mask_stand_b)) m[mask_stand_b, idx[["STAND_A1"]]] <- codes$stand_b

    # deciduous 50–74 % → M
    mask_stand_m <- smpl_ok & .blk_match(spec1, codes$deciduous_codes) == 1L &
      !is.na(pct1) & pct1 >= 50 & pct1 < 75 & .blk_match(m[, idx[["STAND_A1"]]], codes$stand_cb) == 1L
    if (any(mask_stand_m)) m[mask_stand_m, idx[["STAND_A1"]]] <- codes$stand_m

    # conifer ≥75 % → C
    mask_stand_c <- smpl_ok & .blk_match(spec1, codes$conifer_codes) == 1L &
      !is.na(pct1) & pct1 >= 75 & .blk_match(m[, idx[["STAND_A1"]]], codes$stand_m_only) == 1L
    if (any(mask_stand_c)) m[mask_stand_c, idx[["STAND_A1"]]] <- codes$stand_c
  }

  # Blank eco fields for cells that were updated with blank_components = TRUE
  m <- .blk_blank_eco_fields(m, blank_eco, field_groups, idx)

  m
}

# ---------------------------------------------------------------------------
#' Terra-native VRI-driven BEM corrections
#'
#' Builds the ordered on-disk VRI correction stage for the raster-only path.
#' This stage covers duplicate component consolidation, map-code reassignment,
#' removal of inappropriate wetland components in treed units, stand updates,
#' and allowed BEC/BEU correction lookups.
#'
#' All heavy computation runs in a single `terra::blocks()` read/write pass
#' over the raster. No lazy terra DAG is accumulated across rules, which keeps
#' memory usage and I/O proportional to 2× file size regardless of rule count.
#'
#' @param x A `terra::SpatRaster` containing aligned VRI/BEM layers.
#' @param clear_site_ma Logical. If `TRUE`, `SITE_M1A`, `SITE_M2A`, and
#'   `SITE_M3A` are cleared at the start of the stage to match the current
#'   workflow.
#' @param use_ifelse Logical. Preserves the current duplicate-BEUMC behaviour
#'   where those rows are marked updated immediately.
#' @param beu_bec Optional data frame containing the allowed BEC/BEU lookup used
#'   for the final per-component correction pass.
#' @param filename Optional filename for writing the corrected stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` representing the corrected stack.
terra_rrm_correct_bem_from_vri <- function(x,
                                           clear_site_ma = TRUE,
                                           use_ifelse = TRUE,
                                           beu_bec = NULL,
                                           filename = NULL,
                                           overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  raster_conv <- .terra_rrm_get_raster_conv()

  required_layers <- c(
    "SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3",
    "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5",
    "SPEC_CD_1", "SPEC_PCT_1", "AGE_CL_STS", "Area_Ha", "LAND_CD_1",
    "COV_PCT_1", "LBL_VEGCOV", "SLOPE_MOD"
  )
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(
      sprintf("Missing required layers for terra_rrm_correct_bem_from_vri: %s", paste(missing_layers, collapse = ", ")),
      call. = FALSE
    )
  }

  # Named column index: maps layer name → column number in the values matrix
  idx <- setNames(seq_len(terra::nlyr(x)), names(x))

  # Pre-compute all integer codes once from raster_conv — same tables used
  # during rasterization, so these are the exact values stored on disk.
  lkp_vri <- function(layer, labels) .terra_rrm_layer_codes(x, layer, labels, raster_conv$vri, strict = FALSE)
  lkp_bem <- function(layer, labels) .terra_rrm_layer_codes(x, layer, labels, raster_conv$bem, strict = FALSE)

  codes <- list(
    lv1_n          = lkp_vri("BCLCS_LV_1", "N")[[1]],
    lv1_v          = lkp_vri("BCLCS_LV_1", "V")[[1]],
    lv2_n          = lkp_vri("BCLCS_LV_2", "N")[[1]],
    lv2_t          = lkp_vri("BCLCS_LV_2", "T")[[1]],
    lv3_w          = lkp_vri("BCLCS_LV_3", "W")[[1]],
    lv5_la         = lkp_vri("BCLCS_LV_5", "LA")[[1]],
    lv5_re         = lkp_vri("BCLCS_LV_5", "RE")[[1]],
    lv5_ri_rs      = lkp_vri("BCLCS_LV_5", c("RI", "RS")),
    lv5_ap         = lkp_vri("BCLCS_LV_5", "AP")[[1]],
    lv5_bu         = lkp_vri("BCLCS_LV_5", "BU")[[1]],
    lv5_gb         = lkp_vri("BCLCS_LV_5", "GB")[[1]],
    lv5_gl_pn      = lkp_vri("BCLCS_LV_5", c("GL", "PN")),
    lv5_gp         = lkp_vri("BCLCS_LV_5", "GP")[[1]],
    lv5_ll         = lkp_vri("BCLCS_LV_5", "LL")[[1]],
    lv5_mi         = lkp_vri("BCLCS_LV_5", c("MI", "TZ", "MZ")),
    lv5_ro         = lkp_vri("BCLCS_LV_5", c("RO", "BR", "BI")),
    lv5_ta         = lkp_vri("BCLCS_LV_5", "TA")[[1]],
    lv5_tc         = lkp_vri("BCLCS_LV_5", c("TC", "RN", "RZ")),
    lv5_tr         = lkp_vri("BCLCS_LV_5", "TR")[[1]],
    lv5_uv         = lkp_vri("BCLCS_LV_5", c("UV", "RS", "MU", "ES", "CB", "MN", "RM")),
    lv5_ur         = lkp_vri("BCLCS_LV_5", "UR")[[1]],
    spec_sb        = lkp_vri("SPEC_CD_1", "SB")[[1]],
    deciduous_codes = lkp_vri("SPEC_CD_1", c("AC", "ACB", "ACT", "AT", "EP")),
    conifer_codes  = lkp_vri("SPEC_CD_1", c("B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW",
                                             "PA", "PL", "PLI", "S", "SB", "SE", "SS", "SW", "SX", "SXW")),
    land_uv        = lkp_vri("LAND_CD_1", c("UV", "RS", "MU", "ES", "CB", "MN", "RM")),
    lbl_rz         = lkp_vri("LBL_VEGCOV", c(
      "rz", "rz,by", "rz,by,he", "rz,by,he,sl", "rz,by,sl", "rz,by,sl,he", "rz,by,st", "rz,he",
      "rz,by,sl,he", "rz,by,st", "rz,he", "rz,he,by", "rz,he,by,sl", "rz,he,sl", "rz,he,sl,by",
      "rz,he,st", "rz,he,st,by", "rz,hf,by", "rz,hf,sl,by", "rz,hg", "rz,hg,sl", "rz,sl",
      "rz,sl,by", "rz,sl,by,he", "rz,sl,he", "rz,sl,he,by", "rz,sl,hf", "rz,sl,hf,by", "rz,sl,hg",
      "rz,st", "rz,st,he", "rz,st,he,by", "rz,st,hf", "rz,st,hg"
    )),
    slope_mod_qz   = lkp_bem("SLOPE_MOD", c("q", "z")),
    treed_codes    = lkp_vri("BCLCS_LV_4", c("TB", "TC", "TM")),
    wl_code        = lkp_bem("BEUMC_S1", "WL")[[1]],
    beu_ow  = lkp_bem("BEUMC_S1", "OW")[[1]],
    beu_ls  = lkp_bem("BEUMC_S1", "LS")[[1]],
    beu_ll  = lkp_bem("BEUMC_S1", "LL")[[1]],
    beu_re  = lkp_bem("BEUMC_S1", "RE")[[1]],
    beu_ri  = lkp_bem("BEUMC_S1", "RI")[[1]],
    beu_wl  = lkp_bem("BEUMC_S1", "WL")[[1]],
    beu_bb  = lkp_bem("BEUMC_S1", "BB")[[1]],
    beu_ur  = lkp_bem("BEUMC_S1", "UR")[[1]],
    beu_cl  = lkp_bem("BEUMC_S1", "CL")[[1]],
    beu_gb  = lkp_bem("BEUMC_S1", "GB")[[1]],
    beu_gl  = lkp_bem("BEUMC_S1", "GL")[[1]],
    beu_gp  = lkp_bem("BEUMC_S1", "GP")[[1]],
    beu_mi  = lkp_bem("BEUMC_S1", "MI")[[1]],
    beu_ro  = lkp_bem("BEUMC_S1", "RO")[[1]],
    beu_ta  = lkp_bem("BEUMC_S1", "TA")[[1]],
    beu_tc  = lkp_bem("BEUMC_S1", "TC")[[1]],
    beu_tr  = lkp_bem("BEUMC_S1", "TR")[[1]],
    beu_uv  = lkp_bem("BEUMC_S1", "UV")[[1]],
    distcls_f = if ("DISTCLS_1" %in% names(x)) lkp_bem("DISTCLS_1", "F")[[1]] else NA_real_,
    stand_b    = if ("STAND_A1" %in% names(x)) lkp_bem("STAND_A1", "B")[[1]]  else NA_real_,
    stand_m    = if ("STAND_A1" %in% names(x)) lkp_bem("STAND_A1", "M")[[1]]  else NA_real_,
    stand_c    = if ("STAND_A1" %in% names(x)) lkp_bem("STAND_A1", "C")[[1]]  else NA_real_,
    stand_cm   = if ("STAND_A1" %in% names(x)) lkp_bem("STAND_A1", c("C", "M")) else integer(0),
    stand_cb   = if ("STAND_A1" %in% names(x)) lkp_bem("STAND_A1", c("C", "B")) else integer(0),
    stand_m_only = if ("STAND_A1" %in% names(x)) lkp_bem("STAND_A1", "M")      else integer(0)
  )

  layer_lookup <- .terra_rrm_component_layer_lookup()
  field_groups <- .terra_rrm_component_field_groups()

  # Determine output file: use filename if provided, else a temp file.
  # We always write to disk so the block loop has a clean output raster.
  out_file  <- if (!is.null(filename)) filename else tempfile(fileext = ".tif")
  out <- terra::rast(x)  # copy metadata/extent/crs, no data

  terra::writeStart(out, filename = out_file, overwrite = TRUE,
                    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))

  b <- terra::blocks(x)
  for (i in seq_len(b$n)) {
    m <- terra::readValues(x, row = b$row[i], nrows = b$nrows[i], mat = TRUE)

    m <- .blk_apply_vri_rules(
      m           = m,
      idx         = idx,
      codes       = codes,
      layer_lookup = layer_lookup,
      field_groups = field_groups,
      clear_site_ma = clear_site_ma,
      use_ifelse  = use_ifelse
    )

    terra::writeValues(out, m, start = b$row[i], nrows = b$nrows[i])
  }
  terra::writeStop(out)

  result <- terra::rast(out_file)
  result <- set_raster_levels_from_conv(result, c(raster_conv$vri, raster_conv$bem))

  # BEU/BEC correction pass: uses terra::values() on the final result only
  result <- .terra_rrm_apply_beu_bec_corrections(result, beu_bec)

  if (is.null(filename)) {
    return(result)
  }

  # If a filename was requested, the block loop already wrote there.
  # Re-write only if the beu_bec pass changed anything (it operates in-memory).
  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}

.terra_rrm_component_field_groups <- function() {
  list(
    integer_1 = c("TREE_C1", "SHRUB_C1"),
    character_1 = c(
      "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
      "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
      "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", "DISTCLS_1", "DISTSCLS_1",
      "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"
    )
  )
}


.terra_rrm_component_layer_lookup <- function() {
  field_groups <- .terra_rrm_component_field_groups()

  character_2 <- sub("1", "2", field_groups$character_1)
  character_3 <- sub("1", "3", field_groups$character_1)
  integer_2 <- sub("1", "2", field_groups$integer_1)
  integer_3 <- sub("1", "3", field_groups$integer_1)

  list(
    `1` = c(field_groups$character_1, field_groups$integer_1),
    `2` = c(character_2, integer_2),
    `3` = c(character_3, integer_3)
  )
}


.terra_rrm_shift_component_fields <- function(x, mask, shift_pattern) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(inherits(mask, "SpatRaster"), terra::nlyr(mask) == 1L)
  stopifnot(is.list(shift_pattern))

  layer_lookup <- .terra_rrm_component_layer_lookup()
  mask_layer <- mask[[1]]
  result <- x

  for (pattern in shift_pattern) {
    to_component <- as.character(pattern[[1]])
    from_component <- pattern[[2]]

    if (is.na(to_component) || !to_component %in% names(layer_lookup)) {
      next
    }

    target_layers <- layer_lookup[[to_component]]
    source_layers <- if (is.na(from_component)) {
      rep(NA_character_, length(target_layers))
    } else {
      layer_lookup[[as.character(from_component)]]
    }

    for (i in seq_along(target_layers)) {
      target_layer <- target_layers[[i]]
      if (!target_layer %in% names(result)) {
        next
      }

      source_layer <- source_layers[[i]]
      replacement <- if (is.na(source_layer) || !source_layer %in% names(x)) {
        terra::ifel(mask_layer == 1, NA, result[[target_layer]])
      } else {
        terra::ifel(mask_layer == 1, x[[source_layer]], result[[target_layer]])
      }

      names(replacement) <- target_layer
      result[[target_layer]] <- replacement
    }
  }

  result
}