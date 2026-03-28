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

  numeric_mask <- .terra_rrm_compare_numeric_rule(layer, rule_value)
  if (!is.null(numeric_mask)) {
    return(numeric_mask)
  }

  stop(sprintf("Unsupported rule value '%s' for non-categorical layer '%s'.", rule_value, layer_name), call. = FALSE)
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


.terra_rrm_apply_river_adjacency_stage <- function(x, rivers_layer = "rivers") {
  stopifnot(inherits(x, "SpatRaster"))
  if (!rivers_layer %in% names(x) || !"SITE_M3A" %in% names(x)) {
    return(x)
  }

  site_a_code <- .terra_rrm_layer_codes(x, "SITE_M3A", "a", .terra_rrm_get_raster_conv()$bem, strict = FALSE)[[1]]
  if (is.na(site_a_code)) {
    return(x)
  }

  base_mask <- terra::ifel(!is.na(x[[rivers_layer]]), 1, NA)
  river_mask <- .terra_rrm_rule_mask(
    base_mask,
    terra::ifel(x[[rivers_layer]] == 1, 1, NA)
  )

  .terra_rrm_apply_constant(x, "SITE_M3A", river_mask, site_a_code)
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
    mask <- .terra_rrm_non_missing_mask(result)
    for (layer_name in input_layers) {
      mask <- .terra_rrm_rule_mask(mask, .terra_rrm_rule_value_mask(result, layer_name, rules_dt[[layer_name]][rule_idx]))
    }

    if (length(tree_rule_cd_columns) > 0L) {
      for (tree_idx in seq_along(tree_rule_cd_columns)) {
        cd_name <- rule_names[[tree_rule_cd_columns[[tree_idx]]]]
        pct_name <- if (tree_idx <= length(tree_rule_pct_columns)) rule_names[[tree_rule_pct_columns[[tree_idx]]]] else NULL
        pct_value <- if (is.null(pct_name)) NA else rules_dt[[pct_name]][rule_idx]
        tree_mask <- .terra_rrm_tree_rule_mask(result, rules_dt[[cd_name]][rule_idx], pct_value, species_layers, pct_layers)
        mask <- .terra_rrm_rule_mask(mask, tree_mask)
      }
    }

    for (layer_name in output_layers) {
      output_value <- rules_dt[[layer_name]][rule_idx]
      if (is.na(output_value) || !nzchar(trimws(as.character(output_value)))) {
        next
      }

      target_layer <- if (identical(layer_name, "BEUMC")) "BEUMC_S1" else layer_name
      resolved_value <- .terra_rrm_resolve_output_value(result, target_layer, output_value)
      result <- .terra_rrm_apply_constant(result, target_layer, mask, resolved_value)
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
#' @param directions Connectivity passed to [terra::patches()].
#' @param filename Optional filename for writing the corrected stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with lake patches corrected in place.
terra_rrm_correct_small_lakes <- function(x,
                                          lake_layer = "lakes",
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
  candidate_mask <- .terra_rrm_rule_mask(
    .terra_rrm_non_missing_mask(result),
    terra::ifel(result[[lake_layer]] > 0, 1, NA),
    terra::ifel(lake_class_mask == 0, 1, NA)
  )

  lake_patches <- terra::patches(terra::ifel(candidate_mask[[1]] == 1, 1, NA), directions = directions)
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
    lake_mask <- terra::ifel(!is.na(patch_classes), 1, NA)

    result <- .terra_rrm_apply_values(result, "BEUMC_S1", lake_mask, patch_classes)
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


#' Terra-native VRI-driven BEM corrections
#'
#' Builds the ordered on-disk VRI correction stage for the raster-only path.
#' This stage covers duplicate component consolidation, map-code reassignment,
#' removal of inappropriate wetland components in treed units, stand updates,
#' and allowed BEC/BEU correction lookups.
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

  result <- x
  if (clear_site_ma) {
    result <- .terra_rrm_apply_constant(result, "SITE_M1A", .terra_rrm_non_missing_mask(result), NA)
    result <- .terra_rrm_apply_constant(result, "SITE_M2A", .terra_rrm_non_missing_mask(result), NA)
  }
  result <- .terra_rrm_apply_constant(result, "SITE_M3A", .terra_rrm_non_missing_mask(result), NA)

  state <- .terra_rrm_combine_duplicate_beumc(result, use_ifelse = use_ifelse)
  result <- state$x
  row_updated <- state$row_updated
  blank_eco <- state$blank_eco

  smpl_type_na <- .terra_rrm_optional_na_mask(result, "SMPL_TYPE")
  lv1_n <- .terra_rrm_layer_codes(result, "BCLCS_LV_1", c("N"), raster_conv$vri, strict = FALSE)[[1]]
  lv1_v <- .terra_rrm_layer_codes(result, "BCLCS_LV_1", c("V"), raster_conv$vri, strict = FALSE)[[1]]
  lv2_n <- .terra_rrm_layer_codes(result, "BCLCS_LV_2", c("N"), raster_conv$vri, strict = FALSE)[[1]]
  lv3_w <- .terra_rrm_layer_codes(result, "BCLCS_LV_3", c("W"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_la <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("LA"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_re <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("RE"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_ri_rs <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("RI", "RS"), raster_conv$vri, strict = FALSE)
  lv5_ap <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("AP"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_bu <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("BU"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_gb <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("GB"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_gl_pn <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("GL", "PN"), raster_conv$vri, strict = FALSE)
  lv5_gp <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("GP"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_ll <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("LL"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_mi <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("MI", "TZ", "MZ"), raster_conv$vri, strict = FALSE)
  lv5_ro <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("RO", "BR", "BI"), raster_conv$vri, strict = FALSE)
  lv5_ta <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("TA"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_tc <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("TC", "RN", "RZ"), raster_conv$vri, strict = FALSE)
  lv5_tr <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("TR"), raster_conv$vri, strict = FALSE)[[1]]
  lv5_uv <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("UV", "RS", "MU", "ES", "CB", "MN", "RM"), raster_conv$vri, strict = FALSE)
  lv5_ur <- .terra_rrm_layer_codes(result, "BCLCS_LV_5", c("UR"), raster_conv$vri, strict = FALSE)[[1]]
  slope_mod_qz <- .terra_rrm_layer_codes(result, "SLOPE_MOD", c("q", "z"), raster_conv$bem, strict = FALSE)
  spec_sb <- .terra_rrm_layer_codes(result, "SPEC_CD_1", c("SB"), raster_conv$vri, strict = FALSE)[[1]]
  deciduous_codes <- .terra_rrm_layer_codes(result, "SPEC_CD_1", c("AC", "ACB", "ACT", "AT", "EP"), raster_conv$vri, strict = FALSE)
  conifer_codes <- .terra_rrm_layer_codes(result, "SPEC_CD_1", c("B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW", "PA", "PL", "PLI", "S", "SB", "SE", "SS", "SW", "SX", "SXW"), raster_conv$vri, strict = FALSE)
  land_uv <- .terra_rrm_layer_codes(result, "LAND_CD_1", c("UV", "RS", "MU", "ES", "CB", "MN", "RM"), raster_conv$vri, strict = FALSE)
  lbl_rz <- .terra_rrm_layer_codes(
    result,
    "LBL_VEGCOV",
    c(
      "rz", "rz,by", "rz,by,he", "rz,by,he,sl", "rz,by,sl", "rz,by,sl,he", "rz,by,st", "rz,he",
      "rz,by,sl,he", "rz,by,st", "rz,he", "rz,he,by", "rz,he,by,sl", "rz,he,sl", "rz,he,sl,by",
      "rz,he,st", "rz,he,st,by", "rz,hf,by", "rz,hf,sl,by", "rz,hg", "rz,hg,sl", "rz,sl",
      "rz,sl,by", "rz,sl,by,he", "rz,sl,he", "rz,sl,he,by", "rz,sl,hf", "rz,sl,hf,by", "rz,sl,hg",
      "rz,st", "rz,st,he", "rz,st,he,by", "rz,st,hf", "rz,st,hg"
    ),
    raster_conv$vri,
    strict = FALSE
  )
  beumc_codes <- function(values) .terra_rrm_layer_codes(result, "BEUMC_S1", values, raster_conv$bem, strict = FALSE)

  apply_rule <- function(mask, beumc_value, blank_components = TRUE, set_sdec1 = 10, set_sdec2 = NULL, set_sdec3 = NULL) {
    beumc_code <- beumc_codes(beumc_value)[[1]]
    if (is.na(beumc_code)) {
      return(invisible(NULL))
    }

    state <<- .terra_rrm_apply_primary_beumc_rule(
      x = result,
      row_updated = row_updated,
      blank_eco = blank_eco,
      mask = mask,
      beumc_code = beumc_code,
      blank_components = blank_components,
      set_sdec1 = set_sdec1,
      set_sdec2 = set_sdec2,
      set_sdec3 = set_sdec3
    )
    result <<- state$x
    row_updated <<- state$row_updated
    blank_eco <<- state$blank_eco
  }

  mask_base <- function() .terra_rrm_rule_mask(smpl_type_na, terra::ifel(is.na(row_updated), NA, terra::ifel(row_updated == 0, 1, 0)))

  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_1"]] == lv1_n, 1, NA), terra::ifel(result[["BCLCS_LV_5"]] == lv5_la, 1, NA), terra::ifel(result[["Area_Ha"]] <= 2, 1, NA)), "OW")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_1"]] == lv1_n, 1, NA), terra::ifel(result[["BCLCS_LV_5"]] == lv5_la, 1, NA), terra::ifel(result[["Area_Ha"]] > 2 & result[["Area_Ha"]] <= 60, 1, NA)), "LS")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_1"]] == lv1_n, 1, NA), terra::ifel(result[["BCLCS_LV_5"]] == lv5_la, 1, NA), terra::ifel(result[["Area_Ha"]] > 60, 1, NA)), "LL")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_1"]] == lv1_n, 1, NA), terra::ifel(result[["BCLCS_LV_5"]] == lv5_re, 1, NA)), "RE")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_1"]] == lv1_n, 1, NA), .terra_rrm_match_codes(result[["BCLCS_LV_5"]], lv5_ri_rs)), "RI")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_1"]] == lv1_v, 1, NA), terra::ifel(result[["BCLCS_LV_2"]] == lv2_n, 1, NA), terra::ifel(result[["BCLCS_LV_3"]] == lv3_w, 1, NA), terra::ifel(result[["AGE_CL_STS"]] == -1, 1, NA)), "WL")

  wetland_state <- .terra_rrm_remove_inadequate_wetlands(result, row_updated)
  result <- wetland_state$x
  row_updated <- wetland_state$row_updated

  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["SPEC_CD_1"]] == spec_sb, 1, NA), terra::ifel(result[["SPEC_PCT_1"]] >= 90, 1, NA)), "BB")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_ap, 1, NA)), "UR")

  mask_bu <- .terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_bu, 1, NA))
  result <- .terra_rrm_apply_constant(result, "SDEC_1", mask_bu, 10)
  result <- .terra_rrm_apply_constant(result, "DISTCLS_1", mask_bu, .terra_rrm_layer_codes(result, "DISTCLS_1", c("F"), raster_conv$bem, strict = FALSE)[[1]])
  row_updated <- terra::ifel(mask_bu[[1]], 1, row_updated)

  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["SLOPE_MOD"]], slope_mod_qz)), "CL")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_gb, 1, NA)), "GB")
  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["BCLCS_LV_5"]], lv5_gl_pn)), "GL")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_gp, 1, NA)), "GP")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_ll, 1, NA)), "LL")
  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["BCLCS_LV_5"]], lv5_mi)), "MI")
  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["BCLCS_LV_5"]], lv5_ro)), "RO")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_ta, 1, NA)), "TA")
  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["BCLCS_LV_5"]], lv5_tc)), "TC")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_tr, 1, NA)), "TR")
  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["BCLCS_LV_5"]], lv5_uv)), "UV")
  apply_rule(.terra_rrm_rule_mask(mask_base(), .terra_rrm_match_codes(result[["LAND_CD_1"]], land_uv), terra::ifel(result[["COV_PCT_1"]] >= 95, 1, NA)), "UV")
  apply_rule(.terra_rrm_rule_mask(mask_base(), terra::ifel(result[["BCLCS_LV_5"]] == lv5_ur, 1, NA)), "UR")

  mask_tc2 <- .terra_rrm_rule_mask(
    mask_base(),
    terra::ifel(result[["BCLCS_LV_2"]] == .terra_rrm_layer_codes(result, "BCLCS_LV_2", c("T"), raster_conv$vri)[[1]], 1, NA),
    terra::ifel(result[["SDEC_1"]] == 10, 1, NA),
    .terra_rrm_match_codes(result[["LBL_VEGCOV"]], lbl_rz)
  )
  result <- .terra_rrm_apply_constant(result, "SDEC_1", mask_tc2, 8)
  result <- .terra_rrm_apply_constant(result, "SDEC_2", mask_tc2, 2)
  result <- .terra_rrm_apply_constant(result, "BEUMC_S2", mask_tc2, beumc_codes("TC")[[1]])
  row_updated <- terra::ifel(mask_tc2[[1]], 1, row_updated)

  if ("STAND_A1" %in% names(result)) {
    mask_stand_b <- .terra_rrm_rule_mask(
      smpl_type_na,
      .terra_rrm_match_codes(result[["SPEC_CD_1"]], deciduous_codes),
      terra::ifel(result[["SPEC_PCT_1"]] >= 75, 1, NA),
      .terra_rrm_match_codes(result[["STAND_A1"]], .terra_rrm_layer_codes(result, "STAND_A1", c("C", "M"), raster_conv$bem, strict = FALSE))
    )
    result <- .terra_rrm_apply_constant(result, "STAND_A1", mask_stand_b, .terra_rrm_layer_codes(result, "STAND_A1", "B", raster_conv$bem, strict = FALSE)[[1]])

    mask_stand_m <- .terra_rrm_rule_mask(
      smpl_type_na,
      .terra_rrm_match_codes(result[["SPEC_CD_1"]], deciduous_codes),
      terra::ifel(result[["SPEC_PCT_1"]] >= 50 & result[["SPEC_PCT_1"]] < 75, 1, NA),
      .terra_rrm_match_codes(result[["STAND_A1"]], .terra_rrm_layer_codes(result, "STAND_A1", c("C", "B"), raster_conv$bem, strict = FALSE))
    )
    result <- .terra_rrm_apply_constant(result, "STAND_A1", mask_stand_m, .terra_rrm_layer_codes(result, "STAND_A1", "M", raster_conv$bem, strict = FALSE)[[1]])

    mask_stand_c <- .terra_rrm_rule_mask(
      smpl_type_na,
      .terra_rrm_match_codes(result[["SPEC_CD_1"]], conifer_codes),
      terra::ifel(result[["SPEC_PCT_1"]] >= 75, 1, NA),
      .terra_rrm_match_codes(result[["STAND_A1"]], .terra_rrm_layer_codes(result, "STAND_A1", "M", raster_conv$bem, strict = FALSE))
    )
    result <- .terra_rrm_apply_constant(result, "STAND_A1", mask_stand_c, .terra_rrm_layer_codes(result, "STAND_A1", "C", raster_conv$bem, strict = FALSE)[[1]])
  }

  result <- .terra_rrm_blank_eco_fields(result, terra::ifel(blank_eco == 1, 1, NA))
  result <- .terra_rrm_apply_beu_bec_corrections(result, beu_bec)

  if (is.null(filename)) {
    return(result)
  }

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
        suppressWarnings(
          terra::app(
            c(mask_layer, result[[target_layer]]),
            fun = function(values) {
              ifelse(!is.na(values[, 1]) & values[, 1] == 1, NA, values[, 2])
            }
          )
        )
      } else {
        suppressWarnings(
          terra::app(
            c(mask_layer, x[[source_layer]], result[[target_layer]]),
            fun = function(values) {
              ifelse(!is.na(values[, 1]) & values[, 1] == 1, values[, 2], values[, 3])
            }
          )
        )
      }

      names(replacement) <- target_layer
      result[[target_layer]] <- replacement
    }
  }

  result
}