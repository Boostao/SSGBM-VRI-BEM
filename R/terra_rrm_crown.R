#' Terra-native crown dominance stage for the raster-only RRM path

.terra_rrm_blank_like_preserve_levels <- function(template, layer_name) {
  blank <- template[[1]]
  terra::values(blank) <- NA_real_
  names(blank) <- layer_name
  blank
}


.terra_rrm_match_strct_high <- function(x, layer_name) {
  categories <- terra::cats(x[[layer_name]])[[1]]
  if (is.null(categories) || ncol(categories) < 2L) {
    raster_conv <- .terra_rrm_get_raster_conv()
    lookup_source <- if (layer_name %in% names(raster_conv$bem)) raster_conv$bem[[layer_name]] else NULL
    if (!is.null(lookup_source) && nrow(lookup_source) > 0L) {
      categories <- data.frame(value = lookup_source[["factor"]], label = lookup_source[["value"]], stringsAsFactors = FALSE)
    }
  }

  matched_codes <- if (!is.null(categories) && ncol(categories) >= 2L) categories[[1]][grepl("^[4567]", categories[[2]])] else numeric(0)
  .terra_rrm_match_codes(x[[layer_name]], matched_codes)
}


#' Terra-native crown area dominant values
#'
#' Recreates the raster-mode behavior of `find_crown_area_dominant_values()` by
#' copying `CROWN_ALL` into the decile-specific crown layers and blanking any
#' component that is not forested or whose structural stage does not begin with
#' 4, 5, 6, or 7.
#'
#' @param x A `terra::SpatRaster` containing `CROWN_ALL`, `FORESTED_1:3`, and
#'   `STRCT_S1:3`.
#' @param filename Optional filename for writing the updated stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with `CROWN_ALL_1:3`, `CROWN_BEAR_1:3`, and
#'   `CROWN_MOOSE_1:3` updated.
#' @export
terra_rrm_find_crown_area_dominant_values <- function(x,
                                                      filename = NULL,
                                                      overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))

  required_layers <- c("CROWN_ALL", paste0("FORESTED_", 1:3), paste0("STRCT_S", 1:3))
  missing_layers <- setdiff(required_layers, names(x))
  if (length(missing_layers) > 0L) {
    stop(sprintf("Missing required layers for terra_rrm_find_crown_area_dominant_values: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
  }

  result <- x
  for (component in 1:3) {
    for (prefix in c("CROWN_ALL_", "CROWN_BEAR_", "CROWN_MOOSE_")) {
      layer_name <- paste0(prefix, component)
      if (!layer_name %in% names(result)) {
        result <- c(result, .terra_rrm_blank_like_preserve_levels(result[["CROWN_ALL"]], layer_name))
      }
    }
  }

  for (component in 1:3) {
    forest_mask <- .terra_rrm_rule_value_mask(result, paste0("FORESTED_", component), "Y")
    strct_mask <- .terra_rrm_match_strct_high(result, paste0("STRCT_S", component))
    valid_mask <- .terra_rrm_rule_mask(forest_mask, strct_mask)

    crown_values <- terra::ifel(valid_mask[[1]] == 1, result[["CROWN_ALL"]], NA)
    names(crown_values) <- paste0("CROWN_ALL_", component)

    for (prefix in c("CROWN_ALL_", "CROWN_BEAR_", "CROWN_MOOSE_")) {
      layer_name <- paste0(prefix, component)
      categories <- terra::cats(result[[layer_name]])[[1]]
      result[[layer_name]] <- crown_values
      names(result[[layer_name]]) <- layer_name
      if (!is.null(categories) && ncol(categories) >= 2L) {
        levels(result[[layer_name]]) <- categories
      }
    }
  }

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}