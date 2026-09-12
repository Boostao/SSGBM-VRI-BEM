#' Terra helpers for the new raster-only RRM path
#'
#' These helpers are the foundation for a terra-native pipeline that starts
#' from already-materialized aligned rasters on disk. They do not modify the
#' legacy sf/data.table or DuckDB workflows.

#' Derive per-cell terrain layers from the stacked elevation layer
#'
#' Computes `MEAN_SLOPE` (slope in percent, matching the `downscale_elevation`
#' convention: radians * 57.29578 / 90) and `ABOVE_ELEV_THOLD` ("Y"/"N" factor)
#' from the `elevation` layer already present in the input stack, and adds them
#' as new layers.  Must be called after `.terra_rrm_read_input_stack()` and
#' before any correction step that depends on slope or the elevation threshold.
#'
#' @param x A `SpatRaster` containing an `elevation` layer (raw DEM values in
#'   metres).
#' @param elevation_threshold Numeric. Elevation (m) above which
#'   `ABOVE_ELEV_THOLD` is set to `"Y"`.
#' @param filename Optional path to write the augmented stack to disk.
#' @param overwrite Logical passed to `terra::writeRaster`.
#' @return The input `SpatRaster` with two additional layers: `MEAN_SLOPE` and
#'   `ABOVE_ELEV_THOLD`.
#' @export
terra_rrm_compute_terrain_layers <- function(x,
                                             elevation_threshold,
                                             filename = NULL,
                                             overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.numeric(elevation_threshold), length(elevation_threshold) == 1L)

  if (!"elevation" %in% names(x)) {
    stop("terra_rrm_compute_terrain_layers: 'elevation' layer not found in input stack.", call. = FALSE)
  }

  elev_layer <- x[["elevation"]]

  # Slope in percent: terra::terrain returns radians, convert to % using same
  # formula as downscale_elevation(): radians * 57.29578 / 90
  slope_rad <- terra::terrain(elev_layer, v = "slope", unit = "radians")
  mean_slope <- slope_rad * (57.29578 / 90)
  names(mean_slope) <- "MEAN_SLOPE"

  # ABOVE_ELEV_THOLD: "Y" where elevation > threshold, "N" otherwise.
  # Stored as an integer-coded factor (1 = "N", 2 = "Y") to match raster_conv.
  above_raw <- terra::ifel(elev_layer > elevation_threshold, 2L, 1L)
  names(above_raw) <- "ABOVE_ELEV_THOLD"
  levels(above_raw) <- data.frame(value = c(1L, 2L), label = c("N", "Y"))
  names(above_raw) <- "ABOVE_ELEV_THOLD"

  # Only the 2 new terrain layers are written — x is already on disk.
  # The returned SpatRaster is a zero-copy virtual multi-source stack.
  terrain_stack <- c(mean_slope, above_raw)

  if (is.null(filename)) {
    return(c(x, terrain_stack))
  }

  terra::writeRaster(terrain_stack, filename = filename,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"),
                     overwrite = overwrite)
  c(x, terra::rast(filename))
}

set_raster_levels_from_conv <- function(x, factor_conv_list) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.list(factor_conv_list))

  matching_layers <- intersect(names(x), names(factor_conv_list))
  if (length(matching_layers) == 0L) {
    return(x)
  }

  for (layer_name in matching_layers) {
    lookup_dt <- factor_conv_list[[layer_name]]
    if (is.null(lookup_dt) || nrow(lookup_dt) == 0L) {
      next
    }

    levels(x[[layer_name]]) <- data.frame(
      value = lookup_dt[["factor"]],
      label = lookup_dt[["value"]],
      stringsAsFactors = FALSE
    )
  }

  x
}


.terra_rrm_require_aligned_rasters <- function(rasters) {
  raster_names <- names(rasters)
  reference_raster <- rasters[[1]][[1]]

  for (i in seq_along(rasters)[-1]) {
    if (!terra::compareGeom(reference_raster, rasters[[i]][[1]], stopOnError = FALSE)) {
      stop(
        sprintf(
          "Raster '%s' does not share the same geometry as raster '%s'.",
          raster_names[[i]],
          raster_names[[1]]
        ),
        call. = FALSE
      )
    }
  }
}


.terra_rrm_stack_inputs <- function(vri_raster,
                                    bem_raster,
                                    rivers_raster,
                                    wetlands_raster,
                                    lakes_raster = NULL,
                                    ccb_raster = NULL,
                                    elevation_raster = NULL,
                                    aoi = NULL,
                                    filename = NULL,
                                    overwrite = FALSE) {
  rasters <- Filter(
    Negate(is.null),
    list(
      vri = vri_raster,
      bem = bem_raster,
      rivers = rivers_raster,
      wetlands = wetlands_raster,
      lakes = lakes_raster,
      ccb = ccb_raster,
      elevation = elevation_raster
    )
  )

  stopifnot(length(rasters) >= 4L)

  if (!is.null(aoi)) {
    aoi_bbox <- .aoi_to_bbox(aoi)
    crop_extent <- terra::ext(aoi_bbox[c(1, 3, 2, 4)])
    rasters <- lapply(rasters, terra::crop, y = crop_extent, snap = "out")
  }

  .terra_rrm_require_aligned_rasters(rasters)

  layer_names <- unlist(lapply(rasters, names), use.names = FALSE)
  duplicated_names <- unique(layer_names[duplicated(layer_names)])
  if (length(duplicated_names) > 0L) {
    stop(
      sprintf(
        "The stacked raster contains duplicated layer names: %s",
        paste(duplicated_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  stacked_raster <- rasters[[1]]
  if (length(rasters) > 1L) {
    for (i in 2:length(rasters)) {
      stacked_raster <- c(stacked_raster, rasters[[i]])
    }
  }

  if (is.null(filename)) {
    return(stacked_raster)
  }

  terra::writeRaster(stacked_raster, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


.terra_rrm_read_input_stack <- function(vri_dsn,
                                        bem_dsn,
                                        rivers_dsn,
                                        wetlands_dsn,
                                        lakes_dsn = NULL,
                                        ccb_dsn = NULL,
                                        elevation_dsn = NULL,
                                        aoi = NULL,
                                        filename = NULL,
                                        overwrite = FALSE) {
  raster_conv <- .terra_rrm_get_raster_conv()

  vri_raster <- set_raster_levels_from_conv(terra::rast(vri_dsn), raster_conv$vri)
  bem_raster <- set_raster_levels_from_conv(terra::rast(bem_dsn), raster_conv$bem)
  elevation_raster <- if (is.null(elevation_dsn)) NULL else terra::rast(elevation_dsn)
  if (!is.null(elevation_raster) && terra::nlyr(elevation_raster) == 1L && !"elevation" %in% names(elevation_raster)) {
    names(elevation_raster) <- "elevation"
  }

  .terra_rrm_stack_inputs(
    vri_raster = vri_raster,
    bem_raster = bem_raster,
    rivers_raster = terra::rast(rivers_dsn),
    wetlands_raster = terra::rast(wetlands_dsn),
    lakes_raster = if (is.null(lakes_dsn)) NULL else terra::rast(lakes_dsn),
    ccb_raster = if (is.null(ccb_dsn)) NULL else terra::rast(ccb_dsn),
    elevation_raster = elevation_raster,
    aoi = aoi,
    filename = filename,
    overwrite = overwrite
  )
}

# Variant used by the hybrid DuckDB pipeline where VRI and BEM attributes are
# already merged and corrected in a single VRIBEM raster produced by
# rasterize_vribem_materialized().  Factor levels are already embedded by that
# function; this reader simply stacks the combined file with the ancillary layers.
.terra_rrm_read_input_stack_from_vribem <- function(vribem_dsn,
                                                     rivers_dsn,
                                                     wetlands_dsn,
                                                     lakes_dsn = NULL,
                                                     elevation_dsn = NULL,
                                                     aoi = NULL,
                                                     filename = NULL,
                                                     overwrite = FALSE) {
  raster_conv <- .terra_rrm_get_raster_conv()
  combined_conv <- c(raster_conv$vri, raster_conv$bem)
  combined_conv <- combined_conv[!duplicated(names(combined_conv))]

  # rasterize_vribem_materialized already wrote factor levels; applying
  # set_raster_levels_from_conv here is harmless (it only adds/overwrites
  # levels that match layer names) and ensures consistency if the file was
  # produced by a different tool.
  vribem_raster <- set_raster_levels_from_conv(terra::rast(vribem_dsn), combined_conv)
  elevation_raster <- if (is.null(elevation_dsn)) NULL else terra::rast(elevation_dsn)
  if (!is.null(elevation_raster) && terra::nlyr(elevation_raster) == 1L && !"elevation" %in% names(elevation_raster)) {
    names(elevation_raster) <- "elevation"
  }

  rasters <- Filter(
    Negate(is.null),
    list(
      vribem   = vribem_raster,
      rivers   = terra::rast(rivers_dsn),
      wetlands = terra::rast(wetlands_dsn),
      lakes    = if (is.null(lakes_dsn)) NULL else terra::rast(lakes_dsn),
      elevation = elevation_raster
    )
  )

  if (!is.null(aoi)) {
    aoi_bbox <- .aoi_to_bbox(aoi)
    crop_extent <- terra::ext(aoi_bbox[c(1, 3, 2, 4)])
    rasters <- lapply(rasters, terra::crop, y = crop_extent, snap = "out")
  }

  .terra_rrm_require_aligned_rasters(rasters)

  stacked <- rasters[[1]]
  for (i in seq_along(rasters)[-1L]) {
    stacked <- c(stacked, rasters[[i]])
  }

  if (is.null(filename)) {
    return(stacked)
  }

  terra::writeRaster(stacked, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}