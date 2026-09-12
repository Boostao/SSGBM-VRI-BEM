#' Terra helpers for the new raster-only RRM path
#'
#' These helpers are the foundation for a terra-native pipeline that starts
#' from already-materialized aligned rasters on disk. They do not modify the
#' legacy sf/data.table or DuckDB workflows.

#' Derive per-cell terrain layers from the stacked elevation layer
#'
#' Computes `ELEV`, `MEAN_SLOPE`, `MEAN_ASP`, `ABOVE_ELEV_THOLD`, and
#' `SLOPE_MOD` from the `elevation` layer already present in the input stack,
#' and adds them as new layers. Must be called after
#' `.terra_rrm_read_input_stack()` and before correction steps that depend on
#' terrain or elevation-threshold attributes.
#'
#' @param x A `SpatRaster` containing an `elevation` layer (raw DEM values in
#'   metres).
#' @param elevation_threshold Numeric. Elevation (m) above which
#'   `ABOVE_ELEV_THOLD` is set to `"Y"`.
#' @param terrain_raster Optional `SpatRaster` with `slope` and `aspect`
#'   layers in radians. When omitted, terrain layers are derived from
#'   `elevation` using [terra::terrain()].
#' @param filename Optional path to write the augmented stack to disk.
#' @param overwrite Logical passed to `terra::writeRaster`.
#' @return The input `SpatRaster` with five additional layers: `ELEV`,
#'   `MEAN_SLOPE`, `MEAN_ASP`, `ABOVE_ELEV_THOLD`, and `SLOPE_MOD`.
#' @export
terra_rrm_compute_terrain_layers <- function(x,
                                             elevation_threshold,
                                             terrain_raster = NULL,
                                             filename = NULL,
                                             overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.numeric(elevation_threshold), length(elevation_threshold) == 1L)

  if (!"elevation" %in% names(x)) {
    stop("terra_rrm_compute_terrain_layers: 'elevation' layer not found in input stack.", call. = FALSE)
  }

  required_for_slope_mod <- c("BGC_ZONE", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3")
  missing_for_slope_mod <- setdiff(required_for_slope_mod, names(x))
  if (length(missing_for_slope_mod) > 0L) {
    stop(
      sprintf(
        "terra_rrm_compute_terrain_layers: missing required layers for SLOPE_MOD derivation: %s",
        paste(missing_for_slope_mod, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  elev_layer <- x[["elevation"]]

  terrain_layers <- terrain_raster
  if (is.null(terrain_layers)) {
    terrain_layers <- terra::terrain(elev_layer, v = c("slope", "aspect"), unit = "radians")
  }
  if (!inherits(terrain_layers, "SpatRaster") || !all(c("slope", "aspect") %in% names(terrain_layers))) {
    stop("terrain_raster must be a SpatRaster with 'slope' and 'aspect' layers.", call. = FALSE)
  }
  slope_rad <- terrain_layers[["slope"]]
  aspect_rad <- terrain_layers[["aspect"]]

  # Slope in percent with the same conversion as merge_elevation_duckdb.
  mean_slope <- slope_rad * (57.29578 / 90 * 100)
  names(mean_slope) <- "MEAN_SLOPE"

  # Keep aspect undefined over flat cells (slope <= 0), mirroring sf/duckdb
  # behaviour that excludes flat terrain from circular means.
  mean_asp <- terra::ifel(
    !is.na(slope_rad) & slope_rad > 0,
    (aspect_rad * 57.29578 + 360) %% 360,
    NA
  )
  names(mean_asp) <- "MEAN_ASP"

  elev_out <- elev_layer
  names(elev_out) <- "ELEV"

  # ABOVE_ELEV_THOLD: "Y" where elevation > threshold, "N" otherwise.
  # Stored as an integer-coded factor (1 = "N", 2 = "Y") to match raster_conv.
  above_raw <- terra::ifel(elev_layer > elevation_threshold, 2L, 1L)
  names(above_raw) <- "ABOVE_ELEV_THOLD"
  levels(above_raw) <- data.frame(value = c(1L, 2L), label = c("N", "Y"))
  names(above_raw) <- "ABOVE_ELEV_THOLD"

  raster_conv <- .terra_rrm_get_raster_conv()
  no_slope_mod_MC <- c("LL", "LS", "LA", "La", "OW", "Pd", "PD", "RE", "RI", "Ri", "Wa", "WE", "Wm", "Ww", "Ws", "WL")
  excluded_s1 <- .terra_rrm_layer_codes(x, "BEUMC_S1", no_slope_mod_MC, raster_conv$bem, strict = FALSE)
  excluded_s2 <- .terra_rrm_layer_codes(x, "BEUMC_S2", no_slope_mod_MC, raster_conv$bem, strict = FALSE)
  excluded_s3 <- .terra_rrm_layer_codes(x, "BEUMC_S3", no_slope_mod_MC, raster_conv$bem, strict = FALSE)
  cwh_mh_codes <- .terra_rrm_layer_codes(x, "BGC_ZONE", c("CWH", "MH"), raster_conv$bem, strict = FALSE)

  is_excluded_beumc <-
    .terra_rrm_match_codes(x[["BEUMC_S1"]], excluded_s1) == 1 |
    .terra_rrm_match_codes(x[["BEUMC_S2"]], excluded_s2) == 1 |
    .terra_rrm_match_codes(x[["BEUMC_S3"]], excluded_s3) == 1
  is_cwh_mh <- .terra_rrm_match_codes(x[["BGC_ZONE"]], cwh_mh_codes) == 1
  asp_cool <- !is.na(mean_asp) & (mean_asp >= 285 | mean_asp <= 134)
  asp_warm <- !is.na(mean_asp) & (mean_asp >= 135 & mean_asp <= 284)

  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & is_cwh_mh & asp_cool & mean_slope >= 10 & mean_slope < 35, 1L, NA)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & is_cwh_mh & asp_cool & mean_slope >= 35 & mean_slope <= 100, 2L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & is_cwh_mh & asp_cool & mean_slope > 100, 3L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & is_cwh_mh & asp_warm & mean_slope >= 10 & mean_slope < 35, 1L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & is_cwh_mh & asp_warm & mean_slope >= 35 & mean_slope <= 100, 4L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & is_cwh_mh & asp_warm & mean_slope > 100, 5L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & !is_cwh_mh & asp_cool & mean_slope >= 10 & mean_slope < 25, 1L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & !is_cwh_mh & asp_cool & mean_slope >= 25 & mean_slope <= 100, 2L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & !is_cwh_mh & asp_cool & mean_slope > 100, 3L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & !is_cwh_mh & asp_warm & mean_slope >= 10 & mean_slope < 25, 1L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & !is_cwh_mh & asp_warm & mean_slope >= 25 & mean_slope <= 100, 4L, slope_mod)
  slope_mod <- terra::ifel(!is.na(mean_slope) & !is.na(mean_asp) & !is_excluded_beumc & !is_cwh_mh & asp_warm & mean_slope > 100, 5L, slope_mod)
  names(slope_mod) <- "SLOPE_MOD"
  levels(slope_mod) <- data.frame(
    value = c(1L, 2L, 3L, 4L, 5L),
    label = c("j", "k", "q", "w", "z"),
    stringsAsFactors = FALSE
  )
  names(slope_mod) <- "SLOPE_MOD"

  # Overwrite any existing terrain-derived layers while preserving all others.
  drop_existing <- intersect(names(x), c("ELEV", "MEAN_SLOPE", "MEAN_ASP", "ABOVE_ELEV_THOLD", "SLOPE_MOD"))
  if (length(drop_existing) > 0L) {
    x <- x[[setdiff(names(x), drop_existing)]]
  }

  # Only the new terrain layers are written — x is already on disk.
  # The returned SpatRaster is a zero-copy virtual multi-source stack.
  terrain_stack <- c(elev_out, mean_slope, mean_asp, above_raw, slope_mod)

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

    labels <- as.character(lookup_dt[["value"]])
    labels[is.na(labels)] <- ""

    levels(x[[layer_name]]) <- data.frame(
      value = lookup_dt[["factor"]],
      label = labels,
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