#' Terra helpers for the new raster-only RRM path
#'
#' These helpers are the foundation for a terra-native pipeline that starts
#' from already-materialized aligned rasters on disk. They do not modify the
#' legacy sf/data.table or DuckDB workflows.

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
                                        filename = NULL,
                                        overwrite = FALSE) {
  raster_conv <- .terra_rrm_get_raster_conv()

  vri_raster <- set_raster_levels_from_conv(terra::rast(vri_dsn), raster_conv$vri)
  bem_raster <- set_raster_levels_from_conv(terra::rast(bem_dsn), raster_conv$bem)

  .terra_rrm_stack_inputs(
    vri_raster = vri_raster,
    bem_raster = bem_raster,
    rivers_raster = terra::rast(rivers_dsn),
    wetlands_raster = terra::rast(wetlands_dsn),
    lakes_raster = if (is.null(lakes_dsn)) NULL else terra::rast(lakes_dsn),
    ccb_raster = if (is.null(ccb_dsn)) NULL else terra::rast(ccb_dsn),
    elevation_raster = if (is.null(elevation_dsn)) NULL else terra::rast(elevation_dsn),
    filename = filename,
    overwrite = overwrite
  )
}