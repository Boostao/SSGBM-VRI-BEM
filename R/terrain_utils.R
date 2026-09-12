#' Prepare slope/aspect/TRI terrain layers from a DEM
#'
#' Internal helper that ensures a terrain raster contains `slope`, `aspect`,
#' and `TRI` layers. If a caller supplies only `slope`/`aspect`, the missing
#' `TRI` layer is derived from the DEM to preserve backward compatibility.
#'
#' @param elev_raster `SpatRaster` DEM used to derive missing terrain layers.
#' @param terrain_raster Optional `SpatRaster` containing terrain layers.
#' @return `SpatRaster` with layers ordered as `slope`, `aspect`, `TRI`.
prepare_terrain_raster <- function(elev_raster, terrain_raster = NULL) {
  if (is.null(terrain_raster)) {
    return(terra::terrain(elev_raster, v = c("slope", "aspect", "TRI"), unit = "radians"))
  }

  terrain_names <- names(terrain_raster)
  required_names <- c("slope", "aspect")
  missing_names <- setdiff(required_names, terrain_names)
  if (length(missing_names) > 0L) {
    stop(
      "terrain_raster must contain layers named ",
      paste(shQuote(required_names), collapse = ", "),
      ". Missing: ",
      paste(shQuote(missing_names), collapse = ", "),
      call. = FALSE
    )
  }

  if (!"TRI" %in% terrain_names) {
    terrain_raster <- c(terrain_raster, terra::terrain(elev_raster, v = "TRI"))
    terrain_names <- names(terrain_raster)
  }

  terrain_raster[[match(c("slope", "aspect", "TRI"), terrain_names)]]
}