library(sf)
library(terra)

build_mock_large_elevation <- function(
  vector_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
  vector_layers = c("BEM", "vri"),
  reference_dem_dsn = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif",
  output_dem_dsn = "../SSGBM-VRI-BEM-data/mock_dem_large.tif",
  output_stack_dsn = "../SSGBM-VRI-BEM-data/mock_elevation_large.tif",
  same_extent_as_reference = TRUE,
  refine_factor = 2,
  expansion_factor = 1.25,
  extra_buffer_m = 0,
  overwrite = TRUE
) {
  stopifnot(expansion_factor >= 1)
  stopifnot(refine_factor >= 1)
  stopifnot(length(vector_layers) >= 1)

  ref_dem <- terra::rast(reference_dem_dsn)

  layer_bboxes <- lapply(
    vector_layers,
    function(layer_name) sf::st_bbox(sf::st_read(vector_dsn, layer = layer_name, quiet = TRUE))
  )

  xmin_all <- min(vapply(layer_bboxes, function(x) unname(x[["xmin"]]), numeric(1)))
  xmax_all <- max(vapply(layer_bboxes, function(x) unname(x[["xmax"]]), numeric(1)))
  ymin_all <- min(vapply(layer_bboxes, function(x) unname(x[["ymin"]]), numeric(1)))
  ymax_all <- max(vapply(layer_bboxes, function(x) unname(x[["ymax"]]), numeric(1)))

  width_all <- xmax_all - xmin_all
  height_all <- ymax_all - ymin_all
  center_x <- (xmin_all + xmax_all) / 2
  center_y <- (ymin_all + ymax_all) / 2

  target_extent <- terra::ext(
    center_x - (width_all * expansion_factor) / 2 - extra_buffer_m,
    center_x + (width_all * expansion_factor) / 2 + extra_buffer_m,
    center_y - (height_all * expansion_factor) / 2 - extra_buffer_m,
    center_y + (height_all * expansion_factor) / 2 + extra_buffer_m
  )

  if (same_extent_as_reference) {
    template_extent <- terra::ext(ref_dem)
  } else {
    template_extent <- terra::ext(terra::extend(ref_dem[[1]], target_extent, snap = "out"))
  }

  template <- terra::rast(
    xmin = template_extent[1],
    xmax = template_extent[2],
    ymin = template_extent[3],
    ymax = template_extent[4],
    resolution = terra::res(ref_dem) / refine_factor,
    crs = terra::crs(ref_dem)
  )
  x_min <- template_extent[1]
  x_max <- template_extent[2]
  y_min <- template_extent[3]
  y_max <- template_extent[4]

  x_raster <- terra::init(template, "x")
  y_raster <- terra::init(template, "y")
  write_options <- list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "BIGTIFF=YES")
  )

  mock_dem <- terra::lapp(
    c(x_raster, y_raster),
    fun = function(x, y) {
      nx <- (x - x_min) / (x_max - x_min)
      ny <- (y - y_min) / (y_max - y_min)

      700 +
        1100 * ny +
        220 * sin(nx * 6 * pi) +
        140 * cos(ny * 4 * pi) +
        90 * sin((nx + ny) * 8 * pi)
    },
    filename = output_dem_dsn,
    overwrite = overwrite,
    wopt = write_options
  )
  names(mock_dem) <- "ELEV"

  terrain_layers <- terra::terrain(
    mock_dem,
    v = c("slope", "aspect"),
    unit = "radians",
    filename = tempfile(fileext = ".tif"),
    overwrite = overwrite,
    wopt = write_options
  )
  names(terrain_layers) <- c("MEAN_SLOPE", "MEAN_ASP")

  mock_stack <- c(mock_dem, terrain_layers)
  terra::writeRaster(mock_stack, output_stack_dsn, overwrite = overwrite, wopt = write_options)

  invisible(
    list(
      dem = mock_dem,
      stack = mock_stack,
      target_extent = terra::ext(mock_stack),
      resolution = terra::res(mock_stack)
    )
  )
}

result <- build_mock_large_elevation()

print(result$target_extent)
print(result$resolution)
print(dim(result$stack))
print(names(result$stack))