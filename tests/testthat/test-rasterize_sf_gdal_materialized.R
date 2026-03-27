test_that("rasterize_sf_gdal_materialized rasterizes numeric, character, date, and burn fields", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  source_sf <- sf::st_sf(
    num_field = c(10L, 20L),
    char_field = c("A", "B"),
    date_field = c("20240101", "20240102"),
    geometry = geom
  )

  source_file <- tempfile(fileext = ".gpkg")
  output_file <- tempfile(fileext = ".tif")
  sf::st_write(source_sf, source_file, layer = "src", quiet = TRUE)

  conv_list <- list(
    char_field = data.table::data.table(value = c("A", "B"), factor = c(1L, 2L))
  )

  result <- rasterize_sf_gdal_materialized(
    src_datasource = source_file,
    dst_filename = output_file,
    layer = "src",
    numeric_attributes = "num_field",
    character_attributes = "char_field",
    date_attributes = "date_field",
    factor_conv_list = conv_list,
    burn = "burn_field",
    te = c(0, 0, 2, 1),
    tr = c(1, 1),
    output_raster = TRUE,
    verbose = FALSE
  )

  expect_identical(names(result), c("num_field", "char_field", "date_field", "burn_field"))
  expect_equal(as.vector(terra::values(result[[1]])), c(10, 20))
  expect_equal(as.vector(terra::values(result[[2]])), c(1, 2))
  expect_equal(as.vector(terra::values(result[[3]])), c(20240101, 20240102))
  expect_equal(as.vector(terra::values(result[[4]])), c(1, 1))
})

test_that("rasterize_sf_gdal_materialized handles empty character lookup tables", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  geom <- sf::st_sfc(
    sf::st_multipolygon(list(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))),
    crs = 4326
  )

  source_sf <- sf::st_sf(
    char_field = "A",
    geometry = geom
  )

  source_file <- tempfile(fileext = ".gpkg")
  output_file <- tempfile(fileext = ".tif")
  sf::st_write(source_sf, source_file, layer = "src", quiet = TRUE)

  conv_list <- list(
    char_field = data.table::data.table(value = character(), factor = integer())
  )

  result <- rasterize_sf_gdal_materialized(
    src_datasource = source_file,
    dst_filename = output_file,
    layer = "src",
    character_attributes = "char_field",
    factor_conv_list = conv_list,
    te = c(0, 0, 1, 1),
    tr = c(1, 1),
    output_raster = TRUE,
    verbose = FALSE
  )

  expect_identical(names(result), "char_field")
  expect_equal(as.vector(terra::values(result[[1]])), 0)
})