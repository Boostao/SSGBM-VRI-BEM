library(testthat)
library(sf)

source(file.path("..", "..", "R", "utils.R"))
source(file.path("..", "..", "R", "read_layers.R"))
source(file.path("..", "..", "R", "download_layers.R"))

albers <- sf::st_crs(3005)

make_test_gpkg <- function(col_name = "TSA_NUMBER_DESCRIPTION",
                           col_val = "Test TSA 01",
                           layer = "test_layer") {
  tmp <- tempfile(fileext = ".gpkg")
  poly <- sf::st_sf(
    setNames(list(col_val), col_name),
    geom = sf::st_sfc(
      sf::st_polygon(list(cbind(
        c(-127, -126, -126, -127, -127),
        c(54, 54, 55, 55, 54)
      ))),
      crs = 4326
    )
  )
  sf::st_write(poly, tmp, layer = layer, quiet = TRUE)
  list(dsn = tmp, layer = layer)
}

make_download_test_gpkg <- function(columns, values, layer = "src") {
  tmp <- tempfile(fileext = ".gpkg")
  geom <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(-127, -126, -126, -127, -127),
      c(54, 54, 55, 55, 54)
    ))),
    crs = 4326
  )

  source_sf <- sf::st_sf(as.data.frame(values, stringsAsFactors = FALSE), geom = geom)
  names(source_sf)[seq_along(columns)] <- columns
  sf::st_write(source_sf, tmp, layer = layer, quiet = TRUE)

  list(dsn = tmp, layer = layer)
}

with_read_layer_bindings <- function(bindings, code) {
  target_env <- environment(resolve_resource)
  binding_names <- names(bindings)
  had_binding <- vapply(binding_names, exists, logical(1), envir = target_env, inherits = FALSE)

  old_bindings <- list()
  if (any(had_binding)) {
    old_names <- binding_names[had_binding]
    old_bindings <- lapply(old_names, get, envir = target_env, inherits = FALSE)
    names(old_bindings) <- old_names
  }

  for (binding_name in binding_names) {
    assign(binding_name, bindings[[binding_name]], envir = target_env)
  }

  on.exit({
    for (binding_name in binding_names[!had_binding]) {
      if (exists(binding_name, envir = target_env, inherits = FALSE)) {
        rm(list = binding_name, envir = target_env)
      }
    }

    for (binding_name in names(old_bindings)) {
      assign(binding_name, old_bindings[[binding_name]], envir = target_env)
    }
  }, add = TRUE)

  force(code)
}

with_download_bindings <- function(bindings, code) {
  target_env <- environment(download_vri)
  binding_names <- names(bindings)
  had_binding <- vapply(binding_names, exists, logical(1), envir = target_env, inherits = FALSE)

  old_bindings <- list()
  if (any(had_binding)) {
    old_names <- binding_names[had_binding]
    old_bindings <- lapply(old_names, get, envir = target_env, inherits = FALSE)
    names(old_bindings) <- old_names
  }

  for (binding_name in binding_names) {
    assign(binding_name, bindings[[binding_name]], envir = target_env)
  }

  on.exit({
    for (binding_name in binding_names[!had_binding]) {
      if (exists(binding_name, envir = target_env, inherits = FALSE)) {
        rm(list = binding_name, envir = target_env)
      }
    }

    for (binding_name in names(old_bindings)) {
      assign(binding_name, old_bindings[[binding_name]], envir = target_env)
    }
  }, add = TRUE)

  force(code)
}


test_that("download_bem materializes a local datasource to the requested path", {
  src <- make_download_test_gpkg(
    columns = c("TEIS_ID", "BGC_ZONE"),
    values = list(TEIS_ID = 1, BGC_ZONE = "BWBS")
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  result <- download_bem(out, dsn = src$dsn, layer = src$layer, overwrite = TRUE)

  expect_equal(result, out)
  expect_true(file.exists(out))

  bem <- sf::st_read(out, layer = "BEM", quiet = TRUE)
  expect_equal(nrow(bem), 1L)
  expect_true(all(c("TEIS_ID", "BGC_ZONE") %in% names(bem)))
})


test_that("download_vri falls back to the first local layer when the requested layer is absent", {
  src <- make_download_test_gpkg(
    columns = c("FEATURE_ID", "BCLCS_LEVEL_1", "SPECIES_CD_1"),
    values = list(FEATURE_ID = 1, BCLCS_LEVEL_1 = "V", SPECIES_CD_1 = "PL"),
    layer = "fc"
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  result <- download_vri(out, dsn = src$dsn, overwrite = TRUE)

  expect_equal(result, out)
  expect_true(file.exists(out))

  vri <- sf::st_read(out, layer = "VRI", quiet = TRUE)
  expect_equal(nrow(vri), 1L)
  expect_true(all(c("FEATURE_ID", "BCLCS_LV_1", "SPEC_CD_1") %in% names(vri)))
})


test_that("download_ccb renames harvest year for rasterization", {
  src <- make_download_test_gpkg(
    columns = "HARVEST_START_YEAR_CALENDAR",
    values = list(HARVEST_START_YEAR_CALENDAR = 2020)
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  result <- download_ccb(out, dsn = src$dsn, layer = src$layer, overwrite = TRUE)

  expect_equal(result, out)

  ccb <- sf::st_read(out, layer = "CCB", quiet = TRUE)
  expect_true("HARVESTYR" %in% names(ccb))
  expect_equal(ccb$HARVESTYR, 2020)
})


test_that("download_vri keeps BC Data Catalog downloads query-backed", {
  src <- make_download_test_gpkg(
    columns = c("FEATURE_ID", "BCLCS_LEVEL_1", "SPECIES_CD_1"),
    values = list(FEATURE_ID = 1, BCLCS_LEVEL_1 = "V", SPECIES_CD_1 = "PL"),
    layer = "src"
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  with_download_bindings(
    list(
      collect_geojson = function(...) src$dsn,
      resolve_resource = function(...) stop("resolve_resource should not be called", call. = FALSE),
      number_of_records = function(...) 50001L
    ),
    {
      result <- download_vri(out, dsn = NULL, overwrite = TRUE)
      expect_equal(result, out)
    }
  )

  vri <- sf::st_read(out, layer = "VRI", quiet = TRUE)
  expect_equal(nrow(vri), 1L)
  expect_true(all(c("FEATURE_ID", "BCLCS_LV_1", "SPEC_CD_1") %in% names(vri)))
})


test_that("download_vri falls back to the BC Data Catalog resource when query materialization fails", {
  src <- make_download_test_gpkg(
    columns = c("FEATURE_ID", "BCLCS_LEVEL_1", "SPECIES_CD_1"),
    values = list(FEATURE_ID = 1, BCLCS_LEVEL_1 = "V", SPECIES_CD_1 = "PL"),
    layer = "fc"
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  with_download_bindings(
    list(
      collect_geojson = function(...) stop("wfs failure", call. = FALSE),
      resolve_resource = function(...) src$dsn
    ),
    {
      result <- download_vri(out, dsn = NULL, overwrite = TRUE)
      expect_equal(result, out)
    }
  )

  vri <- sf::st_read(out, layer = "VRI", quiet = TRUE)
  expect_equal(nrow(vri), 1L)
  expect_true(all(c("FEATURE_ID", "BCLCS_LV_1", "SPEC_CD_1") %in% names(vri)))
})


test_that("download_ccb keeps BC Data Catalog downloads query-backed", {
  src <- make_download_test_gpkg(
    columns = "HARVEST_START_YEAR_CALENDAR",
    values = list(HARVEST_START_YEAR_CALENDAR = 2020),
    layer = "src"
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  with_download_bindings(
    list(
      collect_geojson = function(...) src$dsn,
      resolve_resource = function(...) stop("resolve_resource should not be called", call. = FALSE),
      number_of_records = function(...) 10001L
    ),
    {
      result <- download_ccb(out, dsn = NULL, overwrite = TRUE)
      expect_equal(result, out)
    }
  )

  ccb <- sf::st_read(out, layer = "CCB", quiet = TRUE)
  expect_true("HARVESTYR" %in% names(ccb))
  expect_equal(ccb$HARVESTYR, 2020)
})


test_that("download_ccb falls back to the BC Data Catalog resource when query materialization fails", {
  src <- make_download_test_gpkg(
    columns = "HARVEST_START_YEAR_CALENDAR",
    values = list(HARVEST_START_YEAR_CALENDAR = 2020),
    layer = "src"
  )
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  with_download_bindings(
    list(
      collect_geojson = function(...) stop("wfs failure", call. = FALSE),
      resolve_resource = function(...) src$dsn
    ),
    {
      result <- download_ccb(out, dsn = NULL, overwrite = TRUE)
      expect_equal(result, out)
    }
  )

  ccb <- sf::st_read(out, layer = "CCB", quiet = TRUE)
  expect_true("HARVESTYR" %in% names(ccb))
  expect_equal(ccb$HARVESTYR, 2020)
})


test_that("download_tsa filters and writes a local TSA datasource", {
  src <- make_test_gpkg(col_val = "Mackenzie TSA", layer = "tsa_src")
  out <- tempfile(fileext = ".gpkg")

  on.exit(unlink(c(src$dsn, out)), add = TRUE)

  result <- download_tsa(
    out,
    tsa_name = "Mackenzie TSA",
    tsa_dsn = src$dsn,
    overwrite = TRUE
  )

  expect_equal(result, out)

  tsa <- sf::st_read(out, layer = "TSA", quiet = TRUE)
  expect_equal(nrow(tsa), 1L)
  expect_equal(tsa$TSA_NUMBER_DESCRIPTION, "Mackenzie TSA")
})


test_that("resolve_resource rebuilds an empty cache directory", {
  fake_cache <- tempfile("resource-cache-")
  resource_id <- "resource-id"
  stale_dir <- file.path(fake_cache, resource_id)
  dir.create(stale_dir, recursive = TRUE)

  on.exit(unlink(fake_cache, recursive = TRUE), add = TRUE)

  resource_tbl <- data.frame(
    format = "fgdb",
    url = "https://example.com/veg_comp_lyr_r1_poly_2024.gdb.zip",
    id = resource_id,
    stringsAsFactors = FALSE
  )

  result <- with_read_layer_bindings(
    list(
      cache_dir = function() fake_cache,
      download.file = function(url, destfile, mode) writeBin(charToRaw("zip"), destfile),
      unzip = function(zipfile, exdir) dir.create(file.path(exdir, "veg_comp_lyr_r1_poly_2024.gdb"), recursive = TRUE)
    ),
    resolve_resource(resource_tbl)
  )

  expect_true(dir.exists(result))
  expect_match(result, "veg_comp_lyr_r1_poly_2024\\.gdb$")
})


test_that("cached_resource_path finds an extracted geodatabase", {
  cache_dir_path <- tempfile("cached-resource-")
  dir.create(cache_dir_path, recursive = TRUE)
  gdb_path <- file.path(cache_dir_path, "veg_comp_lyr_r1_poly_2024.gdb")
  dir.create(gdb_path)

  on.exit(unlink(cache_dir_path, recursive = TRUE), add = TRUE)

  result <- cached_resource_path(cache_dir_path, "https://example.com/veg_comp_lyr_r1_poly_2024.gdb.zip")

  expect_equal(result, gdb_path)
})