source(file.path("..", "..", "R", "terra_rrm_corrections.R"))
source(file.path("..", "..", "R", "terra_rrm_forest_age.R"))
source(file.path("..", "..", "R", "terra_rrm_ecosystem_merge.R"))
source(file.path("..", "..", "R", "terra_rrm_crown.R"))
source(file.path("..", "..", "R", "terra_rrm_export.R"))
source(file.path("..", "..", "R", "format_unique_ecosystem_dt.R"))
source(file.path("..", "..", "R", "read_unique_ecosystem_dt.R"))
source(file.path("..", "..", "R", "terra_rrm_pipeline.R"))
source(file.path("..", "..", "R", "terra_rrm_orchestration.R"))

if (FALSE) {
  stub_state <- NULL
  raster_conv <- NULL
}

raster_conv <- .terra_rrm_get_raster_conv()

test_that("terra_rrm_prepare_ecosystem_stack runs stages in the expected order", {
  skip_if_not_installed("terra")

  make_raster <- function(name, value) {
    raster <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
    terra::values(raster) <- value
    names(raster) <- name
    raster
  }

  raster <- c(
    make_raster("lakes", 1),
    make_raster("ELEV", 1500),
    make_raster("MEAN_SLOPE", 30),
    make_raster("MEAN_ASP", 90),
    make_raster("ABOVE_ELEV_THOLD", 2),
    make_raster("SLOPE_MOD", 2)
  )

  stub_state <- new.env(parent = emptyenv())
  stub_state$calls <- character()
  fn_env <- environment(terra_rrm_prepare_ecosystem_stack)
  stub_names <- c(
    "terra_rrm_correct_bem_from_vri",
    "terra_rrm_correct_small_lakes",
    "terra_rrm_correct_bem_from_wetlands",
    "terra_rrm_correct_bem_from_wetlands_riparian_stage",
    "terra_rrm_apply_rules",
    "terra_rrm_add_ecosystem_keys",
    "terra_rrm_calc_forest_age_class",
    "terra_rrm_merge_unique_ecosystem_fields",
    "terra_rrm_find_crown_area_dominant_values"
  )
  originals <- mget(stub_names, envir = fn_env, inherits = TRUE)
  on.exit(list2env(originals, envir = fn_env), add = TRUE)

  assign("terra_rrm_correct_bem_from_vri", function(x, clear_site_ma = TRUE, use_ifelse = TRUE, ...) {
    stub_state$calls <- c(stub_state$calls, "vri")
    x
  }, envir = fn_env)
  assign("terra_rrm_correct_small_lakes", function(x, lake_layer = "lakes", ...) {
    stub_state$calls <- c(stub_state$calls, paste0("lakes:", lake_layer))
    x
  }, envir = fn_env)
  assign("terra_rrm_correct_bem_from_wetlands", function(x, buc, ...) {
    stub_state$calls <- c(stub_state$calls, "wetlands")
    x
  }, envir = fn_env)
  assign("terra_rrm_correct_bem_from_wetlands_riparian_stage", function(x, ...) {
    stub_state$calls <- c(stub_state$calls, "riparian")
    x
  }, envir = fn_env)
  assign("terra_rrm_apply_rules", function(x, rules_dt, ...) {
    stub_state$calls <- c(stub_state$calls, "rules")
    x
  }, envir = fn_env)
  assign("terra_rrm_add_ecosystem_keys", function(x, ...) {
    stub_state$calls <- c(stub_state$calls, "keys")
    x
  }, envir = fn_env)
  assign("terra_rrm_calc_forest_age_class", function(x, most_recent_harvest_year, ...) {
    stub_state$calls <- c(stub_state$calls, paste0("age:", most_recent_harvest_year))
    x
  }, envir = fn_env)
  assign("terra_rrm_merge_unique_ecosystem_fields", function(x, unique_ecosystem_dt, ...) {
    stub_state$calls <- c(stub_state$calls, "merge")
    x
  }, envir = fn_env)
  assign("terra_rrm_find_crown_area_dominant_values", function(x, ...) {
    stub_state$calls <- c(stub_state$calls, "crown")
    x
  }, envir = fn_env)

  result <- terra_rrm_prepare_ecosystem_stack(
    x = raster,
    buc = data.frame(Code_Orig = numeric()),
    rules_dt = data.frame(INPUTS = character(), OUTPUTS = character()),
    unique_ecosystem_dt = data.frame(BGC_ZONE = character()),
    most_recent_harvest_year = 2024
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(stub_state$calls, c("vri", "lakes:lakes", "wetlands", "riparian", "rules", "keys", "age:2024", "merge", "crown"))
})

test_that("terra_rrm_create_RRM_ecosystem returns requested summaries and optional stack", {
  skip_if_not_installed("terra")

  raster <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  terra::values(raster) <- 1
  names(raster) <- "dummy"

  fn_env <- environment(terra_rrm_create_RRM_ecosystem)
  stub_names <- c(
    "terra_rrm_prepare_ecosystem_stack",
    "terra_rrm_create_RRM_ecosystem_moose",
    "terra_rrm_create_RRM_ecosystem_bear"
  )
  originals <- mget(stub_names, envir = fn_env, inherits = TRUE)
  on.exit(list2env(originals, envir = fn_env), add = TRUE)

  stub_prepare <- local({
    prepared_raster <- raster
    function(...) prepared_raster
  })

  assign("terra_rrm_prepare_ecosystem_stack", stub_prepare, envir = fn_env)
  assign("terra_rrm_create_RRM_ecosystem_moose", function(x) data.table::data.table(kind = "moose"), envir = fn_env)
  assign("terra_rrm_create_RRM_ecosystem_bear", function(x) data.table::data.table(kind = "bear"), envir = fn_env)

  single_result <- terra_rrm_create_RRM_ecosystem(
    x = raster,
    buc = data.frame(Code_Orig = numeric()),
    rules_dt = data.frame(INPUTS = character(), OUTPUTS = character()),
    unique_ecosystem_dt = data.frame(BGC_ZONE = character()),
    most_recent_harvest_year = 2024,
    kind = "moose"
  )
  expect_s3_class(single_result, "data.table")
  expect_equal(single_result$kind, "moose")

  multi_result <- terra_rrm_create_RRM_ecosystem(
    x = raster,
    buc = data.frame(Code_Orig = numeric()),
    rules_dt = data.frame(INPUTS = character(), OUTPUTS = character()),
    unique_ecosystem_dt = data.frame(BGC_ZONE = character()),
    most_recent_harvest_year = 2024,
    kind = c("moose", "bear"),
    return_stack = TRUE
  )

  expect_equal(names(multi_result), c("moose", "bear", "raster"))
  expect_equal(multi_result$moose$kind, "moose")
  expect_equal(multi_result$bear$kind, "bear")
  expect_s4_class(multi_result$raster, "SpatRaster")
})

test_that("terra_rrm_create_RRM_ecosystem_from_rasters reads disk rasters and forwards support tables", {
  skip_if_not_installed("terra")

  make_raster <- function(name, value) {
    raster <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
    terra::values(raster) <- value
    names(raster) <- name
    raster
  }

  write_named_raster <- function(name, value) {
    raster <- make_raster(name, value)
    path <- tempfile(pattern = paste0(name, "_"), fileext = ".tif")
    terra::writeRaster(raster, path, overwrite = TRUE)
    path
  }

  vri_dsn <- write_named_raster("BCLCS_LV_1", 1L)
  bem_dsn <- write_named_raster("BEUMC_S1", 1L)
  rivers_dsn <- write_named_raster("rivers", 0L)
  wetlands_dsn <- write_named_raster("wl_pct", 0L)
  lakes_dsn <- write_named_raster("lakes", 0L)
  ccb_dsn <- write_named_raster("HARVESTYR", 2020L)

  wetland_csv <- tempfile(fileext = ".csv")
  data.table::fwrite(data.table::data.table(Code_Orig = 1110, Code_WL0 = 1110), wetland_csv)

  beu_bec_csv <- tempfile(fileext = ".csv")
  data.table::fwrite(data.table::data.table(BGC_Subzone = "ICHmc", BEU = "XX", Script_Rule = "Error", Change_to_BEU = "YY"), beu_bec_csv)

  unique_csv <- tempfile(fileext = ".csv")
  data.table::fwrite(data.table::data.table(BGC_VRT = "0", BGC_PHASE = ""), unique_csv)

  stub_state <- new.env(parent = emptyenv())
  stub_state$args <- NULL
  fn_env <- environment(terra_rrm_create_RRM_ecosystem_from_rasters)
  original <- get("terra_rrm_create_RRM_ecosystem", envir = fn_env, inherits = TRUE)
  on.exit(assign("terra_rrm_create_RRM_ecosystem", original, envir = fn_env), add = TRUE)

  assign("terra_rrm_create_RRM_ecosystem", function(x,
                                                     buc,
                                                     beu_bec,
                                                     rules_dt,
                                                     unique_ecosystem_dt,
                                                     most_recent_harvest_year,
                                                     elevation_threshold,
                                                     kind,
                                                     lake_layer,
                                                     apply_small_lakes,
                                                     clear_site_ma,
                                                     use_ifelse,
                                                     return_stack,
                                                     stack_filename,
                                                     overwrite) {
    stub_state$args <- list(
      names = names(x),
      buc = buc,
      beu_bec = beu_bec,
      rules_dt = rules_dt,
      unique_ecosystem_dt = unique_ecosystem_dt,
      most_recent_harvest_year = most_recent_harvest_year,
      elevation_threshold = elevation_threshold,
      kind = kind,
      lake_layer = lake_layer,
      apply_small_lakes = apply_small_lakes,
      clear_site_ma = clear_site_ma,
      use_ifelse = use_ifelse,
      return_stack = return_stack,
      stack_filename = stack_filename,
      overwrite = overwrite
    )
    data.table::data.table(kind = "moose")
  }, envir = fn_env)

  result <- terra_rrm_create_RRM_ecosystem_from_rasters(
    vri_dsn = vri_dsn,
    bem_dsn = bem_dsn,
    rivers_dsn = rivers_dsn,
    wetlands_dsn = wetlands_dsn,
    lakes_dsn = lakes_dsn,
    ccb_dsn = ccb_dsn,
    rules_xl = "rules.xlsx",
    most_recent_harvest_year = 2024,
    elevation_threshold = 1500,
    kind = "moose",
    beu_bec_csv = beu_bec_csv,
    beu_wetland_update_csv = wetland_csv,
    unique_ecosystem = unique_csv,
    return_stack = TRUE,
    stack_filename = "prepared.tif",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$kind, "moose")
  expect_equal(
    stub_state$args$names,
    c("BCLCS_LV_1", "BEUMC_S1", "rivers", "wl_pct", "lakes", "HARVESTYR")
  )
  expect_true(is.data.frame(stub_state$args$buc))
  expect_true(is.data.frame(stub_state$args$beu_bec))
  expect_true(is.data.frame(stub_state$args$unique_ecosystem_dt))
  expect_equal(stub_state$args$rules_dt, "rules.xlsx")
  expect_equal(stub_state$args$most_recent_harvest_year, 2024)
  expect_equal(stub_state$args$elevation_threshold, 1500)
  expect_equal(stub_state$args$kind, "moose")
  expect_equal(stub_state$args$lake_layer, "lakes")
  expect_true(stub_state$args$apply_small_lakes)
  expect_true(stub_state$args$return_stack)
  expect_equal(stub_state$args$stack_filename, "prepared.tif")
})