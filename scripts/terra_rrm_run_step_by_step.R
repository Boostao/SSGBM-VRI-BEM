find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "DESCRIPTION"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find the package root from the current working directory.", call. = FALSE)
    }

    current <- parent
  }
}


load_ssgbm_package <- function(repo_root) {
  if ("SSGBM.VRI.BEM" %in% loadedNamespaces()) {
    return(invisible(TRUE))
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(repo_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    return(invisible(TRUE))
  }

  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(repo_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    return(invisible(TRUE))
  }

  stop(
    "Install 'pkgload' or 'devtools' to run this script from the repository checkout.",
    call. = FALSE
  )
}


repo_root <- find_repo_root()
load_ssgbm_package(repo_root)

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
})


# Edit these paths to match your local data. You can either:
# 1. set rasterize_inputs <- TRUE and build fresh aligned rasters with the
#    materialized rasterization functions, or
# 2. set rasterize_inputs <- FALSE and point the *_dsn paths at existing rasters.
data_root <- normalizePath(file.path(repo_root, "..", "SSGBM-VRI-BEM-data"), winslash = "/", mustWork = FALSE)
output_root <- file.path(repo_root, "terra_pipeline_step_outputs")
raster_output_root <- file.path(output_root, "rasterized_inputs")

rasterize_inputs <- TRUE
materialized_verbose <- TRUE

reference_raster <- file.path(repo_root, "..", "elev_test.tif")

vri_src_datasource <- file.path(data_root, "VEG_COMP_LYR_R1_POLY")
vri_layer <- "VEG_COMP_LYR_R1_POLY"

bem_src_datasource <- file.path(data_root, "BEM_VRI")
bem_layer <- "BEM"

hydro_src_datasource <- file.path(data_root, "CodeWithUs.gdb")
wetlands_layer <- "FWA_WETLANDS_POLY"
rivers_layer <- "FWA_RIVERS_POLY"
lakes_layer_source <- "FWA_LAKES_POLY"
ccb_layer <- "Cut_Block_all_BC"

vri_dsn <- file.path(raster_output_root, "vri_materialized.tif")
bem_dsn <- file.path(raster_output_root, "bem_materialized.tif")
rivers_dsn <- file.path(raster_output_root, "rivers_materialized.tif")
wetlands_dsn <- file.path(raster_output_root, "wetlands_materialized.tif")
lakes_dsn <- file.path(raster_output_root, "lakes_materialized.tif")
ccb_dsn <- file.path(raster_output_root, "ccb_materialized.tif")
elevation_dsn <- file.path(repo_root, "..", "elev_test.tif")

rules_xl <- file.path(data_root, "Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx")
beu_bec_csv <- file.path(repo_root, "inst", "csv", "Allowed_BEC_BEUs_NE_ALL.csv")
beu_wetland_update_csv <- file.path(repo_root, "inst", "csv", "beu_wetland_updates.csv")
unique_ecosystem_csv <- file.path(repo_root, "inst", "csv", "Skeena_VRIBEM_LUT.csv")

most_recent_harvest_year <- as.integer(format(Sys.Date(), "%Y"))
lake_layer <- "lakes"
apply_small_lakes <- file.exists(lakes_dsn)
write_stage_rasters <- TRUE
run_moose_export <- TRUE
run_bear_export <- TRUE
max_preview_rows <- 10L

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(raster_output_root, recursive = TRUE, showWarnings = FALSE)


ns_fn <- function(name) {
  get(name, envir = asNamespace("SSGBM.VRI.BEM"))
}


read_input_stack <- ns_fn(".terra_rrm_read_input_stack")
apply_river_adjacency_stage <- ns_fn(".terra_rrm_apply_river_adjacency_stage")


timing_log <- list()


write_stage_raster <- function(x, step_id) {
  file_path <- file.path(output_root, sprintf("%s.tif", step_id))
  terra::writeRaster(x, file_path, overwrite = TRUE)
  file_path
}


layer_preview <- function(x, layer_name, max_rows = 10L) {
  if (!layer_name %in% names(x)) {
    message(sprintf("  - %s: layer not present", layer_name))
    return(invisible(NULL))
  }

  freq_dt <- terra::freq(x[[layer_name]])
  if (is.null(freq_dt) || nrow(freq_dt) == 0L) {
    message(sprintf("  - %s: no non-missing values", layer_name))
    return(invisible(NULL))
  }

  freq_dt <- as.data.table(freq_dt)
  cats <- terra::cats(x[[layer_name]])[[1]]
  if (!is.null(cats) && ncol(cats) >= 2L) {
    cats_dt <- as.data.table(cats)
    setnames(cats_dt, names(cats_dt)[1:2], c("value", "label"))
    freq_dt <- cats_dt[freq_dt, on = "value"]
  }

  if (!"label" %in% names(freq_dt)) {
    freq_dt[["label"]] <- as.character(freq_dt[["value"]])
  }

  freq_dt <- freq_dt[order(-freq_dt[["count"]])]
  message(sprintf("  - %s", layer_name))
  print(freq_dt[seq_len(min(nrow(freq_dt), max_rows))])
  invisible(freq_dt)
}


changed_cell_count <- function(before, after, layer_name) {
  if (!layer_name %in% names(before) || !layer_name %in% names(after)) {
    return(NA_real_)
  }

  change_mask <- terra::ifel(
    is.na(before[[layer_name]]) & is.na(after[[layer_name]]),
    NA,
    terra::ifel(
      (is.na(before[[layer_name]]) & !is.na(after[[layer_name]])) |
        (!is.na(before[[layer_name]]) & is.na(after[[layer_name]])) |
        (before[[layer_name]] != after[[layer_name]]),
      1,
      NA
    )
  )

  as.numeric(terra::global(terra::ifel(!is.na(change_mask), 1, 0), fun = "sum", na.rm = TRUE)[1, 1])
}


report_layer_changes <- function(before, after, layer_names) {
  if (is.null(before) || length(layer_names) == 0L) {
    return(invisible(NULL))
  }

  message("Changed cells in selected layers:")
  for (layer_name in layer_names) {
    changed_n <- changed_cell_count(before, after, layer_name)
    if (is.na(changed_n)) {
      message(sprintf("  - %s: layer not comparable", layer_name))
    } else {
      message(sprintf("  - %s: %s cells changed", layer_name, format(changed_n, big.mark = ",", scientific = FALSE)))
    }
  }
}


run_timed_step <- function(step_id,
                           label,
                           expr,
                           previous = NULL,
                           inspect_layers = character()) {
  message(sprintf("\n[%s] %s", step_id, label))
  started_at <- Sys.time()
  value <- eval.parent(substitute(expr))
  elapsed_sec <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

  timing_log[[length(timing_log) + 1L]] <<- data.table(
    step_id = step_id,
    step = label,
    seconds = elapsed_sec
  )

  message(sprintf("Completed in %.2f seconds", elapsed_sec))

  if (inherits(value, "SpatRaster")) {
    report_layer_changes(previous, value, inspect_layers)

    if (length(inspect_layers) > 0L) {
      message("Top values for selected layers:")
      for (layer_name in inspect_layers) {
        layer_preview(value, layer_name, max_rows = max_preview_rows)
      }
    }

    if (isTRUE(write_stage_rasters)) {
      output_file <- write_stage_raster(value, step_id)
      message(sprintf("Wrote stage raster: %s", output_file))
    }
  }

  value
}


stopifnot(file.exists(vri_dsn))
stopifnot(file.exists(rules_xl))
stopifnot(file.exists(beu_bec_csv))
stopifnot(file.exists(beu_wetland_update_csv))
stopifnot(file.exists(unique_ecosystem_csv))

if (isTRUE(rasterize_inputs)) {
  stopifnot(file.exists(reference_raster))
  stopifnot(file.exists(vri_src_datasource))
  stopifnot(file.exists(bem_src_datasource))
  stopifnot(file.exists(hydro_src_datasource))
}

if (!isTRUE(rasterize_inputs)) {
  stopifnot(file.exists(vri_dsn))
  stopifnot(file.exists(bem_dsn))
  stopifnot(file.exists(rivers_dsn))
  stopifnot(file.exists(wetlands_dsn))
}

if (!file.exists(lakes_dsn)) {
  lakes_dsn <- NULL
  apply_small_lakes <- FALSE
}

if (!file.exists(ccb_dsn)) {
  ccb_dsn <- NULL
}

if (!file.exists(elevation_dsn)) {
  elevation_dsn <- NULL
}


# 0 ---- support tables
buc <- fread(beu_wetland_update_csv)
beu_bec <- fread(beu_bec_csv)
unique_ecosystem_dt <- read_unique_ecosystem_dt(unique_ecosystem_csv)


# 0a ---- materialized rasterization
if (isTRUE(rasterize_inputs)) {
  step_00a_vri_raster <- run_timed_step(
    step_id = "00a_rasterize_vri",
    label = "Rasterize VRI with rasterize_vri_materialized()",
    expr = rasterize_vri_materialized(
      src_datasource = vri_src_datasource,
      dst_filename = vri_dsn,
      layer = vri_layer,
      reference = reference_raster,
      output_raster = FALSE,
      verbose = materialized_verbose
    )
  )

  step_00b_bem_raster <- run_timed_step(
    step_id = "00b_rasterize_bem",
    label = "Rasterize BEM with rasterize_bem_materialized()",
    expr = rasterize_bem_materialized(
      src_datasource = bem_src_datasource,
      dst_filename = bem_dsn,
      layer = bem_layer,
      reference = reference_raster,
      output_raster = FALSE,
      verbose = materialized_verbose
    )
  )

  step_00c_wetlands_raster <- run_timed_step(
    step_id = "00c_rasterize_wetlands",
    label = "Rasterize wetlands with rasterize_wetlands_materialized()",
    expr = rasterize_wetlands_materialized(
      src_datasource = hydro_src_datasource,
      dst_filename = wetlands_dsn,
      layer = wetlands_layer,
      reference = reference_raster,
      output_raster = FALSE,
      verbose = materialized_verbose
    )
  )

  step_00d_rivers_raster <- run_timed_step(
    step_id = "00d_rasterize_rivers",
    label = "Rasterize rivers with rasterize_rivers_materialized()",
    expr = rasterize_rivers_materialized(
      src_datasource = hydro_src_datasource,
      dst_filename = rivers_dsn,
      layer = rivers_layer,
      reference = reference_raster,
      output_raster = FALSE,
      verbose = materialized_verbose
    )
  )

  if (nzchar(lakes_layer_source)) {
    step_00e_lakes_raster <- run_timed_step(
      step_id = "00e_rasterize_lakes",
      label = "Rasterize lakes with rasterize_sf_gdal_materialized()",
      expr = rasterize_sf_gdal_materialized(
        src_datasource = hydro_src_datasource,
        dst_filename = lakes_dsn,
        layer = lakes_layer_source,
        reference = reference_raster,
        burn = "lakes",
        output_raster = FALSE,
        verbose = materialized_verbose
      )
    )
  }

  if (nzchar(ccb_layer)) {
    step_00f_ccb_raster <- run_timed_step(
      step_id = "00f_rasterize_ccb",
      label = "Rasterize cutblocks with rasterize_ccb_materialized()",
      expr = rasterize_ccb_materialized(
        src_datasource = hydro_src_datasource,
        dst_filename = ccb_dsn,
        layer = ccb_layer,
        reference = reference_raster,
        output_raster = FALSE,
        verbose = materialized_verbose
      )
    )
  }
}

stopifnot(file.exists(vri_dsn))
stopifnot(file.exists(bem_dsn))
stopifnot(file.exists(rivers_dsn))
stopifnot(file.exists(wetlands_dsn))


# 1 ---- read and stack aligned raster inputs
step_01_input_stack <- run_timed_step(
  step_id = "01_input_stack",
  label = "Read aligned raster stack",
  expr = read_input_stack(
    vri_dsn = vri_dsn,
    bem_dsn = bem_dsn,
    rivers_dsn = rivers_dsn,
    wetlands_dsn = wetlands_dsn,
    lakes_dsn = lakes_dsn,
    ccb_dsn = ccb_dsn,
    elevation_dsn = elevation_dsn
  ),
  inspect_layers = c("BEUMC_S1", "SDEC_1", "BGC_ZONE", "rivers", "wl_pct")
)


# 2 ---- VRI corrections
step_02_vri <- run_timed_step(
  step_id = "02_vri_corrections",
  label = "Apply terra_rrm_correct_bem_from_vri()",
  expr = terra_rrm_correct_bem_from_vri(
    x = step_01_input_stack,
    clear_site_ma = TRUE,
    use_ifelse = TRUE,
    beu_bec = beu_bec
  ),
  previous = step_01_input_stack,
  inspect_layers = c("BEUMC_S1", "SITE_M3A", "SLOPE_MOD", "STAND_A1")
)


# 3 ---- river adjacency
step_03_river <- run_timed_step(
  step_id = "03_river_adjacency",
  label = "Apply river adjacency stage",
  expr = apply_river_adjacency_stage(step_02_vri),
  previous = step_02_vri,
  inspect_layers = c("SITE_M3A", "BEUMC_S1")
)


# 4 ---- small lakes
if (isTRUE(apply_small_lakes)) {
  step_04_small_lakes <- run_timed_step(
    step_id = "04_small_lakes",
    label = "Apply terra_rrm_correct_small_lakes()",
    expr = terra_rrm_correct_small_lakes(
      x = step_03_river,
      lake_layer = lake_layer
    ),
    previous = step_03_river,
    inspect_layers = c("BEUMC_S1", "SDEC_1", "SDEC_2", "SDEC_3")
  )
} else {
  step_04_small_lakes <- step_03_river
  message("\n[04_small_lakes] Skipped because no lake raster was provided.")
}


# 5 ---- wetlands
step_05_wetlands <- run_timed_step(
  step_id = "05_wetland_corrections",
  label = "Apply terra_rrm_correct_bem_from_wetlands()",
  expr = terra_rrm_correct_bem_from_wetlands(
    x = step_04_small_lakes,
    buc = buc
  ),
  previous = step_04_small_lakes,
  inspect_layers = c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "SDEC_1", "SDEC_2", "SDEC_3")
)


# 6 ---- riparian wetlands
step_06_riparian <- run_timed_step(
  step_id = "06_riparian_wetlands",
  label = "Apply terra_rrm_correct_bem_from_wetlands_riparian_stage()",
  expr = terra_rrm_correct_bem_from_wetlands_riparian_stage(step_05_wetlands),
  previous = step_05_wetlands,
  inspect_layers = c("SITE_M3A", "BEUMC_S1", "SDEC_1")
)


# 7 ---- rules
step_07_rules <- run_timed_step(
  step_id = "07_rules",
  label = "Apply terra_rrm_apply_rules()",
  expr = terra_rrm_apply_rules(
    x = step_06_riparian,
    rules_dt = rules_xl
  ),
  previous = step_06_riparian,
  inspect_layers = c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "SDEC_1", "SDEC_2", "SDEC_3")
)


# 8 ---- forest age
step_08_forest_age <- run_timed_step(
  step_id = "08_forest_age",
  label = "Apply terra_rrm_calc_forest_age_class()",
  expr = terra_rrm_calc_forest_age_class(
    x = step_07_rules,
    most_recent_harvest_year = most_recent_harvest_year
  ),
  previous = step_07_rules,
  inspect_layers = c("PROJ_AGE_1", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD")
)


# 9 ---- unique ecosystem merge
step_09_unique_ecosystem <- run_timed_step(
  step_id = "09_unique_ecosystem",
  label = "Apply terra_rrm_merge_unique_ecosystem_fields()",
  expr = terra_rrm_merge_unique_ecosystem_fields(
    x = step_08_forest_age,
    unique_ecosystem_dt = unique_ecosystem_dt
  ),
  previous = step_08_forest_age,
  inspect_layers = c("STRCT_S1", "STAND_A1", "FORESTED_1")
)


# 10 ---- crown dominance
step_10_crown <- run_timed_step(
  step_id = "10_crown",
  label = "Apply terra_rrm_find_crown_area_dominant_values()",
  expr = terra_rrm_find_crown_area_dominant_values(step_09_unique_ecosystem),
  previous = step_09_unique_ecosystem,
  inspect_layers = c("CROWN_ALL_1", "CROWN_BEAR_1", "CROWN_MOOSE_1")
)


# 11 ---- moose export
if (isTRUE(run_moose_export)) {
  moose_export_dt <- run_timed_step(
    step_id = "11_moose_export",
    label = "Apply terra_rrm_create_RRM_ecosystem_moose()",
    expr = terra_rrm_create_RRM_ecosystem_moose(step_10_crown)
  )

  fwrite(moose_export_dt, file.path(output_root, "11_moose_export.csv"))
}


# 12 ---- bear export
if (isTRUE(run_bear_export)) {
  bear_export_dt <- run_timed_step(
    step_id = "12_bear_export",
    label = "Apply terra_rrm_create_RRM_ecosystem_bear()",
    expr = terra_rrm_create_RRM_ecosystem_bear(step_10_crown)
  )

  fwrite(bear_export_dt, file.path(output_root, "12_bear_export.csv"))
}


timing_dt <- rbindlist(timing_log, use.names = TRUE, fill = TRUE)
fwrite(timing_dt, file.path(output_root, "timing_summary.csv"))
print(timing_dt)
