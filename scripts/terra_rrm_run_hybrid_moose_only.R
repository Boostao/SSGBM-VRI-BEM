# Hybrid DuckDB / terra pipeline (moose-only, no init_db)
#
# Purpose
# - Assumes DuckDB has already been initialized (tables already loaded).
# - Assumes ancillary rasters are already materialized (wetlands/rivers/lakes).
# - Rebuilds corrected VRIBEM in DuckDB, rasterizes VRIBEM only, then runs
#   terra steps end-to-end and exports moose ecosystem CSV.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Please install 'devtools' to run this script.", call. = FALSE)
}

devtools::load_all()

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
})

run_hybrid_moose_only <- function(
  data_folder = file.path("..", "SSGBM-VRI-BEM-data"),
  elevation_raster = file.path("..", "SSGBM-VRI-BEM-data", "dem.tif"),
  aoi_wkt = "POLYGON ((1000000 960000, 1002000 960000, 1002000 962000, 1000000 962000, 1000000 960000))",
  rules_xl = file.path("..", "SSGBM-VRI-BEM-data", "Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx"),
  dbdir = defdb(),
  duckdb_memory_limit = "14GB",
  duckdb_threads = 1L,
  elevation_threshold = 1400L,
  river_buffer_cells = 2L,
  lake_buffer_cells = 2L,
  save_intermediate_rasters = FALSE
) {
  execution_started_at <- proc.time()[["elapsed"]]
  step_timings <- list()

  time_step <- function(step_name, expr) {
    step_started_at <- proc.time()[["elapsed"]]
    value <- eval.parent(substitute(expr))
    elapsed_seconds <- unname(proc.time()[["elapsed"]] - step_started_at)

    step_timings[[length(step_timings) + 1L]] <<- data.table(
      step = step_name,
      seconds = elapsed_seconds
    )

    value
  }

  # Fixed paths -------------------------------------------------------------
  data_folder <- normalizePath(data_folder, winslash = "/", mustWork = TRUE)
  vector_dir  <- file.path(data_folder, "vectors")
  raster_dir  <- file.path(data_folder, "rasters")
  output_dir  <- file.path(data_folder, "outputs")
  duckdb_temp_dir <- file.path(data_folder, "duckdb_tmp")

  dir.create(vector_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(duckdb_temp_dir, recursive = TRUE, showWarnings = FALSE)

  elevation_raster <- normalizePath(elevation_raster, winslash = "/", mustWork = TRUE)
  reference_raster <- terra::rast(elevation_raster)
  reference_resolution_m <- max(terra::res(reference_raster))
  reference_crs_wkt <- terra::crs(reference_raster, proj = TRUE)
  if (is.na(reference_crs_wkt) || !nzchar(reference_crs_wkt)) {
    stop("Reference elevation raster has no CRS; cannot export VRIBEM with a defined projection.", call. = FALSE)
  }
  river_buffer_m <- river_buffer_cells * reference_resolution_m
  lake_buffer_m  <- lake_buffer_cells * reference_resolution_m
  stopifnot(file.exists(rules_xl))

  wetlands_dsn <- file.path(raster_dir, "wetlands.tif")
  rivers_dsn   <- file.path(raster_dir, "rivers.tif")
  lakes_dsn    <- file.path(raster_dir, "lakes.tif")
  stage_output <- function(filename) {
    if (isTRUE(save_intermediate_rasters)) {
      file.path(output_dir, filename)
    } else {
      NULL
    }
  }

  missing_rasters <- c(wetlands_dsn, rivers_dsn, lakes_dsn)[!file.exists(c(wetlands_dsn, rivers_dsn, lakes_dsn))]
  if (length(missing_rasters) > 0) {
    stop(
      sprintf(
        "Missing pre-rasterized ancillary layers: %s",
        paste(missing_rasters, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # D01 — Connect to existing DuckDB ---------------------------------------
  message("\n[D01] Connect to existing DuckDB")

  conn <- time_step("D01_connect_to_existing_duckdb", init_conn(
    dbdir = dbdir,
    temp_dir = duckdb_temp_dir,
    memory_limit = duckdb_memory_limit,
    threads = duckdb_threads
  ))
  time_step("D01_build_filtered_views", filtered_views(
    conn,
    wkt_filter = aoi_wkt,
    build_spatial_index = FALSE
  ))
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # D02 — Build VRIBEM view ------------------------------------------------
  message("\n[D02] Build VRIBEM view")

  time_step("D02_build_vribem_view", {
    vribem_view(conn, validate_intersect = FALSE)
    DBI::dbExecute(conn, "CREATE OR REPLACE TABLE VRIBEM AS SELECT * FROM V_VRIBEM;")
  })

  # D03 — VRI/BEM corrections ----------------------------------------------
  message("\n[D03] Apply VRI/BEM corrections")

  time_step("D03_apply_vri_bem_corrections", {
    duckdb::duckdb_read_csv(
      conn, "beu_bec_corr",
      "inst/csv/Allowed_BEC_BEUs_NE_ALL.csv",
      temporary = TRUE
    )

    vribem_corrections_view(
      conn,
      vri_bem_tbl = "VRIBEM",
      beu_bec = "beu_bec_corr",
      result_tbl = "VRIBEM",
      skip_river_adjacency = TRUE
      # River adjacency deferred to terra T05 with a configurable raster buffer.
    )
  })

  # D04 — Wetland corrections (riparian deferred) --------------------------
  message("\n[D04] Wetland BEU corrections (skip_riparian = TRUE)")

  time_step("D04_load_wetland_corrections", duckdb::duckdb_read_csv(
    conn, "beu_wetland_updates",
    "inst/csv/beu_wetland_updates.csv",
    temporary = TRUE
  ))

 #  vri_bem_wetlands_corrections_view(
 #   conn,
  #  vri_bem = "VRIBEM",
  #  beu_wetland_updates = "beu_wetland_updates",
  #  skip_riparian = TRUE
  #)

  # D05 — Disturbance layers (CCB + fire) ----------------------------------
  message("\n[D05] Merge CCB and fire disturbance")

  time_step("D05_merge_disturbance_layers", {
    merge_geometry_duckdb(
      conn = conn,
      x_tbl = "VRIBEM",
      y_tbl = "V_CCB",
      tolerance_m2 = 10,
      result_tbl = "VRIBEM_FDL"
    )

    fire_tbl_for_merge <- "V_FIRE"
    if (tbl_exists(conn, "V_FIRE")) {
      has_fire_year <- DBI::dbGetQuery(
        conn,
        "SELECT COUNT(*) AS n
         FROM information_schema.columns
         WHERE upper(table_name) = 'V_FIRE'
           AND upper(column_name) = 'FIRE_YEAR';"
      )$n > 0

      if (!has_fire_year) {
        message("[D05] V_FIRE has no FIRE_YEAR; using mock FIRE_YEAR = 2010 for testing")
        DBI::dbExecute(
          conn,
          "CREATE OR REPLACE TEMP VIEW V_FIRE_MOCK AS
           SELECT Shape, CAST(2010 AS INTEGER) AS FIRE_YEAR
           FROM V_FIRE;"
        )
        fire_tbl_for_merge <- "V_FIRE_MOCK"
      }
    }

    merge_fire_perimeters_duckdb(
      conn,
      vri_bem_tbl = "VRIBEM_FDL",
      fire_tbl = fire_tbl_for_merge
    )
  })

  # D06 — Export corrected VRIBEM ------------------------------------------
  message("\n[D06] Export corrected VRIBEM to GeoPackage")

  vribem_gpkg <- file.path(vector_dir, "vribem_corrected.gpkg")

  time_step("D06_export_corrected_vribem", {
    vribem_wkb <- DBI::dbGetQuery(
      conn,
      "SELECT * EXCLUDE Shape, ST_AsWKB(Shape) AS wkb FROM VRIBEM_FDL"
    )
    vribem_sf <- sf::st_sf(
      vribem_wkb[, setdiff(names(vribem_wkb), "wkb")],
      geometry = sf::st_as_sfc(vribem_wkb$wkb)
    )
    vribem_sf <- sf::st_set_crs(vribem_sf, reference_crs_wkt)

    if (!"HARVESTYR" %in% names(vribem_sf)) {
      message("[D06] HARVESTYR missing; mocking HARVESTYR = 2012 for testing")
      vribem_sf$HARVESTYR <- 2012L
    }

    sf::st_write(vribem_sf, dsn = vribem_gpkg, layer = "VRIBEM", append = FALSE)
    rm(vribem_wkb, vribem_sf)
  })

  # Rasterize VRIBEM only ---------------------------------------------------
  message("\n[V01] Rasterize VRIBEM only")

  vribem_dsn <- file.path(raster_dir, "vribem.tif")

  time_step("V01_rasterize_vribem", rasterize_vribem_materialized(
    src_datasource = vribem_gpkg,
    layer = "VRIBEM",
    dst_filename = vribem_dsn,
    reference = elevation_raster,
    aoi = aoi_wkt,
    dynamic_factor_conv = TRUE,
    dynamic_factor_strategy = "append",
    output_raster = FALSE,
    verbose = TRUE
  ))

  # Create the SpatRaster pointer (does not load data into RAM)
 print(ncell(rast(vribem_dsn)))

  
  # Support tables ----------------------------------------------------------
  support_tables <- time_step("support_tables_load", {
    list(
      buc = fread(file.path("inst", "csv", "beu_wetland_updates.csv")),
      unique_ecosystem_dt = read_unique_ecosystem_dt(file.path("inst", "csv", "Skeena_VRIBEM_LUT.csv"))
    )
  })
  buc <- support_tables$buc
  unique_ecosystem_dt <- support_tables$unique_ecosystem_dt

  # T03 — Read stack --------------------------------------------------------
  message("\n[T03] Read aligned raster stack")

  step_stack <- time_step("T03_read_aligned_raster_stack", .terra_rrm_read_input_stack_from_vribem(
    vribem_dsn = vribem_dsn,
    rivers_dsn = rivers_dsn,
    wetlands_dsn = wetlands_dsn,
    lakes_dsn = lakes_dsn,
    elevation_dsn = elevation_raster,
    aoi = aoi_wkt,
    filename = stage_output("T03_input_stack.tif"),
    overwrite = TRUE
  ))

  # T03b — Terrain ----------------------------------------------------------
  message("\n[T03b] Compute per-cell terrain layers")

  step_stack <- time_step("T03b_compute_terrain_layers", terra_rrm_compute_terrain_layers(
    x = step_stack,
    elevation_threshold = elevation_threshold,
    filename = stage_output("T03b_terrain_layers.tif"),
    overwrite = TRUE
  ))

  # T05 → T13 chained sequence ---------------------------------------------
  message("\n[T05] Apply river adjacency")
  step_rivers <- time_step("T05_apply_river_adjacency", .terra_rrm_apply_river_adjacency_stage(
    x = step_stack,
    buffer_m = river_buffer_m
  ))

  message("\n[T06] Correct small lakes")
  step_lakes <- time_step("T06_correct_small_lakes", terra_rrm_correct_small_lakes(
    x = step_rivers,
    lake_layer = "lakes",
    buffer_m = lake_buffer_m,
    filename = stage_output("T06_lakes.tif"),
    overwrite = TRUE
  ))

  message("\n[T08] Correct BEM from wetlands")
  step_wetlands <- time_step("T08_correct_bem_from_wetlands", terra_rrm_correct_bem_from_wetlands(
    x = step_lakes,
    buc = buc,
    filename = stage_output("T08_wetland_corrections.tif"),
    overwrite = TRUE
  ))

  message("\n[T08b] Riparian wetland corrections")
  step_riparian <- time_step("T08b_riparian_wetland_corrections", terra_rrm_correct_bem_from_wetlands_riparian_stage(
    x = step_wetlands
  ))

  message("\n[T09] Apply BEU rules")
  step_rules <- time_step("T09_apply_beu_rules", terra_rrm_apply_rules(
    x = step_riparian,
    rules_dt = rules_xl,
    filename = stage_output("T09_rules.tif"),
    overwrite = TRUE
  ))

  message("\n[T09b] Build final ecosystem keys")
  step_keys <- time_step("T09b_build_final_ecosystem_keys", terra_rrm_add_ecosystem_keys(
    x = step_rules,
    filename = stage_output("T09b_ecosystem_keys.tif"),
    overwrite = TRUE
  ))

  message("\n[T10] Calculate forest age class")
  step_forest_age <- time_step("T10_calculate_forest_age_class", terra_rrm_calc_forest_age_class(
    x = step_keys,
    most_recent_harvest_year = as.integer(format(Sys.Date(), "%Y"))
  ))

  message("\n[T11] Merge unique ecosystem fields")
  step_unique_eco <- time_step("T11_merge_unique_ecosystem_fields", {
    available_ecosystem_codes <- .terra_rrm_available_ecosystem_codes(step_forest_age)
    reduced_unique_ecosystem_dt <- terra_rrm_reduce_unique_ecosystem_dt(
      unique_ecosystem_dt,
      available_codes = available_ecosystem_codes
    )

    terra_rrm_merge_unique_ecosystem_fields(
      x = step_forest_age,
      unique_ecosystem_dt = reduced_unique_ecosystem_dt,
      available_codes = available_ecosystem_codes,
      filename = stage_output("T11_unique_ecosystem.tif"),
      overwrite = TRUE
    )
  })

  message("\n[T12] Find crown area dominant values")
  step_crown <- time_step("T12_find_crown_area_dominant_values", terra_rrm_find_crown_area_dominant_values(
    x = step_unique_eco,
    filename = stage_output("T12_crown.tif"),
    overwrite = TRUE
  ))

  message("\n[T13] Export moose ecosystem")
  moose_csv <- file.path(output_dir, "T13_moose_export.csv")
  time_step("T13_export_moose_ecosystem", {
    moose_export_dt <- terra_rrm_create_RRM_ecosystem_moose(step_crown)
    fwrite(moose_export_dt, moose_csv)
  })

  total_execution_seconds <- unname(proc.time()[["elapsed"]] - execution_started_at)
  timing_dt <- data.table::rbindlist(step_timings)
  timing_dt[["percentage_of_total"]] <- if (total_execution_seconds > 0) {
    round(100 * timing_dt[["seconds"]] / total_execution_seconds, 2)
  } else {
    0
  }
  timing_dt[["seconds"]] <- round(timing_dt[["seconds"]], 3)
  timing_dt <- data.table::rbindlist(list(
    timing_dt,
    data.table(
      step = "full_execution",
      seconds = round(total_execution_seconds, 3),
      percentage_of_total = 100
    )
  ),
    use.names = TRUE,
    fill = TRUE
  )

  message("\nMoose-only hybrid pipeline complete.")

  return(list(
    moose_csv = moose_csv,
    vribem_raster = vribem_dsn,
    vribem_vector = vribem_gpkg,
    timing = timing_dt
  ))
}


# Run with defaults ---------------------------------------------------------
run_hybrid_moose_only()
