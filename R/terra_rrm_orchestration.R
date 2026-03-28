#' Terra-native orchestration for the raster-only RRM path

#' Prepare a terra-native ecosystem stack
#'
#' Runs the currently implemented raster-native stages in the intended order on
#' an already aligned `terra::SpatRaster` stack. This wrapper keeps the new
#' terra path separate from the legacy polygon and DuckDB workflows.
#'
#' The input stack must already contain the precomputed raster layers required
#' by the stage functions it invokes, such as `wl_pct`, `MEAN_SLOPE`,
#' `PROJ_AGE_1`, and `ABOVE_ELEV_THOLD` when those stages depend on them.
#'
#' @param x A `terra::SpatRaster` containing the aligned raster-only inputs.
#' @param buc A data frame containing the wetland lookup columns expected by
#'   [terra_rrm_correct_bem_from_wetlands()].
#' @param beu_bec A data frame containing the allowed BEC/BEU combinations used
#'   by [terra_rrm_correct_bem_from_vri()].
#' @param rules_dt A rules data frame or supported Excel file path passed to
#'   [terra_rrm_apply_rules()].
#' @param unique_ecosystem_dt A data frame containing the unique ecosystem
#'   lookup values expected by [terra_rrm_merge_unique_ecosystem_fields()].
#' @param most_recent_harvest_year Numeric scalar passed to
#'   [terra_rrm_calc_forest_age_class()].
#' @param lake_layer Name of the aligned lake-presence layer.
#' @param apply_small_lakes Logical. If `TRUE`, run
#'   [terra_rrm_correct_small_lakes()].
#' @param clear_site_ma Logical passed to
#'   [terra_rrm_correct_bem_from_vri()].
#' @param use_ifelse Logical passed to
#'   [terra_rrm_correct_bem_from_vri()].
#' @param filename Optional filename for writing the fully prepared stack.
#' @param overwrite Logical passed to terra writes.
#' @return A prepared `SpatRaster` ready for moose and/or bear export.
#' @export
terra_rrm_prepare_ecosystem_stack <- function(x,
                                              buc,
                                              beu_bec = NULL,
                                              rules_dt,
                                              unique_ecosystem_dt,
                                              most_recent_harvest_year,
                                              lake_layer = "lakes",
                                              apply_small_lakes = lake_layer %in% names(x),
                                              clear_site_ma = TRUE,
                                              use_ifelse = TRUE,
                                              filename = NULL,
                                              overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.data.frame(buc))
  stopifnot(is.data.frame(unique_ecosystem_dt))
  stopifnot(length(most_recent_harvest_year) == 1L, !is.na(most_recent_harvest_year))

  result <- terra_rrm_correct_bem_from_vri(
    x,
    clear_site_ma = clear_site_ma,
    use_ifelse = use_ifelse,
    beu_bec = beu_bec
  )
  result <- .terra_rrm_apply_river_adjacency_stage(result)

  if (isTRUE(apply_small_lakes)) {
    if (!lake_layer %in% names(result)) {
      stop(sprintf("Lake layer '%s' is required when apply_small_lakes is TRUE.", lake_layer), call. = FALSE)
    }
    result <- terra_rrm_correct_small_lakes(result, lake_layer = lake_layer)
  }

  result <- terra_rrm_correct_bem_from_wetlands(result, buc = buc)
  result <- terra_rrm_correct_bem_from_wetlands_riparian_stage(result)
  result <- terra_rrm_apply_rules(result, rules_dt = rules_dt)
  result <- terra_rrm_calc_forest_age_class(result, most_recent_harvest_year = most_recent_harvest_year)
  result <- terra_rrm_merge_unique_ecosystem_fields(result, unique_ecosystem_dt = unique_ecosystem_dt)
  result <- terra_rrm_find_crown_area_dominant_values(result)

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}


#' Create moose and/or bear RRM summaries from the terra-native stack path
#'
#' Runs the current terra-native preparation stages and then dispatches to the
#' moose and/or bear export summarizers.
#'
#' @param x A `terra::SpatRaster` containing the aligned raster-only inputs.
#' @param buc A data frame containing the wetland lookup columns expected by
#'   [terra_rrm_correct_bem_from_wetlands()].
#' @param beu_bec A data frame containing the allowed BEC/BEU combinations used
#'   by [terra_rrm_correct_bem_from_vri()].
#' @param rules_dt A rules data frame or supported Excel file path passed to
#'   [terra_rrm_apply_rules()].
#' @param unique_ecosystem_dt A data frame containing the unique ecosystem
#'   lookup values expected by [terra_rrm_merge_unique_ecosystem_fields()].
#' @param most_recent_harvest_year Numeric scalar passed to
#'   [terra_rrm_calc_forest_age_class()].
#' @param kind Character vector containing one or both of `"moose"` and
#'   `"bear"`.
#' @param lake_layer Name of the aligned lake-presence layer.
#' @param apply_small_lakes Logical. If `TRUE`, run
#'   [terra_rrm_correct_small_lakes()].
#' @param clear_site_ma Logical passed to
#'   [terra_rrm_correct_bem_from_vri()].
#' @param use_ifelse Logical passed to
#'   [terra_rrm_correct_bem_from_vri()].
#' @param return_stack Logical. If `TRUE`, include the prepared raster stack in
#'   the return value.
#' @param stack_filename Optional filename for writing the prepared stack before
#'   export.
#' @param overwrite Logical passed to terra writes.
#' @return If `kind` has length 1 and `return_stack = FALSE`, a single export
#'   `data.table`. Otherwise a named list containing one or both summaries and,
#'   when requested, the prepared `raster` stack.
#' @export
terra_rrm_create_RRM_ecosystem <- function(x,
                                           buc,
                                           beu_bec = NULL,
                                           rules_dt,
                                           unique_ecosystem_dt,
                                           most_recent_harvest_year,
                                           kind = c("moose", "bear"),
                                           lake_layer = "lakes",
                                           apply_small_lakes = lake_layer %in% names(x),
                                           clear_site_ma = TRUE,
                                           use_ifelse = TRUE,
                                           return_stack = FALSE,
                                           stack_filename = NULL,
                                           overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))

  kind <- match.arg(kind, choices = c("moose", "bear"), several.ok = TRUE)
  prepared <- terra_rrm_prepare_ecosystem_stack(
    x = x,
    buc = buc,
    beu_bec = beu_bec,
    rules_dt = rules_dt,
    unique_ecosystem_dt = unique_ecosystem_dt,
    most_recent_harvest_year = most_recent_harvest_year,
    lake_layer = lake_layer,
    apply_small_lakes = apply_small_lakes,
    clear_site_ma = clear_site_ma,
    use_ifelse = use_ifelse,
    filename = stack_filename,
    overwrite = overwrite
  )

  outputs <- setNames(vector("list", length(kind)), kind)
  for (idx in seq_along(kind)) {
    outputs[[idx]] <- switch(
      kind[[idx]],
      moose = terra_rrm_create_RRM_ecosystem_moose(prepared),
      bear = terra_rrm_create_RRM_ecosystem_bear(prepared)
    )
  }

  if (length(outputs) == 1L && !isTRUE(return_stack)) {
    return(outputs[[1]])
  }

  if (isTRUE(return_stack)) {
    outputs[["raster"]] <- prepared
  }

  outputs
}


#' Create terra-native RRM summaries directly from aligned raster files
#'
#' Reads the aligned raster inputs from disk, loads the supporting wetland and
#' unique-ecosystem lookup tables, and then runs the terra-native preparation
#' and export path.
#'
#' This entry point is for raster inputs that are already aligned and
#' materialized on disk. It does not rasterize vector layers or derive missing
#' support rasters for you.
#'
#' @param vri_dsn Path to the aligned VRI raster.
#' @param bem_dsn Path to the aligned BEM raster.
#' @param rivers_dsn Path to the aligned rivers raster.
#' @param wetlands_dsn Path to the aligned wetlands raster.
#' @param rules_xl Rules workbook path, or an in-memory rules data frame.
#' @param most_recent_harvest_year Numeric scalar passed to
#'   [terra_rrm_calc_forest_age_class()].
#' @param kind Character vector containing one or both of `"moose"` and
#'   `"bear"`.
#' @param lakes_dsn Optional path to an aligned lake raster.
#' @param ccb_dsn Optional path to an aligned consolidated cutblock raster.
#' @param elevation_dsn Optional path to an aligned elevation raster.
#' @param beu_wetland_update_csv Path to the wetland lookup CSV.
#' @param beu_bec_csv Path to the allowed BEC/BEU lookup CSV.
#' @param unique_ecosystem Path to the unique ecosystem CSV.
#' @param lake_layer Name of the aligned lake-presence layer.
#' @param apply_small_lakes Logical. If `TRUE`, run the terra small-lakes stage.
#' @param clear_site_ma Logical passed to
#'   [terra_rrm_correct_bem_from_vri()].
#' @param use_ifelse Logical passed to
#'   [terra_rrm_correct_bem_from_vri()].
#' @param return_stack Logical. If `TRUE`, include the prepared raster stack in
#'   the return value.
#' @param input_stack_filename Optional filename for writing the merged input
#'   stack before the terra preparation stages.
#' @param stack_filename Optional filename for writing the prepared stack before
#'   export.
#' @param overwrite Logical passed to terra writes.
#' @param verbose Logical. If `TRUE`, emit progress messages for the major disk
#'   loading steps.
#' @return If `kind` has length 1 and `return_stack = FALSE`, a single export
#'   `data.table`. Otherwise a named list containing one or both summaries and,
#'   when requested, the prepared raster stack.
#' @export
terra_rrm_create_RRM_ecosystem_from_rasters <- function(vri_dsn,
                                                        bem_dsn,
                                                        rivers_dsn,
                                                        wetlands_dsn,
                                                        rules_xl,
                                                        most_recent_harvest_year,
                                                        kind = c("moose", "bear"),
                                                        lakes_dsn = NULL,
                                                        ccb_dsn = NULL,
                                                        elevation_dsn = NULL,
                                                        beu_bec_csv = system.file("csv/Allowed_BEC_BEUs_NE_ALL.csv", package = "SSGBM.VRI.BEM"),
                                                        beu_wetland_update_csv = system.file("csv/beu_wetland_updates.csv", package = "SSGBM.VRI.BEM"),
                                                        unique_ecosystem = system.file("csv/Skeena_VRIBEM_LUT.csv", package = "SSGBM.VRI.BEM"),
                                                        lake_layer = "lakes",
                                                        apply_small_lakes = !is.null(lakes_dsn),
                                                        clear_site_ma = TRUE,
                                                        use_ifelse = TRUE,
                                                        return_stack = FALSE,
                                                        input_stack_filename = NULL,
                                                        stack_filename = NULL,
                                                        overwrite = FALSE,
                                                        verbose = TRUE) {
  if (verbose) {
    message("Reading aligned raster inputs")
  }
  input_stack <- .terra_rrm_read_input_stack(
    vri_dsn = vri_dsn,
    bem_dsn = bem_dsn,
    rivers_dsn = rivers_dsn,
    wetlands_dsn = wetlands_dsn,
    lakes_dsn = lakes_dsn,
    ccb_dsn = ccb_dsn,
    elevation_dsn = elevation_dsn,
    filename = input_stack_filename,
    overwrite = overwrite
  )

  if (verbose) {
    message("Reading wetland and unique ecosystem lookup tables")
  }
  buc <- data.table::fread(beu_wetland_update_csv)
  beu_bec <- data.table::fread(beu_bec_csv)
  unique_ecosystem_dt <- read_unique_ecosystem_dt(unique_ecosystem)

  if (verbose) {
    message("Running terra-native RRM workflow")
  }
  terra_rrm_create_RRM_ecosystem(
    x = input_stack,
    buc = buc,
    beu_bec = beu_bec,
    rules_dt = rules_xl,
    unique_ecosystem_dt = unique_ecosystem_dt,
    most_recent_harvest_year = most_recent_harvest_year,
    kind = kind,
    lake_layer = lake_layer,
    apply_small_lakes = apply_small_lakes,
    clear_site_ma = clear_site_ma,
    use_ifelse = use_ifelse,
    return_stack = return_stack,
    stack_filename = stack_filename,
    overwrite = overwrite
  )
}