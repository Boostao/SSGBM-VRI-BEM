#' Creates the RRM ecosystem outpout for skeena bear and moose model
#'
#'
#' @param vri_dsn dsn for vri layer, default to dsn if empty
#' @param bem_dsn dsn for bem layer, default to dsn if empty
#' @param rivers_dsn dsn for rivers layer, default to dsn if empty
#' @param wetlands_dsn dsn for wetlands layer, default to dsn if empty
#' @param ccb_dsn dsn for ccb layer, default to dsn if empty
#' @param elevation_dsn dsn for elevation raster, must be provided because cannot be part of a .gbd , needs to be .tif
#' @param beu_bec_csv dsn for beu bec csv
#' @param beu_wetland_update_csv dsn for wetlands correction csv
#' @param unique_ecosystem dsn for unique ecosystem csv filled with values
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @param use_ifelse boolean, if TRUE correction done after the combine_duplicated_BEUMC will only be applied on rows that were not affected by the correction of duplicated BEUMC
#' @param most_recent_harvest_year integer that represent the most recent harvest year
#' @param elevation_threshold numeric elevation threshold used to create above elevation indicator (`ABOVE_ELEV_THOLD`)
#' @param wkt_filter character; WKT representation of a spatial filter (may be used as bounding box, selecting overlapping geometries)
#' @param n_iterations integer number of iterations, usefull when running out of RAM to load smaller areas and iterate over them one at a time
#' @param verbose boolean , if TRUE function will return message to indicate progress throughout the function execution
#' @details
#' For each of the following combination compute the sum of the area
#'   * ECO_SEC
#'   * BGC_ZONE
#'   * BGC_SUBZON
#'   * BGC_VRT
#'   * BGC_PHASE
#'   * BEUMC  (BEUMC_S1, BEUMC_S2, BEUMC_S3)
#'   * SLOPE_MOD
#'   * SITE_M3A
#'   * SNOW_CODE
#'   * ABOVE_ELEV
#'   * CROWN_MOOSE (CROWN_MOOSE_1, CROWN_MOOSE_2, CROWN_MOOSE_3)
#'   * STRCT (STRCT_S1, STRCT_S2, STRCT_S3)
#'   * STAND (STAND_A1, STAND_A2, STAND_A3)
#'   * FORESTED (FORESTED_1, FORESTED_2, FORESTED_3)
#'
#' For STRCT and STAND every combination is also made with the projected age values.
#'
#' @return Summary of Area by unique ecosystem
#' @import data.table
#' @importFrom terra rast extract vect `add<-
#' @export
#'
create_RRM_ecosystem_from_rasters <- function(vri_dsn = dsn, bem_dsn = dsn, rivers_dsn = dsn, wetlands_dsn = dsn, ccb_dsn = dsn, elevation_dsn,
                                              beu_bec_csv = "csv/Allowed_BEC_BEUs_NE_ALL.csv", beu_wetland_update_csv = "csv/beu_wetland_updates.csv", unique_ecosystem = "csv/Skeena_VRIBEM_LUT.csv",
                                              clear_site_ma = TRUE, use_ifelse = TRUE, most_recent_harvest_year, elevation_threshold, wkt_filter = character(0), n_iterations = 1, verbose = TRUE) {

  # TODO add default wkt_filter when no filter is passed but number of iterations is greater than 1 (maybe default to the whole skeena region, store it as part of the package)
  # TODO check what appends when wkt_filter cover an area where there is no polygon
  # then maybe add a if in the loop to check if the vri is empty just go to the next iteration

  # make grid with wkt filter based on number of iterations
  if (n_iterations %% 1 != 0) {
    n_iterations <- round(n_iterations)
    warning(paste0("n_iterations must be an integer, n_iterations was rounder to ", n_iterations))
  }

  # TODO create a bbox around the whole raster region if wkt_filter is empty
  grid <- create_grid_from_iteration_number(n_iterations, wkt_filter, as.text = TRUE)

  # read csv files and make connexion to raster file
  if (verbose) {
    message("reading csv files, creating connexion with raster files and merging them together")
  }
  vri <- rast(vri_dsn)
  bem <- rast(bem_dsn)
  rivers <- rast(rivers_dsn)
  wetlands <- rast(wetlands_dsn)
  ccb <- rast(ccb_dsn)
  elevation <- rast(elevation_dsn)

  beu_bec_csv <- fread(beu_bec_csv)
  beu_wetland_update_csv <- fread(beu_wetland_update_csv)
  unique_ecosystem_dt <- read_unique_ecosystem_dt(unique_ecosystem)

  # merge raster
  add(vri) <- bem
  add(vri) <- rivers
  add(vri) <- wetlands
  add(vri) <- ccb
  add(vri) <- elevation

  # initialize empty list for RRM ecosystem which will be filled when iterating
  RRM_ecosystem_list <- list()

  for (iteration in seq.int(length.out = n_iterations)) {

    if (verbose) {
      message(paste0("iteration ", iteration, " out of ", n_iterations))
    }

    # read sf objects and csv files
    if (verbose) {
      message("reading raster layers into R")
    }
    vri_bem <- setDT(extract(vri, vect(grid[iteration]), cells = TRUE, xy = TRUE))

    # converting raster values to original values
    if (verbose) {
      message("formating raster values")
    }
    format_vri_raster_extract(vri_bem)
    format_bem_raster_extract(vri_bem)

    if (verbose) {
      message("correction of bem attributes based on vri attributes")
    }
    vri_bem <- correct_bem_from_vri(vri_bem = vri_bem, beu_bec = beu_bec_csv, clear_site_ma = clear_site_ma, use_ifelse = use_ifelse, raster_res = res(vri))


    if (verbose) {
      message("correction of bem attributes based on wetlands csv")
    }
    vri_bem <- correct_bem_from_wetlands(vri_bem = vri_bem, buc = beu_wetland_update_csv)

    if (verbose) {
      message("Computing above threshold elevation indicator and slope mod")
    }
    vri_bem <- compute_elevation_ind_and_slope_mod(vri_bem, elevation_threshold = elevation_threshold)


    if (verbose) {
      message("calc_forest_age_class")
    }
    # merge forest structural informations on vri bem
    vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                     most_recent_harvest_year = most_recent_harvest_year)

    # merge forest structural informations on vri bem
    if (verbose) {
      message("merging ecosystem fields from unique ecosystem csv file")
    }
    vri_bem <- merge_unique_ecosystem_fields(vri_bem = vri_bem,
                                             unique_ecosystem_dt = unique_ecosystem_dt)

    # creation and correction of crown bear 1 to 3 and crown moose 1 to 3
    if (verbose) {
      message("Correction CROWN_BEAR and CROWN_MOOSE based on forestation")
    }
    vri_bem <- correct_crown(vri_bem = vri_bem, raster = TRUE)

    # creating rrm output
    if (verbose) {
      message(paste0("Creating RRM output for iteration ", iteration))
    }
    RRM_ecosystem_list[[iteration]] <- create_RRM_ecosystem(vri_bem = vri_bem, raster_res = res(vri))

  }

  # combining and returning total ouput
  if (verbose) {
    message("Combining all RRM output together to create final ouput")
  }

  if (n_iterations == 1) {
    return(RRM_ecosystem_list[[1]])
  }
  else {
    return(rbindlist(RRM_ecosystem_list)[, .(Hectares = sum(Hectares)),
                                         by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A,
                                                   SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_MOOSE, STRCT, STAND, FORESTED)])
  }

}
