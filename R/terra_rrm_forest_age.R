#' Terra-native forest age class stage for the raster-only RRM path

.terra_rrm_coalesce_numeric <- function(primary, secondary) {
  stopifnot(inherits(primary, "SpatRaster"), inherits(secondary, "SpatRaster"))

  result <- terra::ifel(!is.na(primary[[1]]), primary[[1]], secondary[[1]])
  names(result) <- names(primary)
  result
}


.terra_rrm_year_from_date_raster <- function(x) {
  stopifnot(inherits(x, "SpatRaster"))

  result <- suppressWarnings(
    terra::app(
      x,
      fun = function(values) {
        out <- rep(NA_real_, length(values))
        valid <- !is.na(values)
        out[valid] <- ifelse(values[valid] > 9999, floor(values[valid] / 10000), values[valid])
        out
      }
    )
  )

  names(result) <- names(x)
  result
}


.terra_rrm_age_class_sts <- function(age_raster) {
  stopifnot(inherits(age_raster, "SpatRaster"))

  result <- suppressWarnings(
    terra::app(
      age_raster,
      fun = function(values) {
        out <- rep(-1, length(values))
        valid <- !is.na(values)
        out[valid & values >= 0 & values <= 3] <- 2
        out[valid & values > 3 & values <= 10] <- 7
        out[valid & values > 10 & values <= 30] <- 20
        out[valid & values > 30 & values <= 40] <- 35
        out[valid & values > 40 & values <= 60] <- 50
        out[valid & values > 60 & values <= 80] <- 70
        out[valid & values > 80 & values <= 140] <- 125
        out[valid & values > 140 & values <= 249] <- 195
        out[valid & values > 249] <- 301
        out
      }
    )
  )

  names(result) <- "VRI_AGE_CL_STS"
  result
}


.terra_rrm_age_class_std <- function(age_raster) {
  stopifnot(inherits(age_raster, "SpatRaster"))

  result <- suppressWarnings(
    terra::app(
      age_raster,
      fun = function(values) {
        out <- rep(-1, length(values))
        valid <- !is.na(values)
        out[valid & values >= 0 & values <= 15] <- 15
        out[valid & values > 15 & values <= 30] <- 30
        out[valid & values > 30 & values <= 50] <- 50
        out[valid & values > 50 & values <= 80] <- 80
        out[valid & values > 80] <- 9999
        out
      }
    )
  )

  names(result) <- "VRI_AGE_CL_STD"
  result
}


#' Terra-native forest age class calculation
#'
#' Recreates the `calc_forest_age_class()` logic on an aligned raster stack.
#' If present, disturbance-year layers are used to overwrite `PROJ_AGE_1` before
#' the structural-stage and stand-age classes are computed.
#'
#' Disturbance-year precedence is:
#' 1. `MRSRD_Y`
#' 2. `HARVESTYR`
#' 3. year derived from `HRVSTDT`
#'
#' @param x A `terra::SpatRaster` containing `PROJ_AGE_1` and optional
#'   disturbance-year layers.
#' @param most_recent_harvest_year Numeric scalar used to recompute
#'   `PROJ_AGE_1` where a disturbance year is available.
#' @param filename Optional filename for writing the updated stack.
#' @param overwrite Logical passed to terra writes.
#' @return A `SpatRaster` with updated `PROJ_AGE_1`, `VRI_AGE_CL_STS`, and
#'   `VRI_AGE_CL_STD` layers.
#' @export
terra_rrm_calc_forest_age_class <- function(x,
                                            most_recent_harvest_year,
                                            filename = NULL,
                                            overwrite = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(length(most_recent_harvest_year) == 1L, !is.na(most_recent_harvest_year))

  if (!"PROJ_AGE_1" %in% names(x)) {
    stop("terra_rrm_calc_forest_age_class requires a 'PROJ_AGE_1' layer.", call. = FALSE)
  }

  result <- x
  disturbance_year <- NULL

  if ("MRSRD_Y" %in% names(result)) {
    disturbance_year <- result[["MRSRD_Y"]]
  }

  if ("HARVESTYR" %in% names(result)) {
    disturbance_year <- if (is.null(disturbance_year)) {
      result[["HARVESTYR"]]
    } else {
      .terra_rrm_coalesce_numeric(disturbance_year, result[["HARVESTYR"]])
    }
  }

  if ("HRVSTDT" %in% names(result)) {
    harvest_date_year <- .terra_rrm_year_from_date_raster(result[["HRVSTDT"]])
    disturbance_year <- if (is.null(disturbance_year)) {
      harvest_date_year
    } else {
      .terra_rrm_coalesce_numeric(disturbance_year, harvest_date_year)
    }
  }

  if (!is.null(disturbance_year)) {
    updated_age <- terra::ifel(!is.na(disturbance_year[[1]]), most_recent_harvest_year - disturbance_year[[1]], result[["PROJ_AGE_1"]])
    names(updated_age) <- "PROJ_AGE_1"
    result[["PROJ_AGE_1"]] <- updated_age
  }

  result[["VRI_AGE_CL_STS"]] <- .terra_rrm_age_class_sts(result[["PROJ_AGE_1"]])
  result[["VRI_AGE_CL_STD"]] <- .terra_rrm_age_class_std(result[["PROJ_AGE_1"]])

  if (is.null(filename)) {
    return(result)
  }

  terra::writeRaster(result, filename = filename, overwrite = overwrite)
  terra::rast(filename)
}