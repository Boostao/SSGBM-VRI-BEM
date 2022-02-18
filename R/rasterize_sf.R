#' Convert sf to raster
#'
#' Convert all attributes of interest of sf into raster layers
#'
#' @param src_datasource character, path of source file
#' @param dst_filename character, path of destination file
#' @param layer character, name of the layer of interest
#' @param a_srs Character. (GDAL >= 1.8.0) Override the projection for the output file. If not specified, the projection of the input vector file will be used if available. If incompatible projections between input and output files, no attempt will be made to reproject features. The srs_def may be any of the usual GDAL/OGR forms, complete WKT, PROJ.4, EPSG:n or a file containing the WKT.
#' @param te Numeric. c(xmin,ymin,xmax,ymax) (GDAL >= 1.8.0) set georeferenced extents. The values must be expressed in georeferenced units. If not specified, the extent of the output file will be the extent of the vector layers.
#' @param tr Numeric. c(xres,yres) (GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced units. Both must be positive values.
#' @param reference character of SpatRaster, will be used as reference raster for extent, resolution and crs
#' @param numeric_attributes character vector of names of attributes of type numeric
#' @param character_attributes character vector of names of attributes of type character
#' @param date_attributes character vector of names of attributes of type date
#' @param output_raster boolean, if TRUE, the resulting raster will be returned
#' @param verbose boolean, if TRUE progression message will be printed
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster
#' @importFrom gdalUtils gdal_rasterize
#' @export
rasterize_sf <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL, tr = NULL, reference = NULL, numeric_attributes, character_attributes, date_attributes, output_raster = FALSE, verbose = TRUE) {

  # if provided use parameters from reference raster
  if (!is.null(reference)) {
    if (class(reference) == "character") {
      ref_raster <- rast(reference)
    }
    else {
      ref_raster <- reference
    }
    a_srs <- crs(ref_raster, proj = T)
    extent <- ext(ref_raster)
    te <- c(extent[1], extent[3], extent[2], extent[4])
    tr <- res(ref_raster)
  }

  # get layer name
  if  (is.null(layer)) {
    layer <- st_layers(src_datasource)$name[1]
  }

  # get file extension
  pos <- regexpr("\\.([[:alnum:]]+)$", dst_filename)
  dst_file_extension <- ifelse(pos > -1L, substring(dst_filename, pos + 1L), "")
  dst_file_no_ext <- substr(dst_filename, 1 , nchar(dst_filename) - (nchar(dst_file_extension) +1))

  # creat temp dst_file for all attributes
  dst_filename_att <- paste0(dst_file_no_ext, "_", c(numeric_attributes, character_attributes, date_attributes),".", dst_file_extension)

  # create temp raster file for numeric attributes
  for (i in seq.int(along.with = numeric_attributes)) {
    if (verbose) {
      message(paste0("creating raster layer for ", numeric_attributes[i]))
    }
    gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                              dst_filename =  dst_filename_att[i],
                              a = numeric_attributes[i],
                              a_srs = a_srs,
                              te = te,
                              tr = tr)
  }

  # create temp raster file for character attributes
  for (i in seq.int(along.with = character_attributes)) {
    factor_dt <- tryCatch(expr = get(character_attributes[i]), error = function(x) message(paste0(x, ", skipping layer")))
    if (!is.null(factor_dt)) {
      for (j in seq.int(length.out = nrow(factor_dt))) {
        if (verbose) {
          message(paste0("creating raster layer for ", character_attributes[i], " for value ", factor_dt$value[j]))
        }
        gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                                  dst_filename =  dst_filename_att[i + length(numeric_attributes)],
                                  burn = factor_dt$factor[j],
                                  where = paste0("\"", character_attributes[i], "\"", " = '", factor_dt$value[j], "'"),
                                  a_srs = a_srs,
                                  te = te,
                                  tr = tr)
      }
    }
  }

  for (i in seq.int(along.with = date_attributes)) {
    if (verbose) {
      message(paste0("creating raster layer for ", date_attributes[i]))
    }
    gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                              dst_filename =  dst_filename_att[i +  length(numeric_attributes) + length(character_attributes)],
                              a = date_attributes[i],
                              sql =  paste0("SELECT  CAST(",date_attributes[i]," as integer(3)) as ",date_attributes[i]," FROM ",layer),
                              a_srs = a_srs,
                              te = te,
                              tr = tr)
  }

  # combine all raster layers together
  if (verbose) {
    message("Combining all raster layers into one file")
  }
  total_raster <- rast(dst_filename_att[1])
  for (i in 2:length(dst_filename_att)) {
    add(total_raster) <- rast(dst_filename_att[i])
  }

  # writing new raster
  if (verbose) {
    message(paste0("Writing new raster file at ", dst_filename))
  }
  terra::writeRaster(x = total_raster, filename = dst_filename, overwrite = TRUE)

  # deleting temp files
  if (verbose) {
    message("deleting temporary files")
  }
  lapply(dst_filename_att, unlink)

  if (output_raster) {
    return(rast(dst_filename))
  }
  else {
    return(NULL)
  }

}

#' Convert vri to raster
#'
#' Convert all attributes of interest of vri into raster layers
#' @inheritParams read_vri
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster
#' @importFrom gdalUtils gdal_rasterize
#' @export

rasterize_vri <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL,
                          tr = NULL, reference = NULL,
                          numeric_attributes = c(paste0("SPEC_PCT_", 1:6), "CR_CLOSURE", "COV_PCT_1", "PROJ_AGE_1"),
                          character_attributes = c(paste0("BCLCS_LV_", 1:5), paste0("SPEC_CD_", 1:6), "LAND_CD_1", "LBL_VEGCOV"),
                          date_attributes = "HRVSTDT", output_raster = FALSE, verbose = TRUE) {
  rasterize_sf(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               output_raster = output_raster,
               verbose = verbose)
}
