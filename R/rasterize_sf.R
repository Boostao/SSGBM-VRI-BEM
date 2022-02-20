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
#' @param burn character, create a layer with the name of the "burn" argument which burn the value 1 where polygons intersect with the extent of the raster
#' @param output_raster boolean, if TRUE, the resulting raster will be returned
#' @param verbose boolean, if TRUE progression message will be printed
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom sf st_layers
#' @export
rasterize_sf <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL, tr = NULL, reference = NULL, numeric_attributes = NULL, character_attributes = NULL, date_attributes = NULL, burn = NULL, output_raster = FALSE, verbose = TRUE) {

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
  dst_filename_att <- character(0)
  layers_names <- character(0)

  # create temp raster file for numeric attributes
  if (!is.null(numeric_attributes)) {
    for (i in seq.int(along.with = numeric_attributes)) {
      if (verbose) {
        message(paste0("creating raster layer for ", numeric_attributes[i]))
      }
      dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",numeric_attributes[i],".", dst_file_extension))
      layers_names <- c(layers_names, numeric_attributes[i])
      gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                                dst_filename =  paste0(dst_file_no_ext, "_",numeric_attributes[i],".", dst_file_extension),
                                a = numeric_attributes[i],
                                a_srs = a_srs,
                                te = te,
                                tr = tr,
                                l  = layer)
    }
  }

  # create temp raster file for character attributes
  if (!is.null(character_attributes)) {
    for (i in seq.int(along.with = character_attributes)) {
      factor_dt <- tryCatch(expr = get(character_attributes[i])[!is.na(value)], error = function(x) message(paste0(x, ", skipping layer")))
      if (!is.null(factor_dt)) {
        if (nrow(factor_dt) > 0) {
          if (nrow(factor_dt) <= 30) {
            if (verbose) {
              message(paste0("creating raster layer for ", character_attributes[i]))
            }
            dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension))
            layers_names <- c(layers_names, paste0(character_attributes[i]))
            gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                                      dst_filename =  paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension),
                                      a = character_attributes[i],
                                      sql = paste0("SELECT ", paste0("(",character_attributes[i]," = \'", factor_dt$value ,"\') * ", factor_dt$factor, collapse = " + ")," AS ", character_attributes[i], " FROM ", layer),
                                      a_srs = a_srs,
                                      te = te,
                                      tr = tr)
          }
          else {
            split_layer_into <-  ceiling(nrow(factor_dt)/30)
            for (j in 1:split_layer_into) {
              if (verbose) {
                message(paste0("creating raster layer for ", character_attributes[i], " part ", j, " of ", split_layer_into))
              }
              sub_factor_dt <- factor_dt[(30 * (j-1) + 1):(min(.N,30 * j))]
              dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",character_attributes[i],"_",j,".", dst_file_extension))
              layers_names <- c(layers_names, paste0(character_attributes[i],"_",j))
              gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                                        dst_filename = paste0(dst_file_no_ext, "_",character_attributes[i],"_",j,".", dst_file_extension),
                                        a = character_attributes[i],
                                        sql = paste0("SELECT ", paste0("(",character_attributes[i]," = \'", sub_factor_dt$value ,"\') * ", sub_factor_dt$factor, collapse = " + ")," AS ", character_attributes[i], " FROM ", layer),
                                        a_srs = a_srs,
                                        te = te,
                                        tr = tr)

            }
          }
        }
        else {
          if (verbose) {
            message(paste0("creating raster layer for ", character_attributes[i]))
          }
          dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension))
          layers_names <- c(layers_names, paste0(character_attributes[i]))
          gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                                    dst_filename =  paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension),
                                    burn = 0,
                                    a_srs = a_srs,
                                    te = te,
                                    tr = tr,
                                    l = layer)
        }
      }
    }
  }

  # create temp raster file for date attributes
  if (!is.null(date_attributes)) {
    for (i in seq.int(along.with = date_attributes)) {
      if (verbose) {
        message(paste0("creating raster layer for ", date_attributes[i]))
      }
      dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",date_attributes[i],".", dst_file_extension))
      layers_names <- c(layers_names, date_attributes[i])
      gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                                dst_filename =  paste0(dst_file_no_ext, "_",date_attributes[i],".", dst_file_extension),
                                a = date_attributes[i],
                                sql =  paste0("SELECT  CAST(",date_attributes[i]," as integer(3)) as ",date_attributes[i]," FROM ",layer),
                                a_srs = a_srs,
                                te = te,
                                tr = tr)
    }
  }

  # create temp raster file for burn
  if (!is.null(burn)) {
    if (verbose) {
      message(paste0("creating raster layer for ", burn))
    }
    dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",burn,".", dst_file_extension))
    layers_names <- c(layers_names, burn)
    gdalUtils::gdal_rasterize(src_datasource = src_datasource,
                              dst_filename =  paste0(dst_file_no_ext, "_",burn,".", dst_file_extension),
                              a = burn,
                              sql =  paste0("SELECT 1 as ",burn," FROM ",layer),
                              a_srs = a_srs,
                              te = te,
                              tr = tr)
  }
  # combine all raster layers together
  if (verbose) {
    message("Combining all raster layers into one file")
  }
  total_raster <- rast(dst_filename_att[1])
  if (length(dst_filename_att) > 1) {
    for (i in 2:length(dst_filename_att)) {
      add(total_raster) <- rast(dst_filename_att[i])
    }
  }

  names(total_raster) <- layers_names

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
#' @inheritParams rasterize_sf
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom gdalUtils gdal_rasterize
#' @export

rasterize_vri <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL,
                          tr = NULL, reference = NULL,
                          numeric_attributes = c(paste0("SPEC_PCT_", 1:6), "CR_CLOSURE", "COV_PCT_1", "PROJ_AGE_1"),
                          character_attributes = c(paste0("BCLCS_LV_", 1:5), paste0("SPEC_CD_", 1:6), "LAND_CD_1", "LBL_VEGCOV"),
                          date_attributes = "HRVSTDT",
                          burn = NULL, output_raster = FALSE, verbose = TRUE) {
  rasterize_sf(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               burn = burn,
               output_raster = output_raster,
               verbose = verbose)
}

#' Convert wetlands to raster
#'
#' Create one raster layers for wetlands with value 1 when there is wetlands and 0 otherwise
#' @inheritParams rasterize_sf
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom gdalUtils gdal_rasterize
#' @export

rasterize_wetlands <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL,
                          tr = NULL, reference = NULL,
                          numeric_attributes = NULL,
                          character_attributes = NULL,
                          date_attributes = NULL,
                          burn = "wl_pct", output_raster = FALSE, verbose = TRUE) {
  rasterize_sf(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               burn = burn,
               output_raster = output_raster,
               verbose = verbose)
}

#' Convert rivers to raster
#'
#' Create one raster layers for rivers with value 1 when there is rivers and 0 otherwise
#' @inheritParams rasterize_sf
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom gdalUtils gdal_rasterize
#' @export

rasterize_rivers <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL,
                               tr = NULL, reference = NULL,
                               numeric_attributes = NULL,
                               character_attributes = NULL,
                               date_attributes = NULL,
                               burn = "rivers", output_raster = FALSE, verbose = TRUE) {
  rasterize_sf(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               burn = burn,
               output_raster = output_raster,
               verbose = verbose)
}

#' Convert consolidated cutblocks to raster
#'
#' Create one raster layers for consolidated cutblocks with value from HARVEST_YEAR
#' @inheritParams rasterize_sf
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom gdalUtils gdal_rasterize
#' @export

rasterize_ccb <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL,
                             tr = NULL, reference = NULL,
                             numeric_attributes = "HARVESTYR",
                             character_attributes = NULL,
                             date_attributes = NULL,
                             burn = NULL, output_raster = FALSE, verbose = TRUE) {
  rasterize_sf(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               burn = burn,
               output_raster = output_raster,
               verbose = verbose)
}


#' Convert Broad ecosytem mapping to raster
#'
#' Convert all attributes of interest of vri into raster layers
#' @inheritParams rasterize_sf
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom gdalUtils gdal_rasterize
#' @export

rasterize_bem <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL,
                          tr = NULL, reference = NULL,
                          numeric_attributes = c("TEIS_ID","Area_Ha", "AGE_CL_STS", "SDEC_1", "SDEC_2", "SDEC_3", "BGC_VRT"),
                          character_attributes = c("BGC_ZONE","BGC_SUBZON","BGC_PHASE", "SLOPE_MOD","ECO_SEC",  "BEUMC_S1",   "REALM_1",    "GROUP_1",    "CLASS_1",    "KIND_1",     "SITE_S1",    "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1",  "SITE_M1A",
                                                   "SITE_M1B" ,  "STRCT_S1",   "STRCT_M1",  "STAND_A1",   "SERAL_1",    "DISTCLS_1",  "DISTSCLS_1", "DISSSCLS_1", "SECL_1",     "SESUBCL_1",  "COND_1",     "VIAB_1",     "FORESTED_1", "TREE_C1", "SHRUB_C1", "BEUMC_S2",   "REALM_2",
                                                   "GROUP_2",    "CLASS_2",    "KIND_2",     "SITE_S2",    "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C", "SITEAM_S2D", "SITEMC_S2",  "SITE_M2A",   "SITE_M2B",   "STRCT_S2",   "STRCT_M2",   "STAND_A2",   "SERAL_2",
                                                   "DISTCLS_2",  "DISTSCLS_2", "DISSSCLS_2", "SECL_2",     "SESUBCL_2",  "COND_2",     "VIAB_2",     "FORESTED_2","TREE_C2", "SHRUB_C2", "BEUMC_S3",   "REALM_3",    "GROUP_3",   "CLASS_3",    "KIND_3",     "SITE_S3" ,   "SITEAM_S3A",
                                                   "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3",  "SITE_M3A",   "SITE_M3B",   "STRCT_S3",   "STRCT_M3",   "STAND_A3",   "SERAL_3",    "DISTCLS_3",  "DISTSCLS_3", "DISSSCLS_3", "SECL_3",     "SESUBCL_3",
                                                   "COND_3", "VIAB_3", "FORESTED_3", "TREE_C3", "SHRUB_C3"),
                          date_attributes = NULL,
                          burn = NULL, output_raster = FALSE, verbose = TRUE) {
  rasterize_sf(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               burn = burn,
               output_raster = output_raster,
               verbose = verbose)
}
