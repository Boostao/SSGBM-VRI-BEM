#' Convert sf to raster
#'
#' Convert all attributes of interest of sf into raster layers
#'
#' @param src_datasource character, path of source file
#' @param dst_filename character, path of destination file
#' @param layer character, name of the layer of interest
#' @param reference character of SpatRaster, will be used as reference raster for extent, resolution and crs
#' @param numeric_attributes character vector of names of attributes of type numeric
#' @param character_attributes character vector of names of attributes of type character
#' @param date_attributes character vector of names of attributes of type date
#' @param factor_conv_list list of data.table that contains corresponding factor values for each variable that is not numeric
#' @param burn character, create a layer with the name of the "burn" argument which burn the value 1 where polygons intersect with the extent of the raster
#' @param output_raster boolean, if TRUE, the resulting raster will be returned
#' @param verbose boolean, if TRUE progression message will be printed
#' @inheritParams gdalUtils::gdal_rasterize
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom sf st_layers gdal_utils
#' @export
rasterize_sf_gdal <- function(src_datasource, dst_filename, layer =  NULL, a_srs = NULL, te = NULL, tr = NULL, reference = NULL,
                         numeric_attributes = NULL, character_attributes = NULL, date_attributes = NULL, factor_conv_list = NULL,
                         burn = NULL, output_raster = FALSE, verbose = TRUE) {

  # if provided use parameters from reference raster
  if (!is.null(reference)) {

    if (inherits(reference, "character")) {
      ref_raster <- rast(reference)
    } else {
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

  # create temp dst_file for all attributes
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
      sf::gdal_utils(util = "rasterize",
                     source = src_datasource,
                     destination = paste0(dst_file_no_ext, "_",numeric_attributes[i],".", dst_file_extension),
                     options = c("-a", numeric_attributes[i],
                                 "-a_srs", a_srs,
                                 "-te", te, # Bounding box 
                                 "-tr", tr, # resolution
                                 "-l", layer # layer name
                                 ))
      # gdalUtils::gdal_rasterize(src_datasource = src_datasource,
      #                           dst_filename =  paste0(dst_file_no_ext, "_",numeric_attributes[i],".", dst_file_extension),
      #                           a = numeric_attributes[i],
      #                           a_srs = a_srs,
      #                           te = te,
      #                           tr = tr,
      #                           l  = layer)
    }
  }

  # create temp raster file for character attributes
  if (!is.null(character_attributes)) {
    for (i in seq.int(along.with = character_attributes)) {
      factor_dt <- tryCatch(expr = factor_conv_list[[character_attributes[i]]][!is.na(value)],
                            error = function(x) message(paste0(x, ", skipping layer")))
      if (!is.null(factor_dt)) {
        if (nrow(factor_dt) > 0) {
          if (nrow(factor_dt) <= 30) {
            if (verbose) {
              message(paste0("creating raster layer for ", character_attributes[i]))
            }
            dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension))
            layers_names <- c(layers_names, paste0(character_attributes[i]))
             sf::gdal_utils(util = "rasterize",
                     source = src_datasource,
                     destination = paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension),
                     options = c("-a", character_attributes[i],
                                 "-sql", paste0("SELECT ", paste0("(",character_attributes[i]," = \'", factor_dt$value ,"\') * ", factor_dt$factor, collapse = " + ")," AS ", character_attributes[i], " FROM ", layer),
                                 "-a_srs", a_srs,
                                 "-te", te, # Bounding box 
                                 "-tr", tr # resolution
                                 ))
            
            # gdalUtils::gdal_rasterize(src_datasource = src_datasource,
            #                           dst_filename =  paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension),
            #                           a = character_attributes[i],
            #                           sql = paste0("SELECT ", paste0("(",character_attributes[i]," = \'", factor_dt$value ,"\') * ", factor_dt$factor, collapse = " + ")," AS ", character_attributes[i], " FROM ", layer),
            #                           a_srs = a_srs,
            #                           te = te,
            #                           tr = tr)
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
              sf::gdal_utils(util = "rasterize",
                     source = src_datasource,
                     destination = paste0(dst_file_no_ext, "_",character_attributes[i],"_",j,".", dst_file_extension),
                     options = c("-a", character_attributes[i],
                                 "-sql", paste0("SELECT ", paste0("(",character_attributes[i]," = \'", sub_factor_dt$value ,"\') * ", sub_factor_dt$factor, collapse = " + ")," AS ", character_attributes[i], " FROM ", layer),
                                 "-a_srs", a_srs,
                                 "-te", te, # Bounding box 
                                 "-tr", tr # resolution
                                 ))
              # gdalUtils::gdal_rasterize(src_datasource = src_datasource,
              #                           dst_filename = paste0(dst_file_no_ext, "_",character_attributes[i],"_",j,".", dst_file_extension),
              #                           a = character_attributes[i],
              #                           sql = paste0("SELECT ", paste0("(",character_attributes[i]," = \'", sub_factor_dt$value ,"\') * ", sub_factor_dt$factor, collapse = " + ")," AS ", character_attributes[i], " FROM ", layer),
              #                           a_srs = a_srs,
              #                           te = te,
              #                           tr = tr)

            }
          }
        }
        else {
          if (verbose) {
            message(paste0("creating raster layer for ", character_attributes[i]))
          }
          dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension))
          layers_names <- c(layers_names, paste0(character_attributes[i]))
           sf::gdal_utils(util = "rasterize",
                     source = src_datasource,
                     destination = paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension),
                     options = c("-burn", "0",
                                 "-a_srs", a_srs,
                                 "-te", te, # Bounding box 
                                 "-tr", tr, # resolution
                                 "-l", layer # layer name
                                 ))
          # gdalUtils::gdal_rasterize(src_datasource = src_datasource,
          #                           dst_filename =  paste0(dst_file_no_ext, "_",character_attributes[i],".", dst_file_extension),
          #                           burn = 0,
          #                           a_srs = a_srs,
          #                           te = te,
          #                           tr = tr,
          #                           l = layer)
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
       sf::gdal_utils(util = "rasterize",
                     source = src_datasource,
                     destination = paste0(dst_file_no_ext, "_",date_attributes[i],".", dst_file_extension),
                     options = c("-a", date_attributes[i],
                                 "-sql", paste0("SELECT  CAST(",date_attributes[i]," as integer(3)) as ",date_attributes[i]," FROM ",layer),
                                 "-a_srs", a_srs,
                                 "-te", te, # Bounding box 
                                 "-tr", tr # resolution
                                 ))
      
      
      # gdalUtils::gdal_rasterize(src_datasource = src_datasource,
      #                           dst_filename =  paste0(dst_file_no_ext, "_",date_attributes[i],".", dst_file_extension),
      #                           a = date_attributes[i],
      #                           sql =  paste0("SELECT  CAST(",date_attributes[i]," as integer(3)) as ",date_attributes[i]," FROM ",layer),
      #                           a_srs = a_srs,
      #                           te = te,
      #                           tr = tr)
    }
  }

  # create temp raster file for burn
  if (!is.null(burn)) {
    if (verbose) {
      message(paste0("creating raster layer for ", burn))
    }
    dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_",burn,".", dst_file_extension))
    layers_names <- c(layers_names, burn)

      sf::gdal_utils(util = "rasterize",
                      source = src_datasource,
                      destination = paste0(dst_file_no_ext, "_",burn,".", dst_file_extension),
                      options = c("-a", burn,
                                  "-sql", paste0("SELECT 1 as ",burn," FROM ",layer),
                                  "-a_srs", a_srs,
                                  "-te", te, # Bounding box 
                                  "-tr", tr # resolution
                                  ))
    # gdalUtils::gdal_rasterize(src_datasource = src_datasource,
    #                           dst_filename =  paste0(dst_file_no_ext, "_",burn,".", dst_file_extension),
    #                           a = burn,
    #                           sql =  paste0("SELECT 1 as ",burn," FROM ",layer),
    #                           a_srs = a_srs,
    #                           te = te,
    #                           tr = tr)
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
  rasterize_sf_gdal(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               factor_conv_list = raster_conv$vri,
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
  rasterize_sf_gdal(src_datasource = src_datasource,
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
  rasterize_sf_gdal(src_datasource = src_datasource,
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
  rasterize_sf_gdal(src_datasource = src_datasource,
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


#' Convert Broad ecosystem mapping to raster
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
  rasterize_sf_gdal(src_datasource = src_datasource,
               dst_filename = dst_filename,
               layer =  layer,
               a_srs = a_srs,
               te = te, tr = tr,
               reference = reference,
               numeric_attributes = numeric_attributes,
               character_attributes = character_attributes,
               date_attributes = date_attributes,
               factor_conv_list = raster_conv$bem,
               burn = burn,
               output_raster = output_raster,
               verbose = verbose)
}

gdal_sql_identifier <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

gdal_append_rasterize_grid_options <- function(options, a_srs, te, tr, layer = NULL) {
  if (!is.null(a_srs)) {
    options <- c(options, "-a_srs", a_srs)
  }
  if (!is.null(te)) {
    options <- c(options, "-te", as.character(te))
  }
  if (!is.null(tr)) {
    options <- c(options, "-tr", as.character(tr))
  }
  if (!is.null(layer)) {
    options <- c(options, "-l", layer)
  }
  options
}

combine_materialized_rasters <- function(dst_filename_att, layers_names, dst_filename, output_raster, verbose) {
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

  if (verbose) {
    message(paste0("Writing new raster file at ", dst_filename))
  }
  terra::writeRaster(x = total_raster, filename = dst_filename, overwrite = TRUE)

  if (verbose) {
    message("deleting temporary files")
  }
  lapply(dst_filename_att, unlink)

  if (output_raster) {
    return(rast(dst_filename))
  }

  return(NULL)
}

#' Convert sf to raster using materialized on-disk coded fields
#'
#' Convert selected attributes of a large vector layer into raster layers while
#' avoiding large in-memory joins. Character, date, and burn fields are first
#' materialized as numeric fields in a temporary GeoPackage on disk, then
#' rasterized with GDAL using `-a` on the derived numeric field.
#'
#' This is an alternative to [rasterize_sf_gdal()] that avoids long SQL mapping
#' expressions such as `(value = 'A') * 1 + (value = 'B') * 2 + ...`.
#'
#' @inheritParams rasterize_sf_gdal
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @importFrom terra `add<-` crs ext rast writeRaster res
#' @importFrom sf st_layers gdal_utils st_write
#' @export
rasterize_sf_gdal_materialized <- function(src_datasource, dst_filename, layer = NULL, a_srs = NULL, te = NULL, tr = NULL, reference = NULL,
                                           numeric_attributes = NULL, character_attributes = NULL, date_attributes = NULL, factor_conv_list = NULL,
                                           burn = NULL, output_raster = FALSE, verbose = TRUE) {

  if (!is.null(reference)) {
    if (inherits(reference, "character")) {
      ref_raster <- rast(reference)
    } else {
      ref_raster <- reference
    }

    a_srs <- crs(ref_raster, proj = TRUE)
    extent <- ext(ref_raster)
    te <- c(extent[1], extent[3], extent[2], extent[4])
    tr <- res(ref_raster)
  }

  if (is.null(layer)) {
    layer <- st_layers(src_datasource)$name[1]
  }

  pos <- regexpr("\\.([[:alnum:]]+)$", dst_filename)
  dst_file_extension <- ifelse(pos > -1L, substring(dst_filename, pos + 1L), "")
  dst_file_no_ext <- substr(dst_filename, 1, nchar(dst_filename) - (nchar(dst_file_extension) + 1))

  dst_filename_att <- character(0)
  layers_names <- character(0)

  temp_gpkg <- tempfile(pattern = "rasterize_sf_materialized_", fileext = ".gpkg")
  temp_layer <- paste0(gsub("[^A-Za-z0-9_]+", "_", basename(dst_file_no_ext)), "_src")
  on.exit(unlink(temp_gpkg), add = TRUE)

  if (verbose) {
    message(paste0("Materializing source layer into ", temp_gpkg))
  }

  sf::gdal_utils(
    util = "vectortranslate",
    source = src_datasource,
    destination = temp_gpkg,
    options = c(
      "-f", "GPKG",
      "-overwrite",
      "-nlt", "PROMOTE_TO_MULTI",
      "-nln", temp_layer,
      "-dialect", "SQLite",
      "-sql", paste0("SELECT * FROM ", gdal_sql_identifier(layer))
    )
  )

  if (!is.null(numeric_attributes)) {
    for (i in seq.int(along.with = numeric_attributes)) {
      if (verbose) {
        message(paste0("creating raster layer for ", numeric_attributes[i]))
      }

      dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_", numeric_attributes[i], ".", dst_file_extension))
      layers_names <- c(layers_names, numeric_attributes[i])

      options <- c("-a", numeric_attributes[i])
      options <- gdal_append_rasterize_grid_options(options, a_srs, te, tr, temp_layer)

      sf::gdal_utils(
        util = "rasterize",
        source = temp_gpkg,
        destination = paste0(dst_file_no_ext, "_", numeric_attributes[i], ".", dst_file_extension),
        options = options
      )
    }
  }

  if (!is.null(character_attributes)) {
    for (i in seq.int(along.with = character_attributes)) {
      factor_dt <- tryCatch(
        expr = factor_conv_list[[character_attributes[i]]][!is.na(value)],
        error = function(x) {
          message(paste0(x, ", skipping layer"))
          NULL
        }
      )

      if (is.null(factor_dt)) {
        next
      }

      if (verbose) {
        message(paste0("creating raster layer for ", character_attributes[i]))
      }

      dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_", character_attributes[i], ".", dst_file_extension))
      layers_names <- c(layers_names, character_attributes[i])

      if (nrow(factor_dt) > 0) {
        lookup_layer <- paste0("lookup_", gsub("[^A-Za-z0-9_]+", "_", character_attributes[i]))
        coded_field <- paste0(character_attributes[i], "__coded")

        sf::st_write(
          obj = data.frame(value = factor_dt$value, factor = factor_dt$factor),
          dsn = temp_gpkg,
          layer = lookup_layer,
          quiet = TRUE,
          append = TRUE
        )

        sql_statement <- paste0(
          "SELECT src.*, CAST(lkp.", gdal_sql_identifier("factor"), " AS INTEGER) AS ", gdal_sql_identifier(coded_field),
          " FROM ", gdal_sql_identifier(temp_layer), " src",
          " LEFT JOIN ", gdal_sql_identifier(lookup_layer), " lkp",
          " ON src.", gdal_sql_identifier(character_attributes[i]), " = lkp.", gdal_sql_identifier("value")
        )

        options <- c(
          "-dialect", "SQLite",
          "-sql", sql_statement,
          "-a", coded_field
        )
      } else {
        options <- c("-burn", "0")
        options <- gdal_append_rasterize_grid_options(options, a_srs, te, tr, temp_layer)
      }

      if (nrow(factor_dt) > 0) {
        options <- gdal_append_rasterize_grid_options(options, a_srs, te, tr)
      }

      sf::gdal_utils(
        util = "rasterize",
        source = temp_gpkg,
        destination = paste0(dst_file_no_ext, "_", character_attributes[i], ".", dst_file_extension),
        options = options
      )
    }
  }

  if (!is.null(date_attributes)) {
    for (i in seq.int(along.with = date_attributes)) {
      if (verbose) {
        message(paste0("creating raster layer for ", date_attributes[i]))
      }

      coded_field <- paste0(date_attributes[i], "__coded")
      sql_statement <- paste0(
        "SELECT src.*, CAST(src.", gdal_sql_identifier(date_attributes[i]), " AS INTEGER) AS ", gdal_sql_identifier(coded_field),
        " FROM ", gdal_sql_identifier(temp_layer), " src"
      )

      dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_", date_attributes[i], ".", dst_file_extension))
      layers_names <- c(layers_names, date_attributes[i])

      options <- c(
        "-dialect", "SQLite",
        "-sql", sql_statement,
        "-a", coded_field
      )
      options <- gdal_append_rasterize_grid_options(options, a_srs, te, tr)

      sf::gdal_utils(
        util = "rasterize",
        source = temp_gpkg,
        destination = paste0(dst_file_no_ext, "_", date_attributes[i], ".", dst_file_extension),
        options = options
      )
    }
  }

  if (!is.null(burn)) {
    if (verbose) {
      message(paste0("creating raster layer for ", burn))
    }

    coded_field <- paste0(burn, "__coded")
    sql_statement <- paste0(
      "SELECT src.*, CAST(1 AS INTEGER) AS ", gdal_sql_identifier(coded_field),
      " FROM ", gdal_sql_identifier(temp_layer), " src"
    )

    dst_filename_att <- c(dst_filename_att, paste0(dst_file_no_ext, "_", burn, ".", dst_file_extension))
    layers_names <- c(layers_names, burn)

    options <- c(
      "-dialect", "SQLite",
      "-sql", sql_statement,
      "-a", coded_field
    )
    options <- gdal_append_rasterize_grid_options(options, a_srs, te, tr)

    sf::gdal_utils(
      util = "rasterize",
      source = temp_gpkg,
      destination = paste0(dst_file_no_ext, "_", burn, ".", dst_file_extension),
      options = options
    )
  }

  if (length(dst_filename_att) == 0) {
    if (output_raster) {
      return(NULL)
    }
    return(NULL)
  }

  combine_materialized_rasters(dst_filename_att, layers_names, dst_filename, output_raster, verbose)
}

#' Convert vri to raster using materialized on-disk coded fields
#'
#' @inheritParams rasterize_sf_gdal_materialized
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @export
rasterize_vri_materialized <- function(src_datasource, dst_filename, layer = NULL, a_srs = NULL, te = NULL,
                                       tr = NULL, reference = NULL,
                                       numeric_attributes = c(paste0("SPEC_PCT_", 1:6), "CR_CLOSURE", "COV_PCT_1", "PROJ_AGE_1"),
                                       character_attributes = c(paste0("BCLCS_LV_", 1:5), paste0("SPEC_CD_", 1:6), "LAND_CD_1", "LBL_VEGCOV"),
                                       date_attributes = "HRVSTDT",
                                       burn = NULL, output_raster = FALSE, verbose = TRUE) {
  rasterize_sf_gdal_materialized(
    src_datasource = src_datasource,
    dst_filename = dst_filename,
    layer = layer,
    a_srs = a_srs,
    te = te,
    tr = tr,
    reference = reference,
    numeric_attributes = numeric_attributes,
    character_attributes = character_attributes,
    date_attributes = date_attributes,
    factor_conv_list = raster_conv$vri,
    burn = burn,
    output_raster = output_raster,
    verbose = verbose
  )
}

#' Convert wetlands to raster using materialized on-disk coded fields
#'
#' @inheritParams rasterize_sf_gdal_materialized
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @export
rasterize_wetlands_materialized <- function(src_datasource, dst_filename, layer = NULL, a_srs = NULL, te = NULL,
                                            tr = NULL, reference = NULL,
                                            numeric_attributes = NULL,
                                            character_attributes = NULL,
                                            date_attributes = NULL,
                                            burn = "wl_pct", output_raster = FALSE, verbose = TRUE) {
  rasterize_sf_gdal_materialized(
    src_datasource = src_datasource,
    dst_filename = dst_filename,
    layer = layer,
    a_srs = a_srs,
    te = te,
    tr = tr,
    reference = reference,
    numeric_attributes = numeric_attributes,
    character_attributes = character_attributes,
    date_attributes = date_attributes,
    burn = burn,
    output_raster = output_raster,
    verbose = verbose
  )
}

#' Convert rivers to raster using materialized on-disk coded fields
#'
#' @inheritParams rasterize_sf_gdal_materialized
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @export
rasterize_rivers_materialized <- function(src_datasource, dst_filename, layer = NULL, a_srs = NULL, te = NULL,
                                          tr = NULL, reference = NULL,
                                          numeric_attributes = NULL,
                                          character_attributes = NULL,
                                          date_attributes = NULL,
                                          burn = "rivers", output_raster = FALSE, verbose = TRUE) {
  rasterize_sf_gdal_materialized(
    src_datasource = src_datasource,
    dst_filename = dst_filename,
    layer = layer,
    a_srs = a_srs,
    te = te,
    tr = tr,
    reference = reference,
    numeric_attributes = numeric_attributes,
    character_attributes = character_attributes,
    date_attributes = date_attributes,
    burn = burn,
    output_raster = output_raster,
    verbose = verbose
  )
}

#' Convert consolidated cutblocks to raster using materialized on-disk coded fields
#'
#' @inheritParams rasterize_sf_gdal_materialized
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @export
rasterize_ccb_materialized <- function(src_datasource, dst_filename, layer = NULL, a_srs = NULL, te = NULL,
                                       tr = NULL, reference = NULL,
                                       numeric_attributes = "HARVESTYR",
                                       character_attributes = NULL,
                                       date_attributes = NULL,
                                       burn = NULL, output_raster = FALSE, verbose = TRUE) {
  rasterize_sf_gdal_materialized(
    src_datasource = src_datasource,
    dst_filename = dst_filename,
    layer = layer,
    a_srs = a_srs,
    te = te,
    tr = tr,
    reference = reference,
    numeric_attributes = numeric_attributes,
    character_attributes = character_attributes,
    date_attributes = date_attributes,
    burn = burn,
    output_raster = output_raster,
    verbose = verbose
  )
}

#' Convert Broad ecosystem mapping to raster using materialized on-disk coded fields
#'
#' @inheritParams rasterize_sf_gdal_materialized
#' @return SpatRaster if output_raster is TRUE, NULL otherwise
#' @export
rasterize_bem_materialized <- function(src_datasource, dst_filename, layer = NULL, a_srs = NULL, te = NULL,
                                       tr = NULL, reference = NULL,
                                       numeric_attributes = c("TEIS_ID", "Area_Ha", "AGE_CL_STS", "SDEC_1", "SDEC_2", "SDEC_3", "BGC_VRT"),
                                       character_attributes = c("BGC_ZONE", "BGC_SUBZON", "BGC_PHASE", "SLOPE_MOD", "ECO_SEC", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A",
                                                                "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1", "TREE_C1", "SHRUB_C1", "BEUMC_S2", "REALM_2",
                                                                "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C", "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2", "SERAL_2",
                                                                "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2", "SESUBCL_2", "COND_2", "VIAB_2", "FORESTED_2", "TREE_C2", "SHRUB_C2", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3", "KIND_3", "SITE_S3", "SITEAM_S3A",
                                                                "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3", "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3",
                                                                "COND_3", "VIAB_3", "FORESTED_3", "TREE_C3", "SHRUB_C3"),
                                       date_attributes = NULL,
                                       burn = NULL, output_raster = FALSE, verbose = TRUE) {
  rasterize_sf_gdal_materialized(
    src_datasource = src_datasource,
    dst_filename = dst_filename,
    layer = layer,
    a_srs = a_srs,
    te = te,
    tr = tr,
    reference = reference,
    numeric_attributes = numeric_attributes,
    character_attributes = character_attributes,
    date_attributes = date_attributes,
    factor_conv_list = raster_conv$bem,
    burn = burn,
    output_raster = output_raster,
    verbose = verbose
  )
}