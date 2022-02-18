# convert vri from vectors to raster

# BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3, BCLCS_LEVEL_4, BCLCS_LEVEL_5,
# SPECIES_CD_1, SPECIES_CD_2, SPECIES_CD_3, SPECIES_CD_4, SPECIES_CD_5, SPECIES_CD_6,
# SPECIES_PCT_1, SPECIES_PCT_2, SPECIES_PCT_3, SPECIES_PCT_4, SPECIES_PCT_5, SPECIES_PCT_6,
# CROWN_CLOSURE, LAND_COVER_CLASS_CD_1, EST_COVERAGE_PCT_1, LINE_5_VEGETATION_COVER,
# HARVEST_DATE, PROJ_AGE_1

# create raster for numeric attributes
#library(gdalUtils)


rasterize_vri <- function(src_datasource, dst_filename, layer, a_srs, te, tr, reference, verbose = TRUE) {

  # if provided use parameters from reference raster
  if (!is.null(reference)) {
    ref_raster <- rast(reference)
    a_srs <- crs(ref_raster, proj = T)
    extent <- ext(ref_raster)
    te <- c(extent[1], extent[3], extent[2], extent[4])
    tr <- res(ref_raster)
  }

  # get layer name
  if  (is.null(layer)) {
    layer <- st_layers("../SSGBM-VRI-BEM-data/vri_bcgov.shp")$name[1]
  }

  # get file extension
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  dst_file_extension <- ifelse(pos > -1L, substring(x, pos + 1L), "")
  dst_file_no_ext <- substr(dst_filename, 1 , length(dst_filename) - (length(file_extension) +1))

  # define type of attributes
  numeric_attributes <- c(paste0("SPECT_PCT", 1:6), "CR_CLOSURE", "COV_PCT_1", "PROJ_AGE_1")
  character_attributes <- c(paste0("BCLCS_LV_", 1:6), paste0("SPEC_CD_", 1:6), "LAND_CD_1", "LBL_VEGCOV")
  date_attributes <- "HRVSTDT"

  # creat temp dst_file for all attributes
  dst_filename_att <- paste0(dst_file_no_ext, "_", c(numeric_attributes, character_attributes, date_attributes),".", dst_file_extension)

  # create temp raster file for numeric attributes
  for (i in seq.int(along.with = numeric_attributes)) {
    if (verbose) {
      message(paste0("creating raster layer for ", numeric_attributes[i]))
    }
    gdal_rasterize(src_datasource = src_datasource,
                   dst_filename =  dst_filename_att[i],
                   a = numeric_attributes[i],
                   a_srs = a_srs,
                   te = te,
                   tr = tr)
  }

  # create temp raster file for character attributes
  for (i in seq.int(along.with = character_attributes)) {
    factor_dt <- get(character_attributes[i])
    for (j in seq.int(length.out = nrow(factor_dt))) {
      if (verbose) {
        message(paste0("creating raster layer for ", character_attributes[i], " for value ", factor_dt$value[j]))
      }
      gdal_rasterize(src_datasource = src_datasource,
                     dst_filename =  dst_filename_att[i + length(numeric_attributes)],
                     burn = factor_dt$factor[j],
                     where = cat('\'','\"', character_attributes[i], '\"', ' = \'', factor_dt$value[j], '\'\'', sep = ""),
                     a_srs = a_srs,
                     te = te,
                     tr = tr)
    }
  }

  for (i in seq.int(along.with = date_attributes)) {
    gdal_rasterize(src_datasource = src_datasource,
                   dst_filename =  dst_filename_att[i +  length(numeric_attributes) + length(character_attributes)],
                   a = date_attributes[i],
                   sql =  paste0("SELECT  CAST(",date_attributes[i]," as integer(3)) as ",date_attributes[i]," FROM ",layer),
                   a_srs = a_srs,
                   te = te,
                   tr = tr)
  }



}
