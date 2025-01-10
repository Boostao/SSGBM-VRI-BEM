
#' Initialize a database
#' @rdname init
#' @param dbdir Location for database files. Should be a path to an existing directory in the file system. With the default (or ""), all data is kept in RAM.
#' @export
#' @import duckdb
#' @details This needs to be run once to create the database and load the required spatial dataset into it.
#' The database can be stored locally for faster processing. 
init_db <- function(dbdir = defdb(), bem_dsn) {

  conn <- init_conn(dbdir)
  init_vri(conn)
  init_bem(conn, bem_dsn)
  init_wetlands(conn)
  init_rivers(conn)
  init_lakes(conn)
  init_glaciers(conn)
  init_ccb(conn)
  init_burn(conn)
  init_fire(conn)
  duckdb::dbDisconnect(conn, shutdown = TRUE)

}

#' Initialize and load spatial dataset to database
#'
#' @param conn Connection to database.
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database); in case of GeoJSON, dsn may be the character string holding the geojson data. It can also be an open database connection.
#' When dsn is left empty/NULL the polygons will be fetched from the BC Data Catalog.
#' @param layer layer name (varies by driver, may be a file name without extension); in case layer is missing, st_read will read the first layer of dsn, give a warning and (unless quiet = TRUE) print a message when there are multiple layers, or give an error if there are no layers in dsn. If dsn is a database connection, then layer can be a table name or a database identifier (see Id). It is also possible to omit layer and rather use the query argument.
#' @param geom geometry field name to use. Default to `Shape`.
#' @importFrom bcdata bcdc_tidy_resources
#' @export
#' @rdname init
init_vri <- function(conn = init_conn(), dsn = NULL, layer = "VEG_COMP_LYR_R1_POLY", geom = "Shape") {

  # vegetation resources inventory  (VRI)
  vri_record <- "2ebb35d8-c82f-4a17-9c96-612ac3532d55"
  vri_resources <- bcdata::bcdc_tidy_resources(vri_record)

  vri_vars <- c(
    "FEATURE_ID",
    "BCLCS_LEVEL_1" = "BCLCS_LV_1",
    "BCLCS_LEVEL_2" = "BCLCS_LV_2",
    "BCLCS_LEVEL_3" = "BCLCS_LV_3",
    "BCLCS_LEVEL_4" = "BCLCS_LV_4",
    "BCLCS_LEVEL_5" = "BCLCS_LV_5",
    "BEC_PHASE" = "VRI_BEC_PHASE",
    "BEC_SUBZONE" = "VRI_BEC_SUBZON",
    "BEC_VARIANT" = "VRI_BEC_VRT",
    "BEC_ZONE_CODE" = "VRI_BEC_ZONE",
    "CROWN_CLOSURE" = "CR_CLOSURE",
    "EST_COVERAGE_PCT_1" = "COV_PCT_1",
    "HARVEST_DATE" = "HRVSTDT",
    "INVENTORY_STANDARD_CD",
    "LAND_COVER_CLASS_CD_1" = "LAND_CD_1",
    "LINE_5_VEGETATION_COVER" = "LBL_VEGCOV",
    "PROJ_AGE_1",
    "REFERENCE_YEAR" = "VRI_SURVEY_YEAR",
    "SOIL_MOISTURE_REGIME_1",
    "SOIL_NUTRIENT_REGIME",
    "SPECIES_CD_1" = "SPEC_CD_1",
    "SPECIES_CD_2" = "SPEC_CD_2",
    "SPECIES_CD_3" = "SPEC_CD_3",
    "SPECIES_CD_4" = "SPEC_CD_4",
    "SPECIES_CD_5" = "SPEC_CD_5",
    "SPECIES_CD_6" = "SPEC_CD_6",
    "SPECIES_PCT_1" = "SPEC_PCT_1",
    "SPECIES_PCT_2" = "SPEC_PCT_2",
    "SPECIES_PCT_3" = "SPEC_PCT_3",
    "SPECIES_PCT_4" = "SPEC_PCT_4",
    "SPECIES_PCT_5" = "SPEC_PCT_5",
    "SPECIES_PCT_6" = "SPEC_PCT_6"
  )

  vri_geom <- "%s Shape" |> sprintf(geom)

  if (is.null(dsn)) {
    dsn <- resolve_resource(vri_resources)
    on.exit(unlink(dsn, recursive = TRUE), add = TRUE)
  }

  layer_proj4 <- meta_proj4(dsn, layer, geom)

  if (layer_proj4 != target_proj4) {
    vri_geom <- "ST_MakeValid(ST_Transform(%s, '%s', '%s', always_xy := true)) Shape" |> sprintf(geom, layer_proj4, target_proj4)
  }

  logger::log_info("Loading VRI into duckdb database.")

  duckdb::dbSendQuery(
    conn, "
    CREATE OR REPLACE TABLE VRI AS (
      SELECT %s, %s FROM ST_Read('%s', layer := '%s')
    );" |>
      sprintf(
        paste0(paste(names(vri_vars), vri_vars) |> trimws(), collapse = ","),
        vri_geom,
        dsn,
        layer
      )
  )

  logger::log_info("Creating VRI spatial index.")

  duckdb::dbSendQuery(conn, "DROP INDEX IF EXISTS VRI_IDX; CREATE INDEX VRI_IDX ON VRI USING RTREE (Shape);")

  return(invisible())
  
}

#' @rdname init
#' @export
init_bem <- function(conn = init_conn(), dsn = NULL, layer = "BEM", geom = "geom") {

  # broad ecosystem mapping (BEM)
  bem_geom <- "%s Shape" |> sprintf(geom)

  if (is.null(dsn)) {
    logger::log_error("No value for dsn to read `BEM` from.")
  }

  if (!file.exists(dsn)) {
    logger::log_error("Provided value for dsn is does not exist [%s]." |> sprintf(dsn))
  }

  layer_proj4 <- meta_proj4(dsn, layer, geom)

  if (layer_proj4 != target_proj4) {
    bem_geom <- "ST_MakeValid(ST_Transform(%s, '%s', '%s', always_xy := true)) Shape" |> sprintf(geom, layer_proj4, target_proj4)
  }

  logger::log_info("Loading BEM into duckdb database.")

  bem_vars <- duckdb::dbSendQuery(conn, "SELECT * FROM ST_Read('%s', layer := '%s') LIMIT 0" |> sprintf(dsn, layer)) |>
    duckdb::dbFetch()|> names() |> setdiff(geom)

  duckdb::dbSendQuery(
    conn, "
    CREATE OR REPLACE TABLE BEM AS (
      SELECT %s, %s FROM ST_Read('%s', layer := '%s')
    );" |>
      sprintf(
        paste0(paste(names(bem_vars), bem_vars) |> trimws(), collapse = ","),
        bem_geom,
        dsn,
        layer
      )
  )

  logger::log_info("Creating BEM spatial index.")

  duckdb::dbSendQuery(conn, "DROP INDEX IF EXISTS BEM_IDX; CREATE INDEX BEM_IDX ON BEM USING RTREE (Shape);")

  return(invisible())

}

#' @noRd
init_generic <- function(conn = init_conn(), dsn = NULL, layer, geom = "geom", recordid, filter1 = identity, .include = c(), tablename) {
  
  gen_geom <- "%s Shape" |> sprintf(geom)

  #If dsn is null read information from bcdata
  if (is.null(dsn)) {

    select1 <- if (length(.include)) { \(x) { bcdata::select(x, .include = .include) } } else { bcdata::select }
    dsn <- bcdata::bcdc_query_geodata(record = recordid) |> select1() |> filter1() |> collect_geojson()
    on.exit(unlink(dsn), add = TRUE)

    layer_proj4 <- meta_proj4(dsn[1], geom_f = geom)
    if (layer_proj4 != target_proj4) {
      gen_geom <- "ST_MakeValid(ST_Transform(%s, '%s', '%s', always_xy := true)) Shape" |> sprintf(geom, layer_proj4, target_proj4)
    }
    query <- "SELECT %s FROM (%s)" |>
      sprintf(
        paste0(c(.include, gen_geom), collapse = ","),
        paste0("SELECT ", paste0(c(.include, geom), collapse = ","), " FROM ST_Read('", dsn, "')", collapse = " UNION ALL ")
      )

  } else {
    
    layer_proj4 <- meta_proj4(dsn, layer = layer, geom_f = geom)
    if (layer_proj4 != target_proj4) {
      gen_geom <- "ST_MakeValid(ST_Transform(%s, '%s', '%s', always_xy := true)) Shape" |> sprintf(geom, layer_proj4, target_proj4)
    }
    query <- "SELECT %s FROM ST_Read('%s%', layer := '%s%')" |>
      sprintf(paste0(c(.include, gen_geom), collapse = ","), dsn, layer)

  }

  logger::log_info("Loading %s into duckdb database." |> sprintf(tablename))

  duckdb::dbSendQuery(conn, "CREATE OR REPLACE TABLE %s AS (%s);" |> sprintf(tablename, query)) |> duckdb::dbClearResult()

  logger::log_info("Creating %s spatial index." |> sprintf(tablename))

  duckdb::dbSendQuery(conn, "DROP INDEX IF EXISTS %s_IDX; CREATE INDEX %s_IDX ON %s USING RTREE (Shape);" |> sprintf(tablename, tablename, tablename))

  return(invisible())

}

#' @rdname init
#' @export
init_wetlands <- function(conn = init_conn(), dsn = NULL, layer = "FWA_WETLANDS_POLY", geom = "geom") {

  init_generic(conn, dsn, layer, geom, recordid = "93b413d8-1840-4770-9629-641d74bd1cc6", tablename = "WETLANDS")

}


#' @rdname init
#' @export
init_rivers <- function(conn = init_conn(), dsn = NULL, layer = "FWA_RIVERS_POLY", geom = "geom") {

  init_generic(conn, dsn, layer, geom, recordid = "f7dac054-efbf-402f-ab62-6fc4b32a619e", tablename = "RIVERS")
  
}

#' @rdname init
#' @export
init_lakes <- function(conn = init_conn(), dsn = NULL, layer = "FWA_LAKES_POLY", geom = "geom") {

  init_generic(conn, dsn, layer, geom, recordid = "cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", tablename = "LAKES")

}


#' @rdname init
#' @export
# from WHSE_BASEMAPPING.BTM_PRESENT_LAND_USE_V1_SVW, which CEF Human Disturbance 2021 BTM glaciers and snow is based on
init_glaciers <- function(conn = init_conn(), dsn = NULL, layer = "BTM_PLU_V1", geom = "geom") {

  filter1 <- \(x) { bcdata::filter(x, PRESENT_LAND_USE_LABEL == "Glaciers and Snow") }
  init_generic(conn, dsn, layer, geom, recordid = "134fdc69-7b0c-4c50-b77c-e8f2553a1d40", tablename = "GLACIERS", filter1 = filter1)

}


#' @rdname init
#' @export
init_ccb <- function(conn = init_conn(), dsn = NULL, layer = "Cut_Block_all_BC", geom = "geom") {

  init_generic(conn, dsn, layer, geom, recordid = "b1b647a6-f271-42e0-9cd0-89ec24bce9f7", tablename = "CCB", .include = "HARVEST_YEAR")
  
}

#' @rdname init
#' @export
init_burn <- function(conn = init_conn(), dsn = NULL, layer = "WHSE_FOREST_VEGETATION_VEG_BURN_SEVERITY_SP", geom = "geom") {

  filter1 <- \(x) { bcdata::filter(x, BURN_SEVERITY_RATING %in% c("High", "Low", "Medium")) }
  init_generic(conn, dsn, layer, geom, recordid = "c58a54e5-76b7-4921-94a7-b5998484e697", tablename = "BURN", filter1 = filter1, .include = "BURN_SEVERITY_RATING")

}

#' @rdname init
#' @export
init_fire <- function(conn = init_conn(), dsn = NULL, layer = "WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP", geom = "geom") {

  init_generic(conn, dsn, layer, geom, recordid = "22c7cb44-1463-48f7-8e47-88857f207702", tablename = "FIRE")

}

#' @param tsa_name Character vector. A vector of TSA_NUMBER_DESCRIPTION values to filter.
#' @param Skeena_boundary Boolean. Restrict tsa to Skeena region.
#' @rdname init
#' @export
init_tsa <- function(conn = init_conn(), tsa_name = c(), Skeena_boundary = TRUE) {

  filter1 <- identity
  if (length(tsa_name)) {
    filter1 <- \(x) { bcdata::filter(x, TSA_NUMBER_DESCRIPTION %in% tsa_name) }
  }  
  init_generic(recordid = "8daa29da-d7f4-401c-83ae-d962e3a28980", filter1 = filter1, .include = "TSA_NUMBER_DESCRIPTION", tablename = "TSA")


  skeena_record <- "dfc492c0-69c5-4c20-a6de-2c9bc999301f"

  #make sure aoi within Skeena boundary (if needed)
  if (isTRUE(Skeena_boundary)) {

    filter1 <- \(x) { bcdata::filter(x, ORG_UNIT_NAME == "Skeena Natural Resource Region") }
    init_generic(recordid = "dfc492c0-69c5-4c20-a6de-2c9bc999301f", filter1 = filter1, .include = "ORG_UNIT_NAME", tablename = "SKEENA")

  }

}

#' @export
#' @rdname init
init_conn <- function(dbdir = defdb()) {

  conn <- try(duckdb::dbConnect(duckdb::duckdb(dbdir = dbdir)), silent = TRUE)
  if (inherits(conn, "try-error")) {
    if (grepl("Missing Extension",conditionMessage(attr(conn,"condition")))) {
      if (file.exists(paste0(dbdir,".wal"))) {
        answer <- readline("Found an existing lock file (.wal) for the database. This can happen when the database is not shutdown cleanly using `dbDisconnect(conn, shutdown = TRUE)`.\nIt will be removed. OK? (y/n/c)")
        if (tolower(answer) %in% c("y","yes")) {
          unlink(paste0(dbdir,".wal"))
          conn <- duckdb::dbConnect(duckdb::duckdb(dbdir = dbdir))
        } else {
          logger::log_error("Could not create a connection to the database.")
          return(invisible())
        }
      }
    }
  }
  duckdb::dbSendQuery(conn, "INSTALL spatial FROM core_nightly; LOAD spatial;") |> duckdb::dbClearResult()
  return(conn)

}

resolve_resource <- function(x) {
  logger::log_info("Retrieving resource from BCDC.")
  x <- x[x$format == "fgdb", c("url", "id")][1,]
  path <- file.path(tempdir(), "ssgbm")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(path, basename(x$url))

  invisible(suppressMessages(trace(curl:::print_stream, at = list(2), print = FALSE, tracer = quote({
    prefix <- paste0("\r", attr(logger::INFO, "level"), " [", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]", sep = "")
    args <- list(...)
    args[[1]] <- gsub("\r", "", args[[1]])
    cat(prefix, do.call(sprintf, args), file = stderr())
    sprintf <- \(...) {""}
  }))))

  curl::multi_download(urls = x$url, destfiles = file)

  suppressMessages(untrace(curl:::print_stream))

  if (tools::file_ext(file) |> tolower() == "zip") {
    logger::log_info("Decompressing resource.")
    unzip(file, exdir = path)
    unlink(file)
    file <- file.path(path, gsub("\\.zip$", "", basename(x$url), ignore.case = TRUE))
  } else {
    file <- file.path(path, basename(x$url))
  }
  return(file)
}

#' Cache directory
#' @rdname cache
#' @export
cache_dir <- function() {
  d <- tools::R_user_dir("ssgbm", "cache")
  if (!dir.exists(d)) {
    logger::log_info("Creating directory to cache package assets at ", d)
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  return(d)
}

#' Remove cache
#' @rdname cache
#' @export
cache_remove <- function() {
  unlink(cache_dir(), recursive = TRUE)
}

#' @noRd
meta_proj4 <- function(dsn, layer, geom_f) {

  if (file.info(dsn)$isdir & grepl(".gdb", dsn, fixed = TRUE)) {
    dsn <- list.files(dsn, full.names = TRUE, pattern = "gdbtable")
  }

  conn <- init_conn(":memory:")
  on.exit(duckdb::dbDisconnect(conn), add = TRUE)

  expr <- expression({
    duckdb::dbSendQuery(conn, "
    SELECT * 
    FROM ST_Read_Meta([%s])" |> sprintf(paste0("'", dsn, "'", collapse = ","))
  )})
  
  res <- try(eval(expr), silent = TRUE)
  while (inherits(res, "try-error")) {
    res <- try(eval(expr), silent = TRUE)
  }
  
  res <- res |> duckdb::dbFetch()
  
  if (!length(res$layers)) {
    logger::log_warn("No layer found in dsn for proj4 [%s]." |> sprintf(dsn))
  }

  if (missing(layer)) {
    l <- 1L
    layer <- res$layers[[1]]$name
    logger::log_info("Using the first layer found for proj4 [%s]." |> sprintf(layer))
  } else {
    l <- which(vapply(res$layers, `[[`, character(1), "name") == layer)
    if (!length(l)) {
      logger::log_warn("Could not find the specified layer in the provided dsn for proj4 [%s: %s]." |> sprintf(dsn, layer))
      return("")
    }
  }

  if (!length(res$layers[[l]]$geometry_fields)) {
    logger::log_warn("No geometry field found for layer in dsn for proj4 [%s: %s]." |> sprintf(dsn, layer))
  }

  if (missing(geom_f)) {
    g <- 1L
    geom_f <- res$layers[[l]]$geometry_fields[[g]]$name
    logger::log_info("Using the first geometry field found in layer for proj4 [%s: %s]." |> sprintf(layer, geom_f))  
  } else {
    g <- which(vapply(res$layers[[l]]$geometry_fields, `[[`, character(1), "name") == geom_f)
    if (!length(g)) {
      logger::log_warn("Could not find the specified geometry field in the provided layer for proj4 [%s: %s]." |> sprintf(layer, geom_f))
      return("")
    }
  }

  proj4 <- res$layers[[l]]$geometry_fields[[g]]$crs$proj4

  return(proj4)

}

#' @noRd
collect_geojson <- function(x, ...) {
  
  x$query_list$CQL_FILTER <- bcdata:::finalize_cql(x$query_list$CQL_FILTER)
  query_list <- x$query_list
  cli <- x$cli
  ## Determine total number of records for pagination purposes
  number_of_records <- bcdata:::bcdc_number_wfs_records(query_list, cli)

  target <- query_list |> {\(x) paste0(names(x),"=",URLencode(x, reserved = TRUE),collapse = "&")}()
  if (number_of_records > 10000) {
    si <- c(0, seq_len(number_of_records %/% 10000) * 10000)
    urls <- "https://openmaps.gov.bc.ca/geo/pub/wfs?startIndex=%s&count=10000" |> sprintf(si) |> paste(target, sep = "&")
  } else {
    urls <- "https://openmaps.gov.bc.ca/geo/pub/wfs?" |> paste(target, sep = "&")
  }

  invisible(suppressMessages(trace(curl:::print_stream, at = list(2), print = FALSE, tracer = quote({
    prefix <- paste0("\r", attr(logger::INFO, "level"), " [", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]", sep = "")
    args <- list(...)
    args[[1]] <- gsub("\r", "", args[[1]])
    cat(prefix, do.call(sprintf, args), file = stderr())
    sprintf <- \(...) {""}
  }))))

  destfiles <- tempfile(rep("bcdatageo",length(urls)), fileext = ".geojson")

  alldld <- FALSE
  allurls <- urls
  alldestfiles <- destfiles
  while (!alldld) {
    res <- curl::multi_download(
      urls = allurls,
      destfiles = alldestfiles,
      resume = FALSE,
      progress = TRUE,
      multiplex = TRUE,
      httpheader = curl:::format_request_headers(
        list(
          "Accept-Encoding" = "gzip, deflate",
          "Accept" = "application/json",
          "User-Agent" = "https://github.com/bcgov/bcdata"
        )
      )
    )
    alldld <- all(res$success)
    allurls <- allurls[!res$success]
    alldestfiles <- alldestfiles[!res$success]
  }

  suppressMessages(untrace(curl:::print_stream))

  return(destfiles)
    
}

#' @noRd
defdb <- function() {
  file.path(cache_dir(), "ssgbm.duckdb")
}

#' @noRd
target_proj4 <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
