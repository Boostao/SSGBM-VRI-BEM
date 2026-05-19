#' Download and materialize vector layers for rasterization
#'
#' These helpers save the vector inputs used by [init_db()] to durable files on
#' disk so they can be rasterized later in the terra pipeline without keeping
#' them in DuckDB tables.
#'
#' When `path` is a directory, the helper writes a default GeoPackage file for
#' the requested layer into that directory. When `path` is a file path, that
#' path is used directly.
#'
#' Query-backed downloads are written as a single-layer GeoPackage. When a
#' source datasource is supplied explicitly, that datasource is materialized
#' with the same transformed fields used by the package readers.
#'
#' @param path Output datasource path or output directory.
#' @param dsn Optional source datasource. When left `NULL`, supported layers are
#'   fetched from the BC Data Catalog.
#' @param layer Source layer name.
#' @param overwrite Logical. Overwrite an existing output datasource.
#' @param bem_dsn Source datasource for BEM.
#' @param prem_dsn Source datasource for PEM.
#' @param tsa_dsn Source datasource for TSA.
#' @param tsa_name Character vector of TSA names used to filter the TSA layer.
#' @param Skeena_boundary Logical. Also materialize the Skeena region boundary.
#' @return Character path to the written datasource. `download_db_layers()`
#'   returns a named list of datasource paths.
#' @name download_layers
NULL


#' @export
#' @rdname download_layers
download_db_layers <- function(path,
                               bem_dsn = NULL,
                               prem_dsn = NULL,
                               tsa_dsn = NULL,
                               tsa_name = c(),
                               Skeena_boundary = TRUE,
                               overwrite = FALSE) {
  outdir <- ensure_download_dir(path)

  downloads <- list(
    VRI = download_vri(file.path(outdir, "vri.gpkg"), overwrite = overwrite),
    WETLANDS = download_wetlands(file.path(outdir, "wetlands.gpkg"), overwrite = overwrite),
    RIVERS = download_rivers(file.path(outdir, "rivers.gpkg"), overwrite = overwrite),
    LAKES = download_lakes(file.path(outdir, "lakes.gpkg"), overwrite = overwrite),
    GLACIERS = download_glaciers(file.path(outdir, "glaciers.gpkg"), overwrite = overwrite),
    CCB = download_ccb(file.path(outdir, "ccb.gpkg"), overwrite = overwrite),
    BURN = download_burn(file.path(outdir, "burn.gpkg"), overwrite = overwrite),
    FIRE = download_fire(file.path(outdir, "fire.gpkg"), overwrite = overwrite),
    TSA = download_tsa(
      file.path(outdir, "tsa.gpkg"),
      tsa_name = tsa_name,
      tsa_dsn = tsa_dsn,
      overwrite = overwrite
    )
  )

  if (!is.null(bem_dsn)) {
    downloads$BEM <- download_bem(
      file.path(outdir, "bem.gpkg"),
      dsn = bem_dsn,
      overwrite = overwrite
    )
  } else {
    logger::log_warn("No BEM data source name (dsn) provided. Skipping BEM download.")
  }

  if (!is.null(prem_dsn)) {
    downloads$PEM <- download_pem(
      file.path(outdir, "pem.gpkg"),
      dsn = prem_dsn,
      overwrite = overwrite
    )
  } else {
    logger::log_warn("No PEM data source name (dsn) provided. Skipping PEM download.")
  }

  if (isTRUE(Skeena_boundary)) {
    downloads$SKEENA <- download_skeena(
      file.path(outdir, "skeena.gpkg"),
      overwrite = overwrite
    )
  }

  invisible(downloads)
}


#' @export
#' @rdname download_layers
download_vri <- function(path,
                         dsn = NULL,
                         layer = "fc",
                         overwrite = FALSE) {
  vri_record <- "2ebb35d8-c82f-4a17-9c96-612ac3532d55"
  vri_resources <- bcdata::bcdc_tidy_resources(vri_record)
  vri_vars <- c(
    FEATURE_ID = "FEATURE_ID",
    BCLCS_LV_1 = "BCLCS_LEVEL_1",
    BCLCS_LV_2 = "BCLCS_LEVEL_2",
    BCLCS_LV_3 = "BCLCS_LEVEL_3",
    BCLCS_LV_4 = "BCLCS_LEVEL_4",
    BCLCS_LV_5 = "BCLCS_LEVEL_5",
    SPEC_CD_1 = "SPECIES_CD_1",
    SPEC_CD_2 = "SPECIES_CD_2",
    SPEC_CD_3 = "SPECIES_CD_3",
    SPEC_CD_4 = "SPECIES_CD_4",
    SPEC_CD_5 = "SPECIES_CD_5",
    SPEC_CD_6 = "SPECIES_CD_6",
    SPEC_PCT_1 = "SPECIES_PCT_1",
    SPEC_PCT_2 = "SPECIES_PCT_2",
    SPEC_PCT_3 = "SPECIES_PCT_3",
    SPEC_PCT_4 = "SPECIES_PCT_4",
    SPEC_PCT_5 = "SPECIES_PCT_5",
    SPEC_PCT_6 = "SPECIES_PCT_6",
    CR_CLOSURE = "CROWN_CLOSURE",
    LAND_CD_1 = "LAND_COVER_CLASS_CD_1",
    COV_PCT_1 = "EST_COVERAGE_PCT_1",
    LBL_VEGCOV = "LINE_5_VEGETATION_COVER",
    HRVSTDT = "HARVEST_DATE",
    PROJ_AGE_1 = "PROJ_AGE_1",
    SOIL_MOISTURE_REGIME_1 = "SOIL_MOISTURE_REGIME_1",
    SOIL_NUTRIENT_REGIME = "SOIL_NUTRIENT_REGIME",
    INVENTORY_STANDARD_CD = "INVENTORY_STANDARD_CD",
    VRI_BEC_ZONE = "BEC_ZONE_CODE",
    VRI_BEC_SUBZON = "BEC_SUBZONE",
    VRI_BEC_VRT = "BEC_VARIANT",
    VRI_BEC_PHASE = "BEC_PHASE",
    VRI_SURVEY_YEAR = "REFERENCE_YEAR",
    SITE_POSITION_MESO = "SITE_POSITION_MESO",
    SITE_INDEX = "SITE_INDEX",
    EST_SITE_INDEX = "EST_SITE_INDEX"
  )
  out_dsn <- prepare_download_output(path, stem = "vri", overwrite = overwrite)

  if (is.null(dsn)) {
    vri_query <- bcdata::bcdc_query_geodata(record = vri_record) |>
      bcdata::select(.include = unname(vri_vars))

    query_download <- tryCatch(
      {
        sources <- collect_geojson(vri_query, sort_field = "FEATURE_ID")
        on.exit(unlink(sources), add = TRUE)
        translate_download_sources(
          sources = sources,
          out_dsn = out_dsn,
          dst_layer = "VRI",
          sql_builder = function(source, src_layer) {
            build_select_alias_sql(src_layer, vri_vars, schema_field_names(source, src_layer))
          }
        )
        TRUE
      },
      error = function(e) {
        logger::log_warn(
          "Query-backed VRI materialization failed [%s]. Falling back to BC Data Catalog resource download." |>
            sprintf(conditionMessage(e))
        )
        FALSE
      }
    )

    if (isTRUE(query_download)) {
      return(out_dsn)
    }

    dsn <- resolve_resource(vri_resources)
    on.exit(unlink(dsn, recursive = TRUE), add = TRUE)
  }

  layer <- resolve_source_layer_name(dsn, layer)

  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "VRI",
    src_layer = layer,
    sql = build_select_alias_sql(layer, vri_vars, schema_field_names(dsn, layer))
  )

  out_dsn
}


#' @export
#' @rdname download_layers
download_bem <- function(path,
                         dsn,
                         layer = "BEM",
                         overwrite = FALSE) {
  if (missing(dsn) || is.null(dsn)) {
    logger::log_error("No value for dsn to read `BEM` from.")
  }

  out_dsn <- prepare_download_output(path, stem = "bem", overwrite = overwrite)
  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "BEM",
    src_layer = layer
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_wetlands <- function(path,
                              dsn = NULL,
                              layer = "FWA_WETLANDS_POLY",
                              overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "wetlands", overwrite = overwrite)

  if (is.null(dsn)) {
    wetlands_query <- bcdata::bcdc_query_geodata(record = "93b413d8-1840-4770-9629-641d74bd1cc6") |>
      bcdata::select()
    sources <- collect_geojson(wetlands_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "WETLANDS")
    return(out_dsn)
  }

  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "WETLANDS",
    src_layer = resolve_source_layer_name(dsn, layer)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_rivers <- function(path,
                            dsn = NULL,
                            layer = "FWA_RIVERS_POLY",
                            overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "rivers", overwrite = overwrite)

  if (is.null(dsn)) {
    rivers_query <- bcdata::bcdc_query_geodata(record = "f7dac054-efbf-402f-ab62-6fc4b32a619e") |>
      bcdata::select()
    sources <- collect_geojson(rivers_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "RIVERS")
    return(out_dsn)
  }

  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "RIVERS",
    src_layer = resolve_source_layer_name(dsn, layer)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_lakes <- function(path,
                           dsn = NULL,
                           layer = "FWA_LAKES_POLY",
                           overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "lakes", overwrite = overwrite)

  if (is.null(dsn)) {
    lakes_query <- bcdata::bcdc_query_geodata(record = "cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6") |>
      bcdata::select()
    sources <- collect_geojson(lakes_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "LAKES")
    return(out_dsn)
  }

  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "LAKES",
    src_layer = resolve_source_layer_name(dsn, layer)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_glaciers <- function(path,
                              dsn = NULL,
                              layer = "BTM_PLU_V1",
                              overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "glaciers", overwrite = overwrite)

  if (is.null(dsn)) {
    glaciers_query <- bcdata::bcdc_query_geodata(record = "134fdc69-7b0c-4c50-b77c-e8f2553a1d40") |>
      bcdata::filter(rlang::.data$PRESENT_LAND_USE_LABEL == "Glaciers and Snow") |>
      bcdata::select()
    sources <- collect_geojson(glaciers_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "GLACIERS")
    return(out_dsn)
  }

  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "GLACIERS",
    src_layer = resolve_source_layer_name(dsn, layer)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_ccb <- function(path,
                         dsn = NULL,
                         layer = "CNS_CUT_BL_polygon",
                         overwrite = FALSE) {
  ccb_record <- "b1b647a6-f271-42e0-9cd0-89ec24bce9f7"
  ccb_resources <- bcdata::bcdc_tidy_resources(ccb_record)
  out_dsn <- prepare_download_output(path, stem = "ccb", overwrite = overwrite)

  if (is.null(dsn)) {
    ccb_query <- bcdata::bcdc_query_geodata(record = ccb_record) |>
      bcdata::select(.include = "HARVEST_START_YEAR_CALENDAR")

    query_download <- tryCatch(
      {
        sources <- collect_geojson(ccb_query, sort_field = "HARVEST_START_YEAR_CALENDAR")
        on.exit(unlink(sources), add = TRUE)
        translate_download_sources(
          sources = sources,
          out_dsn = out_dsn,
          dst_layer = "CCB",
          sql_builder = function(source, src_layer) {
            ccb_field <- resolve_ccb_year_field(source, src_layer)
            paste0(
              "SELECT ", download_sql_identifier(ccb_field),
              " AS ", download_sql_identifier("HARVESTYR"),
              " FROM ", download_sql_identifier(src_layer)
            )
          }
        )
        TRUE
      },
      error = function(e) {
        logger::log_warn(
          "Query-backed CCB materialization failed [%s]. Falling back to BC Data Catalog resource download." |>
            sprintf(conditionMessage(e))
        )
        FALSE
      }
    )

    if (isTRUE(query_download)) {
      return(out_dsn)
    }

    dsn <- resolve_resource(ccb_resources)
    on.exit(unlink(dsn, recursive = TRUE), add = TRUE)
  }

  layer <- resolve_ccb_layer_name(dsn, layer)
  ccb_field <- resolve_ccb_year_field(dsn, layer)
  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "CCB",
    src_layer = layer,
    sql = paste0(
      "SELECT ", download_sql_identifier(ccb_field),
      " AS ", download_sql_identifier("HARVESTYR"),
      " FROM ", download_sql_identifier(layer)
    )
  )

  out_dsn
}


#' @export
#' @rdname download_layers
download_burn <- function(path,
                          dsn = NULL,
                          layer = "WHSE_FOREST_VEGETATION_VEG_BURN_SEVERITY_SP",
                          overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "burn", overwrite = overwrite)

  if (is.null(dsn)) {
    burn_query <- bcdata::bcdc_query_geodata(record = "c58a54e5-76b7-4921-94a7-b5998484e697") |>
      bcdata::filter(rlang::.data$BURN_SEVERITY_RATING %in% c("High", "Low", "Medium")) |>
      bcdata::select(.include = "BURN_SEVERITY_RATING")
    sources <- collect_geojson(burn_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "BURN")
    return(out_dsn)
  }

  layer <- resolve_source_layer_name(dsn, layer)
  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "BURN",
    src_layer = layer,
    where = paste0(download_sql_identifier("BURN_SEVERITY_RATING"), " IN ('High','Low','Medium')")
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_fire <- function(path,
                          dsn = NULL,
                          layer = "WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP",
                          overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "fire", overwrite = overwrite)

  if (is.null(dsn)) {
    fire_query <- bcdata::bcdc_query_geodata(record = "22c7cb44-1463-48f7-8e47-88857f207702") |>
      bcdata::select()
    sources <- collect_geojson(fire_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "FIRE")
    return(out_dsn)
  }

  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "FIRE",
    src_layer = resolve_source_layer_name(dsn, layer)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_pem <- function(path,
                         dsn,
                         layer = "PEM_Mar2026",
                         overwrite = FALSE) {
  if (missing(dsn) || is.null(dsn)) {
    logger::log_error("No value for dsn to read `PEM` from.")
  }

  out_dsn <- prepare_download_output(path, stem = "pem", overwrite = overwrite)
  translate_download_source(
    source = dsn,
    out_dsn = out_dsn,
    dst_layer = "PEM",
    src_layer = resolve_source_layer_name(dsn, layer)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_tsa <- function(path,
                         tsa_name = c(),
                         tsa_dsn = NULL,
                         overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "tsa", overwrite = overwrite)

  if (is.null(tsa_dsn)) {
    tsa_query <- bcdata::bcdc_query_geodata(record = "8daa29da-d7f4-401c-83ae-d962e3a28980")
    if (length(tsa_name)) {
      tsa_query <- tsa_query |>
        bcdata::filter(rlang::.data$TSA_NUMBER_DESCRIPTION %in% tsa_name)
    }
    tsa_query <- tsa_query |> bcdata::select(.include = "TSA_NUMBER_DESCRIPTION")
    sources <- collect_geojson(tsa_query)
    on.exit(unlink(sources), add = TRUE)
    translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "TSA")
    return(out_dsn)
  }

  translate_download_source(
    source = tsa_dsn,
    out_dsn = out_dsn,
    dst_layer = "TSA",
    src_layer = first_layer_name(tsa_dsn),
    where = build_in_where("TSA_NUMBER_DESCRIPTION", tsa_name)
  )
  out_dsn
}


#' @export
#' @rdname download_layers
download_skeena <- function(path,
                            overwrite = FALSE) {
  out_dsn <- prepare_download_output(path, stem = "skeena", overwrite = overwrite)
  skeena_query <- bcdata::bcdc_query_geodata("dfc492c0-69c5-4c20-a6de-2c9bc999301f") |>
    bcdata::filter(rlang::.data$ORG_UNIT_NAME == "Skeena Natural Resource Region") |>
    bcdata::select(.include = "ORG_UNIT_NAME")
  sources <- collect_geojson(skeena_query)
  on.exit(unlink(sources), add = TRUE)
  translate_download_sources(sources = sources, out_dsn = out_dsn, dst_layer = "SKEENA")
  out_dsn
}


ensure_download_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}


resolve_output_dsn <- function(path, stem) {
  if (dir.exists(path) || !nzchar(tools::file_ext(path))) {
    ensure_download_dir(path)
    return(file.path(path, paste0(stem, ".gpkg")))
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}


prepare_download_output <- function(path, stem, overwrite) {
  out_dsn <- resolve_output_dsn(path, stem = stem)

  if (file.exists(out_dsn)) {
    if (!isTRUE(overwrite)) {
      logger::log_info("Output already exists [%s]. Reusing it." |> sprintf(out_dsn))
      return(out_dsn)
    }
    unlink(out_dsn)
  }

  out_dsn
}


translate_download_sources <- function(sources, out_dsn, dst_layer, sql_builder = NULL) {
  for (index in seq_along(sources)) {
    source <- sources[[index]]
    src_layer <- first_layer_name(source)
    translate_download_source(
      source = source,
      out_dsn = out_dsn,
      dst_layer = dst_layer,
      src_layer = src_layer,
      sql = if (!is.null(sql_builder)) sql_builder(source, src_layer) else NULL,
      append = index > 1
    )
  }
}


translate_download_source <- function(source,
                                      out_dsn,
                                      dst_layer,
                                      src_layer = NULL,
                                      sql = NULL,
                                      where = NULL,
                                      append = FALSE) {
  options <- c(
    "-f", "GPKG",
    if (isTRUE(append)) "-append" else "-overwrite",
    "-nln", dst_layer,
    "-nlt", "PROMOTE_TO_MULTI",
    "-makevalid",
    "-t_srs", "EPSG:3005"
  )

  if (!is.null(sql)) {
    options <- c(options, "-sql", sql)
  }

  if (!is.null(where)) {
    options <- c(options, "-where", where)
  }

  if (is.null(sql) && !is.null(src_layer)) {
    options <- c(options, src_layer)
  }

  sf::gdal_utils(
    util = "vectortranslate",
    source = source,
    destination = out_dsn,
    options = options
  )
}


first_layer_name <- function(dsn) {
  sf::st_layers(dsn)$name[1]
}


resolve_source_layer_name <- function(dsn, layer = NULL) {
  layer_names <- sf::st_layers(dsn)$name

  if (is.null(layer) || layer %in% layer_names) {
    return(if (is.null(layer)) layer_names[1] else layer)
  }

  logger::log_warn(
    "Layer [%s] not found in datasource [%s]. Using first available layer [%s]." |>
      sprintf(layer, dsn, layer_names[1])
  )

  layer_names[1]
}


build_select_alias_sql <- function(layer, fields, available_fields = NULL) {
  if (is.null(available_fields)) {
    available_fields <- unique(c(unname(fields), names(fields)))
  }

  field_map <- vapply(
    seq_along(fields),
    function(index) resolve_download_field(unname(fields)[index], names(fields)[index], available_fields),
    character(1)
  )

  keep <- nzchar(field_map)
  if (!any(keep)) {
    logger::log_error("None of the requested fields were found in layer [%s]." |> sprintf(layer))
  }

  select_terms <- paste(
    paste0(
      download_sql_identifier(field_map[keep]),
      " AS ",
      download_sql_identifier(names(fields)[keep])
    ),
    collapse = ", "
  )

  paste0("SELECT ", select_terms, " FROM ", download_sql_identifier(layer))
}


resolve_ccb_year_field <- function(dsn, layer) {
  field_names <- schema_field_names(dsn, layer)

  if ("HARVESTYR" %in% field_names) {
    return("HARVESTYR")
  }

  if ("HARVEST_YEAR" %in% field_names) {
    return("HARVEST_YEAR")
  }

  if ("HARVEST_START_YEAR_CALENDAR" %in% field_names) {
    return("HARVEST_START_YEAR_CALENDAR")
  }

  stop("Could not find a harvest year field in the CCB datasource.", call. = FALSE)
}


resolve_ccb_layer_name <- function(dsn, layer = NULL) {
  layer_names <- sf::st_layers(dsn)$name

  if (!is.null(layer) && layer %in% layer_names) {
    return(layer)
  }

  ccb_layers <- Filter(
    function(candidate) {
      fields <- tryCatch(schema_field_names(dsn, candidate), error = function(e) character())
      any(c("HARVESTYR", "HARVEST_YEAR", "HARVEST_START_YEAR_CALENDAR") %in% fields)
    },
    layer_names
  )

  if (length(ccb_layers)) {
    if (!is.null(layer)) {
      logger::log_warn(
        "Layer [%s] not found in datasource [%s]. Using cutblock layer [%s]." |>
          sprintf(layer, dsn, ccb_layers[1])
      )
    }
    return(ccb_layers[1])
  }

  resolve_source_layer_name(dsn, layer)
}


schema_field_names <- function(dsn, layer) {
  query <- paste0("SELECT * FROM ", download_sql_identifier(layer), " WHERE 0 = 1")
  names(sf::st_read(dsn = dsn, query = query, quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE))
}


resolve_download_field <- function(source_name, target_name, available_fields) {
  if (source_name %in% available_fields) {
    return(source_name)
  }

  if (target_name %in% available_fields) {
    return(target_name)
  }

  ""
}


build_in_where <- function(field, values) {
  if (!length(values)) {
    return(NULL)
  }

  paste0(
    download_sql_identifier(field),
    " IN (",
    paste(vapply(values, download_sql_string, character(1)), collapse = ", "),
    ")"
  )
}


download_sql_identifier <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}


download_sql_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}