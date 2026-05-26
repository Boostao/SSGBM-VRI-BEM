#' Rename geometry attribute for sf object
#'
#' @param g sf object
#' @param name name of new sf column
#' @return sf object
#' @export
rename_geometry <- function(g, name) {
  current <- attr(g, "sf_column")
  names(g)[names(g) == current] <- name
  sf::st_geometry(g) <- name
  g
}

#' Union merge between CCB attributes and VRI features
#'
#' Performs a Union merge between y attributes on x features
#'
#' @param x sf object
#' @param y sf object
#' @return sf object that represent the intersections of x and y
#' @export
merge_geometry <- function(x, y, tolerance = units::as_units("100 m2"), label = NULL) {
  if (!inherits(x, "sf") || !inherits(y, "sf")) {
    stop("Both x and y must be sf objects")
  }

  if (!sf::st_crs(x) == sf::st_crs(y)) {
    stop("Both x and y must have the same crs")
  }

  if (!nrow(y)) {
    return(x |> sanitize_geometry(tolerance))
  }

  sf::st_agr(x) <- "constant"
  sf::st_agr(y) <- "constant"

  x_y_intersection <- sf::st_intersection(x, y)
  if (nrow(x_y_intersection) == 0) {
    x_y_intersection <- x[0, ]
  } else {
    x_y_intersection <- sanitize_geometry(x_y_intersection, tolerance)
  }

  if (!is.null(label) && nrow(x_y_intersection) > 0) {
    for (nm in names(label)) {
      x_y_intersection[[nm]] <- rep(label[[nm]], nrow(x_y_intersection))
    }
  }

  x_y_diff <- erase_geometry(x, y)
  if (nrow(x_y_diff) > 0) {
    x_y_diff <- sanitize_geometry(x_y_diff, tolerance)
  }

  res <- dplyr::bind_rows(x_y_intersection, x_y_diff)

  if (sf::st_crs(res) != albers) {
    res <- bcmaps::transform_bc_albers(res)
  }

  return(res)
}

#' Sanitize geometry
#'
#' @param x sf object
#' @return sf object
#' @export
sanitize_geometry <- function(x, tolerance) {
  sf::st_make_valid(x) |>
    sf::st_set_agr("constant") |>
    sf::st_collection_extract() |>
    sf::st_set_agr("constant") |>
    sf::st_cast("POLYGON") |>
    sf::st_make_valid() |>
    {
      \(x) x[which(sf::st_area(x) > tolerance), ]
    }()
}

#' Erase geometry from target sf object
#'
#' @param target sf object
#' @param erase sf object
#' @return sf object
#' @export
erase_geometry <- function(target, erase) {
  if (nrow(erase) < 500L) {
    return(sf::st_difference(target, sf::st_union(sf::st_geometry(erase))))
  }

  # Extract geometry
  crs <- sf::st_crs(target)
  eg <- sf::st_geometry(erase)
  sf::st_crs(eg) <- NA
  tg <- sf::st_geometry(target)
  sf::st_crs(tg) <- NA

  # Find intersections using the spatial index
  intersections <- sf::st_intersects(tg, eg)
  idx <- which(lengths(intersections) > 0)
  inv_idx <- which(lengths(intersections) == 0)

  # Group overlapping erase polygons (could be run multi cpu to make it faster on Linux)
  empty_g <- sf::st_as_sfc("POLYGON EMPTY")
  if (Sys.info()[["sysname"]] == "Windows") {
    erased <- lapply(idx, function(i) {
      diff_result <- sf:::CPL_geos_op2("difference", tg[i], sf::st_union(eg[intersections[[i]]]))

      sfc_result <- sf::st_sfc(diff_result, crs = crs)

      cleaned <- sf::st_collection_extract(sfc_result, "POLYGON")

      if (length(cleaned) == 0) {
        return(empty_g[[1]])
      }

      return(sf::st_cast(cleaned, "MULTIPOLYGON")[[1]])
    })
  } else {
    erased <- parallel::mclapply(idx,
      function(i) {
        diff_result <- sf:::CPL_geos_op2("difference", tg[i], sf::st_union(eg[intersections[[i]]]))

        sfc_result <- sf::st_sfc(diff_result, crs = crs)

        cleaned <- sf::st_collection_extract(sfc_result, "POLYGON")

        if (length(cleaned) == 0) {
          return(empty_g[[1]])
        }

        return(sf::st_cast(cleaned, "MULTIPOLYGON")[[1]])
      },
      mc.cores = getOption("mc.cores", parallel::detectCores())
    )
  }

  # Make sure output list has same # as original geometries
  outg <- vector("list", length = length(tg))
  outg[idx] <- erased
  outg[inv_idx] <- tg[inv_idx]

  new_geom <- sf::st_sfc(outg, crs = crs)

  # Keep rows where geometry is not empty and matches new_geom length
  non_empty_idx <- which(!sf::st_is_empty(new_geom))

  res <- target[non_empty_idx, , drop = FALSE]

  sf::st_geometry(res) <- new_geom[non_empty_idx]

  return(res)
}

#' @noRd
#' @importFrom stringdist stringdist
match_labels <- function(x, y) {
  s1 <- names(x)
  s2 <- names(y)
  res <- character(length(s1))
  first_pass <- lapply(tolower(s1), \(x) head(s2[which(stringdist::stringdist(x, tolower(s2)) == 0)], 1))
  i <- which(lengths(first_pass) > 0)
  res[i] <- first_pass[i] |> unlist()
  for (j in seq_along(s1)[-i]) {
    d <- stringdist::stringdist(tolower(s1[j]), tolower(s2[-i]))
    tol <- nchar(s1[j]) * 1.33
    wm <- which.min(d)
    cur_i <- wm[d[wm] < tol]
    if (length(cur_i)) {
      res[j] <- head(s2[-i][cur_i], 1)
      i <- c(i, seq_along(s1)[-i][cur_i])
    } else {
      res[j] <- s1[j]
    }
  }
  return(res)
}


#' Rasterize an sf object
#'
#' @param x sf object
#' @param crs Target crs
#' @param resolution Target resolution as unit.
#' @param extent Target raster extent.
#' @param field character or numeric. If field is a character, it should a variable name in `x`.
#' If field is numeric it typically is a single number or a vector of length `nrow(x)`.
#' The values are recycled to `nrow(x)`.
#' @param background numeric. Value to put in the cells that are not covered by any
#' of the features of `x`. Default is `NA`.
#' @param ... Not used.
#' @export
rasterize_sf <- function(x, crs = albers, resolution = units::as_units("100 m"), extent = terra::ext(159587.5, 1881187.5, 173787.5, 1748187.5), field = 1, background = 0, ...) {
  ## Smaller extent
  # extent = terra::ext(1020387.5, 1030387.5, 960987.5, 970987.5)
  ## Full extents would be "159587.5, 1881187.5, 173787.5, 1748187.5"

  y <- terra::rast(
    crs = crs,
    resolution = resolution,
    extent = extent
  )

  x <- terra::vect(x)

  return(terra::rasterize(x, y, field = field, background = background))
}

#' @noRd
albers_polys_op <- function(x, y, op) {
  res <- sf::st_sfc(crs = 3005) # Results set
  quickreturn <- switch(op,
    "difference" = x,
    "intersection" = res
  )

  x <- sf::st_geometry(x) # Extract x geometry
  if (!length(x)) {
    return(quickreturn)
  } # Return if empty
  y <- sf::st_geometry(y) |> # Extract y geometry
    sf::st_cast("POLYGON", warn = FALSE) |> # Recast to POLYGON
    sf::st_crop(sf::st_bbox(x)) # Crop to x bbox to reduce compute area
  if (!length(y)) {
    return(quickreturn)
  } # Return if empty

  # Compute intersects
  inter <- sf:::CPL_geos_binop(x, y, "intersects", pattern = NA_character_, prepared = TRUE)

  algo <- function(i, inter, x, y, ...) {
    res <- sf::st_sfc(crs = 3005)
    idx <- inter[[i]]
    x1 <- x[i]
    if (length(idx) > 0L) {
      area_y <- y[idx]
      new_geo <- sf:::CPL_geos_union(area_y) |> # Combine y intersecting polys
        sf::st_sfc(crs = 3005) |> # reclass to sfc
        sf::st_crop(sf::st_bbox(x1)) |> # Crop to x1 bbox to reduce compute area
        sf:::CPL_geos_op2(op, x1, sfcy = _) |> # Compute op
        sf::st_sfc(crs = 3005) |> # reclass to sfc
        sf::st_cast("POLYGON") # Recast to POLYGON
      if (length(new_geo)) {
        res <- c(res, new_geo) # Append to results set
      }
    } else if (op %in% "difference") {
      res <- c(res, x1)
    }
    return(res)
  }

  res <- do.call(c, parlapply()(seq_along(x), algo, inter = inter, x = x, y = y, future.envir = new.env()))

  return(res)
}

#' @noRd
winos <- function() {
  isTRUE(Sys.info()["sysname"] == "Windows")
}

#' @noRd
multicpu <- function() {
  isTRUE(getOption("SSGBM.VRI.BEM.use.parallel", TRUE))
}

#' @importFrom future plan multisession
#' @importFrom parallel mclapply detectCores
#' @importFrom future.apply future_lapply
#' @noRd
parlapply <- function() {
  if (!multicpu()) {
    lapply
  } else if (winos()) {
    options("future.rng.onMisuse" = "ignore")
    future::plan(future::multisession, workers = parallel::detectCores())
    future.apply::future_lapply
  } else {
    options("mc.cores" = parallel::detectCores())
    parallel::mclapply
  }
}

#' @importFrom future plan multisession
#' @importFrom parallel mcmapply
#' @importFrom future.apply future_mapply
#' @noRd
parmapply <- function() {
  if (!multicpu()) {
    mapply
  } else if (winos()) {
    options("future.rng.onMisuse" = "ignore")
    future::plan(future::multisession, workers = parallel::detectCores())
    future.apply::future_mapply
  } else {
    options("mc.cores" = parallel::detectCores())
    parallel::mcmapply
  }
}

#' Parse a DuckDB size string to bytes.
#'
#' Handles both SI-decimal (`KB`, `MB`, `GB`, `TB`, powers of 1000) and
#' IEC-binary (`KiB`, `MiB`, `GiB`, `TiB`, powers of 1024) notations.
#' DuckDB accepts `SET memory_limit = '14GB'` (decimal) but *reports* the
#' stored value in GiB (binary), e.g. `"13.0 GiB"`.  Correctly parsing both
#' forms is required to compare user-supplied strings against reported values.
#'
#' @param s Character string, e.g. `"14.0 GiB"`, `"6GB"`, `"unlimited"`.
#' @return Numeric bytes, `Inf` for unlimited / -1, `NA_real_` if unparseable.
#' @noRd
parse_duckdb_bytes <- function(s) {
  s <- trimws(s)
  if (grepl("^(-1|unlimited)", tolower(s))) return(Inf)
  # Capture: (number)(SI prefix)(optional 'i')(optional 'B')
  pat <- "^([0-9.]+)\\s*([KMGTkmgt]?)(i?)B?$"
  m   <- regexpr(pat, s, perl = TRUE)
  if (m[1L] == -1L) return(NA_real_)
  cs  <- attr(m, "capture.start")
  cl  <- attr(m, "capture.length")
  n      <- as.numeric(substr(s, cs[1L], cs[1L] + cl[1L] - 1L))
  unit   <- toupper(substr(s, cs[2L], cs[2L] + cl[2L] - 1L))
  is_ibi <- cl[3L] > 0L   # 'i' present → IEC binary (1024-based)
  if (!nzchar(unit)) return(n)
  base <- if (is_ibi) 1024 else 1000
  mult <- c("K" = base, "M" = base^2, "G" = base^3, "T" = base^4)
  n * mult[[unit]]
}

#'  Create the filtered views from the source table for the analysis
#'
#' @param conn A database connection.
#' @param wkt_filter A well-known text geometry of the area of interest.
#' @param tables A character vector of tables to create views from.
#' @param materialize Logical (default `TRUE`). When `TRUE` each filtered
#'   result is written to a temporary **table** with an RTREE spatial index
#'   instead of a view. Materialization requires a one-time scan of each base
#'   table but every subsequent spatial join — VRI×BEM intersection, wetland
#'   area aggregation, fire polygon overlay, CCB union-split, etc. — can
#'   exploit the index rather than re-evaluating the AOI filter on each access.
#'   Set to `FALSE` to restore the original view-only behaviour.
#' @export
filtered_views <- function(conn = init_conn(), wkt_filter,
                           tables = c("BEM", "BURN", "CCB", "FIRE", "GLACIERS", "LAKES", "RIVERS", "VRI", "WETLANDS"),
                           materialize = TRUE,
                           build_spatial_index = TRUE,
                           large_table_rows = 1e6,
                           vri_mem_limit = "6GB") {
  t0_total <- proc.time()[["elapsed"]]
  logger::log_info("filtered_views: starting ({length(tables)} tables, materialize={materialize})")

  # Guard: detect geographic (lat/lon) coordinates and abort with a clear
  # message.  All database tables are stored in BC Albers (EPSG:3005) with
  # metre-scale coordinates (~500 000 – 1 600 000).  A WKT whose X or Y values
  # fall inside the lat/lon range (−180..180 / −90..90) will never overlap those
  # bboxes and would silently return 0 rows.
  aoi_bbox <- tryCatch(
    DBI::dbGetQuery(conn, sprintf(
      "SELECT ST_XMin(g) AS xmin, ST_XMax(g) AS xmax,
              ST_YMin(g) AS ymin, ST_YMax(g) AS ymax
       FROM (SELECT ST_GeomFromText('%s') AS g) _t",
      wkt_filter
    )),
    error = function(e) NULL
  )
  if (!is.null(aoi_bbox) && nrow(aoi_bbox) == 1L &&
      !is.na(aoi_bbox$xmax) && aoi_bbox$xmax <= 181 && aoi_bbox$ymax <= 91) {
    stop(
      "filtered_views: AOI bounding box (",
      round(aoi_bbox$xmin, 2), ", ", round(aoi_bbox$ymin, 2), ", ",
      round(aoi_bbox$xmax, 2), ", ", round(aoi_bbox$ymax, 2),
      ") looks like geographic (lat/lon) coordinates. ",
      "Database tables are stored in BC Albers (EPSG:3005). ",
      "Transform your AOI first, e.g.:\n",
      "  sf::st_read(...) |> sf::st_transform(3005) |> sf::st_union() |> wk::as_wkt() |> paste0()"
    )
  }

  DBI::dbExecute(conn, sprintf("SET VARIABLE AOI = (SELECT ST_GeomFromText('%s'));", wkt_filter))
  # Inline the WKT literal for CREATE TABLE so the DuckDB query planner can
  # extract a bbox from the constant geometry and use any RTREE index on the
  # source table.  getvariable() is opaque to the planner; a literal is not.
  aoi_literal <- sprintf("ST_GeomFromText('%s')", wkt_filter)

  # NOTE: use ST_Intersects rather than the && bounding-box operator for the
  # WHERE predicate.  The && operator has a known interaction with DuckDB's
  # CHECKPOINT routine that can silently return 0 matching rows for source
  # tables whose RTREE index pages were written before the last checkpoint
  # (observed with DuckDB 1.5.x on large persistent databases).
  # ST_Intersects is pushed through the RTREE index by the DuckDB ≥1.0
  # optimizer and does not exhibit that behaviour.
  #
  # NOTE: preserve_insertion_order=false is safe here (used for large tables
  # below) because we use ST_Intersects, not &&.  The 0-row interaction only
  # occurred when && + CHECKPOINT were combined with that setting.

  for (t in tables) {
    t0 <- proc.time()[["elapsed"]]
    v_name <- sprintf("V_%s", t)

    # Skip if the source table does not exist in this database
    exists_rows <- DBI::dbGetQuery(
      conn,
      sprintf("SELECT count(*) AS n FROM information_schema.tables WHERE table_name = '%s';", t)
    )
    if (exists_rows$n == 0L) {
      logger::log_info("filtered_views: skipping {t} (not found in database)")
      next
    }

    if (materialize) {
      # Use a persistent TABLE (not TEMP) so pages are flushed to the .duckdb
      # file via CHECKPOINT below.  Checkpointed pages are "clean": the buffer
      # manager can evict them at zero I/O cost when later (larger) tables need
      # memory.  TEMP TABLE pages are never flushed by the buffer manager, so
      # they pin the entire buffer pool and cause OOM on large AOIs.
      #
      # Explicit DROP ensures a clean slate.  DROP VIEW IF EXISTS handles any
      # remnant from a materialize=FALSE run; DROP TABLE IF EXISTS clears the
      # persistent table from a previous materialize=TRUE run.  CREATE TABLE
      # (no OR REPLACE) avoids a DuckDB edge-case where CREATE OR REPLACE on a
      # checkpointed table can silently produce an empty result.
      try(DBI::dbExecute(conn, sprintf("DROP VIEW  IF EXISTS %s;", v_name)), silent = TRUE)
      try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", v_name)), silent = TRUE)

      # For large source tables the bbox-filtered CREATE TABLE AS SELECT
      # requires more memory than a typical limit because DuckDB must read the
      # entire geometry column to evaluate the && predicate (even when an RTREE
      # index exists, nearly all rows match on a large AOI, so the index walk
      # itself is the bottleneck).
      # We temporarily raise memory_limit to `vri_mem_limit` for any source
      # table whose estimated_size exceeds `large_table_rows`, then restore the
      # original limit via on.exit after the CREATE TABLE completes.
      n_src_est <- tryCatch(
        DBI::dbGetQuery(conn, sprintf(
          "SELECT estimated_size AS n FROM duckdb_tables() WHERE table_name = '%s';", t
        ))$n,
        error = function(e) NA_real_
      )
      is_large <- isTRUE(!is.na(n_src_est) && length(n_src_est) == 1L &&
        n_src_est >= large_table_rows)
      if (is_large) {
        cur_limit <- tryCatch(
          DBI::dbGetQuery(
            conn,
            "SELECT value FROM duckdb_settings() WHERE name = 'memory_limit'"
          )$value,
          error = function(e) NULL
        )
        # Only raise the memory limit, never lower it.  vri_mem_limit is a
        # floor (useful when the connection was opened with a low default), but
        # if the connection already has a higher limit (e.g. 14GB) lowering it
        # to vri_mem_limit would leave almost no headroom once the earlier
        # V_* tables have filled the buffer pool.
        cur_bytes <- if (!is.null(cur_limit)) parse_duckdb_bytes(cur_limit) else NA_real_
        vri_bytes <- parse_duckdb_bytes(vri_mem_limit)
        should_raise <- isTRUE(!is.na(cur_bytes) && !is.na(vri_bytes) && vri_bytes > cur_bytes)
        if (should_raise) {
          try(DBI::dbExecute(conn, sprintf("SET memory_limit = '%s';", vri_mem_limit)),
            silent = TRUE
          )
          on.exit(
            try(DBI::dbExecute(conn, sprintf("SET memory_limit = '%s';", cur_limit)),
              silent = TRUE
            ),
            add = TRUE
          )
        }
        # Disabling insertion-order preservation frees the large sort-buffer
        # DuckDB normally allocates to preserve row order during CTAS, cutting
        # peak memory use significantly on multi-million-row sources.
        # Safe here because we use ST_Intersects (not &&), so the CHECKPOINT
        # interaction that caused 0-row results no longer applies.
        cur_pio <- tryCatch(
          DBI::dbGetQuery(
            conn,
            "SELECT current_setting('preserve_insertion_order') AS v"
          )$v,
          error = function(e) NULL
        )
        try(DBI::dbExecute(conn, "SET preserve_insertion_order = false;"), silent = TRUE)
        on.exit(
          try(DBI::dbExecute(conn,
            sprintf("SET preserve_insertion_order = %s;",
                    if (!is.null(cur_pio) && tolower(cur_pio) == "false") "false" else "true")
          ), silent = TRUE),
          add = TRUE
        )
        if (should_raise) {
          logger::log_info("filtered_views: {t} has ~{n_src_est} rows, raising memory_limit to {vri_mem_limit} and disabling preserve_insertion_order for this table")
        } else {
          logger::log_info("filtered_views: {t} has ~{n_src_est} rows, disabling preserve_insertion_order (memory_limit kept at {if (!is.null(cur_limit)) cur_limit else 'default'})")
        }
      }

      DBI::dbExecute(conn, sprintf(
        "CREATE TABLE %s AS (SELECT * FROM %s WHERE ST_Intersects(Shape, %s));",
        v_name, t, aoi_literal
      ))
      n_rows <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) AS n FROM %s;", v_name))$n

      # Build RTREE index only when requested and the table is non-empty.
      # Skipping indexes for empty tables avoids pointless memory overhead.
      # Set build_spatial_index=FALSE on memory-constrained systems; the
      # filtered tables are already small so sequential scans are acceptable.
      if (build_spatial_index && n_rows > 0L) {
        DBI::dbExecute(conn, sprintf(
          "CREATE INDEX %s_rtree ON %s USING RTREE (Shape);",
          v_name, v_name
        ))
      }
      # Flush to disk: makes V_ table pages clean and evictable by the buffer
      # pool when subsequent larger tables need memory.
      # Only checkpoint non-empty tables: checkpointing an empty spatial table
      # can corrupt bounding-box statistics for other source tables.
      if (n_rows > 0L) try(DBI::dbExecute(conn, "CHECKPOINT;"), silent = TRUE)
      logger::log_info("filtered_views: {t} -> {v_name} materialized ({n_rows} rows, {round(proc.time()[['elapsed']] - t0, 1)}s)")
    } else {
      DBI::dbExecute(conn, sprintf(
        "CREATE OR REPLACE TEMP VIEW %s AS (SELECT * FROM %s WHERE ST_Intersects(Shape, getvariable('AOI')));",
        v_name, t
      ))
      logger::log_info("filtered_views: {t} -> {v_name} view created ({round(proc.time()[['elapsed']] - t0, 1)}s)")
    }
  }

  logger::log_info("filtered_views: done (total {round(proc.time()[['elapsed']] - t0_total, 1)}s)")
}
