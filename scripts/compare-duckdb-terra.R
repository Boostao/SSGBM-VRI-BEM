source("scripts/duckdb_run.R")
source("scripts/terra_rrm_run_hybrid_moose_only.R")



terra_result <- run_hybrid_moose_only()

results <- bench::mark(
  duckdb = duckdb_run_moose(),
  iterations = 1,
  check = FALSE
)
duckdb_result <- results$duckdb
terra_result <- results$terra
library(duckdb)
library(DBI)

#' Generate nested bounding boxes using DuckDB
#'
#' @param conn An active DuckDB connection object.
#' @param master_wkt Optional. A character string representing the WKT of the main bounding box. 
#'                   If NULL, `table_name` and `geom_column` must be provided.
#' @param scales A numeric vector of area scaling factors between 0 and 1.
#' @param table_name Optional. The name of the table in DuckDB to compute the bounding box from.
#' @param geom_column Optional. The name of the geometry column inside `table_name`.
#'
#' @return A data frame containing the area factors and their corresponding nested WKT bounding boxes.
get_duckdb_nested_bboxes <- function(conn, master_wkt = NULL, scales = c(0.001, 0.002,0.005), table_name = "VRI", geom_column = "shape") {
  
  # 1. Register the scaling factors vector into your existing connection
  scale_df <- data.frame(area_factor = scales)
  duckdb_register(conn, "scale_factors", scale_df)
  
  # Ensure the temporary registration is wiped out when the function exits
  on.exit(duckdb_unregister(conn, "scale_factors"), add = TRUE)
  
  # 2. Build the subquery for the master bounding box
  if (is.null(master_wkt)) {
    if (is.null(table_name)) {
      stop("You must provide either a 'master_wkt' or a 'table_name' to compute it dynamically.")
    }
    # Dynamically aggregate table limits into a master GEOMETRY envelope
    master_box_subquery <- sprintf("SELECT ST_Envelope(ST_Extent_Agg(%s)) FROM %s", geom_column, table_name)
  } else {
    # Direct parsing of string parameter
    master_box_subquery <- "SELECT ST_GeomFromText(?)"
  }
  
  # 3. Main Query using official native DuckDB spatial extraction functions:
  # ST_XMin, ST_XMax, ST_YMin, ST_YMax
  query <- sprintf("
    WITH master_data AS (
        SELECT (%s) AS master_box,
               area_factor
        FROM scale_factors
    ),
    box_bounds AS (
        SELECT 
            ST_XMin(master_box) AS minx,
            ST_XMax(master_box) AS maxx,
            ST_YMin(master_box) AS miny,
            ST_YMax(master_box) AS maxy,
            SQRT(area_factor) AS linear_factor, 
            area_factor 
        FROM master_data
    ),
    scaled AS (
        SELECT 
            (maxx + minx) / 2.0 AS cx, 
            (maxy + miny) / 2.0 AS cy,
            (maxx - minx) / 2.0 AS half_w, 
            (maxy - miny) / 2.0 AS half_h,
            linear_factor, 
            area_factor
        FROM box_bounds
    )
    SELECT 
        area_factor,
        ST_AsText(ST_MakeEnvelope(
            cx - (half_w * linear_factor), 
            cy - (half_h * linear_factor),
            cx + (half_w * linear_factor), 
            cy + (half_h * linear_factor)
        )) AS nested_bbox_wkt
    FROM scaled;
  ", master_box_subquery)
  
  # 4. Fetch the data depending on whether dynamic calculation or text binding is utilized
  if (is.null(master_wkt)) {
    result <- dbGetQuery(conn, query)
  } else {
    result <- dbGetQuery(conn, query, params = list(master_wkt))
  }
  
  return(result)
}
get_duckdb_nested_bboxes(conn)
library(bench)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Get your test AOI data frame from your DuckDB function
aoi_df <- get_duckdb_nested_bboxes(conn, table_name = "VRI", geom_column = "shape")

aoi_df[1:2,]
# 2. Initialize an empty list to store benchmark results
benchmark_results <- list()

# 3. Loop through each AOI size and test both functions
for(i in 1:nrow(aoi_df[1,])) {
  current_scale <- aoi_df$area_factor[i]
  current_wkt   <- aoi_df$nested_bbox_wkt[i]
  
  # Measure both functions for this specific AOI
  res <- bench::mark(
    `Function A` = run_hybrid_moose_only(aoi_wkt = current_wkt),
    `Function B` = duckdb_run_moose(aoi_wkt = current_wkt),
    iterations = 1,         # Keep iterations low since you are checking scaling trend
    check = FALSE           # Set to FALSE because they return different outputs
  )
  
  # Tag results with the current AOI scale size
  res$scale <- current_scale
  benchmark_results[[i]] <- res
}

# 4. Combine all results into a single data frame
final_bench <- bind_rows(benchmark_results) %>%
  mutate(expression = as.character(expression))




duckdb_result <- duckdb_run_moose(aoi_wkt = aoi_df$nested_bbox_wkt[1]) 
