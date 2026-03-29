#' Create VRIBEM view
#' @param conn A duckdb connection.
#' @param validate_intersect Boolean, display a message for non intersecting VRI and BEM shape.
#' @details
#' This function will create view `V_VRIBEM` and `V_VRIBEM_INTERSECTION` on `conn`.
#' It will contain all VRI and BEM fields.
#' BGC fields from VRI will have priority over matching BEM fields.
#' @export
vribem_view <- function(conn, validate_intersect = FALSE) {

  # check if BEM contains duplicate TEIS_ID
  res <- DBI::dbGetQuery(conn, "
  SELECT COUNT(1) NB
  FROM (
    SELECT TEIS_ID,
           COUNT(1) NB
    FROM V_BEM
    GROUP BY TEIS_ID
    HAVING COUNT(1) > 1
  )")
  if (res$NB > 0) {
    logger::log_error("Duplicate values of TEIS_ID found in BEM table. Check the source and reload with `init_bem()`.")
  }

  vri_columns <- DBI::dbGetQuery(conn, "PRAGMA table_info('VRI')")$name |> setdiff("Shape")
  bem_columns <- DBI::dbGetQuery(conn, "PRAGMA table_info('BEM')")$name |> setdiff("Shape")

  DBI::dbExecute(conn, paste0("
    CREATE OR REPLACE TEMP VIEW V_VRIBEM AS ( 
      SELECT * , 
        round(ST_Area(a.Shape)/10000, 2) AS Area_Ha,
        EXISTS (SELECT 1 FROM V_RIVERS riv WHERE ST_Intersects(riv.Shape, a.Shape)) AS INTERSECTS_RIVER
      FROM  (  
        SELECT 
          ", paste0("VRI.", setdiff(vri_columns, c("VRI_BEC_ZONE","VRI_BEC_SUBZON","VRI_BEC_VRT","VRI_BEC_PHASE","Shape_Area","Area_Ha")), collapse = ","),",
          coalesce(VRI.VRI_BEC_ZONE, BEM.BGC_ZONE) BGC_ZONE,
          coalesce(VRI.VRI_BEC_SUBZON, BEM.BGC_SUBZON) BGC_SUBZON,
          coalesce(VRI.VRI_BEC_VRT, BEM.BGC_VRT::VARCHAR) BGC_VRT,
          coalesce(VRI.VRI_BEC_PHASE, BEM.BGC_PHASE) BGC_PHASE,
          ", paste0("BEM.", setdiff(bem_columns, c("BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE")), collapse = ","),",
          ST_Intersection(BEM.Shape, VRI.Shape) Shape,
          ST_Area(ST_Intersection(BEM.Shape, VRI.Shape)) Shape_Area,
          VRI.Shape AS VRI_Shape, 
          ST_Area(VRI.Shape) AS VRI_Area 
          

          
        
        FROM V_VRI VRI
        JOIN V_BEM BEM
            ON ST_Intersects(BEM.Shape, VRI.Shape)
      ) a
    );
    CREATE OR REPLACE TEMP VIEW V_VRIBEM_INTERSECTION AS (    
      SELECT 
        FEATURE_ID,
        TEIS_ID,
        ST_Area(Shape) AREA,
        RANK() OVER (PARTITION BY FEATURE_ID ORDER BY AREA DESC) RK
      FROM V_VRIBEM
    );
  "))
  

  if (isTRUE(validate_intersect)) {
    DBI::dbExecute(conn, "
      CREATE OR REPLACE TEMP VIEW V_MISSINGBEM AS (
        SELECT V1.FEATURE_ID
        FROM V_VRI V1
        LEFT JOIN V_VRIBEM V2
          ON V1.FEATURE_ID = V2.FEATURE_ID
        WHERE V2.FEATURE_ID IS NULL
      );
    ")
    fid <- DBI::dbGetQuery(conn, "SELECT FEATURE_ID FROM V_MISSINGBEM")
    .n <- length(fid$FEATURE_ID)
    msg <- if (length(fid$FEATURE_ID) > 50) " (first 50)" else ""
    if (nchar(msg)) fid <- head(fid, 50)
    logger::log_warn("The following VRI FEATURE_ID%s had no overlaping BEM Shape [%s]:\n%s" |> sprintf(msg, .n, paste(fid$FEATURE_ID, collapse = ", ")))
  }

}