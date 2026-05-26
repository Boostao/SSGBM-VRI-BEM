get_aoi_wkt_from_tsa <- function(conn, aoi_name) {
  #Set area boundary
aoi_wkt <- DBI::dbGetQuery(conn, sprintf("
  SELECT ST_AsText(
    ST_MakeValid(
      ST_Intersection(
        ST_Union_Agg(ST_MakeValid(ST_GeomFromText(t.Shape))),
        (SELECT ST_Union_Agg(ST_MakeValid(ST_GeomFromText(Shape))) FROM SKEENA)
      )
    )
  ) AS wkt
  FROM TSA t
  WHERE t.TSA_NUMBER_DESCRIPTION = '%s TSA'
", aoi_name))$wkt
  
  aoi_wkt
}