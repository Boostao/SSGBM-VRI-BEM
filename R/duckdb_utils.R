rm_cols_from_tbl <- function(conn, tbl_name, cols) {
  for (col in cols) {
    DBI::dbExecute(conn, sprintf("ALTER TABLE %s DROP COLUMN IF EXISTS %s;", tbl_name, col))
  }
}

add_col_to_tbl <- function(conn, tbl_name, col, type) {
  stopifnot(length(col) == 1L)
  stopifnot(length(type) == 1L)
  
  DBI::dbExecute(conn, 
    sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;", 
    tbl_name, col, type))
}


update_tbl <- function(conn, tbl_name, set_expr, where_expr) {
  DBI::dbExecute(conn, 
    sprintf("UPDATE %s SET %s WHERE %s;", 
    tbl_name, set_expr, ifelse(is.null(where_expr), "1=1", where_expr)))
}

duckdb_tables <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT database_name, schema_name, table_name, temporary, has_primary_key, estimated_size, column_count FROM duckdb_tables();")
}

duckdb_any_null <- function(conn, tbl_name, col_name) {
  stopifnot(length(tbl_name) == 1L)
  stopifnot(length(col_name) == 1L)
  
  res <- DBI::dbGetQuery(conn, 
    sprintf("SELECT COUNT(%s) < COUNT(*) AS any_null FROM %s;", 
    col_name, tbl_name))
  
  return(res$any_null)
}

duckdb_any_value <- function(conn, tbl_name, col_name, value) {
  stopifnot(length(tbl_name) == 1L)
  stopifnot(length(col_name) == 1L)
  
  res <- DBI::dbGetQuery(conn, 
    sprintf("SELECT EXISTS ( SELECT 1 FROM %s WHERE %s = '%s' LIMIT 1) AS any_value;", 
    tbl_name, col_name, value))
  
  return(res$any_value)
}