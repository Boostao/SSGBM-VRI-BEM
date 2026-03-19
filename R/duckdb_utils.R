rm_cols_from_tbl <- function(conn, tbl_name, cols) {
  for (col in cols) {
    duckdb::dbSendQuery(conn, sprintf("ALTER TABLE %s DROP COLUMN IF EXISTS %s;", tbl_name, col))
  }
}

add_col_to_tbl <- function(conn, tbl_name, col, type) {
  stopifnot(length(col) == 1L)
  stopifnot(length(type) == 1L)
  
  duckdb::dbSendQuery(conn, 
    sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;", 
    tbl_name, col, type))
}

