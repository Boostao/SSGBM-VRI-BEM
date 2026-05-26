#' Validate that required attributes are in the object
#'
#' @param ifc input feature class
#' @param required_attributes names of required attributes
#'
#' @return error if attribute is missing
#' @export
#'

validate_required_attributes <- function(ifc, required_attributes){

  missing_attributes <- setdiff(required_attributes,
                                colnames(ifc))

  if (length(missing_attributes) > 0) {
    stop("The following attributes were not found : ", paste(missing_attributes, collapse = ", "))
  }
}


validate_views_column_names <- function(conn, obj, required_names) {

  obj_columns <- DBI::dbGetQuery(conn, sprintf("PRAGMA table_info('%s')", obj))$name
  missing_cols <- setdiff(required_names, obj_columns)

  if (length(missing_cols) > 0) {
    stop("The following columns were not found in the view : ", paste(missing_cols, collapse = ", "))
  }
}