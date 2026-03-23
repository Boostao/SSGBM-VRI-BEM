#' Find area-dominant crown values inside DuckDB (in-place)
#'
#' Replicates \code{find_crown_area_dominant_values()} entirely inside DuckDB,
#' modifying \code{vri_bem_tbl} in-place rather than returning a new object.
#'
#' @param conn A DuckDB connection.
#' @param vri_bem_tbl Character. Name of the VRI-BEM table to modify in-place.
#' @return Invisibly returns \code{vri_bem_tbl}.
#' @details
#' For each decile \code{i} in 1–3 the function adds (if absent) and populates
#' \code{CROWN_ALL_i}:
#' \itemize{
#'   \item \code{CROWN_ALL_i = CROWN_ALL} when
#'         \code{FORESTED_i = 'Y'} \strong{and}
#'         the first character of \code{STRCT_Si} is one of
#'         \code{'4'}, \code{'5'}, \code{'6'}, or \code{'7'}.
#'   \item \code{CROWN_ALL_i = NULL} otherwise.
#' }
#' All operations are idempotent — calling the function twice on the same
#' table recomputes the values without error.
#'
#' The table must already contain \code{CROWN_ALL}, \code{FORESTED_1–3}, and
#' \code{STRCT_S1–3} before this function is called (i.e. it should be called
#' after \code{merge_unique_ecosystem_fields_duckdb()}).
#'
#' @export
find_crown_area_dominant_values_duckdb <- function(conn, vri_bem_tbl) {

  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(vri_bem_tbl), nchar(vri_bem_tbl) > 0L)

  v <- vri_bem_tbl

  for (i in seq_len(3L)) {
    col      <- sprintf("CROWN_ALL_%d", i)
    forested <- sprintf("FORESTED_%d",  i)
    strct    <- sprintf("STRCT_S%d",    i)

    # Add column if absent (no-op when it already exists)
    DBI::dbExecute(conn, sprintf(
      "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s VARCHAR",
      v, col
    ))

    # Set value: copy CROWN_ALL when condition met, NULL otherwise
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s 
       SET %s = CASE
         WHEN %s = 'Y' AND LEFT(%s, 1) IN ('4','5','6','7') THEN CROWN_ALL
         ELSE NULL
        END;",
      v, col, forested, strct
    ))
  }

  invisible(vri_bem_tbl)
}
