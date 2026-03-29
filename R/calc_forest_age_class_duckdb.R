#' Compute forest age class variables inside DuckDB (in-place)
#'
#' @param conn A DuckDB connection.
#' @param vri_bem_tbl Character. Name of the VRI-BEM table to modify in-place.
#' @param ccb_tbl Character. Name of the CCB table in DuckDB from which the
#'   most-recent harvest year is derived (default \code{"V_CCB"}).
#' @param harvest_year_col Character. Name of the harvest-year column inside
#'   \code{ccb_tbl} (default \code{"HARVEST_YEAR"}).
#' @return Invisibly returns \code{vri_bem_tbl}.
#' @details
#' The function modifies \code{vri_bem_tbl} **in-place** by:
#' \enumerate{
#'   \item Deriving \code{most_recent_harvest_year} as
#'         \code{MAX(harvest_year_col)} from \code{ccb_tbl}.
#'   \item Adding columns \code{VRI_AGE_CL_STS} and \code{VRI_AGE_CL_STD}
#'         (idempotent — no-op when they already exist).
#'   \item If \code{MRSRD_Y} is present in \code{vri_bem_tbl}, updating
#'         \code{PROJ_AGE_1 = most_recent_harvest_year - MRSRD_Y} for rows
#'         where \code{MRSRD_Y} is not \code{NULL}.
#'   \item Computing \code{VRI_AGE_CL_STS} and \code{VRI_AGE_CL_STD} via
#'         \code{CASE WHEN} logic matching \code{calc_forest_age_class()}.
#' }
#'
#' Age-class breakpoints:
#'
#' | PROJ_AGE_1 | VRI_AGE_CL_STS | VRI_AGE_CL_STD |
#' |:----------:|---------------:|---------------:|
#' | < 0        |  -1            |  -1            |
#' | 0–3        |   2            |  15            |
#' | 4–10       |   7            |  15            |
#' | 11–15      |  20            |  15            |
#' | 16–30      |  20            |  30            |
#' | 31–40      |  35            |  50            |
#' | 41–50      |  50            |  50            |
#' | 51–60      |  50            |  80            |
#' | 61–80      |  70            |  80            |
#' | 81–140     | 125            |9999            |
#' | 141–249    | 195            |9999            |
#' | > 249      | 301            |9999            |
#' | NULL       |  -1            |  -1            |
#'
#' @export
calc_forest_age_class_duckdb <- function(conn,
                                         vri_bem_tbl,
                                         ccb_tbl          = "CCB",
                                         harvest_year_col = "HARVEST_START_YEAR_CALENDAR") {

  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(vri_bem_tbl), nchar(vri_bem_tbl) > 0L)
  stopifnot(is.character(ccb_tbl),     nchar(ccb_tbl)     > 0L)
  stopifnot(is.character(harvest_year_col), nchar(harvest_year_col) > 0L)

  # ── 1. Derive most_recent_harvest_year from the CCB table ────────────────
  mrhy <- DBI::dbGetQuery(
    conn,
    sprintf('SELECT MAX(%s) AS mrhy FROM %s', harvest_year_col, ccb_tbl)
  )$mrhy

  if (is.null(mrhy) || is.na(mrhy)) {
    warning(
      "calc_forest_age_class_duckdb: MAX(", harvest_year_col, ") from ",
      ccb_tbl, " is NA; MRSRD_Y override of PROJ_AGE_1 will be skipped."
    )
  }

  # ── 2. Add output columns (no-op if they already exist) ─────────────────
  DBI::dbExecute(conn, sprintf(
    "ALTER TABLE %s ADD COLUMN IF NOT EXISTS VRI_AGE_CL_STS DOUBLE",
    vri_bem_tbl
  ))
  DBI::dbExecute(conn, sprintf(
    "ALTER TABLE %s ADD COLUMN IF NOT EXISTS VRI_AGE_CL_STD DOUBLE",
    vri_bem_tbl
  ))

  # ── 3. Override PROJ_AGE_1 from MRSRD_Y where the column is present ──────
  existing_cols <- toupper(DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", vri_bem_tbl)
  )$name)

  if ("MRSRD_Y" %in% existing_cols && !is.null(mrhy) && !is.na(mrhy)) {
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s
         SET PROJ_AGE_1 = %s - TRY_CAST(MRSRD_Y AS DOUBLE)
       WHERE MRSRD_Y IS NOT NULL",
      vri_bem_tbl, mrhy
    ))
  }

  # ── 4. Compute VRI_AGE_CL_STS ────────────────────────────────────────────
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET VRI_AGE_CL_STS = CASE
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <   0 THEN   -1
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <=  3 THEN    2
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 10 THEN    7
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 30 THEN   20
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 40 THEN   35
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 60 THEN   50
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 80 THEN   70
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 140 THEN 125
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <= 249 THEN 195
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) >  249 THEN 301
       ELSE -1
     END",
    vri_bem_tbl
  ))

  # ── 5. Compute VRI_AGE_CL_STD ────────────────────────────────────────────
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET VRI_AGE_CL_STD = CASE
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <   0 THEN   -1
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <=  15 THEN   15
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <=  30 THEN   30
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <=  50 THEN   50
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) <=  80 THEN   80
       WHEN TRY_CAST(PROJ_AGE_1 AS DOUBLE) >   80 THEN 9999
       ELSE -1
     END",
    vri_bem_tbl
  ))

  invisible(vri_bem_tbl)
}
