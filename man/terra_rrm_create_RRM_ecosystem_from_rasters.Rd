\name{terra_rrm_create_RRM_ecosystem_from_rasters}
\alias{terra_rrm_create_RRM_ecosystem_from_rasters}
\title{Create terra-native RRM summaries directly from aligned raster files}
\usage{
terra_rrm_create_RRM_ecosystem_from_rasters(
  vri_dsn,
  bem_dsn,
  rivers_dsn,
  wetlands_dsn,
  rules_xl,
  most_recent_harvest_year,
  kind = c("moose", "bear"),
  lakes_dsn = NULL,
  ccb_dsn = NULL,
  elevation_dsn = NULL,
  beu_bec_csv = system.file("csv/Allowed_BEC_BEUs_NE_ALL.csv", package = "SSGBM.VRI.BEM"),
  beu_wetland_update_csv = system.file("csv/beu_wetland_updates.csv", package = "SSGBM.VRI.BEM"),
  unique_ecosystem = system.file("csv/Skeena_VRIBEM_LUT.csv", package = "SSGBM.VRI.BEM"),
  lake_layer = "lakes",
  apply_small_lakes = !is.null(lakes_dsn),
  clear_site_ma = TRUE,
  use_ifelse = TRUE,
  return_stack = FALSE,
  input_stack_filename = NULL,
  stack_filename = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
  \item{vri_dsn}{Path to the aligned VRI raster.}
  \item{bem_dsn}{Path to the aligned BEM raster.}
  \item{rivers_dsn}{Path to the aligned rivers raster.}
  \item{wetlands_dsn}{Path to the aligned wetlands raster.}
  \item{rules_xl}{Rules workbook path, or an in-memory rules data frame.}
  \item{most_recent_harvest_year}{Numeric scalar passed to the terra forest age stage.}
  \item{kind}{One or both of \code{"moose"} and \code{"bear"}.}
  \item{lakes_dsn}{Optional path to an aligned lake raster.}
  \item{ccb_dsn}{Optional path to an aligned consolidated cutblock raster.}
  \item{elevation_dsn}{Optional path to an aligned elevation raster.}
  \item{beu_bec_csv}{Path to the allowed BEC/BEU lookup CSV.}
  \item{beu_wetland_update_csv}{Path to the wetland lookup CSV.}
  \item{unique_ecosystem}{Path to the unique ecosystem CSV.}
  \item{lake_layer}{Name of the aligned lake-presence layer.}
  \item{apply_small_lakes}{Logical; if \code{TRUE}, run the terra small-lakes stage.}
  \item{clear_site_ma}{Logical passed to \code{terra_rrm_correct_bem_from_vri()}.}
  \item{use_ifelse}{Logical passed to \code{terra_rrm_correct_bem_from_vri()}.}
  \item{return_stack}{Logical; if \code{TRUE}, include the prepared raster stack in the return value.}
  \item{input_stack_filename}{Optional filename for writing the merged input stack before the terra preparation stages.}
  \item{stack_filename}{Optional filename for writing the prepared stack before export.}
  \item{overwrite}{Logical passed to terra writes.}
  \item{verbose}{Logical; if \code{TRUE}, emit progress messages for the major disk loading steps.}
}
\value{
If a single summary is requested and \code{return_stack = FALSE}, returns one export \code{data.table}. Otherwise returns a named list containing the requested summaries and, when requested, the prepared raster stack.
}
\description{
Reads the aligned raster inputs from disk, loads the supporting wetland and unique-ecosystem lookup tables, and then runs the terra-native preparation and export path.
}