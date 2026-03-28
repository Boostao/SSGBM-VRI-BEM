\name{terra_rrm_create_RRM_ecosystem}
\alias{terra_rrm_create_RRM_ecosystem}
\title{Create moose and/or bear RRM summaries from the terra-native stack path}
\usage{
terra_rrm_create_RRM_ecosystem(
  x,
  buc,
  beu_bec = NULL,
  rules_dt,
  unique_ecosystem_dt,
  most_recent_harvest_year,
  kind = c("moose", "bear"),
  lake_layer = "lakes",
  apply_small_lakes = lake_layer \%in\% names(x),
  clear_site_ma = TRUE,
  use_ifelse = TRUE,
  return_stack = FALSE,
  stack_filename = NULL,
  overwrite = FALSE
)
}
\arguments{
  \item{x}{A \code{terra::SpatRaster} containing the aligned raster-only inputs.}
  \item{buc}{Wetland lookup table passed to the terra wetland stage.}
  \item{beu_bec}{Allowed BEC/BEU lookup table passed to the terra VRI correction stage.}
  \item{rules_dt}{Rules data frame or supported Excel file path passed to the terra rules stage.}
  \item{unique_ecosystem_dt}{Unique ecosystem lookup table passed to the terra ecosystem merge stage.}
  \item{most_recent_harvest_year}{Numeric scalar passed to the terra forest age stage.}
  \item{kind}{One or both of \code{"moose"} and \code{"bear"}.}
  \item{lake_layer}{Name of the aligned lake-presence layer.}
  \item{apply_small_lakes}{Logical; if \code{TRUE}, run the terra small-lakes stage.}
  \item{clear_site_ma}{Logical passed to \code{terra_rrm_correct_bem_from_vri()}.}
  \item{use_ifelse}{Logical passed to \code{terra_rrm_correct_bem_from_vri()}.}
  \item{return_stack}{Logical; if \code{TRUE}, include the prepared raster stack in the return value.}
  \item{stack_filename}{Optional filename for writing the prepared stack before export.}
  \item{overwrite}{Logical passed to terra writes.}
}
\value{
If a single summary is requested and \code{return_stack = FALSE}, returns one export \code{data.table}. Otherwise returns a named list containing the requested summaries and, when requested, the prepared raster stack.
}
\description{
Runs the current terra-native preparation stages and then dispatches to the moose and/or bear export summarizers.
}