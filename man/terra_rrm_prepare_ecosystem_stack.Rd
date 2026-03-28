\name{terra_rrm_prepare_ecosystem_stack}
\alias{terra_rrm_prepare_ecosystem_stack}
\title{Prepare a terra-native ecosystem stack}
\usage{
terra_rrm_prepare_ecosystem_stack(
  x,
  buc,
  beu_bec = NULL,
  rules_dt,
  unique_ecosystem_dt,
  most_recent_harvest_year,
  lake_layer = "lakes",
  apply_small_lakes = lake_layer \%in\% names(x),
  clear_site_ma = TRUE,
  use_ifelse = TRUE,
  filename = NULL,
  overwrite = FALSE
)
}
  \item{beu_bec}{Allowed BEC/BEU lookup table passed to the terra VRI correction stage.}
\arguments{
  \item{x}{A \code{terra::SpatRaster} containing the aligned raster-only inputs.}
  \item{buc}{Wetland lookup table passed to the terra wetland stage.}
  \item{rules_dt}{Rules data frame or supported Excel file path passed to the terra rules stage.}
  \item{unique_ecosystem_dt}{Unique ecosystem lookup table passed to the terra ecosystem merge stage.}
  \item{most_recent_harvest_year}{Numeric scalar passed to the terra forest age stage.}
  \item{lake_layer}{Name of the aligned lake-presence layer.}
  \item{apply_small_lakes}{Logical; if \code{TRUE}, run the terra small-lakes stage.}
  \item{clear_site_ma}{Logical passed to \code{terra_rrm_correct_bem_from_vri()}.}
  \item{use_ifelse}{Logical passed to \code{terra_rrm_correct_bem_from_vri()}.}
  \item{filename}{Optional filename for writing the prepared stack.}
  \item{overwrite}{Logical passed to terra writes.}
}
\value{
A prepared \code{SpatRaster} ready for moose and/or bear export.
}
\description{
Runs the currently implemented raster-native stages in the intended order on an already aligned terra stack.
}