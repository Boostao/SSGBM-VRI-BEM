# Internal helpers -----------------------------------------------------------

.rrm_duckdb_join_keys <- function(animal, decile) {
  if (animal == "moose") {
    return(c(
      ECO_SEC = "Eco_sec",
      BGC_ZONE = "Bgc_zone",
      BGC_SUBZON = "Bgc_subzon",
      BGC_VRT = "Bgc_vrt",
      BGC_PHASE = "Bgc_phase",
      stats::setNames("Beumc", sprintf("BEUMC_S%d", decile)),
      SLOPE_MOD = "Slope_mod",
      SITE_M3A = "Site_m3a",
      SNOW_CODE = "Snow_code",
      ABOVE_ELEV_THOLD = "Above_Elev_Thold",
      stats::setNames("Crown_all", sprintf("CROWN_ALL_%d", decile)),
      stats::setNames("Strct_d", sprintf("STRCT_S%d", decile)),
      stats::setNames("Stand_d", sprintf("STAND_A%d", decile))
    ))
  }

  if (animal == "bear") {
    return(c(
      ECO_SEC = "Eco_sec",
      BGC_ZONE = "Bgc_zone",
      BGC_SUBZON = "Bgc_subzon",
      BGC_VRT = "Bgc_vrt",
      BGC_PHASE = "Bgc_phase",
      stats::setNames("Beumc", sprintf("BEUMC_S%d", decile)),
      SLOPE_MOD = "Slope_mod",
      SITE_M3A = "Site_m3a",
      Salmon = "Salmon",
      SNOW_CODE = "Snow_code",
      ABOVE_ELEV_THOLD = "Above_Elev_Thold",
      stats::setNames("Crown_all", sprintf("CROWN_ALL_%d", decile)),
      stats::setNames("Strct_d", sprintf("STRCT_S%d", decile)),
      stats::setNames("Stand_d", sprintf("STAND_A%d", decile))
    ))
  }

  if (animal == "huckleberry") {
    return(c(
      ECO_SEC = "Eco_sec",
      BGC_ZONE = "Bgc_zone",
      BGC_SUBZON = "Bgc_subzon",
      BGC_VRT = "Bgc_vrt",
      BGC_PHASE = "Bgc_phase",
      HUCK_ASP = "Huck_asp",
      HUCK_ELEV_Thold = "HUCK_ELEV_Thold",
      stats::setNames("Crown_All", sprintf("CROWN_ALL_%d", decile)),
      stats::setNames("Strct_d", sprintf("STRCT_S%d", decile)),
      stats::setNames("Stand_d", sprintf("STAND_A%d", decile))
    ))
  }

  stop("Unsupported animal: ", animal, call. = FALSE)
}


.rrm_duckdb_join_condition <- function(v_alias, r_alias, key_map) {
  paste(
    sprintf(
      "%s.%s IS NOT DISTINCT FROM %s.%s",
      v_alias,
      names(key_map),
      r_alias,
      unname(key_map)
    ),
    collapse = " AND\n       "
  )
}


.rrm_duckdb_normalize_lookup_names <- function(rrm_dt, animal) {
  name_map <- c(
    ECO_SEC = "Eco_sec",
    BGC_ZONE = "Bgc_zone",
    BGC_SUBZON = "Bgc_subzon",
    BGC_VRT = "Bgc_vrt",
    BGC_PHASE = "Bgc_phase"
  )

  if (animal %in% c("moose", "bear")) {
    name_map <- c(
      name_map,
      BEUMC = "Beumc",
      SLOPE_MOD = "Slope_mod",
      SITE_M3A = "Site_m3a",
      SNOW_CODE = "Snow_code",
      ABOVE_ELEV_THOLD = "Above_Elev_Thold",
      CROWN_ALL = "Crown_all",
      STRCT = "Strct_d",
      STAND = "Stand_d"
    )
  }

  if (animal == "huckleberry") {
    name_map <- c(
      name_map,
      HUCK_ASP = "Huck_asp",
      CROWN_ALL = "Crown_All",
      STRCT = "Strct_d",
      STAND = "Stand_d"
    )
  }

  current_names <- names(rrm_dt)
  for (src_name in names(name_map)) {
    dest_name <- unname(name_map[[src_name]])
    src_idx <- match(src_name, current_names)
    if (is.na(src_idx) || dest_name %in% current_names) {
      next
    }
    current_names[src_idx] <- dest_name
  }
  names(rrm_dt) <- current_names
  rrm_dt
}


.rrm_duckdb_suit_special_condition <- function(decile) {
  sprintf(
    paste0(
      "((j.FORESTED_%1$d = 'Y' AND j.STRCT_S%1$d = '7a' AND j.VRI_AGE_CL_STS = -1) ",
      "OR (j.FORESTED_%1$d = 'N' AND j.STRCT_S%1$d IS NULL ",
      "AND j.ABOVE_ELEV_THOLD = 'N' AND j.STS_CLIMAX_%1$d IS NOT NULL))"
    ),
    decile
  )
}


.rrm_duckdb_suit_base_expr <- function(raw_ref, decile) {
  sprintf(
    "CASE WHEN %s THEN NULL WHEN (%s) > 6 THEN NULL ELSE (%s) END",
    .rrm_duckdb_suit_special_condition(decile),
    raw_ref,
    raw_ref
  )
}


.rrm_duckdb_cap_base_expr <- function(raw_ref) {
  sprintf("CASE WHEN (%s) > 6 THEN NULL ELSE (%s) END", raw_ref, raw_ref)
}


.rrm_duckdb_best_expr <- function(val1, val2, val3) {
  sprintf(
    paste0(
      "CASE ",
      "WHEN (%1$s) <= (%2$s) AND (%1$s) <= (%3$s) THEN CAST((%1$s) AS DOUBLE) ",
      "WHEN (%2$s) <= (%3$s) THEN CAST((%2$s) AS DOUBLE) ",
      "WHEN (%3$s) <= (%2$s) THEN CAST((%3$s) AS DOUBLE) ",
      "ELSE NULL END"
    ),
    val1,
    val2,
    val3
  )
}


.rrm_duckdb_weighted_avg_expr <- function(values, suffix) {
  numer <- paste(
    sprintf("(COALESCE((%s), 0) * j.SDEC_%d)", values, seq_along(values)),
    collapse = " + "
  )
  denom <- paste(
    sprintf(
      "(j.SDEC_%d * CASE WHEN (%s) IS NOT NULL THEN 1 ELSE 0 END)",
      seq_along(values),
      values
    ),
    collapse = " + "
  )

  sprintf(
    "((%s) / NULLIF((%s), 0))%s",
    numer,
    denom,
    suffix
  )
}


.rrm_duckdb_signif_expr <- function(value_expr, digits = 3L) {
  sprintf(
    paste0(
      "CASE ",
      "WHEN (%1$s) IS NULL THEN NULL ",
      "WHEN (%1$s) = 0 THEN 0 ",
      "ELSE ROUND((%1$s), CAST(%2$d - CEIL(LOG10(ABS(%1$s))) AS INTEGER)) END"
    ),
    value_expr,
    digits
  )
}


.rrm_duckdb_value_to_rating_expr <- function(value_expr) {
  signif_expr <- .rrm_duckdb_signif_expr(value_expr)

  sprintf(
    paste0(
      "CASE ",
      "WHEN (%1$s) IS NULL THEN NULL ",
      "WHEN (%2$s) <= 0 THEN 6 ",
      "WHEN (%2$s) <= 0.05 THEN 5 ",
      "WHEN (%2$s) <= 0.25 THEN 4 ",
      "WHEN (%2$s) <= 0.5 THEN 3 ",
      "WHEN (%2$s) <= 0.75 THEN 2 ",
      "ELSE 1 END"
    ),
    value_expr,
    signif_expr
  )
}


.rrm_duckdb_metric_sql <- function(rating_var) {
  rsi_var <- sub("_6C$", "_RSI", rating_var)
  cap_var <- paste0(rating_var, "_CAP")
  cap_rsi_var <- paste0(rsi_var, "_CAP")

  suit_raw <- sprintf("j.%s_RAW_%d", rating_var, 1:3)
  suit_rsi_raw <- sprintf("j.%s_RAW_%d", rsi_var, 1:3)
  cap_raw <- sprintf("j.%s_RAW_%d", cap_var, 1:3)
  cap_rsi_raw <- sprintf("j.%s_RAW_%d", cap_rsi_var, 1:3)

  suit_base <- mapply(
    .rrm_duckdb_suit_base_expr,
    raw_ref = suit_raw,
    decile = 1:3,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  cap_base <- vapply(cap_raw, .rrm_duckdb_cap_base_expr, character(1))

  suit_rsi_base <- sprintf(
    "CASE WHEN (%s) IS NULL THEN NULL ELSE (%s) END",
    suit_base,
    suit_rsi_raw
  )
  cap_rsi_base <- sprintf(
    "CASE WHEN (%s) IS NULL THEN NULL ELSE (%s) END",
    cap_base,
    cap_rsi_raw
  )

  suit_9 <- sprintf("COALESCE((%s), 9)", suit_base)
  cap_9 <- sprintf("COALESCE((%s), 9)", cap_base)

  suit_hv_raw <- .rrm_duckdb_best_expr(suit_9[1], suit_9[2], suit_9[3])
  cap_hv_raw <- .rrm_duckdb_best_expr(cap_9[1], cap_9[2], cap_9[3])
  suit_wa_rsi_raw <- .rrm_duckdb_weighted_avg_expr(suit_rsi_base, "")
  cap_wa_rsi_raw <- .rrm_duckdb_weighted_avg_expr(cap_rsi_base, "")
  suit_wa_rsi <- sprintf(
    "CASE WHEN (%s) IS NULL THEN NULL ELSE (%s) END",
    suit_rsi_base[1],
    suit_wa_rsi_raw
  )
  cap_wa_rsi <- sprintf(
    "CASE WHEN (%s) IS NULL THEN NULL ELSE (%s) END",
    cap_rsi_base[1],
    cap_wa_rsi_raw
  )

  c(
    sprintf(
      "CASE WHEN j.SDEC_%d = 0 THEN NULL ELSE (%s) END AS %s_SU_%d",
      1:3,
      suit_base,
      rating_var,
      1:3
    ),
    sprintf(
      "CASE WHEN j.SDEC_%d = 0 THEN NULL ELSE (%s) END AS %s_SU_%d",
      1:3,
      suit_rsi_base,
      rsi_var,
      1:3
    ),
    sprintf(
      "CASE WHEN (%s) > 8 THEN NULL ELSE (%s) END AS %s_SU_HV",
      suit_hv_raw,
      suit_hv_raw,
      rating_var
    ),
    sprintf(
      "(%s) AS %s_SU_WA",
      suit_wa_rsi,
      rsi_var
    ),
    sprintf(
      "(%s) AS %s_SU_WA",
      .rrm_duckdb_value_to_rating_expr(suit_wa_rsi),
      rating_var
    ),
    sprintf(
      "CASE WHEN j.SDEC_%d = 0 THEN NULL ELSE (%s) END AS %s_%d",
      1:3,
      cap_base,
      cap_var,
      1:3
    ),
    sprintf(
      "CASE WHEN j.SDEC_%d = 0 THEN NULL ELSE (%s) END AS %s_%d",
      1:3,
      cap_rsi_base,
      cap_rsi_var,
      1:3
    ),
    sprintf(
      "CASE WHEN (%s) > 8 THEN NULL ELSE (%s) END AS %s_HV",
      cap_hv_raw,
      cap_hv_raw,
      cap_var
    ),
    sprintf(
      "(%s) AS %s_WA",
      cap_wa_rsi,
      cap_rsi_var
    ),
    sprintf(
      "(%s) AS %s_WA",
      .rrm_duckdb_value_to_rating_expr(cap_wa_rsi),
      cap_var
    )
  )
}


#' Merge RRM ratings onto a DuckDB VRI-BEM table
#'
#' Replicates `merge_rrm_on_vri()` using DuckDB tables instead of an
#' in-memory `sf` / `data.table` object. The function stages the
#' formatted RRM lookup in DuckDB, joins it to the requested VRI-BEM table for
#' each active decile, and computes the same suitability / capability outputs as
#' the R implementation.
#'
#' @param conn A DuckDB connection.
#' @param vri_bem_tbl Character. Name of the DuckDB table or view containing the
#'   VRI-BEM attributes.
#' @param rrm_dt A data.frame or data.table containing the species-specific RRM
#'   lookup.
#' @param animal Character. One of `"bear"`, `"moose"`, or `"huckleberry"`.
#' @param result_tbl Character. Name of the output table. Defaults to
#'   `vri_bem_tbl`.
#' @return Invisibly returns `result_tbl`.
#' @details
#' The function preserves all source columns and appends the same decile-level,
#' high-value, weighted-average, and `rrm_merge_ind` fields produced by
#' `merge_rrm_on_vri()`. Existing rating columns are recomputed when the
#' function is called again on the same table.
#' @import DBI
#' @import duckdb
#' @export
merge_rrm_on_vri_duckdb <- function(conn,
                                    vri_bem_tbl,
                                    rrm_dt,
                                    animal,
                                    result_tbl = vri_bem_tbl) {

  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(vri_bem_tbl), length(vri_bem_tbl) == 1L, nzchar(vri_bem_tbl))
  stopifnot(is.character(result_tbl), length(result_tbl) == 1L, nzchar(result_tbl))
  stopifnot(is.data.frame(rrm_dt))

  if (!animal %in% c("bear", "moose", "huckleberry")) {
    stop("animal must be one of 'bear', 'moose', or 'huckleberry'", call. = FALSE)
  }

  tmp_rrm_tbl <- "_tmp_rrm_merge_lookup"
  on.exit(
    try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_rrm_tbl)), silent = TRUE),
    add = TRUE
  )

  rrm_prepped <- data.table::copy(data.table::as.data.table(rrm_dt))
  rrm_prepped <- .rrm_duckdb_normalize_lookup_names(rrm_prepped, animal = animal)
  rrm_prepped <- format_rrm_dt(rrm_dt = rrm_prepped, animal = animal)
  rrm_prepped <- calc_capability_rating(rrm_dt = rrm_prepped, animal = animal)
  rating_variables <- grep("_6C$", names(rrm_prepped), value = TRUE)
  rsi_variables <- sub("_6C$", "_RSI", rating_variables)
  if (length(rating_variables) == 0L) {
    stop("rrm_dt must contain at least one '*_6C' rating column", call. = FALSE)
  }
  if (!all(rsi_variables %in% names(rrm_prepped))) {
    stop("rrm_dt must contain matching '*_RSI' columns for each '*_6C' rating column", call. = FALSE)
  }
  if (!"Hectares" %in% names(rrm_prepped)) {
    stop("rrm_dt must contain a Hectares column", call. = FALSE)
  }

  DBI::dbWriteTable(
    conn,
    tmp_rrm_tbl,
    as.data.frame(rrm_prepped, stringsAsFactors = FALSE),
    temporary = TRUE,
    overwrite = TRUE
  )

  raw_metric_aliases <- character(0)
  join_metric_selects <- character(0)
  for (decile in 1:3) {
    join_metric_selects <- c(
      join_metric_selects,
      sprintf("r%d.Hectares AS Hectares_%d", decile, decile)
    )

    for (rating_var in rating_variables) {
      rsi_var <- sub("_6C$", "_RSI", rating_var)
      suit_alias <- sprintf("%s_RAW_%d", rating_var, decile)
      suit_rsi_alias <- sprintf("%s_RAW_%d", rsi_var, decile)
      cap_var <- paste0(rating_var, "_CAP")
      cap_rsi_var <- paste0(rsi_var, "_CAP")
      cap_alias <- sprintf("%s_RAW_%d", cap_var, decile)
      cap_rsi_alias <- sprintf("%s_RAW_%d", cap_rsi_var, decile)
      raw_metric_aliases <- c(raw_metric_aliases, suit_alias, suit_rsi_alias, cap_alias, cap_rsi_alias)
      join_metric_selects <- c(
        join_metric_selects,
        sprintf("r%d.%s AS %s", decile, rating_var, suit_alias),
        sprintf("r%d.%s AS %s", decile, rsi_var, suit_rsi_alias),
        sprintf("r%d.%s AS %s", decile, cap_var, cap_alias),
        sprintf("r%d.%s AS %s", decile, cap_rsi_var, cap_rsi_alias)
      )
    }
  }

  join_blocks <- vapply(1:3, function(decile) {
    sprintf(
      "LEFT JOIN %s r%d ON %s",
      tmp_rrm_tbl,
      decile,
      .rrm_duckdb_join_condition("v", sprintf("r%d", decile), .rrm_duckdb_join_keys(animal, decile))
    )
  }, character(1))

  derived_cols <- unlist(lapply(rating_variables, .rrm_duckdb_metric_sql), use.names = FALSE)
  output_cols <- c(
    "rrm_merge_ind",
    unlist(lapply(rating_variables, function(rating_var) {
      rsi_var <- sub("_6C$", "_RSI", rating_var)
      cap_var <- paste0(rating_var, "_CAP")
      cap_rsi_var <- paste0(rsi_var, "_CAP")
      c(
        sprintf("%s_SU_%d", rating_var, 1:3),
        sprintf("%s_SU_%d", rsi_var, 1:3),
        sprintf("%s_SU_HV", rating_var),
        sprintf("%s_SU_WA", rsi_var),
        sprintf("%s_SU_WA", rating_var),
        sprintf("%s_%d", cap_var, 1:3),
        sprintf("%s_%d", cap_rsi_var, 1:3),
        sprintf("%s_HV", cap_var),
        sprintf("%s_WA", cap_rsi_var),
        sprintf("%s_WA", cap_var)
      )
    }), use.names = FALSE)
  )

  source_cols <- DBI::dbGetQuery(conn, sprintf("PRAGMA table_info('%s')", vri_bem_tbl))$name
  source_cols <- source_cols[!is.na(source_cols)]
  exclude_cols <- c(
    intersect(source_cols, output_cols),
    sprintf("Hectares_%d", 1:3),
    raw_metric_aliases
  )
  exclude_cols <- unique(exclude_cols)

  base_select <- if (length(exclude_cols) > 0L) {
    sprintf("j.* EXCLUDE (%s)", paste(exclude_cols, collapse = ", "))
  } else {
    "j.*"
  }

  sql <- sprintf(
    paste0(
      "CREATE OR REPLACE TEMP TABLE %s AS\n",
      "WITH v_base AS (\n",
      "  SELECT * REPLACE (\n",
      "    COALESCE(SDEC_1, 0) AS SDEC_1,\n",
      "    COALESCE(SDEC_2, 0) AS SDEC_2,\n",
      "    COALESCE(SDEC_3, 0) AS SDEC_3\n",
      "  )\n",
      "  FROM %s\n",
      "),\n",
      "joined AS (\n",
      "  SELECT\n",
      "    v.*,\n",
      "    %s\n",
      "  FROM v_base v\n",
      "  %s\n",
      ")\n",
      "SELECT\n",
      "  %s,\n",
      "  COALESCE(j.Hectares_1, j.Hectares_2, j.Hectares_3) IS NOT NULL AS rrm_merge_ind,\n",
      "  %s\n",
      "FROM joined j"
    ),
    result_tbl,
    vri_bem_tbl,
    paste(join_metric_selects, collapse = ",\n    "),
    paste(join_blocks, collapse = "\n  "),
    base_select,
    paste(derived_cols, collapse = ",\n  ")
  )

  DBI::dbExecute(conn, sql)
  invisible(result_tbl)
}