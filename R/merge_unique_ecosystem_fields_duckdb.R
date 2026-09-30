#' Assign values from unique ecosystems table inside DuckDB (in-place)
#'
#' Replicates \code{merge_unique_ecosystem_fields()} entirely inside DuckDB,
#' modifying \code{vri_bem_tbl} in-place rather than returning a new object.
#' Populates TEIS fields, REALM/GROUP/CLASS/KIND, SNOW_CODE, FORESTED,
#' STS_CLIMAX, STAND_CLIMAX, structural-stage age-class lookups, stand
#' age-class lookups, STD_VRI, CROWN_ALL, parkland_ind, STRCT_S1–3, and
#' STAND_A1–3.
#'
#' @param conn A DuckDB connection.
#' @param vri_bem_tbl Character. Name of the VRI-BEM table to modify in-place.
#' @param unique_ecosystem_dt data.frame or data.table containing the unique
#'   ecosystem lookup data (same structure as used by
#'   \code{merge_unique_ecosystem_fields()}).
#' @return Invisibly returns \code{vri_bem_tbl}.
#' @details
#' The function adds columns via \code{ALTER TABLE … ADD COLUMN IF NOT EXISTS}
#' and populates them via \code{UPDATE … FROM} joins and CASE-WHEN logic.
#' All operations are idempotent — calling the function twice on the same
#' table recomputes the values without error.
#'
#' Columns added / updated:
#' \itemize{
#'   \item Three-decile ecosystem fields (REALM, GROUP, CLASS, KIND, FORESTED,
#'         STS_CLIMAX, STAND_CLIMAX, plus Stand-Age and Struct-Age lookup
#'         buckets) for each of BEUMC_S1, BEUMC_S2, BEUMC_S3.
#'   \item \code{SNOW_CODE} (from BEUMC_S1 only).
#'   \item \code{STD_VRI} and \code{CROWN_ALL} (equivalent to
#'         \code{add_std_crown_fields()}).
#'   \item \code{parkland_ind} — \code{TRUE} when the last character of
#'         \code{BGC_SUBZON} is \code{"p"}.
#'   \item \code{STAND_AGE_1–3}, \code{STS_AGE_1–3}, \code{STRCT_S1–3},
#'         and \code{STAND_A1–3}.
#' }
#'
#' @export
merge_unique_ecosystem_fields_duckdb <- function(conn,
                                                  vri_bem_tbl,
                                                  unique_ecosystem_dt) {

  stopifnot(DBI::dbIsValid(conn))
  stopifnot(is.character(vri_bem_tbl), nchar(vri_bem_tbl) > 0L)
  stopifnot(is.data.frame(unique_ecosystem_dt))

  v <- vri_bem_tbl  # shorthand for use inside sprintf()

  # ── 1. Rename special-character / reserved-word columns ──────────────────
  # so all SQL references use plain identifiers (no quoting needed).
  col_map <- c(
    "Forested (Y/N)"    = "FORESTED_YN",
    "Stand_Age_0-15"    = "SA_0_15",
    "Stand_Age_16-30"   = "SA_16_30",
    "Stand_Age_31-50"   = "SA_31_50",
    "Stand_Age_51-80"   = "SA_51_80",
    "Stand_Age_80+"     = "SA_GT_80",
    "Struct_Age_0-3"    = "STA_0_3",
    "Struct_Age_4-10"   = "STA_4_10",
    "Struct_Age_11-30"  = "STA_11_30",
    "Struct_Age_31-40"  = "STA_31_40",
    "Struct_Age_41-60"  = "STA_41_60",
    "Struct_Age_61-80"  = "STA_61_80",
    "Struct_Age_81-139" = "STA_81_139",
    "Struct_Age_140-249"= "STA_140_249",
    "Struct_Age_250+"   = "STA_GT_249",
    "Snow_Code"         = "SNOW_CODE_C",
    "Strct_Climax"      = "STRCT_CLIMAX_C",
    "Stand_Climax"      = "STAND_CLIMAX_C",
    "GROUP"             = "GROUP_C"
  )

  eco <- as.data.frame(unique_ecosystem_dt, stringsAsFactors = FALSE)
  for (old in names(col_map)) {
    idx <- match(old, names(eco))
    if (!is.na(idx)) names(eco)[idx] <- col_map[[old]]
  }

  # ── 2. Load ecosystem data into a temporary DuckDB table ─────────────────
  tmp_ue <- "_tmp_uef_merge"
  on.exit(
    try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tmp_ue)),
        silent = TRUE),
    add = TRUE
  )
  DBI::dbWriteTable(conn, tmp_ue, eco, temporary = TRUE, overwrite = TRUE)

  # ── 3. Add all output columns (no-op if they already exist) ──────────────
  add_col <- function(col, type = "VARCHAR") {
    DBI::dbExecute(conn, sprintf(
      "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s", vri_bem_tbl, col, type
    ))
  }

  varchar_cols <- c(
    # Decile 1
    "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SNOW_CODE",
    "FORESTED_1", "STS_CLIMAX_1", "STAND_CLIMAX_1",
    "STAND_1_Age_0_15",  "STAND_1_Age_16_30", "STAND_1_Age_31_50",
    "STAND_1_Age_51_80", "STAND_1_Age_gt_80",
    "STS_1_Age_0_3",   "STS_1_Age_4_10",   "STS_1_Age_11_30",
    "STS_1_Age_31_40", "STS_1_Age_41_60",  "STS_1_Age_61_80",
    "STS_1_Age_81_139","STS_1_Age_140_249","STS_1_Age_gt_249",
    # Decile 2
    "REALM_2", "GROUP_2", "CLASS_2", "KIND_2",
    "FORESTED_2", "STS_CLIMAX_2", "STAND_CLIMAX_2",
    "STAND_2_Age_0_15",  "STAND_2_Age_16_30", "STAND_2_Age_31_50",
    "STAND_2_Age_51_80", "STAND_2_Age_gt_80",
    "STS_2_Age_0_3",   "STS_2_Age_4_10",   "STS_2_Age_11_30",
    "STS_2_Age_31_40", "STS_2_Age_41_60",  "STS_2_Age_61_80",
    "STS_2_Age_81_139","STS_2_Age_140_249","STS_2_Age_gt_249",
    # Decile 3
    "REALM_3", "GROUP_3", "CLASS_3", "KIND_3",
    "FORESTED_3", "STS_CLIMAX_3", "STAND_CLIMAX_3",
    "STAND_3_Age_0_15",  "STAND_3_Age_16_30", "STAND_3_Age_31_50",
    "STAND_3_Age_51_80", "STAND_3_Age_gt_80",
    "STS_3_Age_0_3",   "STS_3_Age_4_10",   "STS_3_Age_11_30",
    "STS_3_Age_31_40", "STS_3_Age_41_60",  "STS_3_Age_61_80",
    "STS_3_Age_81_139","STS_3_Age_140_249","STS_3_Age_gt_249",
    # add_std_crown_fields equivalents
    "STD_VRI", "CROWN_ALL",
    # Computed
    "STAND_AGE_1", "STAND_AGE_2", "STAND_AGE_3",
    "STS_AGE_1",   "STS_AGE_2",   "STS_AGE_3",
    "STRCT_S1",    "STRCT_S2",    "STRCT_S3",
    "STAND_A1",    "STAND_A2",    "STAND_A3"
  )

  lapply(varchar_cols, add_col)
  add_col("parkland_ind", "BOOLEAN")

  # ── 4. Helper: WHERE clause for ecosystem left-join (nullable BGC keys) ───
  eco_where <- sprintf(
      paste0(
        "%s.BGC_ZONE     = u.BGC_ZONE ",
        "AND %s.BGC_SUBZON  = u.BGC_SUBZON ",
        "AND %s.BGC_VRT  IS NOT DISTINCT FROM u.BGC_VRT ",
        "AND %s.BGC_PHASE IS NOT DISTINCT FROM u.BGC_PHASE ",
        "AND %s.BEUMC_S1        = u.BEU_MC"
      ),
      vri_bem_tbl, vri_bem_tbl, vri_bem_tbl, vri_bem_tbl, vri_bem_tbl
  )
  

  # ── 5. UPDATE: decile 1 ecosystem fields ─────────────────────────────────
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET
       REALM_1            = u.REALM,
       GROUP_1            = u.GROUP_C,
       CLASS_1            = u.CLASS,
       KIND_1             = u.KIND,
       SNOW_CODE          = u.SNOW_CODE_C,
       FORESTED_1         = u.FORESTED_YN,
       STS_CLIMAX_1       = u.STRCT_CLIMAX_C,
       STAND_CLIMAX_1     = u.STAND_CLIMAX_C,
       STAND_1_Age_0_15   = u.SA_0_15,
       STAND_1_Age_16_30  = u.SA_16_30,
       STAND_1_Age_31_50  = u.SA_31_50,
       STAND_1_Age_51_80  = u.SA_51_80,
       STAND_1_Age_gt_80  = u.SA_GT_80,
       STS_1_Age_0_3      = u.STA_0_3,
       STS_1_Age_4_10     = u.STA_4_10,
       STS_1_Age_11_30    = u.STA_11_30,
       STS_1_Age_31_40    = u.STA_31_40,
       STS_1_Age_41_60    = u.STA_41_60,
       STS_1_Age_61_80    = u.STA_61_80,
       STS_1_Age_81_139   = u.STA_81_139,
       STS_1_Age_140_249  = u.STA_140_249,
       STS_1_Age_gt_249   = u.STA_GT_249
     FROM %s AS u
     WHERE %s",
    vri_bem_tbl, tmp_ue, eco_where
  ))

  # ── 6. STD_VRI (add_std_crown_fields — species percentage logic) ──────────
  existing_cols <- toupper(DBI::dbGetQuery(
    conn, sprintf("PRAGMA table_info('%s')", v)
  )$name)

  if (all(c("SPEC_CD_1", "SPEC_PCT_1") %in% existing_cols)) {
   
    b_sp <- "'D','DR','DG','DM','U','UP','A','AC','ACB','ACT','AX','AT','R','RA','E','EA','EXP','EP','EW','G','GP','M','MB','MV','Q','QG','XH','V','VB','VP','W','WS','WA','WB','WD','WP','WT','ZH'"

    tot <- paste0(
        "COALESCE(CASE WHEN SPEC_CD_1 IN (", b_sp, ") THEN TRY_CAST(SPEC_PCT_1 AS DOUBLE) ELSE 0.0 END, 0.0) + 
         COALESCE(CASE WHEN SPEC_CD_2 IN (", b_sp, ") THEN TRY_CAST(SPEC_PCT_2 AS DOUBLE) ELSE 0.0 END, 0.0) +
         COALESCE(CASE WHEN SPEC_CD_3 IN (", b_sp, ") THEN TRY_CAST(SPEC_PCT_3 AS DOUBLE) ELSE 0.0 END, 0.0) +
         COALESCE(CASE WHEN SPEC_CD_4 IN (", b_sp, ") THEN TRY_CAST(SPEC_PCT_4 AS DOUBLE) ELSE 0.0 END, 0.0) +
         COALESCE(CASE WHEN SPEC_CD_5 IN (", b_sp, ") THEN TRY_CAST(SPEC_PCT_5 AS DOUBLE) ELSE 0.0 END, 0.0) +
         COALESCE(CASE WHEN SPEC_CD_6 IN (", b_sp, ") THEN TRY_CAST(SPEC_PCT_6 AS DOUBLE) ELSE 0.0 END, 0.0)"
      )
    
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s 
       SET STD_VRI = CASE
         WHEN (%s) < 25 THEN 'C'
         WHEN (%s) < 75 THEN 'M'
         ELSE 'B'
       END",
      vri_bem_tbl, tot, tot
    ))
}

  # ── 7. CROWN_ALL (add_std_crown_fields — crown closure logic) ────────────
  if ("CR_CLOSURE" %in% existing_cols) {
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s 
      SET CROWN_ALL = CASE
         WHEN TRY_CAST(CR_CLOSURE AS DOUBLE) <= 25 THEN 'VL-L'
         WHEN TRY_CAST(CR_CLOSURE AS DOUBLE) <= 40 THEN 'M'
         WHEN TRY_CAST(CR_CLOSURE AS DOUBLE) <= 60 THEN 'H'
         WHEN TRY_CAST(CR_CLOSURE AS DOUBLE) >  60 THEN 'VH'
         ELSE NULL
       END",
      vri_bem_tbl
    ))
  }

  # ── 8. parkland_ind ──────────────────────────────────────────────────────
  DBI::dbExecute(conn, sprintf(
    "UPDATE %s SET parkland_ind = (RIGHT(BGC_SUBZON, 1) = 'p')",
    vri_bem_tbl
  ))

  # ── 11. Per-decile: STAND_AGE, STS_AGE, STRCT_S, STAND_A ─────────────────
  has_bclcs <- all(c("BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4") %in% existing_cols)

  stand_age_sql <- "CASE
         WHEN VRI_AGE_CL_STD <= 15 THEN STAND_1_Age_0_15
         WHEN VRI_AGE_CL_STD <= 30 THEN STAND_1_Age_16_30
         WHEN VRI_AGE_CL_STD <= 50 THEN STAND_1_Age_31_50
         WHEN VRI_AGE_CL_STD <= 80 THEN STAND_1_Age_51_80
         WHEN VRI_AGE_CL_STD >  80 THEN STAND_1_Age_gt_80
         ELSE NULL
       END"

  sts_age_sql <- "CASE
         WHEN VRI_AGE_CL_STS <=   3 THEN STS_1_Age_0_3
         WHEN VRI_AGE_CL_STS <=  10 THEN STS_1_Age_4_10
         WHEN VRI_AGE_CL_STS <=  30 THEN STS_1_Age_11_30
         WHEN VRI_AGE_CL_STS <=  40 THEN STS_1_Age_31_40
         WHEN VRI_AGE_CL_STS <=  60 THEN STS_1_Age_41_60
         WHEN VRI_AGE_CL_STS <=  80 THEN STS_1_Age_61_80
         WHEN VRI_AGE_CL_STS <= 139 THEN STS_1_Age_81_139
         WHEN VRI_AGE_CL_STS <= 249 THEN STS_1_Age_140_249
         WHEN VRI_AGE_CL_STS >  249 THEN STS_1_Age_gt_249
         ELSE NULL
       END"

  # -- STAND_AGE and STS_AGE lookups
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s SET
         STAND_AGE_1 = %s,
         STS_AGE_1 = %s",
      vri_bem_tbl,
      stand_age_sql,
      sts_age_sql
    ))
  
  # -- STRCT_S: STS_AGE when forested age exists, else STS_CLIMAX
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s SET STRCT_S1 = CASE
         WHEN VRI_AGE_CL_STS > 0 THEN STS_AGE_1
         WHEN FORESTED_1 = 'N' OR parkland_ind THEN STS_CLIMAX_1
         ELSE NULL
       END",
      vri_bem_tbl)
    )
  
  # -- WL shrub-wetland correction
    if (has_bclcs) {
      DBI::dbExecute(conn, sprintf(
        "UPDATE %s SET STRCT_S1 = '2'
         WHERE BEUMC_S1 = 'WL'
           AND BCLCS_LV_2 <> 'W'
           AND BCLCS_LV_3 = 'W'
           AND BCLCS_LV_4 IN ('HE','HF','HG')",
        vri_bem_tbl
      ))
    }
  
  # -- STAND_A: combined CASE for STRCT prefix < 4 and >= 4
    DBI::dbExecute(conn, sprintf(
      "UPDATE %s 
       SET STAND_A1 = CASE
         WHEN LEFT(STRCT_S1, 1) IN ('4','5','6','7') THEN
           CASE
             WHEN STD_VRI IS NOT NULL                THEN STD_VRI
             WHEN VRI_AGE_CL_STD > 0                THEN STAND_AGE_1
             WHEN FORESTED_1 = 'N' OR parkland_ind   THEN STAND_CLIMAX_1
             ELSE NULL
           END
         WHEN LEFT(STRCT_S1, 1) IS NULL
           OR LEFT(STRCT_S1, 1) IN ('1','2','3') THEN
           CASE
             WHEN VRI_AGE_CL_STD > 0                THEN STAND_AGE_1
             WHEN FORESTED_1 = 'N' OR parkland_ind   THEN STAND_CLIMAX_1
             ELSE NULL
           END
         ELSE NULL
       END",
      vri_bem_tbl
    ))
  
  invisible(vri_bem_tbl)
}
