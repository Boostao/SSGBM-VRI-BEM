library(testthat)
library(duckdb)
library(DBI)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_conn <- function() {
  duckdb::dbConnect(duckdb::duckdb(), ":memory:")
}

# Write a data.frame to a temporary DuckDB table.
make_vri_tbl <- function(conn, tbl = "VRIBEM_TEST", df) {
  DBI::dbWriteTable(conn, tbl, df, temporary = TRUE, overwrite = TRUE)
  invisible(conn)
}

# Build a minimal VRI-BEM row (data.frame).  Override fields via name = value.
default_vri_row <- function(...) {
  base <- data.frame(
    BGC_ZONE       = "SBS",
    BGC_SUBZON     = "mc",
    BGC_VRT        = NA_character_,
    BGC_PHASE      = NA_character_,
    BEUMC_S1       = "AG",
    BEUMC_S2       = NA_character_,
    BEUMC_S3       = NA_character_,
    VRI_AGE_CL_STS = 50.0,
    VRI_AGE_CL_STD = 50.0,
    SPEC_CD_1      = NA_character_,  SPEC_PCT_1 = NA_real_,
    SPEC_CD_2      = NA_character_,  SPEC_PCT_2 = NA_real_,
    SPEC_CD_3      = NA_character_,  SPEC_PCT_3 = NA_real_,
    SPEC_CD_4      = NA_character_,  SPEC_PCT_4 = NA_real_,
    SPEC_CD_5      = NA_character_,  SPEC_PCT_5 = NA_real_,
    SPEC_CD_6      = NA_character_,  SPEC_PCT_6 = NA_real_,
    CR_CLOSURE     = NA_real_,
    BCLCS_LV_2     = NA_character_,
    BCLCS_LV_3     = NA_character_,
    BCLCS_LV_4     = NA_character_,
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

# Build a minimal unique ecosystem data.frame row.  Override fields via name = value.
# Column names match the CSV exactly (check.names = FALSE preserves them).
default_eco_row <- function(...) {
  base <- data.frame(
    BGC_ZONE       = "SBS",
    BGC_SUBZON     = "mc",
    BGC_VRT        = NA_character_,
    BGC_PHASE      = NA_character_,
    BEU_MC         = "AG",
    REALM          = "T",
    GROUP          = "A",
    CLASS          = "g",
    KIND           = "M",
    "Forested (Y/N)"    = "N",
    Strct_Climax        = "2b",
    Stand_Climax        = "C",
    "Stand_Age_0-15"    = "1a",
    "Stand_Age_16-30"   = "2a",
    "Stand_Age_31-50"   = "3a",
    "Stand_Age_51-80"   = "4",
    "Stand_Age_80+"     = "5",
    "Struct_Age_0-3"    = "1",
    "Struct_Age_4-10"   = "2",
    "Struct_Age_11-30"  = "3",
    "Struct_Age_31-40"  = "4",
    "Struct_Age_41-60"  = "5",
    "Struct_Age_61-80"  = "6",
    "Struct_Age_81-139" = "7a",
    "Struct_Age_140-249"= "7b",
    "Struct_Age_250+"   = "7b",
    Snow_Code           = "1",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("all expected output columns are added", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_vri_row())
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  cols <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRIBEM_TEST LIMIT 0"))

  # spot-check a few expected columns across all groups
  expected <- c(
    "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SNOW_CODE",
    "FORESTED_1", "STS_CLIMAX_1", "STAND_CLIMAX_1",
    "STAND_1_Age_0_15", "STS_1_Age_0_3",
    "REALM_2", "FORESTED_2",
    "REALM_3", "FORESTED_3",
    "STD_VRI", "CROWN_ALL", "parkland_ind",
    "STAND_AGE_1", "STS_AGE_1", "STRCT_S1", "STAND_A1",
    "STAND_AGE_2", "STS_AGE_2", "STRCT_S2", "STAND_A2",
    "STAND_AGE_3", "STS_AGE_3", "STRCT_S3", "STAND_A3"
  )
  for (col in expected) {
    expect_true(col %in% cols, info = paste("missing column:", col))
  }
})

test_that("function returns vri_bem_tbl invisibly", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_vri_row())
  result <- merge_unique_ecosystem_fields_duckdb(
    conn, "VRIBEM_TEST", default_eco_row()
  )
  expect_equal(result, "VRIBEM_TEST")
})

test_that("S1 ecosystem fields are populated from JOIN", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row(
    BEU_MC = "AG", REALM = "T", GROUP = "A", CLASS = "g", KIND = "M",
    Snow_Code = "3", "Forested (Y/N)" = "Y",
    Strct_Climax = "5", Stand_Climax = "B"
  )
  make_vri_tbl(conn, df = default_vri_row(BEUMC_S1 = "AG"))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn,
    "SELECT REALM_1, GROUP_1, CLASS_1, KIND_1, SNOW_CODE,
            FORESTED_1, STS_CLIMAX_1, STAND_CLIMAX_1
     FROM VRIBEM_TEST")

  expect_equal(r$REALM_1,      "T")
  expect_equal(r$GROUP_1,      "A")
  expect_equal(r$CLASS_1,      "g")
  expect_equal(r$KIND_1,       "M")
  expect_equal(r$SNOW_CODE,    "3")
  expect_equal(r$FORESTED_1,   "Y")
  expect_equal(r$STS_CLIMAX_1, "5")
  expect_equal(r$STAND_CLIMAX_1, "B")
})

test_that("S1 struct-age and stand-age lookups are populated from JOIN", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row(
    "Struct_Age_0-3" = "1a", "Struct_Age_4-10" = "2a",
    "Stand_Age_0-15" = "XA"
  )
  make_vri_tbl(conn, df = default_vri_row(BEUMC_S1 = "AG"))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn,
    "SELECT STS_1_Age_0_3, STS_1_Age_4_10, STAND_1_Age_0_15 FROM VRIBEM_TEST"
  )
  expect_equal(r$STS_1_Age_0_3,    "1a")
  expect_equal(r$STS_1_Age_4_10,   "2a")
  expect_equal(r$STAND_1_Age_0_15, "XA")
})

test_that("S2 ecosystem fields populated; SNOW_CODE not set by S2", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Two eco rows: S1 → "AG", S2 → "BK"
  eco <- rbind(
    default_eco_row(BEU_MC = "AG", REALM = "T", Snow_Code = "1"),
    default_eco_row(BEU_MC = "BK", REALM = "S", Snow_Code = "2")
  )
  vri <- default_vri_row(BEUMC_S1 = "AG", BEUMC_S2 = "BK")
  make_vri_tbl(conn, df = vri)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn,
    "SELECT REALM_1, REALM_2, SNOW_CODE FROM VRIBEM_TEST"
  )
  expect_equal(r$REALM_1,   "T")
  expect_equal(r$REALM_2,   "S")
  # SNOW_CODE should be from S1, not S2
  expect_equal(r$SNOW_CODE, "1")
})

test_that("S3 ecosystem fields populated", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- rbind(
    default_eco_row(BEU_MC = "AG", REALM = "T"),
    default_eco_row(BEU_MC = "BK", REALM = "S"),
    default_eco_row(BEU_MC = "WL", REALM = "H")
  )
  vri <- default_vri_row(
    BEUMC_S1 = "AG", BEUMC_S2 = "BK", BEUMC_S3 = "WL"
  )
  make_vri_tbl(conn, df = vri)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn,
    "SELECT REALM_1, REALM_2, REALM_3 FROM VRIBEM_TEST"
  )
  expect_equal(r$REALM_1, "T")
  expect_equal(r$REALM_2, "S")
  expect_equal(r$REALM_3, "H")
})

test_that("no-match rows remain NULL for ecosystem fields", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # VRI row has BEUMC_S1 = "ZZ", which is not in the eco table
  eco <- default_eco_row(BEU_MC = "AG")
  make_vri_tbl(conn, df = default_vri_row(BEUMC_S1 = "ZZ"))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT REALM_1, SNOW_CODE FROM VRIBEM_TEST")
  expect_true(is.na(r$REALM_1))
  expect_true(is.na(r$SNOW_CODE))
})

test_that("nullable BGC_VRT and BGC_PHASE matched as NULLs", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Both VRI row and eco row have NULL for BGC_VRT and BGC_PHASE
  eco  <- default_eco_row(BGC_VRT = NA, BGC_PHASE = NA, REALM = "T")
  make_vri_tbl(conn, df = default_vri_row(BGC_VRT = NA, BGC_PHASE = NA))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT REALM_1 FROM VRIBEM_TEST")
  expect_equal(r$REALM_1, "T")
})

test_that("STD_VRI: all-non-b-species → 'C', mixed → 'M', all-b-species → 'B'", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    # < 25% b_species → C
    default_vri_row(SPEC_CD_1 = "FD", SPEC_PCT_1 = 100),
    # 25–74% b_species → M  (D is in b_species)
    default_vri_row(SPEC_CD_1 = "D",  SPEC_PCT_1 = 50,
                    SPEC_CD_2 = "FD", SPEC_PCT_2 = 50),
    # >= 75% b_species → B
    default_vri_row(SPEC_CD_1 = "D",  SPEC_PCT_1 = 80)
  )
  make_vri_tbl(conn, df = df)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  r <- DBI::dbGetQuery(
    conn, "SELECT STD_VRI FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$STD_VRI, c("C", "M", "B"))
})

test_that("CROWN_ALL breakpoints are correct", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    default_vri_row(CR_CLOSURE = 10),
    default_vri_row(CR_CLOSURE = 30),
    default_vri_row(CR_CLOSURE = 50),
    default_vri_row(CR_CLOSURE = 80),
    default_vri_row(CR_CLOSURE = NA_real_)
  )
  make_vri_tbl(conn, df = df)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  r <- DBI::dbGetQuery(
    conn, "SELECT CROWN_ALL FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$CROWN_ALL, c("VL-L", "M", "H", "VH", NA_character_))
})

test_that("parkland_ind is TRUE when BGC_SUBZON ends with 'p'", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  df <- rbind(
    default_vri_row(BGC_SUBZON = "mcp"),    # parkland
    default_vri_row(BGC_SUBZON = "mc")      # not parkland
  )
  make_vri_tbl(conn, df = df)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  r <- DBI::dbGetQuery(
    conn, "SELECT parkland_ind FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$parkland_ind, c(TRUE, FALSE))
})

test_that("STAND_AGE_1 lookup uses VRI_AGE_CL_STD breakpoints", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Set VRI_AGE_CL_STD to values spanning all age bands
  ages_std <- c(10, 20, 40, 65, 100)
  # Expected STAND_AGE_1 values from default_eco_row: 1a,2a,3a,4,5
  expected  <- c("1a", "2a", "3a", "4", "5")

  df <- do.call(rbind, lapply(ages_std, function(a) {
    default_vri_row(VRI_AGE_CL_STD = a)
  }))
  make_vri_tbl(conn, df = df)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  r <- DBI::dbGetQuery(
    conn, "SELECT STAND_AGE_1 FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$STAND_AGE_1, expected)
})

test_that("STS_AGE_1 lookup uses VRI_AGE_CL_STS breakpoints", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Struct-age values from default_eco_row: 0-3→1, 4-10→2, 11-30→3, 31-40→4,
  #   41-60→5, 61-80→6, 81-139→7a, 140-249→7b, >249→7b
  ages_sts <- c(2,   7,   20,  35,   50,   70,   100,  200,  300)
  expected  <- c("1","2",  "3","4",  "5",  "6",  "7a", "7b", "7b")

  df <- do.call(rbind, lapply(ages_sts, function(a) {
    default_vri_row(VRI_AGE_CL_STS = a)
  }))
  make_vri_tbl(conn, df = df)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  r <- DBI::dbGetQuery(
    conn, "SELECT STS_AGE_1 FROM VRIBEM_TEST ORDER BY rowid"
  )
  expect_equal(r$STS_AGE_1, expected)
})

test_that("STRCT_S1 = STS_AGE_1 when VRI_AGE_CL_STS > 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # VRI_AGE_CL_STS = 35 → STS_1_Age_31_40 → "4" from default_eco_row
  make_vri_tbl(conn, df = default_vri_row(VRI_AGE_CL_STS = 35))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1, STS_AGE_1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, r$STS_AGE_1)
  expect_equal(r$STRCT_S1, "4")
})

test_that("STRCT_S1 = STS_CLIMAX_1 when FORESTED = 'N' and VRI_AGE_CL_STS <= 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row("Forested (Y/N)" = "N", Strct_Climax = "2b")
  make_vri_tbl(conn, df = default_vri_row(VRI_AGE_CL_STS = 0))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1, STS_CLIMAX_1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, "2b")
  expect_equal(r$STRCT_S1, r$STS_CLIMAX_1)
})

test_that("STRCT_S1 = STS_CLIMAX_1 when parkland and VRI_AGE_CL_STS <= 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  # Eco row uses BGC_SUBZON = "mcp" so the JOIN fires for the parkland row.
  eco <- default_eco_row(
    BGC_SUBZON = "mcp",
    "Forested (Y/N)" = "Y", Strct_Climax = "3a"
  )
  # BGC_SUBZON ending in 'p' → parkland_ind = TRUE
  make_vri_tbl(conn, df = default_vri_row(
    VRI_AGE_CL_STS = 0, BGC_SUBZON = "mcp"
  ))
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, "3a")
})

test_that("WL shrub-wetland correction overrides STRCT_S1 to '2'", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row(BEU_MC = "WL", Strct_Climax = "5",
                         "Forested (Y/N)" = "Y",
                         "Struct_Age_41-60" = "5")
  vri <- default_vri_row(
    BEUMC_S1   = "WL",
    VRI_AGE_CL_STS = 50,   # → STS_AGE via age > 0 → "5"
    BCLCS_LV_2 = "T",      # != 'W' → triggers correction
    BCLCS_LV_3 = "W",
    BCLCS_LV_4 = "HE"
  )
  make_vri_tbl(conn, df = vri)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, "2")
})

test_that("STAND_A1 from STD_VRI when STRCT >= 4 and STD_VRI is present", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row(
    "Forested (Y/N)" = "Y",
    "Struct_Age_41-60" = "5",   # VRI_AGE_CL_STS = 50 → STS_1_Age_41_60 = "5"
    "Stand_Age_51-80" = "XSTD"  # VRI_AGE_CL_STD = 50 → STAND_1_Age_31_50 = "3a"
  )
  vri <- default_vri_row(
    VRI_AGE_CL_STS = 50,
    VRI_AGE_CL_STD = 50,
    # SPEC_CD_1 = "D" (in b_species) with 80% → STD_VRI = "B"
    SPEC_CD_1 = "D", SPEC_PCT_1 = 80
  )
  make_vri_tbl(conn, df = vri)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1, STD_VRI, STAND_A1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, "5")   # STRCT >= 4
  expect_equal(r$STD_VRI,  "B")   # STD_VRI present
  expect_equal(r$STAND_A1, "B")   # STAND_A1 = STD_VRI
})

test_that("STAND_A1 from STAND_AGE when STRCT < 4 and STD_VRI is present", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row(
    "Forested (Y/N)" = "Y",
    # VRI_AGE_CL_STS = 2 → STS_1_Age_0_3 = "1"
    "Struct_Age_0-3" = "1",
    # VRI_AGE_CL_STD = 10 → STAND_1_Age_0_15 = "XSTAND"
    "Stand_Age_0-15" = "XSTAND"
  )
  vri <- default_vri_row(
    VRI_AGE_CL_STS = 2,
    VRI_AGE_CL_STD = 10,
    SPEC_CD_1 = "D", SPEC_PCT_1 = 80   # STD_VRI = "B"
  )
  make_vri_tbl(conn, df = vri)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1, STAND_A1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, "1")        # STRCT < 4
  expect_equal(r$STAND_A1, "XSTAND")   # uses STAND_AGE (not STD_VRI)
})

test_that("STAND_A1 from STAND_CLIMAX when FORESTED='N' and STRCT < 4 and STD is <= 0", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  eco <- default_eco_row(
    "Forested (Y/N)" = "N",
    Strct_Climax  = "2b",
    Stand_Climax  = "XC",
    "Struct_Age_0-3" = "1"   # VRI_AGE_CL_STS = 0 → no STS_AGE match
  )
  vri <- default_vri_row(VRI_AGE_CL_STS = 0, VRI_AGE_CL_STD = 0)
  make_vri_tbl(conn, df = vri)
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", eco)

  r <- DBI::dbGetQuery(conn, "SELECT STRCT_S1, STAND_A1 FROM VRIBEM_TEST")
  expect_equal(r$STRCT_S1, "2b")   # STS_CLIMAX because FORESTED='N'
  expect_equal(r$STAND_A1, "XC")   # STAND_CLIMAX because FORESTED='N'
})

test_that("function is idempotent (second call does not error)", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  make_vri_tbl(conn, df = default_vri_row())
  merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())
  expect_no_error(
    merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())
  )
})

test_that("invalid conn raises error", {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  duckdb::dbDisconnect(conn, shutdown = TRUE)
  expect_error(
    merge_unique_ecosystem_fields_duckdb(conn, "VRIBEM_TEST", default_eco_row())
  )
})
