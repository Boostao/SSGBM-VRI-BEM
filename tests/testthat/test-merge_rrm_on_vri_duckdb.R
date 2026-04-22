library(testthat)
library(duckdb)
library(DBI)
library(data.table)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_conn <- function() {
  duckdb::dbConnect(duckdb::duckdb(), ":memory:")
}


write_vri_tbl <- function(conn, tbl, df) {
  DBI::dbWriteTable(conn, tbl, df, temporary = TRUE, overwrite = TRUE)
  invisible(conn)
}


normalize_rrm_fixture_names <- function(rrm_df, animal) {
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

  out <- as.data.frame(rrm_df, stringsAsFactors = FALSE)
  current_names <- names(out)
  for (src_name in names(name_map)) {
    dest_name <- unname(name_map[[src_name]])
    src_idx <- match(src_name, current_names)
    if (is.na(src_idx) || dest_name %in% current_names) {
      next
    }
    current_names[src_idx] <- dest_name
  }
  names(out) <- current_names
  out
}


base_vri_row <- function(...) {
  row <- data.frame(
    id = 1L,
    ECO_SEC = "E1",
    BGC_ZONE = "Z1",
    BGC_SUBZON = "SZ",
    BGC_VRT = NA_character_,
    BGC_PHASE = NA_character_,
    SDEC_1 = 4,
    SDEC_2 = 6,
    SDEC_3 = NA_real_,
    BEUMC_S1 = "MC1",
    BEUMC_S2 = "MC2",
    BEUMC_S3 = NA_character_,
    FORESTED_1 = "Y",
    FORESTED_2 = "Y",
    FORESTED_3 = NA_character_,
    STRCT_S1 = "4",
    STRCT_S2 = "5",
    STRCT_S3 = NA_character_,
    STAND_A1 = "C",
    STAND_A2 = "B",
    STAND_A3 = NA_character_,
    STS_CLIMAX_1 = "2",
    STS_CLIMAX_2 = "3",
    STS_CLIMAX_3 = NA_character_,
    VRI_AGE_CL_STS = 50,
    ABOVE_ELEV_THOLD = "N",
    SLOPE_MOD = "k",
    SITE_M3A = "a",
    SNOW_CODE = "1",
    CROWN_ALL_1 = "M",
    CROWN_ALL_2 = "H",
    CROWN_ALL_3 = NA_character_,
    Salmon = "Y",
    HUCK_ASP = "N",
    HUCK_ELEV_Thold = "Y",
    stringsAsFactors = FALSE
  )

  args <- list(...)
  for (nm in names(args)) {
    row[[nm]] <- args[[nm]]
  }
  row
}


moose_rrm_fixture <- function() {
  data.frame(
    ECO_SEC = c("E1", "E1", "E2"),
    BGC_ZONE = c("Z1", "Z1", "Z2"),
    BGC_SUBZON = c("SZ", "SZ", "SX"),
    BGC_VRT = c(NA_character_, NA_character_, NA_character_),
    BGC_PHASE = c(NA_character_, NA_character_, NA_character_),
    BEUMC = c("MC1", "MC2", "MC3"),
    SLOPE_MOD = c("k", "k", "j"),
    SITE_M3A = c("a", "a", "b"),
    SNOW_CODE = c("1", "1", "2"),
    ABOVE_ELEV_THOLD = c("N", "N", "N"),
    CROWN_ALL = c("M", "H", "L"),
    STRCT = c("4", "5", NA_character_),
    STAND = c("C", "B", NA_character_),
    MALAN_WFD_6C = c(2, 4, 3),
    MALAN_GFD_6C = c(4, 2, 5),
    Hectares = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
}


bear_rrm_fixture <- function() {
  data.frame(
    ECO_SEC = c("E3", "E3"),
    BGC_ZONE = c("Z3", "Z3"),
    BGC_SUBZON = c("SB", "SB"),
    BGC_VRT = c(NA_character_, NA_character_),
    BGC_PHASE = c(NA_character_, NA_character_),
    BEUMC = c("BR1", "BR1"),
    SLOPE_MOD = c("w", "w"),
    SITE_M3A = c("c", "c"),
    Salmon = c("Y", "N"),
    SNOW_CODE = c("3", "3"),
    ABOVE_ELEV_THOLD = c("Y", "Y"),
    CROWN_ALL = c("M", "M"),
    STRCT = c("4", "4"),
    STAND = c("C", "C"),
    MURAR_PEFD_6C = c(2, 6),
    Hectares = c(11, 99),
    stringsAsFactors = FALSE
  )
}


huck_rrm_fixture <- function() {
  data.frame(
    ECO_SEC = "E4",
    BGC_ZONE = "Z4",
    BGC_SUBZON = "SH",
    BGC_VRT = NA_character_,
    BGC_PHASE = NA_character_,
    HUCK_ASP = "S",
    HUCK_ELEV_Thold = "Y",
    CROWN_ALL = "M",
    STRCT = "4",
    STAND = "C",
    VACCMEM_6C = 4,
    Hectares = 7,
    stringsAsFactors = FALSE
  )
}


compare_merge_outputs <- function(vri_df, rrm_df, animal, order_col = "id") {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  expected <- merge_rrm_on_vri(
    vri_bem = data.table::copy(vri_df),
    rrm_dt = data.table::copy(data.table::as.data.table(normalize_rrm_fixture_names(rrm_df, animal = animal))),
    animal = animal,
    return_sf = FALSE
  )
  expected <- as.data.frame(expected)
  expected <- expected[order(expected[[order_col]]), , drop = FALSE]

  write_vri_tbl(conn, "VRI_INPUT", vri_df)
  out_tbl <- merge_rrm_on_vri_duckdb(
    conn = conn,
    vri_bem_tbl = "VRI_INPUT",
    rrm_dt = rrm_df,
    animal = animal,
    result_tbl = "VRI_RESULT"
  )

  actual <- DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s ORDER BY %s", out_tbl, order_col))
  actual <- actual[, names(expected), drop = FALSE]

  list(expected = expected, actual = actual, conn = conn, out_tbl = out_tbl)
}


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("merge_rrm_on_vri_duckdb matches merge_rrm_on_vri for moose fixtures", {
  vri_df <- rbind(
    base_vri_row(id = 1L),
    base_vri_row(
      id = 2L,
      ECO_SEC = "E2",
      BGC_ZONE = "Z2",
      BGC_SUBZON = "SX",
      SDEC_1 = 10,
      SDEC_2 = NA_real_,
      SDEC_3 = NA_real_,
      BEUMC_S1 = "MC3",
      BEUMC_S2 = NA_character_,
      FORESTED_1 = "N",
      FORESTED_2 = NA_character_,
      STRCT_S1 = NA_character_,
      STRCT_S2 = NA_character_,
      STAND_A1 = NA_character_,
      STAND_A2 = NA_character_,
      STS_CLIMAX_1 = "2",
      STS_CLIMAX_2 = NA_character_,
      VRI_AGE_CL_STS = 30,
      ABOVE_ELEV_THOLD = "N",
      SLOPE_MOD = "j",
      SITE_M3A = "b",
      SNOW_CODE = "2",
      CROWN_ALL_1 = "L",
      CROWN_ALL_2 = NA_character_
    )
  )

  res <- compare_merge_outputs(vri_df, moose_rrm_fixture(), animal = "moose")
  expect_equal(res$actual, res$expected, ignore_attr = TRUE)
})


test_that("merge_rrm_on_vri_duckdb matches merge_rrm_on_vri for bear salmon joins", {
  vri_df <- base_vri_row(
    id = 10L,
    ECO_SEC = "E3",
    BGC_ZONE = "Z3",
    BGC_SUBZON = "SB",
    SDEC_1 = 10,
    SDEC_2 = 0,
    SDEC_3 = 0,
    BEUMC_S1 = "BR1",
    BEUMC_S2 = NA_character_,
    BEUMC_S3 = NA_character_,
    FORESTED_2 = NA_character_,
    FORESTED_3 = NA_character_,
    STRCT_S1 = "4",
    STRCT_S2 = NA_character_,
    STRCT_S3 = NA_character_,
    STAND_A1 = "C",
    STAND_A2 = NA_character_,
    STAND_A3 = NA_character_,
    STS_CLIMAX_2 = NA_character_,
    STS_CLIMAX_3 = NA_character_,
    VRI_AGE_CL_STS = 40,
    ABOVE_ELEV_THOLD = "Y",
    SLOPE_MOD = "w",
    SITE_M3A = "c",
    SNOW_CODE = "3",
    CROWN_ALL_1 = "M",
    Salmon = "Y"
  )

  res <- compare_merge_outputs(vri_df, bear_rrm_fixture(), animal = "bear")
  expect_equal(res$actual, res$expected, ignore_attr = TRUE)
})


test_that("merge_rrm_on_vri_duckdb matches merge_rrm_on_vri for huckleberry fixtures", {
  vri_df <- base_vri_row(
    id = 20L,
    ECO_SEC = "E4",
    BGC_ZONE = "Z4",
    BGC_SUBZON = "SH",
    SDEC_1 = 10,
    SDEC_2 = 0,
    SDEC_3 = 0,
    BEUMC_S1 = "HK1",
    BEUMC_S2 = NA_character_,
    BEUMC_S3 = NA_character_,
    FORESTED_1 = "Y",
    FORESTED_2 = NA_character_,
    FORESTED_3 = NA_character_,
    STRCT_S1 = "4",
    STRCT_S2 = NA_character_,
    STRCT_S3 = NA_character_,
    STAND_A1 = "C",
    STAND_A2 = NA_character_,
    STAND_A3 = NA_character_,
    STS_CLIMAX_1 = "2",
    STS_CLIMAX_2 = NA_character_,
    STS_CLIMAX_3 = NA_character_,
    VRI_AGE_CL_STS = 15,
    ABOVE_ELEV_THOLD = "Y",
    HUCK_ASP = "S",
    HUCK_ELEV_Thold = "Y",
    CROWN_ALL_1 = "M"
  )

  res <- compare_merge_outputs(vri_df, huck_rrm_fixture(), animal = "huckleberry")
  expect_equal(res$actual, res$expected, ignore_attr = TRUE)
})


test_that("merge_rrm_on_vri_duckdb returns the table name and drops temporary hectare columns", {
  conn <- make_conn()
  on.exit(duckdb::dbDisconnect(conn, shutdown = TRUE))

  write_vri_tbl(conn, "VRI_INPUT", base_vri_row())
  out_tbl <- merge_rrm_on_vri_duckdb(
    conn = conn,
    vri_bem_tbl = "VRI_INPUT",
    rrm_dt = moose_rrm_fixture(),
    animal = "moose",
    result_tbl = "VRI_OUT"
  )

  expect_equal(out_tbl, "VRI_OUT")

  cols <- names(DBI::dbGetQuery(conn, "SELECT * FROM VRI_OUT LIMIT 0"))
  expect_false(any(c("Hectares_1", "Hectares_2", "Hectares_3") %in% cols))
  expect_true("rrm_merge_ind" %in% cols)
})