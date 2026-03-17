library(duckplyr)
library(dplyr)
library(testthat)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a minimal duckplyr data frame with 3 rows.
# Character columns hold "<colname>_<row_id>"; integer cols hold row-based values.
make_duck_tbl <- function(n = 3) {
  char_vars_1 <- c(
    "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1",
    "SITE_S1", "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C",
    "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
    "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
    "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
    "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"
  )
  char_vars_2 <- sub("1", "2", char_vars_1)
  char_vars_3 <- sub("1", "3", char_vars_1)
  int_vars_1  <- c("TREE_C1", "SHRUB_C1")
  int_vars_2  <- sub("1", "2", int_vars_1)
  int_vars_3  <- sub("1", "3", int_vars_1)

  rows <- seq_len(n)
  df   <- data.frame(id = rows, cond = c(TRUE, FALSE, TRUE))

  for (v in char_vars_1) df[[v]] <- paste0(v, "_", rows)
  for (v in char_vars_2) df[[v]] <- paste0(v, "_", rows)
  for (v in char_vars_3) df[[v]] <- paste0(v, "_", rows)
  for (v in int_vars_1)  df[[v]] <- rows
  for (v in int_vars_2)  df[[v]] <- rows + 10L
  for (v in int_vars_3)  df[[v]] <- rows + 20L

  duckplyr::as_duckdb_tibble(df, prudence = "stingy")
}

# Convenience column-name vectors reused across tests
char_vars_1 <- c(
  "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1",
  "SITE_S1", "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C",
  "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
  "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
  "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
  "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"
)
char_vars_2 <- sub("1", "2", char_vars_1)
char_vars_3 <- sub("1", "3", char_vars_1)
int_vars_1  <- c("TREE_C1", "SHRUB_C1")
int_vars_2  <- sub("1", "2", int_vars_1)
int_vars_3  <- sub("1", "3", int_vars_1)

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("shift 2->1: conditioned rows get eco-2 values in eco-1 columns", {
  tbl    <- make_duck_tbl()
  result <- collect(shift_eco_variables(tbl, cond, list(c(1, 2))))

  conditioned   <- result[result$cond, ]
  unconditioned <- result[!result$cond, ]

  for (k in seq_along(char_vars_1)) {
    expect_equal(
      conditioned[[char_vars_1[k]]],
      paste0(char_vars_2[k], "_", conditioned$id),
      info = paste("char col", char_vars_1[k])
    )
  }
  for (k in seq_along(int_vars_1)) {
    expect_equal(conditioned[[int_vars_1[k]]], conditioned$id + 10L,
                 info = paste("int col", int_vars_1[k]))
  }

  # Unconditioned rows must be unchanged
  for (k in seq_along(char_vars_1)) {
    expect_equal(
      unconditioned[[char_vars_1[k]]],
      paste0(char_vars_1[k], "_", unconditioned$id),
      info = paste("unchanged char col", char_vars_1[k])
    )
  }
  for (k in seq_along(int_vars_1)) {
    expect_equal(unconditioned[[int_vars_1[k]]], unconditioned$id,
                 info = paste("unchanged int col", int_vars_1[k]))
  }
})

test_that("shift 3->2: conditioned rows get eco-3 values in eco-2 columns", {
  tbl    <- make_duck_tbl()
  result <- collect(shift_eco_variables(tbl, cond, list(c(2, 3))))

  conditioned <- result[result$cond, ]

  for (k in seq_along(char_vars_2)) {
    expect_equal(
      conditioned[[char_vars_2[k]]],
      paste0(char_vars_3[k], "_", conditioned$id),
      info = paste("char col", char_vars_2[k])
    )
  }
  for (k in seq_along(int_vars_2)) {
    expect_equal(conditioned[[int_vars_2[k]]], conditioned$id + 20L,
                 info = paste("int col", int_vars_2[k]))
  }
})

test_that("set eco-1 to NA: conditioned rows have NA in eco-1 columns", {
  tbl    <- make_duck_tbl()
  result <- collect(shift_eco_variables(tbl, cond, list(c(1, NA))))

  conditioned   <- result[result$cond, ]
  unconditioned <- result[!result$cond, ]

  for (v in char_vars_1) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("char col", v))
  }
  for (v in int_vars_1) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("int col", v))
  }

  for (k in seq_along(char_vars_1)) {
    expect_equal(
      unconditioned[[char_vars_1[k]]],
      paste0(char_vars_1[k], "_", unconditioned$id)
    )
  }
})

test_that("set eco-2 to NA: conditioned rows have NA in eco-2 columns", {
  tbl    <- make_duck_tbl()
  result <- collect(shift_eco_variables(tbl, cond, list(c(2, NA))))

  conditioned <- result[result$cond, ]
  for (v in char_vars_2) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("char col", v))
  }
  for (v in int_vars_2) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("int col", v))
  }
})

test_that("set eco-3 to NA: conditioned rows have NA in eco-3 columns", {
  tbl    <- make_duck_tbl()
  result <- collect(shift_eco_variables(tbl, cond, list(c(3, NA))))

  conditioned <- result[result$cond, ]
  for (v in char_vars_3) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("char col", v))
  }
  for (v in int_vars_3) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("int col", v))
  }
})

test_that("combined cascade 2->1, 3->2, 3->NA: all three sets shifted correctly", {
  tbl    <- make_duck_tbl()
  result <- collect(shift_eco_variables(tbl, cond, list(c(1, 2), c(2, 3), c(3, NA))))

  conditioned <- result[result$cond, ]

  # eco-1 <- original eco-2 values (snapshot semantics: all expressions see original columns)
  for (k in seq_along(char_vars_1)) {
    expect_equal(
      conditioned[[char_vars_1[k]]],
      paste0(char_vars_2[k], "_", conditioned$id),
      info = paste("eco-1 char", char_vars_1[k])
    )
  }
  for (k in seq_along(int_vars_1)) {
    expect_equal(conditioned[[int_vars_1[k]]], conditioned$id + 10L,
                 info = paste("eco-1 int", int_vars_1[k]))
  }

  # eco-2 <- original eco-3 values
  for (k in seq_along(char_vars_2)) {
    expect_equal(
      conditioned[[char_vars_2[k]]],
      paste0(char_vars_3[k], "_", conditioned$id),
      info = paste("eco-2 char", char_vars_2[k])
    )
  }
  for (k in seq_along(int_vars_2)) {
    expect_equal(conditioned[[int_vars_2[k]]], conditioned$id + 20L,
                 info = paste("eco-2 int", int_vars_2[k]))
  }

  # eco-3 <- NA
  for (v in char_vars_3) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("eco-3 char", v))
  }
  for (v in int_vars_3) {
    expect_true(all(is.na(conditioned[[v]])), info = paste("eco-3 int", v))
  }
})

test_that("no rows match condition: data frame returned unchanged", {
  tbl    <- make_duck_tbl() |> dplyr::mutate(cond = FALSE)
  result <- collect(shift_eco_variables(tbl, cond, list(c(1, 2), c(2, 3), c(3, NA))))

  all_vars <- c(char_vars_1, char_vars_2, char_vars_3, int_vars_1, int_vars_2, int_vars_3)
  original <- collect(make_duck_tbl() |> dplyr::mutate(cond = FALSE))

  for (v in all_vars) {
    expect_equal(result[[v]], original[[v]], info = paste("unchanged col", v))
  }
})
