library(testthat)

source(file.path("..", "..", "R", "init_db.R"))

test_that("collect_geojson_start_indexes avoids an extra empty page", {
  expect_equal(collect_geojson_start_indexes(1L), 0L)
  expect_equal(collect_geojson_start_indexes(10000L), 0L)
  expect_equal(collect_geojson_start_indexes(10001L), c(0L, 10000L))
  expect_equal(collect_geojson_start_indexes(20000L), c(0L, 10000L))
  expect_equal(collect_geojson_start_indexes(20001L), c(0L, 10000L, 20000L))
})

test_that("collect_geojson_start_indexes handles empty counts", {
  expect_equal(collect_geojson_start_indexes(0L), 0L)
})

test_that("collect_geojson_readable detects readable and unreadable files", {
  good <- tempfile(fileext = ".geojson")
  bad <- tempfile(fileext = ".geojson")

  on.exit(unlink(c(good, bad)), add = TRUE)

  writeLines(
    c(
      '{"type":"FeatureCollection","features":[',
      '{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}}',
      ']}'
    ),
    good
  )
  writeLines("not geojson", bad)

  expect_true(collect_geojson_readable(good))
  expect_false(collect_geojson_readable(bad))
  expect_false(collect_geojson_readable(tempfile(fileext = ".geojson")))
})

test_that("collect_geojson_batches splits inputs into bounded batches", {
  batches <- collect_geojson_batches(1:7, batch_size = 3L)

  expect_length(batches, 3L)
  expect_equal(unname(batches[[1]]), 1:3)
  expect_equal(unname(batches[[2]]), 4:6)
  expect_equal(unname(batches[[3]]), 7)
})

test_that("collect_geojson_retry_tracker initializes named counters", {
  tracker <- collect_geojson_retry_tracker(c("a.geojson", "b.geojson"))

  expect_equal(unname(tracker), c(0L, 0L))
  expect_equal(names(tracker), c("a.geojson", "b.geojson"))
})