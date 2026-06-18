# tests/testthat/test_inflation_helpers.R

library(testthat)

test_that("get_inflation_factor handles missing data correctly", {
  inflation_df <- data.frame(
    `Series ID` = "CUUR0000SA0",
    `Annual 2024` = 320.0,
    `Jan 2025` = 321.2,
    `Sep 2025` = 322.5,
    `Oct 2025` = NA,  # Missing data
    `Nov 2025` = 323.1,
    check.names = FALSE
  )
  
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "CUUR0000SA0",
    effective_date = 2024
  )
  
  result <- get_inflation_factor("transportation_fixed", inflation_df, meta_df)
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result > 1)
})

test_that("month_order includes Sep (abbreviated)", {
  inflation_df <- data.frame(
    `Series ID` = "CUUR0000SA0",
    `Annual 2024` = 320.0,
    `Sep 2025` = 322.5,
    check.names = FALSE
  )
  
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "CUUR0000SA0",
    effective_date = 2024
  )
  
  result <- get_inflation_factor("transportation_fixed", inflation_df, meta_df)
  expect_true(is.numeric(result))
  expect_gt(result, 1)
})

test_that("get_inflation_factor skips missing months and finds most recent", {
  inflation_df <- data.frame(
    `Series ID` = "CUUR0000SA0",
    `Annual 2024` = 320.0,
    `Aug 2025` = 322.1,
    `Sep 2025` = 322.5,
    `Oct 2025` = NA,
    `Nov 2025` = NA,
    check.names = FALSE
  )
  
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "CUUR0000SA0",
    effective_date = 2024
  )
  
  result <- get_inflation_factor("transportation_fixed", inflation_df, meta_df)
  expect_true(is.numeric(result))
  expect_equal(result, 322.5 / 320.0, tolerance = 0.0001)
})