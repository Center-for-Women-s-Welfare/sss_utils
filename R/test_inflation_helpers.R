# tests/testthat/test_inflation_helpers.R

library(sssUtils)
library(testthat)

code_path <- sss_code_path(repo = "sss_utils")
data_path <- sss_data_path("data","2026","processed")

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

# Hypothetical tests passed. Try using our data, check against calculations
# in Excel.
# Load the function
source(file.path(code_path,"R","inflation_helpers.R"))

# Create test data matching your real structure
test_inflation_df <- read_csv(file.path(data_path,"state_data","inflation.csv"))
test_inflation_df_clean <- clean_inflation_month_columns(test_inflation_df)
test_meta_df <- read_csv(file.path(data_path,"state_data",
                                   "meta_state_data.csv"))

meta_row <- test_meta_df %>%
  filter(dataset_name == "health_premium" & sss_year == 2026)  # adjust if you have a different column name for the identifier

# Run the function
result <- get_inflation_factor("health_premium", test_inflation_df_clean, meta_row)
print(result)

# All series checked out.
