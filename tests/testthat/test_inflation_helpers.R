# tests/testthat/test_inflation_helpers.R

# library(sssUtils)
library(testthat)

code_path <- sss_code_path(repo = "sss_utils")
data_path <- sss_data_path("data","2026","processed")

test_that("get_inflation_factor skips NA months when choosing fallback", {
  inflation_df <- data.frame(
    `Series ID` = "CUUS0000SETD",
    `Annual 2024` = 407.275,
    `Jan 2026` = 440.180,
    `Feb 2026` = 444.110,
    `Mar 2026` = 449.675,
    `Apr 2026` = NA,
    check.names = FALSE
  )
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "CUUS0000SETD",
    effective_date = 2024
  )
  
  expect_warning(
    result <- get_inflation_factor(
      "transportation_fixed", inflation_df, meta_df,
      target = parse_inflation_target("2026-04"),
      on_missing_target = "latest"
    ),
    "Falling back"
  )
  expect_equal(as.numeric(result), 449.675 / 407.275, tolerance = 0.0001)
  expect_identical(attr(result, "used_target"), "Mar 2026")
  expect_true(attr(result, "fell_back"))
})

test_that("Sept column spelling is normalized to Sep", {
  inflation_df <- data.frame(
    `Series ID` = "CUUR0000SA0",
    `Annual 2024` = 320.0,
    `Sept 2025` = 322.5,
    check.names = FALSE
  )
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "CUUR0000SA0",
    effective_date = 2024
  )
  
  suppressWarnings(
    result <- get_inflation_factor(
      "transportation_fixed", inflation_df, meta_df,
      target = parse_inflation_target("2026-01"),
      on_missing_target = "latest"
    )
  )
  expect_identical(attr(result, "used_target"), "Sep 2025")
  expect_gt(result, 1)
})

test_that("parse_inflation_target accepts valid January/April and rejects others", {
  t1 <- parse_inflation_target("2026-01")
  expect_equal(t1$year, 2026L)
  expect_equal(t1$month, 1L)
  expect_equal(t1$month_abb, "Jan")
  expect_equal(t1$iso, "2026-01")
  expect_equal(t1$key, 202601L)
  
  t2 <- parse_inflation_target("2026-04")
  expect_equal(t2$month, 4L)
  expect_equal(t2$month_abb, "Apr")
  expect_equal(t2$label, "Apr 2026")
  
  expect_error(parse_inflation_target("2026-03"), "not allowed")
  expect_error(parse_inflation_target("April 2026"), "Expected 'YYYY-MM'")
  expect_error(parse_inflation_target(202604), "must be a single non-missing character")
})


test_that("get_inflation_factor uses exact target when available", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = "S1",
    `Annual 2023` = 100,
    `Jan 2026` = 118,
    `Apr 2026` = 120
  )
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "S1",
    effective_date = 2023
  )
  
  target <- parse_inflation_target("2026-04")
  f <- get_inflation_factor(
    data_point_name = "transportation_fixed",
    inflation_df = inflation_df,
    meta_df = meta_df,
    target = target,
    on_missing_target = "error"
  )
  
  expect_equal(as.numeric(f), 120 / 100)
  expect_identical(attr(f, "requested_target"), "Apr 2026")
  expect_identical(attr(f, "used_target"), "Apr 2026")
  expect_false(isTRUE(attr(f, "fell_back")))
})


test_that("get_inflation_factor falls back to nearest prior when target missing (latest mode)", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = "S1",
    `Annual 2023` = 100,
    `Jan 2026` = 118
    # Apr 2026 intentionally missing
  )
  meta_df <- data.frame(
    dataset_name = "health_oop",
    series_id = "S1",
    effective_date = 2023
  )
  
  target <- parse_inflation_target("2026-04")
  expect_warning(
    f <- get_inflation_factor(
      data_point_name = "health_oop",
      inflation_df = inflation_df,
      meta_df = meta_df,
      target = target,
      on_missing_target = "latest"
    ),
    "Falling back"
  )
  
  expect_equal(as.numeric(f), 118 / 100)
  expect_true(isTRUE(attr(f, "fell_back")))
  expect_identical(attr(f, "used_target"), "Jan 2026")
})


test_that("get_inflation_factor errors when target missing and mode=error", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = "S1",
    `Annual 2023` = 100,
    `Jan 2026` = 118
  )
  meta_df <- data.frame(
    dataset_name = "health_premium",
    series_id = "S1",
    effective_date = 2023
  )
  target <- parse_inflation_target("2026-04")
  
  expect_error(
    get_inflation_factor(
      data_point_name = "health_premium",
      inflation_df = inflation_df,
      meta_df = meta_df,
      target = target,
      on_missing_target = "error"
    ),
    class = "sss_inflation_target_unavailable"
  )
})


test_that("ask mode accepts proposed fallback with prompt_fn", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = "S1",
    `Annual 2023` = 100,
    `Jan 2026` = 110
  )
  meta_df <- data.frame(
    dataset_name = "transportation_insurance",
    series_id = "S1",
    effective_date = 2023
  )
  target <- parse_inflation_target("2026-04")
  
  answers <- c("Y")
  i <- 0L
  fake_prompt <- function(...) {
    i <<- i + 1L
    answers[[i]]
  }
  
  f <- get_inflation_factor(
    data_point_name = "transportation_insurance",
    inflation_df = inflation_df,
    meta_df = meta_df,
    target = target,
    on_missing_target = "ask",
    prompt_fn = fake_prompt
  )
  
  expect_equal(as.numeric(f), 110 / 100)
  expect_true(isTRUE(attr(f, "fell_back")))
  expect_identical(attr(f, "used_target"), "Jan 2026")
})


test_that("ask mode decline then enter explicit fallback month", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = "S1",
    `Annual 2023` = 100,
    `Jan 2026` = 110
  )
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "S1",
    effective_date = 2023
  )
  target <- parse_inflation_target("2026-04")
  
  answers <- c("N", "2026-01")
  i <- 0L
  fake_prompt <- function(...) {
    i <<- i + 1L
    answers[[i]]
  }
  
  f <- get_inflation_factor(
    data_point_name = "transportation_fixed",
    inflation_df = inflation_df,
    meta_df = meta_df,
    target = target,
    on_missing_target = "ask",
    prompt_fn = fake_prompt
  )
  
  expect_equal(as.numeric(f), 110 / 100)
  expect_identical(attr(f, "used_target"), "Jan 2026")
})


test_that("ask mode can abort via q", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = "S1",
    `Annual 2023` = 100,
    `Jan 2026` = 110
  )
  meta_df <- data.frame(
    dataset_name = "transportation_fixed",
    series_id = "S1",
    effective_date = 2023
  )
  target <- parse_inflation_target("2026-04")
  
  answers <- c("N", "q")
  i <- 0L
  fake_prompt <- function(...) {
    i <<- i + 1L
    answers[[i]]
  }
  
  expect_error(
    get_inflation_factor(
      data_point_name = "transportation_fixed",
      inflation_df = inflation_df,
      meta_df = meta_df,
      target = target,
      on_missing_target = "ask",
      prompt_fn = fake_prompt
    ),
    class = "sss_inflation_fallback_aborted"
  )
})


test_that("get_all_inflation_factors: child care base missing defaults to 1.0 only for classified missing-base errors", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = c("S_TF", "S_TI", "S_HO", "S_HP", "S_CC"),
    `Annual 2023` = c(100, 100, 100, 100, 100),
    `Jan 2026` = c(120, 110, 130, 140, 150),
    `Apr 2026` = c(121, 111, 131, 141, 151)
  )
  
  meta_df <- data.frame(
    dataset_name = c("transportation_fixed", "transportation_insurance", "health_oop", "health_premium", "child care"),
    series_id = c("S_TF", "S_TI", "S_HO", "S_HP", "S_CC"),
    effective_date = c(2023, 2023, 2023, 2023, 2023),
    stringsAsFactors = FALSE
  )
  
  # Missing Alabama row should trigger only child-care default
  child_care_inflation_df <- data.frame(
    state_name = "Washington",
    base_cpi_year = 2023,
    base_cpi_month = "January",
    stringsAsFactors = FALSE
  )
  
  target <- parse_inflation_target("2026-04")
  
  expect_warning(
    out <- get_all_inflation_factors(
      state_name = "Alabama",
      inflation_df = inflation_df,
      meta_df = meta_df,
      child_care_inflation_df = child_care_inflation_df,
      target = target,
      on_missing_target = "error"
    ),
    "Using 1.0"
  )
  
  expect_equal(as.numeric(out$transportation_fixed), 121 / 100)
  expect_equal(as.numeric(out$transportation_insurance), 111 / 100)
  expect_equal(as.numeric(out$health_oop), 131 / 100)
  expect_equal(as.numeric(out$health_premium), 141 / 100)
  
  expect_equal(as.numeric(out[["child care"]]), 1.0)
  expect_true(isTRUE(attr(out[["child care"]], "defaulted")))
  expect_identical(attr(out[["child care"]], "requested_target"), "Apr 2026")
})


test_that("get_all_inflation_factors propagates non-child-care errors (e.g., missing target month with mode=error)", {
  inflation_df <- data.frame(
    check.names = FALSE,
    `Series ID` = c("S_TF", "S_TI", "S_HO", "S_HP", "S_CC"),
    `Annual 2023` = c(100, 100, 100, 100, 100),
    `Jan 2026` = c(120, 110, 130, 140, 150)
    # Apr missing for all series
  )
  
  meta_df <- data.frame(
    dataset_name = c("transportation_fixed", "transportation_insurance", "health_oop", "health_premium", "child care"),
    series_id = c("S_TF", "S_TI", "S_HO", "S_HP", "S_CC"),
    effective_date = c(2023, 2023, 2023, 2023, 2023),
    stringsAsFactors = FALSE
  )
  
  child_care_inflation_df <- data.frame(
    state_name = "Alabama",
    base_cpi_year = 2023,
    base_cpi_month = "January",
    stringsAsFactors = FALSE
  )
  
  target <- parse_inflation_target("2026-04")
  
  expect_error(
    get_all_inflation_factors(
      state_name = "Alabama",
      inflation_df = inflation_df,
      meta_df = meta_df,
      child_care_inflation_df = child_care_inflation_df,
      target = target,
      on_missing_target = "error"
    ),
    class = "sss_inflation_target_unavailable"
  )
})

