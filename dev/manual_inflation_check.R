# Manual check only (not CI)
library(sssUtils)
library(dplyr)
code_path <- sss_code_path(repo = "sss_utils")
data_path <- sss_data_path("data", "2026", "processed")
source(file.path(code_path, "R", "inflation_helpers.R"))
source(file.path(code_path, "R", "save_sheets_to_csv.R"))

test_inflation_df <- readr::read_csv(file.path(data_path, "state_data", "inflation.csv"))
test_inflation_df_clean <- clean_inflation_month_columns(test_inflation_df)
test_meta_df <- readr::read_csv(file.path(data_path, "state_data", "meta_state_data.csv"))
test_target <- parse_inflation_target("2026-04")

meta_row <- dplyr::filter(test_meta_df, dataset_name == "health_premium", sss_year == 2026)

result <- get_inflation_factor(
  "health_premium",
  test_inflation_df_clean,
  meta_row,
  target = test_target,
  on_missing_target = "latest"
)

print(as.numeric(result))
print(attributes(result))
