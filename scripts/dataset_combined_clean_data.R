library(tidyverse)

massachusetts_district_data <- read_rds(here::here("dataset", "massachusetts_district_data.rds"))
final_data <- read_rds(here::here("dataset", "PerPupilExpenditures_combined_filtered.rds"))

massachusetts_district_data <- massachusetts_district_data |>
  mutate(
    `District Name` = tolower(`District Name`),
    Year = as.integer(Year)
  )

final_data <- final_data |>
  mutate(
    `District Name` = tolower(`District Name`),
    Year = as.integer(Year)
  )

final_data <- semi_join(final_data, massachusetts_district_data, by = c("District Name", "Year"))

combined_data <- final_data |>
  left_join(massachusetts_district_data, by = c("District Name", "Year")) |>
  rename(`District Code` = `District_Code.x`) |>
  select(-`District_Code.y`)

combined_data <- combined_data |>
  mutate(across(
    .cols = -c(`District Name`, `District Code`),
    .fns = ~ suppressWarnings(as.numeric(gsub("[$,]", "", .)))
  )) |>
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

saveRDS(combined_data, file = here::here("dataset", "Combined_District_Data.rds"))
