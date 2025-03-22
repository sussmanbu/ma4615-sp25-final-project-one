# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

library(tidyverse)

loan_data <- read_csv(here::here("dataset", "loan_refusal.csv"))

## CLEAN the data
loan_data_clean <- loan_data |>
  pivot_longer(2:5, names_to = "group", values_to = "refusal_rate")

write_rds(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.rds"))

#State level cleaned data
state_graduation_data <- read_csv(here::here("dataset" , "Report_Card_Graduation_2023-24.csv"))

state_grad_data_clean <- state_graduation_data |>
  filter(OrganizationLevel == "State") |>
  select(-SchoolCode, -SchoolOrganizationId, -DistrictOrganizationId, -ESDOrganizationId, -DistrictCode, -DAT)

write_rds(state_grad_data_clean, file = here::here("dataset", "state_grad_clean.rds"))

school_graduation_data <- read_csv(here::here("dataset" , "Report_Card_Graduation_2023-24.csv"))

school_grad_data_clean <- school_graduation_data |>
  filter(OrganizationLevel == "School") |> 
  mutate_all(~ replace(., is.na(.) | . == "NULL", 0)) |>
  filter(GraduationRate != 0)

write_rds(school_grad_data_clean, file = here::here("dataset", "school_grad_clean.rds"))

county_graduation_data <- read_csv(here::here("dataset" , "Report_Card_Graduation_2023-24.csv"))

county_grad_data_clean <- county_graduation_data |>
  filter(OrganizationLevel == "County") |>
  select( -ESDName, -ESDOrganizationId, -DistrictCode, -DistrictName, -DistrictOrganizationId, -SchoolCode, -SchoolName, -SchoolOrganizationId) |>
  filter(GraduationRate != "NULL")

write_rds(county_grad_data_clean, file = here::here("dataset", "county_grad_clean.rds"))
