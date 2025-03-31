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

all_groups <- data.frame(StudentGroup = c("Non Section 504", "Section 504", "All Students", "English Language Learners", "Non-English Language Learners", "Foster Care", "Non-Foster Care", "Female", "Gender X", "Male", "Highly Capable", "Non-Highly Capable", "Homeless", "Non-Homeless", "Low-Income", "Non-Low Income", "Migrant", "Non Migrant", "Military Parent", "Non Military Parent", "American Indian/ Alaskan Native", "Asian", "Black/ African American", "Hispanic/ Latino of any race(s)", "Native Hawaiian/ Other Pacific Islander", "Two or More Races", "White", "Students with Disabilities", "Students without Disabilities"          
))
combined_grad_data <- read_rds(here::here("dataset","combined_grad_income.rds"))
combined_grad_income_expanded <- combined_grad_data |>
  mutate(Row = row_number()) |>
  pivot_wider(names_from = StudentGroup, values_from = StudentGroup)

write_rds(combined_grad_income_expanded, file = here::here("dataset", "combined_grad_income_expanded.rds"))
