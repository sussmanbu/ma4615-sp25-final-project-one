
library(tidyverse)
final_merged_no_charter <- read_csv(here::here("dataset", "final_merged_no_charter.csv"))

massachusetts_district_data <- final_merged_no_charter |>
  filter(!str_detect(tolower(`District Name`), "technical|vocational|agricultural|virtual|voc|hmcs|agr|careers|tech")) |>
  mutate(across(everything(), ~str_replace(., "nil", "0"))) |>
  arrange(`District Name`, `Year`) |>
  select(-`...1`, -`District Code_y`) |>
  rename(
    District_Code = `District Code_x`,
    Cohort_Size = `# in Cohort`,
    Percent_Graduated = `% Graduated`,
    Percent_Still_in_School = `% Still in School`,
    Percent_Non_Grad_Completers = `% Non-Grad Completers`,
    Percent_HS_Equiv = `% H.S. Equiv.`,
    Percent_Dropped_Out = `% Dropped Out`,
    Percent_Excluded = `% Permanently Excluded`,
    Percent_American_Indian = `American Indian or Alaska Native`,
    Percent_Asian = Asian,
    Percent_Black = `Black or African American`,
    Percent_Hispanic = `Hispanic or Latino`,
    Percent_Multi_Race = `Multi-Race, Not Hispanic or Latino`,
    Percent_Pacific_Islander = `Native Hawaiian or Other Pacific Islander`,
    Percent_White = White,
    Percent_Female = Female,
    Percent_Male = Male,
    SAT_Tests_Taken = `Tests Taken`,
    SAT_Reading_Writing_Mean_Score = `Reading / Writing`,
    SAT_Math_Mean_Score = Math,
    Student_Teacher_Ratio = `Student / Teacher Ratio`,
    Low_Income_Percent = `Low Income %.y`)
write_rds(massachusetts_district_data, file = here::here("dataset", "massachusetts_district_data.rds"))