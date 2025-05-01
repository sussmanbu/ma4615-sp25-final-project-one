
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


folder <- "dataset"

rds_files <- list.files(path = folder, pattern = "^PerPupilExpenditures_\\d{4}\\.rds$", full.names = TRUE)

combined_data <- map_dfr(rds_files, function(file) {
  year <- str_extract(file, "\\d{4}")
  df <- readRDS(file)
  
  colnames(df) <- as.character(unlist(df[1, ]))
  df <- df[-1, ]
  df <- df[, !is.na(colnames(df)) & colnames(df) != ""]
  
  df |> 
    mutate(`District Name` = tolower(`District Name`)) |> 
    mutate(year = as.integer(year)) |> 
    filter(!str_detect(`District Name`, "charter|technical|vocational|agricultural|virtual|voc|hmcs|agr|careers|tech")) |> 
    select(year, everything()) |> 
    arrange(`District Name`, year) 
})

final_data <- combined_data |>
  rename(
    Year = `year`,
    District_Code = `District Code`,
    In_District_Expenditures = `In-District Expenditures`,
    Total_In_District_FTEs = `Total In-district FTEs`,
    In_District_Expenditures_Per_Pupil = `In-District Expenditures per Pupil`,
    Total_Expenditures = `Total Expenditures`,
    Total_Pupil_FTEs = `Total Pupil FTEs`,
    Total_Expenditures_Per_Pupil = `Total Expenditures per Pupil`
  )

final_data <- final_data |> 
  arrange(`District Name`, Year)

write_rds(final_data, file = here::here("dataset", "PerPupilExpenditures_combined_filtered.rds"))

folder <- "dataset"

massachusetts_district_data <- read_rds(file.path(folder, "massachusetts_district_data.rds"))
final_data <- read_rds(file.path(folder, "PerPupilExpenditures_combined_filtered.rds"))

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

combined_data <- final_data |>
  left_join(massachusetts_district_data, by = c("District Name", "Year"))

saveRDS(combined_data, file = file.path(folder, "Combined_District_Data.rds"))
write_csv(combined_data, file = file.path(folder, "Combined_District_Data.csv"))
