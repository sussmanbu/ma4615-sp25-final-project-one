library(tidyverse)
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
    filter(
      !str_detect(`District Name`, "charter|technical|vocational|agricultural|virtual|voc|hmcs|agr|careers|tech"),
      !`District Name` %in% c("state total", "state totals")
    ) |> 
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
