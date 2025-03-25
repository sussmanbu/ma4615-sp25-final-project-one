# Load required libraries
library(ggplot2)
library(dplyr)

# Load the dataset
state_data <- readRDS("dataset/state_grad_clean.rds")

# Ensure GraduationRate is numeric
state_data <- state_data %>%
  mutate(GraduationRate = as.numeric(as.character(GraduationRate)))

# ---- Graph 1: Average Graduation Rate by Student Group Type ----
p1 <- state_data %>%
  group_by(StudentGroupType) %>%
  summarise(AverageGradRate = mean(GraduationRate, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(StudentGroupType, -AverageGradRate), y = AverageGradRate)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  geom_text(aes(label = round(AverageGradRate * 100, 1)),
              vjust = -0.5, size = 3.5) + 
  theme_minimal() +
  labs(title = "Average Graduation Rate by Student Group Type",
       x = "Student Group Type",
       y = "Average Graduation Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# ---- Graph 2: Graduation Rate by Cohort for 'All Students' ----
# Filter for "All Students"
all_students <- state_data %>%
  filter(StudentGroup == "All Students")

# Reorder Cohort levels (so they appear logically on the X-axis)
all_students$Cohort <- factor(all_students$Cohort,
                              levels = c("Four Year", "Five Year", "Six Year", "Seven Year"))

# Create the plot
p2 <- ggplot(all_students, aes(x = Cohort, y = GraduationRate)) +
  geom_line(group = 1, color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Graduation Rate Over Time for All Students",
       x = "Cohort",
       y = "Graduation Rate (%)")

print(p2)

# ---- Save plots to folder ----
ggsave("plots/grad_by_group_type.png", p1, width = 8, height = 5)
ggsave("plots/grad_trend_all_students.png", p2, width = 8, height = 5)

