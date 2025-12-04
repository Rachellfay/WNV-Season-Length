# average county temperature 1999 - 2024 #
# set working directory
setwd("~/Desktop/rachelWNV")
 
########## county level###########
# Load necessary libraries
library(readxl)
library(dplyr)

# Read in the data
data <- read_excel("NewYork_1999-2024_long.xlsx")

# Display column names to verify structure
colnames(data)

# Assuming the dataset has columns 'year', 'county', and 'temperature'
annual_avg_temp <- data %>%
  group_by(year, district) %>%
  summarize(Avg_Temperature = mean(tmeanc, na.rm = TRUE), .groups = "drop")

# Print the result
print(annual_avg_temp)
write.xlsx(annual_avg_temp, "county_avg_temp_1999_2024_12.1.25.xlsx", row.names = FALSE)

########### county average 1999-2024 (1 value per county) for fig 2E
# Load libraries
library(readxl)
library(dplyr)
library(openxlsx)  # optional, for Excel output

# Read your data
data <- read_excel("NewYork_1999-2024_long.xlsx")

# Check column names
colnames(data)
# Make sure you have 'district' (or 'county') and temperature column (e.g., 'tmeanc')

# Filter years 1999-2024 (if your dataset has extra years)
data_filtered <- data %>%
  filter(year >= 1999 & year <= 2024)

# Calculate average temperature per county over all years
county_avg_temp <- data_filtered %>%
  group_by(district) %>%
  summarize(
    Avg_Temperature_1999_2024 = mean(tmeanc, na.rm = TRUE),
    .groups = "drop"
  )

# Print result
print(county_avg_temp)

# Save as Excel
write.xlsx(county_avg_temp, "county_avg_temp_1999_2024_12.1.25.xlsx")
