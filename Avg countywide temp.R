# average county temperature 1999 - 2024 #
# set working directory
setwd("~/Desktop/rachelWNV")
# Load necessary libraries
library(readxl)
library(dplyr)

# Read in the data
data <-read_excel("nys_avg_temp.xlsx")

# Inspect the data structure
str(data)

# Assuming the dataset has columns 'Year' and 'Temperature'
# Calculate the annual average temperature
annual_avg_temp <- data %>%
  group_by(year) %>%
  summarize(Avg_Temperature = mean(tmeanc, na.rm = TRUE))

# Print the result
print(annual_avg_temp, n = 25)

## county level###
# Load necessary libraries
library(readxl)
library(dplyr)

# Read in the data
file_path <- "/mnt/data/avgtemp1999-2024.xlsx"
data <- read_excel("avgtemp1999-2024.xlsx")

# Display column names to verify structure
colnames(data)

# Assuming the dataset has columns 'year', 'county', and 'temperature'
# Replace with actual column names if different
annual_avg_temp <- data %>%
  group_by(year, county) %>%
  summarize(Avg_Temperature = mean(tmeanc, na.rm = TRUE), .groups = "drop")

# Print the result
print(annual_avg_temp)
write.csv(annual_avg_temp, "county_avg_temp_1999_2024.csv", row.names = FALSE)
