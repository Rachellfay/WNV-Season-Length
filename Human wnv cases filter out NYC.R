###human WNV cases filter out NYC##
# set working directory
setwd("~/Desktop/rachelWNV")

# install.packages
library(ggplot2)
library(readxl)
library(dplyr)

# Read the Excel file
file_path <- "WNV human cases no NYC.xlsx"
df <- read_excel(file_path, sheet = "Sheet1")

# Calculate annual WNV cases
annual_wnv_cases <- df %>%
  group_by(Year) %>%
  summarise(Reported_Human_Cases = sum(`Reported human cases`, na.rm = TRUE))

# Print results
print(annual_wnv_cases)
print(n=27, annual_wnv_cases)

write.csv(annual_wnv_cases, "annual human wnv no nyc.csv", row.names = FALSE)
