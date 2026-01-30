##### calculate season length ####

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(writexl)

setwd("~/Desktop/rachelWNV/temp data")

# Load CSV, read 'time' as character #
df <- read_csv("NY_county_gridMET_timeseries JT.csv",
               col_types = cols(.default = col_double(), time = col_character()))

# Convert from wide to long format #
df_long <- df %>%
  pivot_longer(
    cols = -time,
    names_to = "district",
    values_to = "tmeanc"
  ) %>%
  # Robust date parsing for two-digit years #
  mutate(
    date = parse_date_time(time, orders = c("m/d/y")), 
    year = year(date)
  ) %>%
  # Filter only 1979-1998 #
  filter(!is.na(year) & year >= 1979 & year <= 1998)

# Check how many rows survived #
cat("Number of rows in 1979-1998:", nrow(df_long), "\n")

# Annual average temperature by district #
annual_avg_by_district <- df_long %>%
  group_by(district, year) %>%
  summarize(annual_avg_temp = mean(tmeanc, na.rm = TRUE), .groups = "drop")

# Save results #
write_xlsx(annual_avg_by_district, "annual_avg_temperature_by_district_1979_1998.xlsx")

# Optional: check first few rows #
head(annual_avg_by_district)

### to get one value ###
setwd("~/Desktop/rachelWNV/temp data")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(writexl)

setwd("~/Desktop/rachelWNV/temp data")

# Load CSV #
df <- read_csv("NY_county_gridMET_timeseries JT.csv",
               col_types = cols(.default = col_double(), time = col_character()))

# Convert from wide to long format #
df_long <- df %>%
  pivot_longer(
    cols = -time,
    names_to = "district",
    values_to = "tmeanc"
  ) %>%
  mutate(
    date = parse_date_time(time, orders = c("m/d/y")),
    year = year(date)
  ) %>%
  filter(!is.na(year) & year >= 1979 & year <= 1998)  # filter period

# Calculate one average temperature per district for 1979-1998 #
avg_temp_by_county <- df_long %>%
  group_by(district) %>%
  summarize(avg_temp_1979_1998 = mean(tmeanc, na.rm = TRUE), .groups = "drop")

# View results #
print(avg_temp_by_county)

getwd()  # check current working directory
list.files()  # see files currently in this folder


# Save to Excel #
write_xlsx(avg_temp_by_county, "avg_temperature_by_county_1979_1998_singlepoint.xlsx")
