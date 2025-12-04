##Filter human case data ##
# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

#set working directory
setwd("~/Desktop")

# Read the Excel file
df <- read_excel("human wnv case.xlsx")

## filter last date to be within 13 days of another
# Prepare date column and year
df <- df %>%
  rename_with(tolower) %>%
  mutate(
    date = mdy(date),     # change mdy() to your actual format if needed
    year = year(date)
  ) %>%
  arrange(year, date)

# For each year, flag dates within 13 days of another (prev or next)
df_flagged <- df %>%
  group_by(year) %>%
  mutate(
    prev_date = lag(date),
    prev_diff = as.numeric(date - prev_date),
    next_date = lead(date),
    next_diff = as.numeric(next_date - date),
    within_13_days = ( (!is.na(prev_diff) & prev_diff <= 13) | (!is.na(next_diff) & next_diff <= 13) )
  ) %>%
  ungroup()

# Keep only dates flagged within 14 days
df_within_13 <- df_flagged %>% filter(within_13_days)

# For each year, find the latest date among flagged
last_dates_per_year <- df_within_13 %>%
  group_by(year) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(-prev_date, -prev_diff, -next_date, -next_diff, -within_13_days)

# View
print(last_dates_per_year)

print(last_dates_per_year, n = nrow(last_dates_per_year))

write_xlsx(last_dates_per_year, "last_dates_per_year.xlsx")

## filter for first day

# Read the Excel file
df <- read_excel("human wnv case.xlsx")

# Prepare date and year columns
df <- df %>%
  rename_with(tolower) %>%
  mutate(
    date = mdy(date),   # change to ymd()/dmy() if needed
    year = year(date)
  ) %>%
  arrange(year, date)

# Flag dates within 13 days of another (previous or next)
df_flagged <- df %>%
  group_by(year) %>%
  mutate(
    prev_date = lag(date),
    prev_diff = as.numeric(date - prev_date),
    next_date = lead(date),
    next_diff = as.numeric(next_date - date),
    within_13_days = ( (!is.na(prev_diff) & prev_diff <= 13) | (!is.na(next_diff) & next_diff <= 13) )
  ) %>%
  ungroup()

# Filter to only flagged dates
df_within_13 <- df_flagged %>%
  filter(within_13_days)

# For each year, keep the *first* date within 13 days
first_dates_per_year <- df_within_13 %>%
  group_by(year) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  select(-prev_date, -prev_diff, -next_date, -next_diff, -within_13_days)

# Save to Excel
write_xlsx(first_dates_per_year, "first_dates_within_13_days_annually.xlsx")

