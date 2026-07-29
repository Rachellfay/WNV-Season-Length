# Run from the project root (paths below are relative to it)

####### WNV transmission season metrics per NY county — gridMET data
####### Calculates (per county × year, 1980–2024):
#######   1) First day of transmission season (first day where next ≥16.7°C day is ≤13 days away)
#######   2) Last day of transmission season (last day where prev ≥16.7°C day was ≤13 days ago)
#######   3) Season length (last - first)
#######   4) Linear trend in each metric per county (predictions at 1980 and 2024)

library(tidyverse)
library(data.table)

##--------- Load and reshape data

raw <- read.csv("data/county_gridmet_data/NY_county_gridMET_TMEAN.csv",
                check.names = FALSE)   # keep county names with spaces/special chars

# Pivot wide → long; rename columns to match downstream code
data <- raw %>%
  pivot_longer(-time, names_to = "county", values_to = "tmean") %>%
  mutate(
    date = as.Date(time),
    year = as.integer(format(date, "%Y")),
    doy  = as.integer(format(date, "%j"))
  ) %>%
  filter(year >= 1980) %>%
  select(county, year, doy, tmean)

setDT(data)

##--------- Identify transmission season boundaries

# Keep only days at or above 16.7°C threshold
over_16 <- data[tmean >= 16.7]

# For each county-year, find first and last valid days:
#   first: earliest day where the NEXT ≥16.7°C day is ≤13 days later
#   last:  latest  day where the PREV ≥16.7°C day was ≤13 days earlier
valid_dates <- over_16[order(county, year, doy)][
  , `:=`(
    dnext = shift(doy, type = "lead") - doy,
    dprev = doy - shift(doy)
  ),
  by = .(county, year)
][
  , .(
    FirstValid  = suppressWarnings(min(doy[dnext <= 13], na.rm = TRUE)),
    LastValid   = suppressWarnings(max(doy[dprev <= 13], na.rm = TRUE))
  ),
  by = .(county, year)
]

# Replace Inf / -Inf (years with no qualifying pair) with NA
valid_dates[is.infinite(FirstValid), FirstValid := NA_integer_]
valid_dates[is.infinite(LastValid),  LastValid  := NA_integer_]

valid_dates[, SeasonLength := LastValid - FirstValid]

# Summary
cat("Mean first day: ",  mean(valid_dates$FirstValid,  na.rm = TRUE), "\n")
cat("Mean last day:  ",  mean(valid_dates$LastValid,   na.rm = TRUE), "\n")
cat("Mean season length:", mean(valid_dates$SeasonLength, na.rm = TRUE), "\n")

write.csv(valid_dates, "data/wnv_transmission_season_gridMET_1980_2024.csv",
          row.names = FALSE)
