# Run from the project root (paths below are relative to it)

####### WNV transmission season metrics — temperature threshold sensitivity analysis
####### Two sensitivity frameworks:
#######
####### 1) State-wide (confidence bounds around 16.7°C Tmin):
#######      state_low  = 14.9°C applied uniformly to all counties
#######      state_high = 17.8°C applied uniformly to all counties
#######
####### 2) County population-specific (subtropical vs. non-subtropical populations):
#######      population_sp_low  = 15.6°C subtropical / 15.0°C non-subtropical
#######      population_sp_mean = 17.8°C subtropical / 16.7°C non-subtropical
#######      population_sp_high = 20.2°C subtropical / 17.9°C non-subtropical
#######
####### Köppen subtropical classification from data/NY_county_koppen_zones.csv
####### Output has one row per county × year × threshold_id.

library(tidyverse)
library(data.table)

##--------- Load and reshape temperature data (done once)

raw <- read.csv("data/county_gridmet_data/NY_county_gridMET_TMEAN.csv",
                check.names = FALSE)

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

##--------- Load Köppen subtropical classification

koppen <- fread("data/NY_county_koppen_zones.csv")[, .(county, subtropical)]

##--------- Define threshold scenarios
##
## State-wide: one numeric threshold applied to all counties
## Population-specific: subtropical counties get a different threshold than non-subtropical

state_scenarios <- list(
  state_low  = 14.9,
  state_high = 17.8
)

# Each entry: c(subtropical threshold, non-subtropical threshold)
pop_scenarios <- list(
  population_sp_low  = c(15.6, 15.0),
  population_sp_mean = c(17.8, 16.7),
  population_sp_high = c(20.2, 17.9)
)

##--------- Helper: compute season metrics given data with a per-row `threshold` column

compute_season <- function(dat, scenario_name) {
  cat("Processing:", scenario_name, "\n")

  over_thresh <- dat[tmean >= threshold]

  valid_dates <- over_thresh[order(county, year, doy)][
    , `:=`(
      dnext = shift(doy, type = "lead") - doy,
      dprev = doy - shift(doy)
    ),
    by = .(county, year)
  ][
    , .(
      FirstValid = suppressWarnings(min(doy[dnext <= 13], na.rm = TRUE)),
      LastValid  = suppressWarnings(max(doy[dprev <= 13], na.rm = TRUE))
    ),
    by = .(county, year)
  ]

  valid_dates[is.infinite(FirstValid), FirstValid := NA_integer_]
  valid_dates[is.infinite(LastValid),  LastValid  := NA_integer_]
  valid_dates[, SeasonLength := LastValid - FirstValid]
  valid_dates[, threshold_id := scenario_name]

  valid_dates
}

##--------- Run state-wide scenarios (uniform threshold across all counties)

all_results <- list()

for (nm in names(state_scenarios)) {
  thresh <- state_scenarios[[nm]]
  dat    <- copy(data)[, threshold := thresh]
  all_results[[nm]] <- compute_season(dat, nm)
}

##--------- Run population-specific scenarios (threshold varies by subtropical classification)

for (nm in names(pop_scenarios)) {
  thresholds <- pop_scenarios[[nm]]  # [1] subtropical, [2] non-subtropical

  county_thresh <- koppen[, .(
    county,
    threshold = ifelse(subtropical, thresholds[1], thresholds[2])
  )]

  dat <- merge(data, county_thresh, by = "county")
  all_results[[nm]] <- compute_season(dat, nm)
}

##--------- Combine and write

combined <- rbindlist(all_results)
setcolorder(combined, c("threshold_id", "county", "year", "FirstValid", "LastValid", "SeasonLength"))

write.csv(combined,
          "data/wnv_transmission_season_gridMET_sensitivity.csv",
          row.names = FALSE)

cat("Done. Rows:", nrow(combined), "\n")
