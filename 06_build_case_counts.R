library(dplyr)
library(readxl)

# Run from the project root (paths below are relative to it)

###----- Human cases: 0-pad to all county x 1999-2024 -----

cases_raw <- read_excel("data/NYS Human WNV cases.xlsx", sheet = "annual county cases") %>%
  mutate(
    county = tolower(gsub("[^a-zA-Z]", "", sub("^NY, ", "", County))),
    year   = as.integer(Year),
    total_cases = `Reported human cases`
  ) %>%
  group_by(county, year) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE), .groups = "drop")

all_years    <- 1999:2024
all_counties <- sort(unique(cases_raw$county))

cases_by_county_year <- tidyr::expand_grid(county = all_counties, year = all_years) %>%
  left_join(cases_raw, by = c("county", "year")) %>%
  mutate(total_cases = tidyr::replace_na(total_cases, 0))

cat("Dimensions:", dim(cases_by_county_year), "\n")
cat("Counties:", length(unique(cases_by_county_year$county)),
    "| Years:", length(unique(cases_by_county_year$year)), "\n")
cat("Case total:", sum(cases_by_county_year$total_cases), "\n")

write.csv(cases_by_county_year, "data/cases_by_county_year.csv", row.names = FALSE)
cat("Saved to data/cases_by_county_year.csv\n")
