### Human case data sensitivity ####

library(dplyr)
library(readxl)
library(lubridate)
library(writexl)

df_orig <- read_excel("case_data_loop/Human WNV cases 7.30.25.xlsx") %>%
  rename_with(tolower) %>%
  mutate(
    date = mdy(`week start date`),
    
    # Extract year and DOY (day-of-year)
    year = year(date)
  ) %>%
  arrange(year, doy) %>%
  select(`number of cases`, doy, year) |>
  distinct()

Season_Length_Days <- read_excel("case_data_loop/Season length Days.xlsx") |>
  rename(
    year = Year,
    season_length = `Season length (Days)`
  )

# Vector of all 25 years in the data
all_years <- tibble(year = sort(unique(df_orig$year)))

# Initialize results table
reg_results <- data.frame(
  n_days = integer(),
  type = character(),  # "first" or "last"
  intercept = numeric(),
  slope = numeric(),
  r2 = numeric(),
  p = numeric(),
  stringsAsFactors = FALSE
)

for (n_days in 10:20) {
  
  df_flagged <- df_orig %>%
    group_by(year) %>%
    arrange(doy) %>%
    mutate(
      prev_doy = lag(doy),
      prev_diff = doy - prev_doy,
      next_doy = lead(doy),
      next_diff = next_doy - doy,
      within_n_days = ((!is.na(prev_diff) & prev_diff <= n_days) |
                         (!is.na(next_diff) & next_diff <= n_days))
    ) %>%
    ungroup()
  
  df_within_n <- df_flagged %>% filter(within_n_days)
  
  ## First date per year
  first_dates <- df_within_n %>%
    group_by(year) %>%
    filter(doy == min(doy)) %>%
    rename(first_day = doy) %>%
    ungroup() %>%
    select(year, first_day)
  
  ## Last date per year
  last_dates <- df_within_n %>%
    group_by(year) %>%
    filter(doy == max(doy)) %>%
    rename(last_day = doy) %>%
    ungroup() %>%
    select(year, last_day)
  
  # Merge with full year list and season length
  first_dates_full <- all_years %>%
    left_join(first_dates, by = "year") %>%
    left_join(Season_Length_Days, by = "year") %>%
    select(year, first_day, season_length)
  
  last_dates_full <- all_years %>%
    left_join(last_dates, by = "year") %>%
    left_join(Season_Length_Days, by = "year") %>%
    select(year, last_day, season_length)
  
  ## --- FIRST DATE REGRESSION ---
  if (nrow(first_dates_full) == 25 && all(!is.na(first_dates_full$first_day)) && all(!is.na(first_dates_full$season_length))) {
    fit_first <- lm(first_day ~ season_length, data = first_dates_full)
    summary_first <- summary(fit_first)
    
    reg_results <- reg_results %>%
      add_row(
        n_days = n_days,
        type = "first",
        intercept = coef(fit_first)[1],
        slope = coef(fit_first)[2],
        r2 = summary_first$r.squared,
        p = summary_first$coefficients[2, 4]
      )
    
    cat("FIRST date regression — n_days:", n_days, "\n",
        "  Intercept:", round(coef(fit_first)[1], 3),
        "  Slope:", round(coef(fit_first)[2], 3),
        "  R²:", round(summary_first$r.squared, 3),
        "  P-value:", signif(summary_first$coefficients[2, 4], 3), "\n\n")
  }
  
  ## --- LAST DATE REGRESSION ---
  if (nrow(last_dates_full) == 25 && all(!is.na(last_dates_full$last_day)) && all(!is.na(last_dates_full$season_length))) {
    fit_last <- lm(last_day ~ season_length, data = last_dates_full)
    summary_last <- summary(fit_last)
    
    reg_results <- reg_results %>%
      add_row(
        n_days = n_days,
        type = "last",
        intercept = coef(fit_last)[1],
        slope = coef(fit_last)[2],
        r2 = summary_last$r.squared,
        p = summary_last$coefficients[2, 4]
      )
    
    cat("LAST date regression — n_days:", n_days, "\n",
        "  Intercept:", round(coef(fit_last)[1], 3),
        "  Slope:", round(coef(fit_last)[2], 3),
        "  R²:", round(summary_last$r.squared, 3),
        "  P-value:", signif(summary_last$coefficients[2, 4], 3), "\n\n")
  }
  
  ## Save output tables
  write_xlsx(first_dates_full, paste0("case_data_loop/final_df/first_dates_within_", n_days, "_doy.xlsx"))
  write_xlsx(last_dates_full, paste0("case_data_loop/final_df/last_dates_within_", n_days, "_doy.xlsx"))
}

# Save all regression summaries
write.csv(reg_results, "case_data_loop/final_df/first_last_regression_results.csv", row.names = FALSE)

reg_results_first <- reg_results |> filter(type == 'first')
plot(reg_results_first$n_days, reg_results_first$p)

reg_results_last <- reg_results |> filter(type == 'last')
plot(reg_results_last$n_days, reg_results_last$p)