####### Changes in WNV transmission season per NY county

# set working directory
setwd("~/Desktop/rachelWNV")

# read in data & packages
library(tidyverse); library(readxl); library(data.table); library(dplyr)

data <- read_excel("NewYork1999-2024.xlsx")

# Convert to data.table for efficient processing
setDT(data)

# Step 2: Filter days over 16.7
over_16 <- data[tmeanc >= 16.7]

# Step 3 & 4: Identify the first and last valid occurrences
valid_dates <- over_16[, .(
  FirstValid = {
    valid_days = doy[which(sapply(doy, function(x) sum(between(doy, x, x + 13)) >= 2))]
    if (length(valid_days) > 0) min(valid_days) else NA_integer_
  },
  LastDay = max(doy)
), by = .(district, year)]

# Step 5: Calculate the number of days between the first and last occurrences
valid_dates[, DaysBetween := LastDay - FirstValid]

# View results
# print(valid_dates)
# plot(valid_dates$year, valid_dates$DaysBetween)

write.csv(valid_dates, 'wnv_transmission_season_1999_2024.csv', row.names = FALSE)

# Step 6: Get average for climate comparison

### Option 1: Compare historical averages to contemporary
### Calculate averages per district
avg_1999_2003 <- valid_dates |>
  filter(year < 2004) |>
  group_by(district) |>
  mutate(avg_length_1999_2003 = mean(DaysBetween)) |>
  select(district, avg_length_1999_2003) |>
  distinct()

avg_2004_2024 <- valid_dates |>
  filter(year > 2003) |>
  group_by(district) |>
  mutate(avg_length_2004_2024 = mean(DaysBetween)) |>
  select(district, avg_length_2004_2024) |>
  distinct()

avg_1999_2008 <- valid_dates |>
  filter(year < 2009) |>
  group_by(district) |>
  mutate(avg_length_1999_2008 = mean(DaysBetween)) |>
  select(district, avg_length_1999_2008) |>
  distinct()

avg_2009_2024 <- valid_dates |>
  filter(year > 2008) |>
  group_by(district) |>
  mutate(avg_length_2009_2024 = mean(DaysBetween)) |>
  select(district, avg_length_2009_2024) |>
  distinct()

#### combine
final_avgs <- avg_1999_2003 |>
  left_join(avg_2004_2024, by = "district") |>
  left_join(avg_1999_2008, by = "district") |>
  left_join(avg_2009_2024, by = "district")

write.csv(final_avgs, 'wnv_transmission_season_averages.csv', row.names = FALSE)  

### Option 2: Get avg from district level linear regression from 1999 & 2024

# set up empty list that will be populated in for loop
season_change_lm <- c()

for(d in unique(valid_dates$district)){
  district_data <- valid_dates |>
    filter(district == d)
  
  model <- lm(DaysBetween ~ year, data = district_data)
  
  new_data <- data.frame(year = c(1999, 2024))
  
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  
  estimate_df = data.frame(
    district = d,  # Ensure 'd' is defined appropriately in your context
    avg_length_1999 = predictions$fit[1],
    avg_length_2024 = predictions$fit[2],
    change_in_length = predictions$fit[2] - predictions$fit[1],
    rate_change = coef(model)["year"],
    se_1999 = predictions$se.fit[1],
    se_2024 = predictions$se.fit[2],
    se_change_in_length = sqrt(predictions$se.fit[1]^2 + predictions$se.fit[2]^2),
    se_slope = summary(model)$coefficients["year", "Std. Error"]
  )
  
  season_change_lm <- rbind(season_change_lm, estimate_df)
  
}

write.csv(season_change_lm, 'wnv_transmission_season_averages_lm_test.csv', row.names = FALSE)  
