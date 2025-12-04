## plot state map in R ##
###https://r-charts.com/spatial/maps-ggplot2/

####### Changes in WNV transmission season per NY county
####### this code calculates:
####### 1) the first day of transmission of the WNV per county-year based on thermal threshold
####### 2) change in season length per county-year using a per-county linear regression
####### 3) change in first day per county-year using a per-county linear regression
####### 4) change in last day per county-year using a per-county linear regression

# set working directory
setwd("~/Desktop/rachelWNV")

##--------- Set data and pacakges up 

# packages
library(tidyverse); library(readxl); library(data.table); library(dplyr); library(openxlsx) 

# data
data <- read_excel("NewYork_1999-2024_long.xlsx")

head(data)

# convert to data.table for efficient processing
setDT(data)

# filter days over 16.7
over_16 <- data[tmeanc >= 16.7]

# identify the first and last valid occurrences: 
# based on the first and last time two days above 16.7 occur within 13 days of each other
valid_dates <- over_16[order(district, year, doy)][ # first make sure data is ordered by county, year, date of year
  , `:=`(
    dnext = shift(doy, type = "lead") - doy, # calculate the number of days between the next day it was 16.7
    dprev = doy - shift(doy) # calculate number of days between the last day it was 16.7
  ),
  by = .(district, year) # do this for each district (county) and year
 ][
  , .(
    FirstValid = min(doy[dnext <= 13], na.rm = TRUE), # find the first day of the year where the lag is less than or equal to 13 days
    LastValid  = max(doy[dprev <= 13], na.rm = TRUE) # find the last day of the year where the lag is less than or equal to 13 days
  ),
  by = .(district, year) # do this for each district (county) and year
][
  , DaysBetween := LastValid - FirstValid]

# quick look at result summary
mean(valid_dates$FirstValid) # 121.4882
mean(valid_dates$LastValid) # 282.531

write.xlsx(valid_dates, 'wnv_transmission_season_1999_2024_12.1.25.xlsx', rowNames = FALSE)

## ---------- Calculating change in the season length per county using linear regression
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

hist(season_change_lm$change_in_length)
mean(season_change_lm$change_in_length) # 20.05776
mean(season_change_lm$se_change_in_length) # 10.59665

write.xlsx(season_change_lm, 'wnv_transmission_season_averages_lm_test_new.xlsx', rowNames = FALSE)  

## ---------- Calculating change in the first day per county using linear regression
season_first_lm <- data.frame()

for (d in unique(valid_dates$district)) {
  district_data <- valid_dates |>
    dplyr::filter(district == d) |>
    dplyr::arrange(year)
  
  if (nrow(district_data) < 2 || length(unique(district_data$year)) < 2) {
    # not enough data to fit a slope
    estimate_df <- data.frame(
      district = d,
      avg_first_1999 = NA_real_,
      avg_first_2024 = NA_real_,
      change_in_first = NA_real_,
      rate_change = NA_real_,
      se_1999 = NA_real_,
      se_2024 = NA_real_,
      se_change_in_first = NA_real_,
      se_slope = NA_real_
    )
    season_first_lm <- rbind(season_first_lm, estimate_df)
    next
  }
  
  model <- lm(FirstValid ~ year, data = district_data)
  
  new_data <- data.frame(year = c(1999, 2024))
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  
  estimate_df <- data.frame(
    district = d,
    avg_first_1999 = predictions$fit[1],
    avg_first_2024 = predictions$fit[2],
    change_in_first = predictions$fit[2] - predictions$fit[1],
    rate_change = coef(model)["year"],
    se_1999 = predictions$se.fit[1],
    se_2024 = predictions$se.fit[2],
    se_change_in_first = sqrt(predictions$se.fit[1]^2 + predictions$se.fit[2]^2),
    se_slope = summary(model)$coefficients["year", "Std. Error"]
  )
  
  season_first_lm <- rbind(season_first_lm, estimate_df)
}

# quick view of results
hist(season_first_lm$change_in_first)
mean(season_first_lm$change_in_first, na.rm = TRUE) # -3.782327
mean(season_first_lm$se_change_in_first, na.rm = TRUE) # 8.602073

## save
write.xlsx(season_first_lm, 'season_first_day_regression_lm.xlsx')

##--------- Calculating change in the last day per county using linear regression
season_last_lm <- data.frame()

for (d in unique(valid_dates$district)) {
  district_data <- valid_dates |>
    dplyr::filter(district == d, !is.na(LastValid)) |>
    dplyr::arrange(year)
  
  if (nrow(district_data) < 2 || length(unique(district_data$year)) < 2) {
    estimate_df <- data.frame(
      district = d,
      avg_last_1999 = NA_real_,
      avg_last_2024 = NA_real_,
      change_in_last = NA_real_,
      rate_change = NA_real_,
      se_1999 = NA_real_,
      se_2024 = NA_real_,
      se_change_in_last = NA_real_,
      se_slope = NA_real_
    )
    season_last_lm <- rbind(season_last_lm, estimate_df)
    next
  }
  
  model <- lm(LastValid ~ year, data = district_data)
  
  new_data <- data.frame(year = c(1999, 2024))
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  
  estimate_df <- data.frame(
    district = d,
    avg_last_1999 = predictions$fit[1],
    avg_last_2024 = predictions$fit[2],
    change_in_last = predictions$fit[2] - predictions$fit[1],
    rate_change = coef(model)["year"],
    se_1999 = predictions$se.fit[1],
    se_2024 = predictions$se.fit[2],
    se_change_in_last = sqrt(predictions$se.fit[1]^2 + predictions$se.fit[2]^2),
    se_slope = summary(model)$coefficients["year", "Std. Error"]
  )
  
  season_last_lm <- rbind(season_last_lm, estimate_df)
}

# quick view of results
hist(season_last_lm$change_in_last)
mean(season_last_lm$change_in_last, na.rm = TRUE) # 16.275443
mean(season_last_lm$se_change_in_last, na.rm = TRUE) # 5.552685


## save
write.xlsx(season_last_lm, 'season_last_day_regression_lm.xlsx')

#### make maps #########################
###fig 2B###
# set working directory
setwd("~/Desktop/rachelWNV")

# install.packages
library(ggplot2)
library(sf)
library(dplyr)
library(readxl)
library(openxlsx) 

# Import a geojson or shapefile
map <- read_sf("Counties.shp")
colnames(map)

## state shape
ggplot(map) +
  geom_sf() 

### anomaly in first day
# read in data
anomaly_df <-read_excel("season_first_day_regression_lm.xlsx")

View(anomaly_df)

#merge data and shapefile
anomaly_df <- anomaly_df |> rename(NAME = district)

#merge
anomaly_shape <- anomaly_df |> left_join(anomaly_df, by = "NAME")

print(anomaly_shape)

## merge anomaly10
map$data <- anomaly_shape$change_in_first.x

## plot
ggplot(map) + 
  geom_sf(aes(fill = data)) +  
  scale_fill_gradientn(colors = c("purple3", "lightpink", "white","lightgoldenrodyellow"  ),
                       values = scales::rescale(c(-20, -10, 0, 10)),
                       limits = c(-20, 10)) +  
  theme_classic()

## chanage in last day  fig 2D##################
# install.packages
library(ggplot2)
library(sf)
library(dplyr)
library(readxl)

# Import a geojson or shapefile
map <- read_sf("Counties.shp")
colnames(map)

## state shape
ggplot(map) +
  geom_sf() 


### anomaly in first day
# read in data
anomaly_df <-read_excel("season_last_day_regression_lm.xlsx")

View(anomaly_df)

#merge data and shapefile
anomaly_df <- anomaly_df |> rename(NAME = district)

#merge
anomaly_shape <- anomaly_df |> left_join(anomaly_df, by = "NAME")

print(anomaly_shape)

# merge anomaly10
map$data <- anomaly_shape$change_in_last.x

##plot
ggplot(map) + 
 geom_sf(aes(fill = data), color = "black") +  # Ensure fill uses the merged column
 scale_fill_gradientn(colors = c("bisque", "white", "lightgreen", "darkgreen", "#014122"), 
                      values = scales::rescale(c(-10, 0, 10, 20, 30)), 
                     limits = c(-10, 30)) + 
  theme_classic()

### map for change in season length map fig 1C

# set working directory
setwd("~/Desktop/rachelWNV")

library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx) 

# Import a geojson or shapefile ### season length change
map <- read_sf("Counties.shp")
colnames(map)

## state shape
ggplot(map) +
  geom_sf() 


# Read only the columns you need from Excel
anomaly_df <- read_excel("wnv_transmission_season_averages_lm_test_new.xlsx") %>%
  select(district, change_in_length) %>%   # keep only district and change_in_length
  rename(NAME = district)                  # match shapefile column

#merge data and shapefile
anomaly_df <- anomaly_df |> rename(NAME = district)

#merge
anomaly_shape <- anomaly_df |> left_join(anomaly_df, by = "NAME")

print(anomaly_shape)

# merge anomaly10
map$data <- anomaly_shape$change_in_length.x

#plot map and change theme
ggplot(map) + 
  geom_sf(aes(fill = data), color = "black") +  # Ensure fill uses the merged column
  scale_fill_gradientn(colors = c("#7588A1", "white","#CCCCCC", "#999999", "#666666", "#333333", "#000000"), 
                       values = scales::rescale(c(-5, 0, 10, 20, 30, 40, 50)), 
                       limits = c(-5, 50)) + 
  theme_classic()

