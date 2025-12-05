## fit quadratic function to historical temp vs season length  12.1##

################################
#----------------------------------------------------------
# Libraries
#----------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(lubridate)

#----------------------------------------------------------
# Set working directory
#----------------------------------------------------------
setwd("~/Desktop/rachelWNV")

#----------------------------------------------------------
# 1. Read data
#----------------------------------------------------------
season_data <- read_excel("wnv_transmission_season_averages_lm_test_new.xlsx")
temp_data   <- read_excel("avg_temperature_by_county_1979_1998_singlepoint.xlsx")

#----------------------------------------------------------
# 2. Select required columns
#----------------------------------------------------------
season_df <- season_data %>%
  select(district, change_in_length)

temp_df <- temp_data %>%
  select(district, avg_temp_1979_1998)

#----------------------------------------------------------
# 3. Standardize district names for merging
#----------------------------------------------------------
season_df <- season_df %>%
  mutate(district = tolower(trimws(district)))

temp_df <- temp_df %>%
  mutate(district = tolower(trimws(district)))

#----------------------------------------------------------
# 4. Merge datasets
#----------------------------------------------------------
merged_data <- inner_join(season_df, temp_df, by = "district") %>%
  mutate(district_label = str_to_title(district),
         mean_temperature = avg_temp_1979_1998,  # rename for consistency
         temp_squared = mean_temperature^2)      # quadratic term

#----------------------------------------------------------
# 5. Fit quadratic model
#----------------------------------------------------------
model <- lm(change_in_length ~ mean_temperature + temp_squared, data = merged_data)
summary(model)

#----------------------------------------------------------
# 6. Plot with quadratic curve + repelled labels
#----------------------------------------------------------
ggplot(merged_data, aes(x = mean_temperature, y = change_in_length)) +
  
  geom_point(size = 2, color = "black") +
  
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = FALSE,
              color = "blue",
              linewidth = 1) +
  
  geom_text_repel(
    aes(label = district_label),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.8,
    point.padding = 0.4,
    segment.color = "grey50"
  ) +
  
  labs(
    title = "Quadratic Relationship: Temperature vs Change in WNV Season Length",
    x = "Mean Annual Temperature (1979–1998)",
    y = "Change in Season Length (1999–2024)"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black")
  )
