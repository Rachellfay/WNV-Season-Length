## fit quadratic function to historical temp vs season length ##

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)  # for geom_text_repel

#----------------------------------------------------------
# 1. Read transmission season change data
#----------------------------------------------------------
season_data <- read_excel("wnv_transmission_season_averages_lm_test_new.xlsx")

# Keep only needed columns
season_data_filtered <- season_data %>%
  select(district, change_in_length)

#----------------------------------------------------------
# 2. Read temperature data
#----------------------------------------------------------
temp_data <- read_excel("county_avg_temp_1999_2024_12.1.25.xlsx")

#----------------------------------------------------------
# 3. Merge datasets on district
#----------------------------------------------------------
merged_data <- inner_join(season_data_filtered, temp_data, by = "district")

# Rename temperature column for convenience
merged_data <- merged_data %>%
  rename(mean_temperature = Avg_Temperature_1999_2024) %>%
  mutate(temp_squared = mean_temperature^2)

#----------------------------------------------------------
# 4. Fit quadratic regression model
#----------------------------------------------------------
model <- lm(change_in_length ~ temp_squared + mean_temperature,
            data = merged_data)

# Show model summary
summary(model)

#----------------------------------------------------------
# 5. Base R scatter plot
#----------------------------------------------------------
plot(merged_data$mean_temperature, merged_data$change_in_length,
     main = "Quadratic Fit: Change in Season Length vs Temperature",
     xlab = "Mean Temperature (1999-2024)",
     ylab = "Change in Season Length",
     pch = 19)

#----------------------------------------------------------
# 6. ggplot2 scatter plot with quadratic fit and labels
#----------------------------------------------------------

library(ggplot2)
library(ggrepel)

ggplot(merged_data, aes(x = mean_temperature, y = change_in_length)) +
  
  geom_point(color = "black") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  
  # Repelled labels with stronger force
  geom_text_repel(
    aes(label = district),
    size = 2.5,               # smaller text helps with crowding
    max.overlaps = Inf,
    box.padding = 1,          # more space around labels
    point.padding = 0.5,
    segment.color = "grey50",
    segment.size = 0.3,
    force = 2,                # increase repelling force
    nudge_y = 0,              # optional: nudge labels up/down
    nudge_x = 0
  ) +
  
  labs(
    title = "Quadratic Fit: Change in Season Length vs Temperature",
    x = "Mean Temperature (1999-2024)",
    y = "Change in Season Length"
  ) +
  
  scale_x_continuous(
    breaks = seq(floor(min(merged_data$mean_temperature)),
                 ceiling(max(merged_data$mean_temperature)), by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(merged_data$change_in_length)),
                 ceiling(max(merged_data$change_in_length)), by = 10)
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 0.8)
  )
