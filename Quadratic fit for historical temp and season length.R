## fit quadratic function to historical temp vs season length ##

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

#set working directory
setwd("~/Desktop")

# Read the Excel sheets
season_data <- read_excel("change in season length.xlsx")
temp_data <- read_excel("county_temp_annualavg.xlsx")

# Merge datasets on the common 'county' column
merged_data <- inner_join(season_data, temp_data, by = "county")

# Check column names: assume 'season_length' and 'temperature' are the relevant ones
head(merged_data)

# Fit quadratic model: season_length = a*(temp^2) + b*temp + c
merged_data <- merged_data %>%
  mutate(temp_squared = mean_temperature^2)

quadratic_model <- lm(season_length ~ temp_squared + mean_temperature, data = merged_data)

# View summary of the model
summary(quadratic_model)

# visualize the fit
plot(merged_data$mean_temperature, merged_data$season_length,
     main = "Quadratic Fit: Season Length vs Temperature",
     xlab = "Temperature", ylab = "Season Length", pch = 19)

# Scatter plot
ggplot(merged_data, aes(x = mean_temperature, y = season_length)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  geom_text_repel(aes(label = county), size = 3, max.overlaps = Inf) +
  labs(title = "Quadratic Fit: Season Length vs Temperature",
       x = "Mean Temperature",
       y = "Season Length") +
  theme_minimal()

