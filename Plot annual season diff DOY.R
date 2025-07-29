## plot annual tran season 16.7 and DOY in R ##
# set working directory
setwd("~/Desktop/rachelWNV")

# install.packages
library(ggplot2)
library(readxl)

# read in data
data <-read_excel("NYSavgseason rf.xlsx")

View(data)

# Convert year to a factor if it's not already
data$year <- as.factor(data$Year)

# Check for missing values in the relevant columns
sum(is.na(data$year))   # Check for missing values in 'year'
sum(is.na(data$start_date))   # Check for missing values in 'start_date'
sum(is.na(data$last_date))     # Check for missing values in 'last_date'

# Define the breaks for each month's starting day of the year
month_start_days <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_names <- month.name  # Use built-in month names

# Plot the data using ggplot2
ggplot(data) +
  geom_segment(aes(x = start_date, xend = last_date, y = year, yend = year), 
               linewidth = 2, color = "gray") +   # Use linewidth instead of size
  labs(x = "Day of Year (DOY)", y = "Year", title = "Transmission Season Start and End DOY") +
  scale_x_continuous(
    breaks = month_start_days,   # Breaks at the start of each month
    labels = month_names        # Label with month names
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        axis.text.y = element_text(size = 10))  # Adjust y-axis text size

## Load necessary libraries
library(ggplot2)
library(readxl)

# Read in the new data
data <- read_excel("NYSavgseason rf.xlsx")

# Convert year to a factor for better categorical plotting
data$year <- as.factor(data$Year)

# Define breaks for months in DOY format
month_start_days <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_names <- month.name  

# Plot the data
ggplot(data) +
  # Transmission season (Start Date to Last Date)
  geom_segment(aes(x = start_date, xend = last_date, y = year, yend = year), 
               linewidth = 2, color = "gray") +
  
  # Shade Start Difference (Start Difference to Start Date)
  geom_rect(aes(xmin = `start difference`, xmax = start_date, ymin = as.numeric(year) - 0.4, ymax = as.numeric(year) + 0.4),
            fill = "purple3", alpha = 0.5) +  
  
  # Shade End Difference (Last Date to End Difference)
  geom_rect(aes(xmin = last_date, xmax = `end difference`, ymin = as.numeric(year) - 0.4, ymax = as.numeric(year) + 0.4),
            fill = "darkgreen", alpha = 0.5) +  
  
  # Labels and formatting
  labs(x = "Day of Year (DOY)", y = "Year", title = "Transmission Season with Start & End Difference Shading") +
  scale_x_continuous(
    breaks = month_start_days,   
    labels = month_names        
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))  

