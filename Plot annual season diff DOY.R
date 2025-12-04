## plot annual tran season 16.7 and DOY in R Fig 1A##
# set working directory
setwd("~/Desktop/rachelWNV")

library(ggplot2)
library(readxl)
library(dplyr)
library(writexl)
library(openxlsx) 

#### calculate statewide average season length ###########

# Read your data (replace with your filename)
df <- read_xlsx("wnv_transmission_season_1999_2024_12.1.25.xlsx")

# Ensure numeric columns
df <- df %>%
  mutate(
    year = as.integer(year),
    FirstValid = as.integer(FirstValid),
    LastValid = as.integer(LastValid),
    DaysBetween = as.integer(DaysBetween)
  )

# If DaysBetween needs to be recalculated:
df <- df %>%
  mutate(DaysBetween = LastValid - FirstValid)

# ---- Calculate annual average season length across all districts ----
annual_avg <- df %>%
  group_by(year) %>%
  summarise(
    avg_season_length = mean(DaysBetween, na.rm = TRUE),
    sd_season_length = sd(DaysBetween, na.rm = TRUE),
    n_districts = n()
  )

# Print the result
annual_avg

write.xlsx(annual_avg,'season_average_length_statewide_12.1.25.xlsx', rowNames = FALSE)
#### calculate statewide average season length

#################### Show difference across annual average season length ####

# Read your data
df <- read_xlsx("wnv_transmission_season_1999_2024_12.1.25.xlsx")

# Ensure numeric columns
df <- df %>%
  mutate(
    year = as.integer(year),
    FirstValid = as.integer(FirstValid),
    LastValid  = as.integer(LastValid),
    DaysBetween = as.integer(DaysBetween)
  )

# Recalculate DaysBetween if needed
df <- df %>%
  mutate(DaysBetween = LastValid - FirstValid)

# ---- Calculate annual averages across all districts ----
annual_avg <- df %>%
  group_by(year) %>%
  summarise(
    avg_first_day      = mean(FirstValid, na.rm = TRUE),
    avg_last_day       = mean(LastValid, na.rm = TRUE),
    avg_season_length  = mean(DaysBetween, na.rm = TRUE),
    sd_season_length   = sd(DaysBetween, na.rm = TRUE),
    n_districts        = n()
  )

# Print to console
annual_avg

# Write to Excel
write.xlsx(annual_avg,
           "season_average_length_statewide_12.1.25.xlsx",
           rowNames = FALSE)


######## make fig 1A ##########
###-----------------------------------------------------------
### 1. Load data
###-----------------------------------------------------------

data <- read_excel("season_average_length_statewide_12.1.25.xlsx")

### Reference season (1999)
ref_start <- data$avg_first_day[data$year == 1999]  # 125
ref_end   <- data$avg_last_day[data$year == 1999]   # 272

###-----------------------------------------------------------
### 2. Compute anomalies
###-----------------------------------------------------------

data <- data %>%
  mutate(
    start.diff = avg_first_day - ref_start,
    end.diff   = avg_last_day - ref_end
  )

write_xlsx(data, "statewide_avg_season_length_diff_12.1.xlsx")

### Reload cleanly
data <- read_excel("statewide_avg_season_length_diff_12.1.xlsx")
data$year <- factor(data$year)

###-----------------------------------------------------------
### 3. Build shading coordinates
###-----------------------------------------------------------


data <- data %>%
  mutate(
    # Purple start anomaly—from actual start to baseline start
    start_xmin = pmin(avg_first_day, ref_start),
    start_xmax = pmax(avg_first_day, ref_start),
    
    # Green end anomaly—from actual end to baseline end
    end_xmin   = pmin(avg_last_day, ref_end),
    end_xmax   = pmax(avg_last_day, ref_end)
  )

###-----------------------------------------------------------
### 4. Fix row order for stable y-axis alignment
###-----------------------------------------------------------

data$ypos <- seq_len(nrow(data))

###-----------------------------------------------------------
### 5. Month labels
###-----------------------------------------------------------

month_start_days <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_names <- month.name

###-----------------------------------------------------------
### 6. PLOT
###-----------------------------------------------------------

ggplot(data) +
  
  # -----------------------------------------------------------
# GRAY REFERENCE SEASON (same baseline for every row)
# -----------------------------------------------------------
geom_segment(
  aes(x = ref_start, xend = ref_end, 
      y = ypos, yend = ypos),
  linewidth = 5, color = "gray80"
) +
  
  # -----------------------------------------------------------
# PURPLE — start anomalies
# -----------------------------------------------------------
geom_rect(
  aes(xmin = start_xmin, xmax = start_xmax,
      ymin = ypos - 0.3, ymax = ypos + 0.3),
  fill = "purple3", alpha = 0.5
) +
  
  # -----------------------------------------------------------
# GREEN — end anomalies
# -----------------------------------------------------------
geom_rect(
  aes(xmin = end_xmin, xmax = end_xmax,
      ymin = ypos - 0.3, ymax = ypos + 0.3),
  fill = "darkgreen", alpha = 0.5
) +
  
  # -----------------------------------------------------------
# BLACK — Actual average season line for each year
# -----------------------------------------------------------
geom_segment(
  aes(
    x = avg_first_day,
    xend = avg_last_day,
    y = ypos,
    yend = ypos
  ),
  linewidth = 0.7, color = "black"
) +
  
  # Y-axis (years)
  scale_y_continuous(
    breaks = data$ypos,
    labels = data$year
  ) +
  
  # X-axis (DOY → month names)
  scale_x_continuous(
    breaks = month_start_days,
    labels = month_names
  ) +
  
  labs(
    title = "Season Timing Relative to 1999 Baseline (Baseline = ref_start–ref_end)",
    subtitle = "Purple = Early/Late Start Relative to Baseline | Green = Early/Late End",
    x = "Day of Year (DOY)",
    y = "Year"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

