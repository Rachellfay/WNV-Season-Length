# average transmission season temperature NYS #
# install
install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)
# set working directory
setwd("~/Desktop/rachelWNV")

# Read the Excel file
data <- read_excel("NewYork1999-2024 daily mean.xlsx")

# Filter districts in the range 121 to 304 and compute average per year
state_avg_temp <- data %>%
  filter(doy >= 121 & doy <= 304) %>%        # Filter for day of the year
  group_by(year) %>%                         # Group by year
  summarise(state_avg_tmeanc = mean(tmeanc, na.rm = TRUE)) %>%  # Calculate average temperature per year
  arrange(year)                              # Arrange by year

# View the result
print(state_avg_temp)
View(state_avg_temp)

write.csv(state_avg_temp, "state_avg_temp.csv", row.names = FALSE)
