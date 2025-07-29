## plot state map in R ##
###https://r-charts.com/spatial/maps-ggplot2/
# set working directory
setwd("~/Desktop/rachelWNV")

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
anomaly_df <-read_excel("firstdayregression.xlsx")

View(anomaly_df)

#merge data and shapefile
anomaly_df <- anomaly_df |> rename(NAME = district)

#merge
anomaly_shape <- anomaly_df |> left_join(anomaly_df, by = "NAME")

print(anomaly_shape)

## merge anomaly10
map$data <- anomaly_shape$change_first_day.x

## plot
ggplot(map) + 
  geom_sf(aes(fill = data)) +  
  scale_fill_gradientn(colors = c("purple3", "lightpink", "white","lightgoldenrodyellow"  ),
                       values = scales::rescale(c(-20, -10, 0, 10)),
                       limits = c(-20, 10)) +  
  theme_classic()

## chanage in last day ##
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
anomaly_df <-read_excel("lastdayregression.xlsx")

View(anomaly_df)

#merge data and shapefile
anomaly_df <- anomaly_df |> rename(NAME = district)

#merge
anomaly_shape <- anomaly_df |> left_join(anomaly_df, by = "NAME")

print(anomaly_shape)

# merge anomaly10
map$data <- anomaly_shape$change_last_day.x

##plot
ggplot(map) + 
 geom_sf(aes(fill = data), color = "black") +  # Ensure fill uses the merged column
 scale_fill_gradientn(colors = c("bisque", "white", "lightgreen", "darkseagreen4", "darkgreen", "#014122"), 
                      values = scales::rescale(c(-10, 0, 10, 20, 30, 40)), 
                     limits = c(-10, 40)) + 
  theme_classic()

### map for change in season length

# set working directory
setwd("~/Desktop/rachelWNV")

# install.packages
library(ggplot2)
library(sf)
library(dplyr)
library(readxl)

# Import a geojson or shapefile ###SEASon length change
map <- read_sf("Counties.shp")
colnames(map)

## state shape
ggplot(map) +
  geom_sf() 

# read in data
anomaly_df <-read_excel("changeseasonlengthlinear.xlsx")

View(anomaly_df)

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
  scale_fill_gradientn(colors = c("#7588A1", "white","lightgray", "#8B8680" , "#53504D", "#1C1B1A"), 
                       values = scales::rescale(c(-5, 0, 15, 30, 35, 60)), 
                       limits = c(-5, 60)) + 
  theme_classic()




  

