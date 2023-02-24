### A visualization of NOAA Winter Storm Impact prediction data for the storm 
#this weekend. Using tools we've learned in space-time analytics
#Miles Moore
#2022-02-24

rm(list=ls())

library(ggplot2)
library(sf)
library(tidyverse)
library(gridExtra)
################################################################################
###### Read in data from NOAA. There are three discrete time ranges for WSI ####

#define a function to read data from link, find .shp path & read in data
read_in_NOAA_data<- function(url){
temp <- tempfile()
download.file(url,temp)
temp<-unzip(temp)
return(sf::st_read(grep('.shp', temp, value = T)))
unlink(temp)
}

#define list of URLS
noaaURLS <- c("https://origin.wpc.ncep.noaa.gov/wwd/wssi/gis/shp/WSSI_OVERALL_Day_1_latest.zip",
              "https://origin.wpc.ncep.noaa.gov/wwd/wssi/gis/shp/WSSI_OVERALL_Day_2_latest.zip",
              "https://origin.wpc.ncep.noaa.gov/wwd/wssi/gis/shp/WSSI_OVERALL_Day_3_latest.zip")

#read in all three using lapply and bind with rbind
wsi_data <- lapply(noaaURLS, read_in_NOAA_data) %>% do.call("rbind", .)

#### Reformatting NOAA data & getting basemaps #######--------------------------
#need to relevel these impact factors otherwise they display in the legend alphabetically. 
wsi_data$IMPACT <- factor(wsi_data$IMPACT, levels = c('LIMITED', 'MINOR', 'MODERATE', 
                                                      'MAJOR', 'EXTREME'))

#Use tigris to get states and counties
counties_cali <- tigris::counties(state= "CA")
us_boundaries <- tigris::states()

#transform NOAA data, currently has unknown CRS
wsi_data <- st_transform(wsi_data,crs=st_crs(us_boundaries)) %>% st_make_valid()

day1 <- subset(wsi_data, START_TIME == "13Z 02/24/23")
day2 <- subset(wsi_data, START_TIME == "12Z 02/25/23")
day3 <- subset(wsi_data, START_TIME == "12Z 02/26/23")

#Get one version of NOAA data that is clipped to California.
#conveniently, the "Overall" WSI data resembles the WSI data for day 1 because it
#the most severe. 
wsi_cali<- st_intersection(day1, counties_cali)


#plot western US.
d1 <- ggplot()+
  geom_sf(data = us_boundaries, fill = 'lightgrey')+
  geom_sf(data = day1, mapping = aes(fill = IMPACT), alpha = 0.7)+
  scale_fill_viridis_d(direction = -1)+
  theme(legend.position = 'none')+
  xlim(c(-125, -100))+
  ylim(c(30, 50))+
  ggtitle("Potential Winter Storm Impact 02/24")
d2 <- ggplot()+
  geom_sf(data = us_boundaries, fill = 'lightgrey')+
  geom_sf(data = day2, mapping = aes(fill = IMPACT), alpha = 0.7)+
  scale_fill_viridis_d(direction = -1)+
  theme(legend.position = 'none')+
  xlim(c(-125, -100))+
  ylim(c(30, 50))+
  ggtitle("Potential Winter Storm Impact 02/25")
d3 <- ggplot()+
  geom_sf(data = us_boundaries, fill = 'lightgrey')+
  geom_sf(data = day3, mapping = aes(fill = IMPACT), alpha = 0.7)+
  scale_fill_viridis_d(direction = -1)+
  # theme(legend.position = 'none')+
  xlim(c(-125, -100))+
  ylim(c(30, 50))+
  ggtitle("Potential Winter Storm Impact 02/26")
gridExtra::grid.arrange(d1,d2,d3, nrow = 2, ncol = 2)


#plot California only
# ggplot()+
#   geom_sf(data = counties_cali, fill = 'lightgrey')+
#   geom_sf(data = wsi_cali, mapping = aes(fill = IMPACT), alpha = 0.7)+
#   scale_fill_viridis_d(direction = -1)+
#   # theme(legend.position = 'none')+
#   xlim(c(-125, -114))+
#   ylim(c(32, 43))+
#   ggtitle("Potential Winter Storm in California\n02/24-02/25")

