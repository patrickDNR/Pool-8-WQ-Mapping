library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(paletteer)
library(shiny)
library(bslib)
library(viridis)

#load data
wq <- read.csv('Data/LTRM_WQ_all.csv') %>%
  dplyr::mutate(Year = format(mdy(DATE), '%Y')) %>%
  dplyr::mutate(Month = month(mdy(DATE), label = T)) %>%
  dplyr::mutate(DATE = as.Date(format(mdy(DATE), '%Y-%m-%d')))

#convert UTM to lat lon
utm_crs <- 32615

#Convert to sf object
utm_sf <- st_as_sf(wq, coords = c('EASTING', 'NORTHING'), crs = utm_crs)

#transform to geographic coordinates
latlon_sf <- st_transform(utm_sf, crs = 4326)

#make into data frame
latlon_df <- as.data.frame(st_coordinates(latlon_sf))
colnames(latlon_df) <- c('lng', 'lat')

#combine with water quality data
wq <- cbind(wq, latlon_df)

#years
years.num <- seq(1993, 2024)
years.char <- as.character(years.num)

#Save data...
write.csv(wq, 'Data/WaterQual_SRS.csv')

