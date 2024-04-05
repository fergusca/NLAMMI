
library(sf)
library(readr)
library(tidyverse)

## READ SHAPEFILE -
#Aggregated Ecoregion 9
dams12 <-read_sf("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/GIS/Dam_All_Marc_12MAR20/NLA_Lakes_Dams.shp")

nabd12 <-read_sf("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/GIS/Dam_All_Marc_12MAR20/NLA_Lakes_NABD.shp")


eco<- st_transform(eco, 4269)
