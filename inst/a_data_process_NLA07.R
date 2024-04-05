#################
## PROCESS NLA 2007
##  for lake hydrology, hydrap, effects on lake ecology project
##  3/1/2024
#################

## Subset NLA data to include climate, landscape, lake morphometry, phab, water chemistry, and biotic indices

## ORIGINAL DATA
##  LAKE ISOTOPE: M:\Net MyDocuments\a_Water_Level\Data\ nla0712isodrawdown_20160219.csv"
##  NARS: in a_Water_Level\Data\ and Data\NLA_2007 folders:
##    Lake information: "NLA2007_SampledLakeInformation_20091113.csv",
##    Water quality (chem + secchi): "NLA2007_WaterQuality_20091123"
##    Physical habitat: "NLA2007_PHab_Metrics_A" & "NLA2007_PHab_Metrics_B"
##    Condition: "NLA2007_Chemical_ConditionEstimates_20091123"
##    Visual: "NLA2007_VisualAssessment_20091015"

##  NLA Data NOT used: "NLA2007_PHab_IndexValues","NLA2007_Secchi_20091008"
##
##  NLA 2007 Basin data is 1992 LULC: "NLA2007_Basin_Landuse_Metrics_20061022",
##  Updated to have tabulated NLCD 2006 within NLA watershed
##
##  CLIMATE data (2007) used in stable isotope analysis in \a_Water_Level\Data\Climate data
#     "NLA_climate_data_2012.csv" (update 2/10/17)
####################

rm(list=ls())

library(tidyverse)
library(ggplot2)

library(devtools)
library(NLAMMI)
#C:\Users\efergus\NLAMMI\R
#################
## LOAD DATA
# Original NLA data from website

# On OneDrive
#C:\Users\efergus\OneDrive - Environmental Protection Agency (EPA)\a_NLA_MMI_project\Data\
# Site info
site_org <-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_SampledLakeInformation_20091113.csv")

# Water quality
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_WaterQuality_20091123.csv")

# PHab
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# Recreational cond (cyano)
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# Trophic cond
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# Visual assessment
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# Isotope
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# LU NLCD 2006 data
# With corrected LULC area for watersheds that overlap with interntional boundaries - Had to use the percentage values and adjust for the watershed area within the US
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# Climate
# PRISM
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")

# Survey year
chem<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/")





