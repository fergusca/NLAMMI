#############################
## R SCRIPT TO CREATE DATASET WTIH LAKE HYDRO ALTERATION RANKING
##  NLA 2007 + 2012 ALL ECOREGIONS 
##    To create excel spreadsheet to handinspect each lake and assign a rank
##
##  Reference "NLA07_12_data_creation_20FEB2020.R" for original script or "NLA07_12_data_creation_for_Marc.R" for 1ha 
##  Reference for NID, NABD processing and merging with NLA dataset
##  3/14/20 - Marc linked additional Dam and NABD data to NLA sites (deposited in public L:drive)
##            Can load .dbf file directly in R and output as .csv file rather than going through ArcMap
##  
##  3/19/20
##  3/27/20 - Updated LakeCat NLCD 2006, 2011
##
##############################

remove(list=ls())

library(dplyr)
library(ggplot2)

#######################
## LOAD DATASETS
#######################

## FULL NLA DATASET (2007+2012 lakes >=1ha) 
# SHARED WITH MARC to link to dam data n = 2066
#nla_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/for_Marc/NLA_FULL_1ha.csv")
# WITH PROCESSED VARIABLES
nla_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/nla_full_1ha_trans_all.csv") #a_Stata/Data/nla0712/nla_full_trans_all.csv")

# FOR LAPTOP
# WITH PROCESSED VARIABLES n = 2066
nla_all<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla_full_1ha_trans_all.csv") #a_Stata/Data/nla0712/nla_full_trans_all.csv")

todrop <- names(nla_all)%in% c("X")
nla_all <- nla_all[!todrop]

## READ FULL NLA 07+12 processed dataset (n = 1979 obs w/354 variables for 1629 unique lake sites
## lakes >= 4 ha in SIZE
#nla_full<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/nla_full_trans_all.csv")
#names(nla_full)


###################
## 
test<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NLA_NABD_ALL_clean.csv")
names(test)

####################################
## LINK NABD DAM DATA WITH NLA INFO

##############################
### FOR FULL NLA OBSERVATIONS ACROSS 9 ECOREGIONS
## 3/14/20

#########################
## DAM DATA PROCESSING
# ORIGINAL NABD dataset (n = 2247 obs) that imported as .dbf and exported as .csv
nabd_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_org.csv")
# FOR LAPTOP
nabd_all<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_org.csv")

## MULTIPLE DAMS PER LAKE within the same year
length(unique(nabd_all$SITE_ID)) # 2066 out of 2247
# List of lakes with more than one dam observation - includes duplicates
z<-nabd_all[duplicated(nabd_all$SITE_ID),] # 181 observations

# observations with more than one dam observation per lake n = 294 observations (113 unique SITE_ID -some lakes are resampled between years tho)
mult_all<-nabd_all[nabd_all$SITE_ID %in% nabd_all$SITE_ID[duplicated(nabd_all[,c(1)])],] # based on SITE_ID
# Does same thing
#mult2 <-nabd[nabd$SITE_ID %in% z$SITE_ID,]

length(unique(mult_all$SITE_ID)) # 113 lakes

# WRITE DATASET WITH MULTIPLE DAM OBSERVATIONS TO HANDPICK AND CONSOLIDATE
write.csv(mult_all, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/LAKE_ORIGIN_ALL_multiple_dams.csv")

################################
## CLEAN UP OBSERVATIONS MANUALLY WITH MULTIPLE DAMS ON LAKE - "LAKE_ORIGIN_ALL_multiple_dams.xlsx"
##  I selected the largest dam on a lake and retained that information for each SITE_ID for each year 
##    and deleted additional dams or duplicate records
##    I noted that there were multiple dams on a lake when there were more than one NABD record
##    Exported trimmed dataset as "NABD_ALL_multi_dam_clean.csv" 

## READ MODIFIED MULTIPLE OBS DATASET (n = 113 obs)
clean_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_multi_dam_clean.csv")
names(clean_all)

# FOR LAPTOP
clean_all<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_multi_dam_clean.csv")

###########
## SELECT DATA WITHOUT MULTIPLE DAM OBSERVATIONS (n = 1953 obs)
## Will merge this with the cleaned up multi-dam dataset
single_all<-nabd_all[!nabd_all$SITE_ID %in% nabd_all$SITE_ID[duplicated(nabd_all[,c(1)])],] # based on SITE_ID
# check
length(unique(single_all$SID))# 1626
length(unique(single_all$SITE_ID)) #1953
single_all$multi_dam<-"n"
table(single_all$multi_dam)

# Rearrange variables to match data to merge with
myvars<-c("SITE_ID","VISIT_NO","SID","UID","COMID","STATE","ECOP5_2015",
          "ECOREG_use","multi_dam","YEAR","LATdd_use","LONdd_use","XCOORD","YCOORD",
          "Dam_length","Dam_name","Dam_Name2","Dam_type","Dam_height","NIDStorM3","NID_height",
          "NrmStorM3","Condition","Owner_type","Purposes")

single_all_cl<-single_all[myvars]

# WRITE DATASET WITH SINGLE OBSERVATIONS - Add columns to make similar to the modified duplicated dataset
#write.csv(single_all, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_SINGLE_dam.csv")

## MANUALLY ADD COLUMN to make this dataset similar to the multi dam

## READ SINGLE DATASET after adding column for multi_dam between ECOREG_use and YEAR
#sing_cl_all <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_SINGLE_dam.csv")

##################
## COMBINE DAM DATASETS

nabd_all2<-rbind(single_all_cl,clean_all) # n = 2066 obs (single=1953; multi after processing=113)

# Replace zeros with NA for observations missing dam numerical attribute data [15,19:22]
names(nabd_all2)
summary(nabd_all2$Dam_length)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0     420     900    1863    1688   40940    1095 

nabd_all2[, c(15,19:22)][nabd_all2[, c(15,19:22)] == 0] <- NA # dam length, ht, storage etc
summary(nabd_all2$Dam_length)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.0   486.5   960.0  1957.0  1800.0 40940.0    1142  

#################
# REDUCE VARIABLES SO CAN MERGE WITH NLA FULL DATASET
myvars<-c("SITE_ID","YEAR","multi_dam",
          "Dam_length","Dam_name","Dam_Name2","Dam_type","Dam_height","NIDStorM3","NID_height",
          "NrmStorM3","Condition","Owner_type","Purposes")

nabd_all3<-nabd_all2[myvars]

###############
## CONVERT DAM HEIGHT from ft to meters
#z<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/NLA_NABD_clean.csv")

nabd_all3$dam_ht_m<-nabd_all3$Dam_height*0.3048
summary(nabd_all3$Dam_height)
summary(nabd_all3$dam_ht_m)

nabd_all3$NID_ht_m <- nabd_all3$NID_height*0.3048
summary(nabd_all3$NID_ht_m)

# Create a new Dam Purpose column with just first purpose listed
# https://stackoverflow.com/questions/7723549/getting-and-removing-the-first-character-of-a-string
nabd_all3$purpose_red<-substring(nabd_all3$Purposes, 1,1)
head(nabd_all3$purpose_red)


#########################
## MERGE DATASETS - NLA full and PROCESSED DAM n = 2066 obs w/437 variables
nla_all_dam <- merge(nla_all, nabd_all3, by=c("SITE_ID","YEAR"),all.x=TRUE)
names(nla_all_dam)

################
## Dam ht relative to lake depth - to use as an indication of hydrologic alteration potential
### USE THIS - CREATE DAMHT/ZMAX variable - similar to scaled drawdown (vertical DD/lake depth)
##    Larger values indicate greater hydro alteration potential
nla_all_dam$damht_zmax <- nla_all_dam$NID_ht_m/nla_all_dam$DpthMx_mod
summary(nla_all_dam$damht_zmax)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.021   1.537   2.208   2.827   3.213  49.780    1102 


##################
## WRITE DATASET FULL NLA 2007 and 2012 >=1 ha lakes and NABD dam attributes
#     n = 2066 obs w/438 variables
write.csv(nla_all_dam,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_trans_NABD.csv")

# FOR LAPTOP
write.csv(nla_all_dam,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_trans_NABD.csv")

#####################
## WRITE REDUCED DATASET TO LOOK AT IN EXCEL
##  ASSIGN HYDRO ALTERATION CLASSES AND RANKS - FIRST ROUND
## REARRANGE VARIABLES
myvarsII <- c("SITE_ID","SID","UID","YEAR","RESAMPLED12_b","NARS_NAME","STATE",
              "LkArea_km2","LATdd_use","LONdd_use","Lake_Origin_mod",#"INLET_OUTLET","INLET_OUTLET_notes",
              #"DAM","DAM_notes","Hydro_alteration","hap_rank","questionable","in_NID","nat_modif","NOTES",
              "XCOORD","YCOORD","ECOP5_2015","ECOREG_use","OUTLET_DAMS","inStreamCat",
              "HYDRO_TYPE_f","lk_hydro_iso",
              "DpthMx_mod","SLD","PERIM_KM",
              "BASINAreaSqKM","ELEV_use","ELEVMAX_BSN_m","elev_relief_m",
              "VertDD_use","HorizDD_use","E_I","d_excess","RT_iso","Drawdown_CONDus15",
              "RT_NLA12_2015","MAN_SCORE","PCT_FOREST_BSN","PCT_WETLAND_BSN",
              "PCT_AGRIC_BSN","PCT_DEVELOPED_BSN",
              "hiiAg","hiiAgCirca","hiiNonAg","hiiNonAgCirca",
              "hiiAll","hiiAllCirca","hifpAny","hifpAnyCirca",
              "PctIrrigated.AgLandWs","PctDEVELOPED_Cat","PctAGR_Cat",
              "Dam_length","Dam_name","Dam_Name2","Dam_type","Dam_height", #"nabd_multi_dam",
              "NIDStorM3","NID_height",
              "NrmStorM3","Condition","Owner_type","Purposes",
              "purpose_red","dam_ht_m","NID_ht_m","damht_zmax") #,"nabd_Notes"

nla_all_dam_red<-nla_all_dam[myvarsII]
write.csv(nla_all_dam_red, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NLA_0712_1ha_NABD_forexcel.csv")

# FOR LAPTOP
write.csv(nla_all_dam_red, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NLA_0712_1ha_NABD_forexcel.csv")


names(nla_all)
