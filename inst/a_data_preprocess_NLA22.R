####################
## PREPROCESSING DATA FOR HYDRAP CALCULATION
# NLA 2022
##
## Need some data to run HydrAP processing
## 1/23/2024
####################

remove(list=ls())

library(tidyverse)
library(devtools)


###########
## LOAD DATA

# Site information - Email Phil and Karen for status
nla22_site<-read_csv("")

# Physical habitat n = 21771 (Phil emailed 10/24/2023 - this is the one with multiple observations per lake)
nla22_phab<-read_csv("data_original/nla22/nla_2022_phab_wide_use.csv")
# Count number of obs by SITE_ID - unique ID =1129 (but many are NES22??)
#numb_obs<-count(nla22_phab,SITE_ID)
numb_obs<-nla22_phab%>%
  filter(str_detect(SITE_ID,'NLA'))%>% # remove sites that start with NES
  group_by(SITE_ID)%>%
  summarise(count = n())
# reduces to n = 981
summary(numb_obs$count)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#1.00   20.00   20.00   22.04   20.00   80.00


# NLA Watershed Landscape - Email Phil and Karen for status
nla22_land<-read_csv("")

# Visual assessment n = 2465 (Karen emailed 10/24/2023) n = 1129
nla22_va<-read_csv("data_original/nla22/NLA22_visual_assessment_wide.csv")

# Landscape metrics from Marc emailed 10/27/23 n = 976
# BUT NEED MAX ELEVATION IN WATERSHED - so will just grab that and hope it's not too incorrect
nla22_lakecat<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla22_landscape_metrics.csv")

# Landscape metrics 200m from Marc emailed 11/3/2023 n = 976
nla22_lakecat_200<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla22_lake_metrics.csv")
names(nla22_lakecat_200)


# NABD DAM ATTRIBUTES from Marc emailed 11/1/2023 n = 111
nabd22<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla22_detailed_daminfo.csv")

############
## NLA 2022 - Data for lake PHab Phil emailed 8/14/23
# n = 1072 w/ 533 vars
#nla22<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2022/NLA22_phabmetWide20230524C.csv")


######################
## REDUCE VARIABLES TO ONES OF INTEREST
# NLA17 Sites with visit_no n = 1210
#nla22_site_red<-nla22_site%>%
#  filter(!is.na(RT_NLA17))

# Physical habitat data - n = 12076 bc obs from each of 10 stations around a lake site
# PHab variables
myvars<- c("SITE_ID","VISIT_NO","DATE_COL","COMMENT_ORIGIN",
           "LkArea_km2","L_LkAreakm2","ELEV_use","L_ELEV_use",
           "LkPerim_km","L_LkPerimkm","DpthMx_use","L_DpthMx_use",
           "HORIZDD_use","VERTDD_use")

nla17_phab_red<-nla17_phab%>%
  select(all_of(myvars))

# Landscape variables
myvars_lu<-c("PUBLICATION_DATE","UID","DATE_COL")

nla17_land_red<-nla17_land%>%
  select(!myvars_lu)

# Visual assessment variables
myvars_va<-c("SITE_ID","VISIT_NO","OUTLET_DAMS") #

nla22_va_red<-nla22_va%>%
  select(all_of(myvars_va))

## WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write_csv(nla22_site_red,"nla22_data_to_merge/a_site.csv")
write_csv(nla22_phab_red,"nla22_data_to_merge/b_phab.csv")
write_csv(nla22_va_red,"nla22_data_to_merge/c_visual.csv")
write_csv(nla22_land_red,"nla22_data_to_merge/d_land.csv")

############################
## CLEAN UP DAM DATA
## MULTIPLE DAMS PER LAKE within the same year
length(unique(nabd22$SITE_ID)) # 104 out of 111
# List of lakes with more than one dam observation - includes duplicates
z<-nabd22[duplicated(nabd22$SITE_ID),] # 7 sites with multiple observations

# observations with more than one dam observation per lake n = 13 observations (6 unique SITE_ID -some lakes are resampled between years tho)
mult_all<-nabd22[nabd22$SITE_ID %in% nabd22$SITE_ID[duplicated(nabd22[,c(59)])],] # based on SITE_ID

length(unique(mult_all$SITE_ID)) # 6 lakes

# WRITE DATASET WITH MULTIPLE DAM OBSERVATIONS TO HANDPICK AND CONSOLIDATE
write.csv(mult_all, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/NABD/nla22_multiple_dams.csv",
          row.names = FALSE) #stringsAsFactors = FALSE,
