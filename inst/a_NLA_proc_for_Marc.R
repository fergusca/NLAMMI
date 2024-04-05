###########################
## DATA PROCESSING TO ASK FOR GIS DATA FROM MARC
##  MMI Project with Alan - Need to estimate HydrAP for NLA 2017
##  But while at it, would be worth estimating for NLA 2022 - even tho won't be used for this project
##
##
## 8/17/2023
###########################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(readr)

##########################
## LOAD DATA
## NLA 2017 - data from EPA NARS website n = 5721 w/80 obs
nla17<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2017/nla_2017_site_information-data.csv")


############
## NLA 2022 - Data for lake PHab Phil emailed 8/14/23
# n = 1072 w/ 533 vars
#nla22<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2022/NLA22_phabmetWide20230524C.csv")

# Visual assessment n = 2465 (Karen emailed 10/24/2023) n = 1129
nla22_va<-read_csv("data_original/nla22/NLA22_visual_assessment_wide.csv")
length(unique(nla22_va$SITE_ID))
# 1129


##############
## QUICK QAQC
#############
## MISSING VALUES
na_count_nla17 <-nla17 %>%
  summarise(across(everything(),~sum(is.na(.))))

t<- as.data.frame(t(na_count_nla17))
na_count_long<-t%>%
  filter(V1>0)

# NLA 2017 COLUMNS MISSING VALUES -
na_count_long
#               V1
#UID           4511
#VISIT_NO      4511
#DATE_COL      4511
#SITESAMP      4511
#INDEX_NLA     4511
#RT_NLA17      4511
#RT_NLA17_BENT 4511
#RT_NLA17_ZOOP 4511


##############
na_count_nla22 <-nla22 %>%
  summarise(across(everything(),~sum(is.na(.))))

t_22<- as.data.frame(t(na_count_nla22))
na_count_long22<-t_22%>%
  filter(V1>0)

# NLA 2022 COLUMNS MISSING VALUES -
na_count_long22
#               V1
#LAKE_SHAPE    12
#LOC_NAME     293
#FCFPBRUSH_DD   1

##########################
## REDUCE DATASETS TO IDENTIFYING INFORMATION

# NLA17 Drop observations missing date collected n = 1210
nla17_red<-nla17%>%
  filter(!is.na(DATE_COL))%>%
  select(SITE_ID, VISIT_NO,DATE_COL, UNIQUE_ID,COMID,
         LAT_DD83,LON_DD83,XCOORD, YCOORD)

# NLA22
# n = 976 unique sites
nla22_red<-nla22%>%
  select(SITE_ID,VISIT_NO,DATE_COL)

# n = 1129 unique sites - use this one from visual assessment for now
nla22_va_red<-nla22_va%>%
  select(SITE_ID,VISIT_NO,DATE_COL)%>%
  distinct(SITE_ID, .keep_all = TRUE)%>%
  filter(VISIT_NO==1)


###############
## WRITE CSV
write_csv(nla17_red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data_to_share/NLA17_siteid.csv")

write_csv(nla22_red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data_to_share/NLA22_old_siteid.csv")

# 1/23/2024
write_csv(nla22_va_red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data_to_share/NLA22_siteid.csv")
