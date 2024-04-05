####################
## PREPROCESSING DATA FOR HYDRAP CALCULATION
# NLA 2017
##
## 10/24/2023
####################

remove(list=ls())

library(tidyverse)
library(devtools)

# 11/4/23
#Identified lakes that were not getting assigned HydrAP bc listed as having a dam but missing NID data
# Going to change OUTLET_DAM to "NONE" but create a new column that notes that they had dams in the visual assessment but missing NID dat

###########
## LOAD DATA
# Site information n = 5721
nla17_site<-read_csv("data_original/nla17/nla_2017_site_information-data.csv")

# Physical habitat n = 1266 (Phil emailed 10/24/2023 "nla_2017_PHabCond_2019_1029.csv")
nla17_phab<-read_csv("data_original/nla17/nla_2017_phab.csv")

# Landscape n = 1210 NLA watersheds for NLCD 2006 and 2011 (from NARS website)
nla17_land<-read_csv("data_original/nla17/nla_2017_landMets.csv")

# Visual assessment n = 1244 (Karen emailed 10/24/2023)
nla17_va<-read_csv("data_original/nla17/nla_2017_visual_assessment_wide.csv")

# Landscape metrics from Marc emailed 10/27/23
# There are issues with some watersheds - LU summaries don't match images on Google Earth
# Decided not to use until watersheds are QA
# BUT NEED MAX ELEVATION IN WATERSHED - so will just grab that and hope it's not too incorrect
nla17_lakecat<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla17_landscape_metrics.csv")

# Landscape metrics 200m from Marc emailed 11/3/2023
nla17_lakecat_200<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla17_NLCD2011_200m.csv")
#nla17_lakecat_200_16<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla17_landscape_200m.csv") # 11/1/23
names(nla17_lakecat_200)
#names(nla17_lakecat_200_16)

######################
## REDUCE VARIABLES TO ONES OF INTEREST
# NLA17 Sites with visit_no n = 1210
nla17_site_red<-nla17_site%>%
  filter(!is.na(RT_NLA17))

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

nla17_va_red<-nla17_va%>%
  select(all_of(myvars_va))

## WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write_csv(nla17_site_red,"data_to_merge/a_site.csv")
write_csv(nla17_phab_red,"data_to_merge/b_phab.csv")
write_csv(nla17_va_red,"data_to_merge/c_visual.csv")
write_csv(nla17_land_red,"data_to_merge/d_land.csv")

## MERGE PROCESSED DATASETS
# Use multimerge function
devtools::load_all()

nla17_v1<-multimerge("data_to_merge") # n = 1210


###############
## REDUCE LAKECAT VARIABLES
nla17_lakecat_200_red<-nla17_lakecat_200%>%
  mutate(PCT_AGRIC_200 = PCTCROP2011+PCTHAY2011)%>%
  mutate(PCT_DEVELOPED_200 = PCTURBOP2011+PCTURBLO2011+PCTURBMD2011+PCTURBHI2011)%>%
  mutate(PCT_AG_URB_200 = PCT_AGRIC_200 + PCT_DEVELOPED_200)%>%
  rename(PCTCROP_200 = PCTCROP2011)%>%
  rename(PCTHAY_200 = PCTHAY2011)%>%
  select(SITE_ID,PCTCROP_200,PCTHAY_200,PCT_AGRIC_200,PCT_DEVELOPED_200,
         PCT_AG_URB_200)

summary(nla17_lakecat_200_red$PCT_AG_URB_200)

# REDUCE LAKECAT Landscape variables to elevation
nla17_lakecat_red<-nla17_lakecat%>%
  select(SITE_ID,MaxElev,Elev_Pt,IrrAg,AgDrain)

#########
# JOIN NLA NLCD 2011 200m  LANDSCAPE VARIABLES MARC EMAILED 11/03/23
# n = 1210 w/244 obs
nla17_v2<-left_join(nla17_v1,nla17_lakecat_200_red, by="SITE_ID")

# JOIN Max Elevation in NLA watershed - not sure if good enough
nla17_v3<-left_join(nla17_v2,nla17_lakecat_red, by="SITE_ID")

#11/3/23 Need to make irrigated and drainage agriculture a percentage (NOTE for bsn - may need to update)
nla17_v3<-nla17_v3%>%
  mutate(IrrAg_pct = IrrAg*100)%>%
  mutate(AgDrain_pct = AgDrain*100)
  #select(SITE_ID,LAT_DD83,LON_DD83,IrrAg,PCT_AGRIC_200)

#####################
## CALCULATE VARIABLES

# TOPOGRAPHIC RELIEF
# lake elevation = nla17_site$ELEVATION
# Difference in maximum elevation in watershed and lake elevation (Elev_Pt - Marc gathered)
nla17_v3<- nla17_v3%>%
  mutate(elev_relief_m=MaxElev-Elev_Pt)

summary(nla17_v3$elev_relief_m)
# Some negative numbers (n=6 sites)
neg_elev<-nla17_v3%>%
  filter(elev_relief_m<0)%>%
  select(SITE_ID, VISIT_NO, LAT_DD83,LON_DD83,MaxElev,ELEV_use, Elev_Pt, ELEVATION, ELEV,elev_relief_m)

# Six sites have negative elevation relief but the difference is very small for most of them
# Will change negative values to zero
nla17_v3<-nla17_v3%>%
  mutate(elev_relief_m=if_else(elev_relief_m<0,0,elev_relief_m))

summary(nla17_v3$elev_relief_m)


# TRANSFORM (log10)
nla17_v3$L_elev_relief <- log10(nla17_v3$elev_relief_m+0.01) # NOTE- four (+ one resampled lake) obs with negative difference and get NA (NLA06608-0377, NLA06608-1114, NLA06608-1227,NLA12_MI-138) These lakes look like not connected to surface inflows, oxbow lake, or man-made lake perched on landscape
summary(nla17_v3$L_elev_relief)

# FULL POOL ZMAX
#  To provide a more logical dam height to zmax estimate - we need a more reliable measure of maximum lake depth
#    the NLA zmax is an approximate max depth and can be influenced if the lake water level is drawn down at the time of sampling
#    TO address this - we propose calculating the full zmax by adding the NLA zmax and vertical DD estimate to get a more accurat measure of maximum lake basin depth
# NOTE: 498 sites out of in 2017 have NA for vertical decline
#  Will replace NA with zero
# https://sparkbyexamples.com/r-programming/replace-na-values-with-zero-in-r-dataframe/
nla17_v3<-nla17_v3%>%
  mutate_at(c("VERTDD_use"),~replace_na(.,0))
summary(nla17_v3$VERTDD_use)

###########
## MISSING MAX DEPTH
# There are 12 sites with missing maximum lake depth
# NLA17_FL-10001 (Lake Okeechobee) 13 ft ~3.96 m
dpth_na<-nla17_v3%>%
  filter(is.na(DpthMx_use))%>%
  select(SITE_ID,NARS_NAME,LAKE_ORGN,LAT_DD83,LON_DD83,DpthMx_use)

write_csv(dpth_na,"data_processed/NLA17_miss_maxdpth.csv")

# FILL IN MISSING max depth where available
nla17_v3$DpthMx_use[nla17_v3$SITE_ID=="NLA17_FL-10001"]<- 3.96
nla17_v3$DpthMx_use[nla17_v3$SITE_ID=="NLA17_MS-10003"]<- 35.66
nla17_v3$DpthMx_use[nla17_v3$SITE_ID=="NLA17_NV-10005"]<- 108.5
nla17_v3$DpthMx_use[nla17_v3$SITE_ID=="NLA17_SC-10002"]<- 23.70
nla17_v3$DpthMx_use[nla17_v3$SITE_ID=="NLA17_SD-10001"]<- 62.48
nla17_v3$DpthMx_use[nla17_v3$SITE_ID=="NLA17_SD-10053"]<- 42.67

# Calculate Full pool depth
nla17_v3$zmax_full<-nla17_v3$DpthMx_use + nla17_v3$VERTDD_use
summary(nla17_v3$zmax_full)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  0.900   2.700   4.700   7.874   9.000 108.500       3
test<- nla17_v3%>%
  filter(is.na(zmax_full))%>%
  select(SITE_ID,zmax_full,DpthMx_use,VERTDD_use)
# Missing zmax

##################
# MODIFY OUTLET_DAMS
# CONDITIONAL STATEMENT WHEN MISSING OUTLET_DAMS AND HAVE dam_ht_m data
#nla17_v1<-nla_all_dam%>%mutate(OUTLET_DAMS_rev = case_when(OUTLET_DAMS=="ARTIFICIAL" ~"ARTIFICIAL",
#                                                              OUTLET_DAMS=="NATURAL" ~ "NATURAL",
#                                                              OUTLET_DAMS=="NONE"~ "NONE",
#                                                              OUTLET_DAMS=="" & !is.na(dam_ht_m) ~ "ARTIFICIAL", # for blank Outlet_dam obs, if there is dam ht data in NABD, then label as "ARTIFICIAL"
#                                                              TRUE ~ "NONE") ) # For all others, lable "NONE:

#table(nla_all_dam$OUTLET_DAMS_rev)

######################
## CONSOLIDATE OUTLET_DAM CATEGORIES NONE = NONE + NATURAL
nla17_v3<- nla17_v3%>%
  mutate(OUTLET_DAMS_red = case_when(OUTLET_DAMS== "ARTIFICIAL" ~"DAM",
                                     OUTLET_DAMS=="NATURAL"~"NONE",
                                     OUTLET_DAMS=="NONE" ~"NONE",
                                     TRUE~"NONE"))
table(nla17_v3$OUTLET_DAMS_red)


####################
## COMBINE NLCD LAND USE CLASSES
# ONLY HAVE LAND USE FROM NLA (not LakeCat) - PCTCROP2011, PCTHAY2011
# PCTURBOP2011,PCTURBLO2011,PCTURBMD2011,PCTURBHI2011

# Sum 2011 NLCD data for NLA17 (others are 19 and 21)
nla17_v3<- nla17_v3%>%
  mutate(PCT_AGRIC_BSN = PCTCROP2011+PCTHAY2011)%>%
  mutate(PCT_DEVELOPED_BSN = PCTURBOP2011+PCTURBLO2011+PCTURBMD2011+PCTURBHI2011)%>%
  mutate(PCT_AG_URB_BSN = PCT_AGRIC_BSN + PCT_DEVELOPED_BSN)

summary(nla17_v3$PCT_AGRIC_BSN)
summary(nla17_v3$PCT_DEVELOPED_BSN)
summary(nla17_v3$PCT_AG_URB_BSN)

###############
## CHECK LANDUSE SUMMARIES IN BASIN AND 200m buffer
plot(nla17_v3$PCT_AG_URB_BSN,nla17_v3$PCT_AG_URB_200)

plot(nla17_v3$PCT_AGRIC_BSN,nla17_v3$PCT_AGRIC_200)

hi_ag_bsn<-nla17_v3%>%
  filter(PCT_AGRIC_BSN>90)%>%
  select(SITE_ID,LAT_DD83,LON_DD83,PCT_AGRIC_BSN,PCT_AGRIC_200,
         PCTCROP2011,PCTHAY2011)

lo_ag_bsn<-nla17_v3%>%
  filter(PCT_AGRIC_BSN==0 & PCT_AGRIC_200>0)%>%
  select(SITE_ID,LAT_DD83,LON_DD83,PCT_AGRIC_BSN,PCT_AGRIC_200,PCTCROP2011,PCTHAY2011)

ND_lk<-nla17_lakecat_200%>%
  filter(SITE_ID=="NLA17_ND-10037")

ND_lk_bsn<-nla17_lakecat%>%
  filter(SITE_ID=="NLA17_ND-10037")

ND_lk_ot<-nla17_lakecat_200%>%
  filter(SITE_ID=="NLA17_ND-10189")

ND_lk_ot_bsn<-nla17_lakecat%>%
  filter(SITE_ID=="NLA17_ND-10189")

# For some lakes in NDakota, watershed level land use = 100% cropland, but the 200m buffer includes other land use classes and sometimes no agriculture. Seems like there's some discrepancy
# Marc is looking at those two lakes


#######################
# NABD DAM DATA FROM MARC 11/1/2023 n = 278
nabd<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla17_detailed_daminfo.csv")
#nabd<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_org.csv")

## MULTIPLE DAMS PER LAKE within the same year
length(unique(nabd$SITE_ID)) # 239 out of 278
# List of lakes with more than one dam observation - includes duplicates
z<-nabd[duplicated(nabd$SITE_ID),] # 39 sites with multiple observations

# observations with more than one dam observation per lake n = 66 observations (27 unique SITE_ID -some lakes are resampled between years tho)
mult_all<-nabd[nabd$SITE_ID %in% nabd$SITE_ID[duplicated(nabd[,c(59)])],] # based on SITE_ID
# Does same thing
#mult2 <-nabd[nabd$SITE_ID %in% z$SITE_ID,]

length(unique(mult_all$SITE_ID)) # 27 lakes

# WRITE DATASET WITH MULTIPLE DAM OBSERVATIONS TO HANDPICK AND CONSOLIDATE
write.csv(mult_all, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/NABD/nla17_multiple_dams.csv",
          row.names = FALSE) #stringsAsFactors = FALSE,

################################
## CLEAN UP OBSERVATIONS MANUALLY WITH MULTIPLE DAMS ON LAKE - "LAKE_ORIGIN_ALL_multiple_dams.xlsx"
##  I selected the largest dam on a lake and retained that information for each SITE_ID for each year
##    and deleted additional dams or duplicate records
##    I noted that there were multiple dams on a lake when there were more than one NABD record
##    Exported trimmed dataset as "NABD_ALL_multi_dam_clean.csv"

## READ MODIFIED MULTIPLE OBS DATASET (n = 27 obs)
clean_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/NABD/nla17_multiple_dams_clean.csv",stringsAsFactors = FALSE)
names(clean_all)

###########
## SELECT DATA WITHOUT MULTIPLE DAM OBSERVATIONS (n = 212 obs)
## Will merge this with the cleaned up multi-dam dataset
single_all<-nabd[!nabd$SITE_ID %in% nabd$SITE_ID[duplicated(nabd[,c(59)])],] # based on SITE_ID
# check
length(unique(single_all$SITE_ID)) #212
single_all$multi_dams<-"n"
table(single_all$multi_dams)

# Rearrange variables to match data to merge with
myvars<-c("SITE_ID","multi_dams","COMMENT","NIDID","COMID","UNIQUE_STR","Longitude","Latitude",
          "RecordID","Dam_name","Dam_former","STATEID","Section","County","River","City",
          "Distance","Owner_name","Owner_type","Dam_type","Core","Foundation","Purposes",
          "Year_compl","Year_modif","Dam_length","Dam_height","NID_height","Hazard",
          "EAP","Inspection","Outlet_gat","Volume","State","Dam_Name2","Designer",
          "Private","Str_Height","Hyd_Height","Max_Disch","Max_stor","Norm_stor","NID_stor",
          "Surf_area","Drain_area","Insp_Freq","St_reg","St_reg_ag","Spill_type",
          "Num_locks","Len_locks","Wid_locks","Source","Condition","Cond_Date",
          "Cond_desc","Spill_wid","NrmStorM3","NIDStorM3","geometry")

single_all_cl<-single_all[myvars]

##################
## COMBINE DAM DATASETS
nabd_all_b<-rbind(single_all_cl,clean_all) # n = 239 obs (single=212; multi after processing=20, duplicates=6)
length(unique(nabd_all_b$SITE_ID))
table(nabd_all_b$multi_dams)

#################
# REDUCE VARIABLES SO CAN MERGE WITH NLA FULL DATASET
myvars<-c("SITE_ID","NIDID","multi_dams","NID_height",
          "Dam_length","Dam_name","Dam_type",
          "NIDStorM3","NrmStorM3","Condition","Owner_type","Purposes")

nabd_all_c<-nabd_all_b[myvars]

###############
## CONVERT DAM HEIGHT from ft to meters
nabd_all_c<- nabd_all_c%>%
  mutate(NID_ht_m=NID_height/3.2808399)
summary(nabd_all_c$NID_ht_m)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#2.134   6.096  11.887  16.944  19.964 133.502

####################
## JOIN NABD DATA TO PROCESSED NLA DATA n = 1210 w/317 vars
nla17_v4<-left_join(nla17_v3,nabd_all_c, by="SITE_ID")

summary(nla17_v4$NID_ht_m)


#######################
## CALCULATE DAMhtzmax based on full pool lake depth
nla17_v4<- nla17_v4%>%
  mutate(damht_zmax_full=NID_ht_m/zmax_full)
summary(nla17_v4$damht_zmax_full)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#0.0878  1.4471  2.0137  2.2960  2.8440  8.6751     940

# 937 lakes have no dams associated with them; 3 lakes are missing max depth
# 270 NLA17 lakes have dams

####################
## UPDATE NLA OUTLET_DAMS_red to put "DAM" when there is NID data
# USE case_when to have conditional statements
# https://mgimond.github.io/ES218/Week03a.html

### CREATE VARIABLE INDICATING IF THERE IS NID ATTRIBUTES AVAILABLE or Attributes were supplemented or estimated
nla17_v4$in_NID<-"n"
nla17_v4$in_NID[!(is.na(nla17_v4$NID_ht_m))] <-"y"
table(nla17_v4$in_NID)
#  n   y
#938 272

# CONDITIONAL STATEMENT TO INDICATE IF DAM PRESENT
nla17_v4<-nla17_v4%>%mutate(OUTLET_DAMS_red1 = case_when(OUTLET_DAMS_red=="DAM" ~"DAM",
                                                               OUTLET_DAMS_red=="NONE"& in_NID=="y" ~ "DAM" ,# if other criteria | = "or"   |in_NID=="e"|in_NID=="y,s"|in_NID=="s"
                                                               TRUE ~ "NONE") )
table(nla17_v4$OUTLET_DAMS_red)
# DAM NONE
#590  620

table(nla17_v4$OUTLET_DAMS_red1)
#DAM NONE
#609  601
# CHECK TO SEE IF DOING WHAT I THINK - LOOKS GOOD
test<-nla17_v4%>%filter(in_NID=="y",OUTLET_DAMS_red=="NONE") %>%
  select(SITE_ID,LAT_DD83,LON_DD83,in_NID,NID_height,OUTLET_DAMS_red,OUTLET_DAMS_red1)

table(nla17_v4$OUTLET_DAMS_red1,nla17_v4$in_NID)
#      n   y
#DAM  337 272
#NONE 601   0

# NOTE: There are 33 obs that are listed as having a dam but missing NID
# SITES where OUTLET_DAM="DAM" but missing NID data so can't assign HydrAP rank
nla17_miss_dam<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/NLA17_miss_dam.csv")

# CONDITIONAL STATEMENT TO CHANGE DAM to NONE if there is no NID data
nla17_v4<-nla17_v4%>%mutate(OUTLET_DAMS_red2 = case_when(OUTLET_DAMS_red1=="DAM" & in_NID=="n"~"NONE",
                                                         OUTLET_DAMS_red1=="DAM"& in_NID=="y" ~ "DAM" ,# if other criteria | = "or"   |in_NID=="e"|in_NID=="y,s"|in_NID=="s"
                                                         TRUE ~ "NONE") )
table(nla17_v4$OUTLET_DAMS_red2,nla17_v4$in_NID)
#        n   y
#DAM     0 272
#NONE  938   0

# CHECK TO SEE IF DOING WHAT I THINK - LOOKS GOOD
test<-nla17_v4%>%filter(in_NID=="n",OUTLET_DAMS_red=="DAM") %>%
  select(SITE_ID,LAT_DD83,LON_DD83,in_NID,NID_height,OUTLET_DAMS_red,OUTLET_DAMS_red1,OUTLET_DAMS_red2)

##############
# WRITE DATASET WITH TRANSFORMATIONS
#
##############
## FULL DATASET NLA 17 >=1ha RESAMPLED n = 1210 obs w/272 vars (VISIT_NO 1= 1113; VISIT_NO 2 = 97)
write.csv(nla17_v4, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/nla17_processed_for_Hydrap.csv",
          row.names = FALSE)



test<-nla17_v4%>%
  filter(damht_zmax_full>5)%>%
  select(SITE_ID,VISIT_NO,LAT_DD83,LON_DD83,NID_ht_m,Dam_length,zmax_full,damht_zmax_full)
