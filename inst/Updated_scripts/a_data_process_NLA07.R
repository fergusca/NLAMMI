#################
## PROCESS NLA 2007
##  EFFECTS OF LAKE HYDRO ON LAKE ECOLOGY PROJECT
##  3/1/2024
#################

rm(list=ls())

library(tidyverse)
library(ggplot2)

library(devtools)
library(SEMNLA)

##################
## LOAD DATA
# On OneDrive C:\Users\efergus\OneDrive - Environmental Protection Agency (EPA)\a_NLA_MMI_project\Data\

# Site info n = 1252
site_org <-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_SampledLakeInformation_20091113.csv")

# Water quality n = 1326
chem_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_WaterQuality_20091123.csv")

# PHab categories were broken up into two csv files n = 1442
phab_a_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_PHab_Metrics_A.csv")
phab_b_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_PHab_Metrics_B.csv")
phab_index_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_PHab_IndexValues.csv")

###########################
## 12/11/19 UPDATED Phys Habitat index and condition metrics from Phil's email
##    INCLUDES 2007 + 2012 Data -
##    Habitat index and condition metrics were adjusted for the revised 2015 ecoregions
habitat_c<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/a_Habitat/NLA0712PHab_20150511x.csv")
names(habitat_c)

# Recreational cond (cyano) n = 1252
cond_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_Recreational_ConditionEstimates_20091123.csv")

# Trophic cond n = 1252
troph_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_Trophic_ConditionEstimate_20091123.csv")

# Visual assessment n = 1252
visual_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA2007_VisualAssessment_20091015.csv")

# Isotope n = 2492 (2007 & 2012)
iso_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/a_nla0712_isodrawdown_20160219.csv")

# READ IN MODIFIED isotope derived water balance parameters - e.g., Water Yield
#  n = 1158 obs with 8 vars
iso<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/i_iso_nla07_05JUL19.csv")
table(iso$VISIT_NO) #C:\Users\efergus\OneDrive - Environmental Protection Agency (EPA)\a_NLA_MMI_project\Data\NLA07

# LU NLCD 2006 data n = 1159
# With corrected LULC area for watersheds that overlap with interntional boundaries - Had to use the percentage values and adjust for the watershed area within the US
lu_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA07_NLCD06_LULC_area_sqkm_ALL.csv")

# Climate
# PRISM n = 1252
prism_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/NLA_2007_Isotope_master.csv")

# Survey year n = 1157
clim_org<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/climate_2007_all.csv")


#######################
## PROCESS ORIGINAL DATA
#######################
# SITE
# Subset of variables
lkvars<- c("SITE_ID","VISIT_NO","DATE_COL","SITE_TYPE","TNT","LON_DD","LAT_DD",
           "ALBERS_X","ALBERS_Y","ST","STATE_NAME",
           "NHDNAME","LAKENAME","AREA_CAT7","WGT_NLA","WSA_ECO3","WSA_ECO9",
           "LAKE_ORIGIN",
           "AREA_HA","SIZE_CLASS","LAKEAREA","LAKEPERIM","SLD","DEPTH_X","DEPTHMAX",
           "HUC_2","HUC_8","REACHCODE","COM_ID")
lk.red<-site_org[lkvars]

# WATER CHEMISTRY n = 74 duplicates
# Check chemistry units
summary(chem_org$NTL)# TN is in ug/L (I think)
summary(chem_org$PTL) #TP ug/L
#duplicate_ct_chem <- chem_org%>%
#  add_count(SITE_ID,VISIT_NO)%>% # adds new column called "n" - count of observations for each combination of SITE_ID and VISIT_NO
#  filter(n >1)%>%
#  distinct(SITE_ID,VISIT_NO, .keep_all = T)%>% # Removes any duplicate rows that may exist after previous steps
#  select(SITE_ID, VISIT_NO,NTL,n)

#duplicates_chem<-chem_org%>%
#  group_by(SITE_ID,VISIT_NO)%>%
#  filter(n()>1)%>%
#  ungroup()%>%
#  select(SITE_ID, VISIT_NO)

chemvars<- c("SITE_ID","VISIT_NO","PH_LAB","COND","ANC",
             "TURB","DOC",
             "NH4N_PPM","NO3_NO2","NO3N_PPM",
             "NTL","PTL","CHLA",
             "CL_PPM","SO4_PPM","CA_PPM",
             "MG_PPM","NA_PPM","K_PPM",
             "COLOR","SIO2","CATSUM","ANSUM2","SOBC",
             "SECMEAN","CLEAR_TO_BOTTOM")
# n = 1252 after dropping 74 duplicates
chem.red<-chem_org%>%
  select(all_of(chemvars))%>%
  group_by(SITE_ID,VISIT_NO)%>%
  distinct(SITE_ID,VISIT_NO, .keep_all = T)%>%
  ungroup()

chem.red<-chem.red%>%
  mutate(NTL=NTL/1000)# Convert from ug/L to mg/L
summary(chem.red$NTL)

## TROPHIC CONDITION
trphvars<-c("SITE_ID","VISIT_NO","TSTATE_TP","TSTATE_TN","TSTATE_CHL",
            "TSTATE_SECCHI")
trophic.red<-troph_org%>%
  select(all_of(trphvars))

## RECREATIONAL CONDITION
recvars<-c("SITE_ID","VISIT_NO","MCYST_TL_UGL","CYANDENS")
cond.red<-cond_org%>%
  select(all_of(recvars))

# PHAB
# Check for duplicate observations
# https://www.r-bloggers.com/2023/07/finding-duplicate-values-in-a-data-frame-in-r-a-guide-using-base-r-and-dplyr/
#duplicates<-phab_a_org%>%
#  group_by(SITE_ID,VISIT_NO)%>%
#  filter(n()>1)%>%
#  ungroup()%>%
#  select(SITE_ID, VISIT_NO, UID)

# Get table of counts of duplicates
#duplicate_counts <- phab_a_org%>%
#  add_count(SITE_ID, VISIT_NO)%>% # adds new column called "n" - count of observations for each combination of SITE_ID and VISIT_NO
#   filter(n >1)%>%
#  distinct()%>% # Removes any duplicate rows that may exist after previous steps
#  select(SITE_ID, VISIT_NO, UID,n)

hab_a_vars<- c("SITE_ID","VISIT_NO","sixDepth","sivDepth","L_sixDepth","L_sivDepth","bsfcBedrock","bsfcBoulders","bsfcCobble",
               "bsfcGravel","bsfcSand","bsfcSilt","bsfcOrganic","bsfcWood","bsiStaVariety","bsiSiteVariety","bsiStStaVariety",
               "bsxLdia","bsvLdia","amfcEmergent","amfcFloating","amfcSubmergent","amfcAll","amfcFltEmg",
               "fcfcAquatic","fcfcBoulders","fcfcBrush","fcfcLedges","fcfcLiveTrees","fcfcOverhang","fcfcSnag","fcfcStructures",
               "fciAll","fciBig","fciNatural","fciRipVeg",
               "rvfcCanBig","rvfcCanSmall","rvfcUndNonw","rvfcUndWoody","rvfcGndBare","rvfcGndInundated","rvfcGndNonw","rvfcGndWoody",               "rviCanopy","rviUnderstory",
               "rviGround","rviWoody","rviTallWood","rviHerbs","rviCanUnd","rviTotalVeg")
# Select variables and drop duplicates (n = 190 duplicated SITE_ID & VISIT_NO) n = 1252
phab.a.red<-phab_a_org%>%
  select(all_of(hab_a_vars))%>%
  group_by(SITE_ID,VISIT_NO)%>%
  distinct(SITE_ID,VISIT_NO, .keep_all = T)%>%
  ungroup()

# PHAB B
hab_b_vars<- c("SITE_ID","VISIT_NO","ssfcBedrock","ssfcBoulders",
               "ssfcCobble","ssfcGravel","ssfcSand","ssfcSilt","ssfcOrganic","ssfcWood","ssfcOther",
               "ssiStaVariety","ssiSiteVariety","ssiStStaVariety","ssxLdia","ssvLdia",
               "hipwBuildings","hipwCommercial","hipwRoads","hipwWalls","hipwDocks","hipwPowerlines",
               "hipwLandfill","hipwLawn","hipwPark","hipwCrops","hipwOrchard","hipwPasture",
               "hiiAll","hiiNonAg","hiiAg","hiiAllCirca","hiiNonAgCirca","hiiAgCirca","hifpAny","hifpAnyCirca",
               "bffFlat","bffGradual","bffSteep","bffVertical","bfoAngle","bfnAngle",
               "bfxHorizDist","bfxVertHeight","bfnHorizDist","bfnVertHeight","L_RtHzVrt",
               "RDisInEx1a")

phab.b.red<-phab_b_org%>%
  select(all_of(hab_b_vars))%>%
  mutate(bffFlat_grad = bffFlat+bffGradual)%>%
  group_by(SITE_ID,VISIT_NO)%>%
  distinct(SITE_ID,VISIT_NO, .keep_all = T)%>%
  ungroup()

# PHAB C - Reduce number of HABITAT VARIABLES (PHIL EMAILED 12/11/19)
myvars <- c("SITE_ID","VISIT_NO","UID","PRISTINE","APPEALNG","RECREATIONAL_VALUE",
            "RDisIn","RDisInAg","RDisInNonAG","RDisIXAgAdj5",
            "L_RVegQ","L_LitCvrQ","L_LitRipCvQ",
            "RVegQc15","LitCvrQc15","LitRipCvrQc15",
            "L_RVegQc15","L_LitCvrQc15","L_LitRipCvrQc15",
            "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
            "RVeg_CONDus15","LitCvr_CONDus15","LitRipCvr_CONDus15","RDIS_CONDus15")

hab_c_red <- habitat_c%>%
  rename(SITE_ID=site_id)%>%
  filter(YEAR==2007)%>%
  select(all_of(myvars))

# PHAB Index
hab_ind_vars<- c("SITE_ID","VISIT_NO","DATEPHAB","ssiBedBld","ssiNATBedBld","rviLowWood",
                 "RVegQ_7","RVegQ_8","L_RVegQ_8","LRCVQ_7A", "LRCVQ_7B","LRCVQ_7C","LRCVQ_7D","LRCVQ_8D","L_LRCVQ_8D",
                 "ElevXLat","ElevDLat",
                 "ElevXLon","RDis_InEx","RDis_IX",
                 "RvegQ_Var","RVegQ","LogRVegQ",
                 "Pre3A_L_RVegQ_8","Adj3A_L_RVegQ_8","LOE_RVQ_west",
                 "LitCvrQ_Var", "LitCvrQ", "LogLitCvrQ",
                 "Pre3A_L_LitCvrQ","Adj3A_L_LitCvrQ","LOE_LitCv_west",
                 "LitRipCVQ_Var","LitRipCVQ","LogLitRipCvQ",
                 "Pre3A_L_LRCvQ_8D","Adj3A_L_LRCvQ_8D","LOE_LitRipCv_west",
                 "RVeg_OE","LitCvr_OE","LitRipCvr_OE")
phab.ind.red<-phab_index_org%>%
  select(all_of(hab_ind_vars))%>%
  group_by(SITE_ID,VISIT_NO)%>%
  distinct(SITE_ID,VISIT_NO, .keep_all = T)%>%
  ungroup()

# VISUAL ASSESSMENT
visual_vars<- c("SITE_ID","VISIT_NO","RES_PIPES","AGR_CROPLAND","AGR_PASTURE",
                "AGR_LIVESTOCK","AGR_ORCHARDS","AGR_POULTRY","AGR_FEEDLOT",
                "AGR_WITHDRAWL","IND_INDUSTRIAL","IND_MINES","IND_OIL","IND_POWER",
                "IND_LOGGING","IND_FIRE","IND_ODORS","IND_COMMERCIAL","MAN_LEVEL_FLUCTUATIONS","RES_SCORE","REC_SCORE",
                "AGR_SCORE","IND_SCORE","MAN_SCORE","HYDRO_TYPE","OUTLET_DAMS","SWIMMABILITY",
                "LAKE_LEVEL", "LEVEL_CHANGES","TROPHIC_STATE")
visual.red<-visual_org[visual_vars]

## PRISIM 30yr
prism_vars<- c("SITE_ID","VISIT_NO","IRT","PRT","ORT","MRT","Lake_Vol",
              "ELEV_PT","Max_WSelev","Mean_WSelev","Stdev_WSelev",
              "DOM_GEOL","GEOL_PT","Precip_WS","Precip_PT",
              "P_WY","E","RH_WS","RH_PT","RH_COLMO","RH_FW","T_FW",
              "TMAX_WS","TMEAN_WS","TMIN_WS",
              "TMAX_PT","TMEAN_PT","TMIN_PT",
              "POP_DEN","FarmFert","LvStckCon",
              "LvStckUnC","NnFarmFert","ATM_N","Hum_N","Fert","manure","largest_source","AN")
prism.red<-prism_org[prism_vars]

# Survey year 2007 climate (mean seasonal temp: summer (June-Sept), winter(Dec-Feb), spring(Mar-May))
#  Cumulative seasonal precip
clim.red<-clim_org%>%
  mutate(temp_degC_summer = (tmean_200706+tmean_200707+tmean_200708+tmean_200709)/5,
         temp_degC_winter = (tmean_200612 +tmean_200701+tmean_200702)/3,
         temp_degC_spring = (tmean_200703 +tmean_200704 + tmean_200705)/3,
         precip_mm_summer = precip_200706 + precip_200707 + precip_200708 + precip_200709,
         precip_mm_winter = precip_200612+precip_200701+precip_200702,
         precip_mm_spring = precip_200703 +precip_200704 +precip_200705
         )%>%
  rename("Precip_mm_avg_yr"="Precip_PT_avg_06_07",
         "Precip_mm_total_yr"="Precip_WY_06_07",
         "Temp_degC_avg_yr"="TMEAN_PT_avg_06_07")%>%
  select(SITE_ID,Precip_mm_avg_yr,Precip_mm_total_yr,Temp_degC_avg_yr,
         E_avg_06_07,RH_PT_avg_06_07,temp_degC_summer,temp_degC_winter,temp_degC_spring,
         precip_mm_summer,precip_mm_winter,precip_mm_spring)

lu.red<-lu_org[2:21]

names(lu.red) <-c("SITE_ID", "Summed_area_sqkm","NLCD06_11_KM2_BSN","NLCD06_12_KM2_BSN",
          "NLCD06_21_KM2_BSN","NLCD06_22_KM2_BSN","NLCD06_23_KM2_BSN","NLCD06_24_KM2_BSN","NLCD06_31_KM2_BSN",
          "NLCD06_41_KM2_BSN","NLCD06_42_KM2_BSN","NLCD06_43_KM2_BSN","NLCD06_52_KM2_BSN",
          "NLCD06_71_KM2_BSN","NLCD06_81_KM2_BSN","NLCD06_82_KM2_BSN","NLCD06_90_KM2_BSN","NLCD06_95_KM2_BSN",
          "BASINAREA_06_sqkm", "TOTAL")

# Isotope subset 2007 obs n = 1252 from 2492
isodata_07<- iso_org%>%
  filter(YEAR==2007)


#######
## Write reduced datasets
#######
#setwd("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07")
write_csv(isodata_07,"dat07_merge/a_isodata_07.csv" )
write_csv(lk.red,"dat07_merge/b_lk.red.csv")
write_csv(chem.red,"dat07_merge/c_chem.red.csv")
write_csv(trophic.red,"dat07_merge/d_trophic.red.csv")
write_csv(cond.red,"dat07_merge/e_cond.red.csv")
write_csv(phab.a.red,"dat07_merge/f_habitat_a.red.csv")
write_csv(phab.b.red,"dat07_merge/f_habitat_b.red.csv")
write_csv(hab_c_red,"dat07_merge/f_habitat_c.red.csv")
write_csv(phab.ind.red,"dat07_merge/f_habitat_index.red.csv")
write_csv(visual.red,"dat07_merge/g_visual.red.csv")
write_csv(prism.red,"dat07_merge/h_prism.red.csv")
#write.csv(LU_92.red,"i_lu_1992.red.csv")

write_csv(lu.red, "data_processed/nla07_proc/j_lu_2006.csv")
write_csv(clim.red, "data_processed/nla07_proc/k_climate07.csv")

######################
## MERGE DATA
##  use multi-merge function
#  from https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
#  from http://www.talkstats.com/showthread.php/22305-Merging-Multiple-Data-Frames
#  Merging based on SITE_ID and VISIT_NO
devtools::load_all()
##############
# MERGE USING multimerge fxn with processed data in dat07_merge folder
#  n = 1252 w/322 vars
nla07_v1<- multimerge("dat07_merge")

table(nla07_v1$VISIT_NO)
#  1    2
#1157   95

#########
## MANUALLY MERGE NLCD 2006 AND CLIMATE 2007 DATA
nla07_v2<-left_join(nla07_v1,lu.red,
                    by=c("SITE_ID"))
nla07_v3<-left_join(nla07_v2,clim.red,
                    by=c("SITE_ID"))

##################
## RENAME VARS TO MATCH NLA12
# LAKE ORIGIN
nla07_v3<-nla07_v3%>%
  mutate(Lake_Origin_use = recode(Lake_Origin_use, "MAN-MADE"="MAN_MADE",
                                  "NATURAL" = "NATURAL"))
###########
## NLCD 2006 Land use/land cover %
###########
nla07_v3<- nla07_v3%>%
  mutate(PCT06_OPENH2O_BSN=NLCD06_11_KM2_BSN/TOTAL*100,
         PCT06_ICESNOW_BSN=NLCD06_12_KM2_BSN/TOTAL*100,
         PCT06_DEVOPEN_BSN=NLCD06_21_KM2_BSN/TOTAL*100,
         PCT06_DEVLOW_BSN=NLCD06_22_KM2_BSN/TOTAL*100,
         PCT06_DEVMED_BSN=NLCD06_23_KM2_BSN/TOTAL*100,
         PCT06_DEVHIGH_BSN=NLCD06_24_KM2_BSN/TOTAL*100,
         PCT06_BARREN_BSN=NLCD06_31_KM2_BSN/TOTAL*100,
         PCT06_DECID_BSN=NLCD06_41_KM2_BSN/TOTAL*100,
         PCT06_CONIF_BSN=NLCD06_42_KM2_BSN/TOTAL*100,
         PCT06_MIXED_BSN=NLCD06_43_KM2_BSN/TOTAL*100,
         PCT06_SHRUBLAND_BSN=NLCD06_52_KM2_BSN/TOTAL*100,
         PCT06_GRASS_BSN=NLCD06_71_KM2_BSN/TOTAL*100,
         PCT06_PASTURE_BSN=NLCD06_81_KM2_BSN/TOTAL*100,
         PCT06_CROPS_BSN=NLCD06_82_KM2_BSN/TOTAL*100,
         PCT06_WDYWET_BSN=NLCD06_90_KM2_BSN/TOTAL*100,
         PCT06_EMHERBWET_BSN=NLCD06_95_KM2_BSN/TOTAL*100)

############
## WRITE MERGED DATA BEFORE PROCESSING FURTHER
write_csv(nla07_v3,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Processed_07/nla07_merge1.csv")

############
## DERIVE VARIABLES AND TRANSFORMATIONS
############
# Drop obs missing data
# Create E:I flow0through classes (based on Brooks et al. 2014, Wolfe et al. 2007)
# Compile LU/LC classes
nla07_proc<-nla07_v3%>%
  #filter(!is.na(DpthMx_use))%>% # drop obs missing lake depth (n=1)
  mutate(lk_hydro_iso=cut(E_I,breaks=c(-Inf,0.39,0.99,Inf),
                          labels=c("Flow_through","Restricted","Closed")))%>%
  mutate(PCT06_FOREST_BSN=(PCT06_DECID_BSN+PCT06_CONIF_BSN+PCT06_MIXED_BSN)/100,
         PCT06_DEVELOPED_BSN=(PCT06_DEVLOW_BSN+PCT06_DEVMED_BSN+PCT06_DEVHIGH_BSN)/100,
         PCT06_AGRIC_BSN=(PCT06_PASTURE_BSN+PCT06_CROPS_BSN)/100,
         PCT06_WETLAND_BSN=(PCT06_WDYWET_BSN+PCT06_EMHERBWET_BSN)/100)

#nla07_proc%>%
#  group_by(lk_hydro_iso)%>%
#  summarize(min=min(E_I),
#            max=max(E_I),
#            mean=mean(E_I))

table(nla07_proc$lk_hydro_iso)
#Flow_through   Restricted       Closed
#   935          309            8
#boxplot(nla07_proc$E_I~nla07_proc$lk_hydro_iso)

###############
###############
## ADD VARIABLES
# 9/26/17 - MAX DEPTH - populate understimated max depth with lit values; 7/5/19 - updated zmax for 24 NLA07 lakes
################
# Create dataset of lakes with underestimated max depth - see excel spreadsheet "NLA12_lakes_35m_maxdepth.xlsx"
#   Order by alphabetized SITE_ID n = 29
z<-nla07_proc%>%
  filter(SITE_ID %in% c("NLA06608-0021","NLA06608-0041","NLA06608-0079",
                        "NLA06608-0129","NLA06608-0181","NLA06608-0191",
                        "NLA06608-0291","NLA06608-0373","NLA06608-0495",
                        "NLA06608-0561","NLA06608-0580","NLA06608-0794",
                        "NLA06608-0870","NLA06608-0930","NLA06608-0934",
                        "NLA06608-0970","NLA06608-1045","NLA06608-1153",
                        "NLA06608-1348","NLA06608-1717","NLA06608-1818",
                        "NLA06608-1873","NLA06608-1958","NLA06608-2881"))
z[,c(1:4)]

# Entering max depth from lit - make sure put depth in twice for lakes with revists
depth_lit<- c(84,84,99.7,99.7,55,55,
              69,69,51,76,72,
              56,122,119,51,185,185,
              64,52,130,85,
              57,144,44,61,62,79,91,71)
z$DpthMx_mod <-depth_lit
head(z[,c(1,348)])

# Create column indicating source of max depth
z$Zmax_source <- "LIT"
head(z[,c(1,23,348,349)])

# Create dataset of all other lakes
other<-nla07_proc%>%
  filter(!SITE_ID %in% c("NLA06608-0021","NLA06608-0041","NLA06608-0079",
                         "NLA06608-0129","NLA06608-0181","NLA06608-0191",
                         "NLA06608-0291","NLA06608-0373","NLA06608-0495",
                         "NLA06608-0561","NLA06608-0580","NLA06608-0794",
                         "NLA06608-0870","NLA06608-0930","NLA06608-0934",
                         "NLA06608-0970","NLA06608-1045","NLA06608-1153",
                         "NLA06608-1348","NLA06608-1717","NLA06608-1818",
                         "NLA06608-1873","NLA06608-1958","NLA06608-2881"))
# Create column that populates the updated depth
other$DpthMx_mod <-other$DpthMx_use

# Create column that indicates source of max depth
other$Zmax_source <-"NLA"
head(other[,c(1, 23,348,349)])

## Bind two dataframes together
nla07_bind<-bind_rows(other,z)
nla07_bind<-nla07_bind%>%
  arrange(SITE_ID)

# Scale vertical drawdown using modified lake depth
nla07_bind<-nla07_bind%>%
  mutate(DDVrtDix_sc_MOD = VertDD_use/DpthMx_mod,
         L_DDVrtDix_sc_MOD = log10(VertDD_use/DpthMx_mod +0.01))
#summary(nla07_bind$L_DDVrtDix_sc_MOD)
#summary(nla07_bind$L_DDVrtDix_sc)

## ADD ISOTOPE DERIVED WATER BALANCE PARAMETERS
nla07_rev<-left_join(nla07_bind,iso,
                      by=c("SITE_ID","VISIT_NO"))

nla07_rev<-nla07_rev%>%
  mutate(L_RT_iso = log10(RT_iso+0.01))

##############
## ADD PALMER HYDROLOGIC DROUGHT INDEX - Average in Water Year
##  See Palmer_data_29MAY18.R for script working with original data layers
##  Marc Weber gathered values for lakes and emailed on 5/25/2018
##  10/8/18
##############
# LOAD processed PHDI data in Data>Climate data folder n=5169
phdi_07<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Climate data/Palmer_Drought_Data/NLA07/PHDI_NLA07_04JUN18.csv")
phdi_07<-phdi_07%>%
  distinct(SITE_ID,.keep_all = T) # n = 5074

#########################
## ADD LAKECAT VARIABLES - processed in Data>LakeCat>Scripts>Merge_NLA_LakeCat_08MAY19.R
#########################
#LAKECAT compiled data for 2007 lakes n=1028
lkcat<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/LakeCat/merged_vars_nla07.csv")
vars_del<-c("X.1","X.x","X.y","X.x.1","X.y.1","X.x.2","X.y.2","X.x.3",
            "X.y.3","X.x.4","X.y.4","X",
            "COMID","VISIT_NO","LATdd_use","LONdd_use"
            ) #,"Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws"

lkcat<-lkcat%>%
  select(-c(vars_del))

####################
## ADD MODIFIED DRAWDOWN MEASURES
## Phil emailed 2/11/19
##  Processed in Data>Additional_NLA_data>From_Phil_11FEB19>Scripts>NLA0712_create_datasets_modified_DD_12FEB19.R

# Modified Drawdown for 2007 lakes (n= 1252 obs)
dd<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Additional_NLA_data/From_Phil_11FEB19/NLA07_modDD_only.csv")
dd<-dd%>%
  select(c(2:19))

####################
## ADD UPDATED POPULATION WEIGHTS 6/25/19
####################
## NLA POPULATION WEIGHTS - UPDATED 6/24/19 - processed in Analysis>NLA_weighted_calculations>Scripts>a_UPDATED_NLA_POP_WGTS_24JUN19.R
nla07wt <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Additional_NLA_data/From_Tony_42JUN19/NLA07_popwts_25JUN19.csv")
nla07wt<-nla07wt%>%
  select(-c("X"))

##################
## MERGE ADDITIONAL DATASETS WITH PROCESSED NLA07
nla07_rev2<-left_join(nla07_rev,phdi_07,
                      by="SITE_ID")

nla07_rev3<-left_join(nla07_rev2,lkcat,
                      by="SITE_ID")

nla07_rev4<-left_join(nla07_rev3,dd,
                      by=c("SITE_ID","VISIT_NO"))

nla07_rev5<-left_join(nla07_rev4,nla07wt,
                      by=c("SITE_ID"))

# n = 1252 obs w/ 474 vars

###############
## WRITE PROCESSED NLA 2007 data (keeping observations) - need to drop non-target lakes and zero weights
#  n = 1252 obs with 474 vars
write_csv(nla07_rev5,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Processed_07/nla07_merge2.csv")

###################
## REDUCE TO TARGET LAKES WITH WEIGHTS n = 1123 w/1028 unique sites
nla07_rev5<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Processed_07/nla07_merge2.csv")

nla07_v6 <- nla07_rev5%>%
  filter(WGT_NLA>0)%>%
  filter(TNT=="Target")

length(unique(nla07_v6$SITE_ID))# want this to be 1028

table(nla07_v6$VISIT_NO)
#   1    2
# 1028   95

###########
## READ HYDRAP DATA (07 + 12) n = 2066 w.473 vars
hydrap0712<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Data/NLA0712_1ha_opt1_HydAP_v9_RESAMPLED.csv")
#2007 2012
#1028 1038
hydrap07<-hydrap0712%>%
  filter(YEAR==2007)%>%
  select(SITE_ID,OUTLET_DAMS_red2,elev_relief_m,
                multi_dam,dam_ht_m,damht_zmax_full,
                PCT_AG_URB_BSN,PCT_AG_URB_CAT,PctDEVELOPED_Cat,
                PctIrrigated.AgLandCat,PctAgDrainageCat,
                hap_rank_9)

# Join with NLA07 processed data
nla07_full<-left_join(nla07_v6,hydrap07,
                      by=c("SITE_ID")) #,VISIT_NO,YEAR

###############
## WRITE FULL NLA 2007 data (keeping observations) n = 1123 w/511 vars
write_csv(nla07_full,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Processed_07/nla07_full.csv")

# Variables Names
nla07_vars<-as.data.frame(names(nla07_full))
write.csv(nla07_vars,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Processed_07/nla07_vars.csv")

