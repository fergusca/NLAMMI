#################
## PROCESS NLA 2017
##  EFFECTS OF LAKE HYDRO ON LAKE ECOLOGY PROJECT
##  3/15/2024
#################

rm(list=ls())

library(tidyverse)
library(ggplot2)

library(devtools)
library(SEMNLA)

##################
## LOAD DATA
# On OneDrive C:\Users\efergus\OneDrive - Environmental Protection Agency (EPA)\a_NLA_MMI_project\Data\

# Site info n = 5721
site_org <-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla_2017_site_information-data.csv")

# water chemistry n = 22873
chem <- read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla_2017_water_chemistry_chla-data.csv")

# secchi n = 1210
secchi<- read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla_2017_secchi-data.csv")

# Physical habitat n = 1266 (Phil emailed 10/24/2023 "nla_2017_PHabCond_2019_1029.csv")
nla17_phab<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla_2017_phab.csv")

# Visual assessment n = 1244 (Karen emailed 10/24/2023)
nla17_va<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla_2017_visual_assessment_wide.csv")

# condition n = 3529 - population estimates - not useful
#condition<- read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla2017_condition_estimates_20220217_ForWebsite.csv")

# Landscape n = 1210 NLA watersheds
nla17_land<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla_2017_landMets.csv")

# isotope n = 1124
isodata<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/NLA17_EI_estimates_220316.csv")

# Landscape metrics from Marc emailed 10/27/23 n = 1113
# There are issues with some watersheds - LU summaries don't match images on Google Earth
# Decided not to use until watersheds are QA
# BUT NEED MAX ELEVATION IN WATERSHED - so will just grab that and hope it's not too incorrect
nla17_lakecat<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla17_landscape_metrics.csv")

# Landscape metrics 200m from Marc emailed 11/3/2023 n=1113
nla17_lakecat_200<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Original17/nla17_NLCD2011_200m.csv")
#nla17_lakecat_200_16<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla17_landscape_200m.csv") # 11/1/23
names(nla17_lakecat_200)
#names(nla17_lakecat_200_16)


######################
## REDUCE VARIABLES TO ONES OF INTEREST
# NLA17 Sites with visit_no n = 1210
nla17_site_red<-site_org%>%
  filter(!is.na(RT_NLA17))

#########
# Physical habitat data - from Phil - will keep most bc looks like it was already compiled with other data
myvars<-c("SITE_ID","VISIT_NO","LAKENAME","LAKE_ORIGIN_FINAL",
          "CLEAR_TO_BOTTOM","SECMEAN","STATE",
          "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
          "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE",#"BFXHORIZDIST","BFXHORIZDIST_DD","BFXVERTHEIGHT","BFXVERTHEIGHT_DD",
          "BSFCBEDROCK","BSFCBOULDERS","BSFCCOBBLE","BSFCGRAVEL","BSFCORGANIC","BSFCSAND","BSFCSILT","BSFCWOOD",
          "BSISITEVARIETY","BSISTAVARIETY","BSVLDIA","BSXLDIA",
          "FCFCAQUATIC_DD","FCFCAQUATIC_LIT","FCFCAQUATIC_SIM",
          "FCFCBOULDERS_DD","FCFCBOULDERS_LIT","FCFCBOULDERS_SIM",
          "FCFCBRUSH_DD","FCFCBRUSH_LIT","FCFCBRUSH_SIM",
          "FCFCLEDGES_DD","FCFCLEDGES_LIT","FCFCLEDGES_SIM",
          "FCFCLIVETREES_DD","FCFCLIVETREES_LIT","FCFCLIVETREES_SIM",
          "FCFCOVERHANG_DD","FCFCOVERHANG_LIT","FCFCOVERHANG_SIM",
          "FCFCSNAGS_DD","FCFCSNAGS_LIT","FCFCSNAGS_SIM",
          "FCFCSTRUCTURES_DD","FCFCSTRUCTURES_LIT","FCFCSTRUCTURES_SIM",
          "FCFPALL_DD","FCFPALL_LIT","FCFPALL_SIM",
          "HIFPANY_SYN","HIFPANYCIRCA_SYN","HIIAG_SYN","HIIAGCIRCA_SYN","HIIALL_SYN","HIIALLCIRCA_SYN","HIINONAG_SYN","HIINONAGCIRCA_SYN",
          "HIPWAG_SYN","HIPWALL_SYN","HIPWBUILDINGS_SYN","HIPWCOMMERCIAL_SYN","HIPWCROPS_SYN","HIPWDOCKS_SYN","HIPWLANDFILL_SYN","HIPWLAWN_SYN","HIPWNONAG_SYN","HIPWORCHARD_SYN","HIPWOTHER_SYN","HIPWPARK_SYN","HIPWPASTURE_SYN","HIPWPOWERLINES_SYN","HIPWROADS_SYN","HIPWWALLS_SYN",
          #"HORIZDD","VERTDD",
          #"LitCvrQc3OE","LitRipCvrQc3OE","RDis_IX","RVegQc3OE",
          "RVFCCANBIG_DD","RVFCCANBIG_RIP","RVFCCANBIG_SYN",
          "RVFCCANSMALL_DD","RVFCCANSMALL_RIP","RVFCCANSMALL_SYN",
          "RVFCGNDBARE_DD","RVFCGNDBARE_RIP","RVFCGNDBARE_SYN",
          "RVFCGNDINUNDATED_DD","RVFCGNDINUNDATED_RIP","RVFCGNDINUNDATED_SYN",
          "RVFCGNDNONW_DD","RVFCGNDNONW_RIP","RVFCGNDNONW_SYN",
          "RVFCGNDWOODY_DD","RVFCGNDWOODY_RIP","RVFCGNDWOODY_SYN",
          "RVFCUNDNONW_DD","RVFCUNDNONW_RIP","RVFCUNDNONW_SYN",
          "RVFCUNDWOODY_DD","RVFCUNDWOODY_RIP","RVFCUNDWOODY_SYN",
          "RVICANOPY_DD","RVICANOPY_RIP","RVICANOPY_SYN",
          "RVICANUND_DD","RVICANUND_RIP","RVICANUND_SYN",
          "RVIGROUND_DD","RVIGROUND_RIP","RVIGROUND_SYN",
          "RVIHERBS_DD","RVIHERBS_RIP","RVIHERBS_SYN",
          "RVITALLWOOD_DD","RVITALLWOOD_RIP","RVITALLWOOD_SYN",
          "RVITOTALVEG_DD","RVITOTALVEG_RIP","RVITOTALVEG_SYN",
          "RVIUNDERSTORY_DD","RVIUNDERSTORY_RIP","RVIUNDERSTORY_SYN",
          "RVIWOODY_DD","RVIWOODY_RIP","RVIWOODY_SYN",
          "SIVDEPTH","SIXDEPTH",
          "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",
          "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA",
          "ECOWSA9_2015","ECOWSA3_2015",
          "YEAR","LkArea_km2","L_LkAreakm2","ELEV_use","L_ELEV_use",
          "LkPerim_km","L_LkPerimkm","DpthMx_use","L_DpthMx_use",
          "LATdd_use","LONdd_use","ElevXLat_use","ElevXLon_use",
          "Lake_Origin_use","Reservoir","ECOP5_2015","ECOP6_2015",
          "RVegQc15","LitCvrQc15","LitRipCvrQc15","L_RVegQc15",
          "L_LitCvrQc15","L_LitRipCvrQc15","RVegQc3x15",
          "LitCvrQc3x15","LitRipCvrQc3x15","L_RVegQc3x15","L_LitCvrQc3x15",
          "L_LitRipCvrQc3x15","RVegQc3OE15","LitCvrQc3OE15",
          "LitRipCvrQc3OE15","HORIZDD_use" ,"VERTDD_use",
          "L_HORIZDD_use","L_VERTDD_use")
#"FCFCBRUSH",
nla17_phab_red<-nla17_phab%>%
  select(all_of(myvars))

############
## WATER CHEMISTRY - Pivot_wider by analyte
chem_mod<-chem%>%
  select(SITE_ID,VISIT_NO,ANALYTE,RESULT)%>%
  pivot_wider(names_from = ANALYTE, values_from = RESULT)%>%
  rename(ALUMINUM_mgL=ALUMINUM, AMMONIA_N_mgL=AMMONIA_N,
         ANC_ueqL=ANC,CALCIUM_mgL=CALCIUM,CHLA_ugL=CHLA,
           CHLORIDE_mgL=CHLORIDE,COLOR_ALPHA= COLOR,
           COND_USCM=COND,DOC_mgL=DOC,
           MAGNESIUM_mgL=MAGNESIUM,
           NITRATE_N_mgL=NITRATE_N,
         NITRATE_NITRITE_N_mgL=NITRATE_NITRITE_N,
           NITRITE_N_mgL=NITRITE_N,NTL_mgL=NTL,
           PH=PH, POTASSIUM_mgL=POTASSIUM,
           PTL_ugL=PTL,
           SILICA_mgL=SILICA,SODIUM_mgL=SODIUM,
         SULFATE_mgL=SULFATE,
           TKN_mgL=TKN,TURB_NTU=TURB)

#########
## SECCHI n = 1210
#secchi.vars<- c("SITE_ID","VISIT_NO",
#                "DISAPPEARS","CLEAR_TO_BOTTOM")

#secchi.red<- secchi%>%
#  select(all_of(secchi.vars))%>%
#  rename("SECCHI_m"="DISAPPEARS")

############
# Visual assessment variables - from NARS website
myvars_va<-c("SITE_ID","VISIT_NO","OUTLET_DAMS") #

nla17_va_red<-nla17_va%>%
  select(all_of(myvars_va))

###########
## ISOTOPE
# Reduce dataset to just important variables
myvars<- c("SITE_ID", "VISIT_NO","d18O","dD","d_excess","E_I",
           "RT_iso","RT_modeled","Lk_vol_m3")
isodata_red <-isodata[myvars]

##############
# Landscape variables
myvars_lu<-c("PUBLICATION_DATE","UID","DATE_COL")

nla17_land_red<-nla17_land%>%
  select(!myvars_lu)

####################
## WRITE REDUCED DATASETS TO MERGE
write_csv(nla17_site_red, "dat17_merge/a_site_info.csv")
write_csv(chem_mod,"dat17_merge/b_chem.csv")
write_csv(nla17_phab_red,"dat17_merge/c_phab.csv")
write_csv(nla17_va_red,"dat17_merge/d_visual.csv" )
write_csv(isodata_red,"dat17_merge/e_isodata_red.csv") #
write_csv(nla17_land_red,"dat17_merge/f_nlaland_red.csv")

##################
## MERGE NLA17 ORIGINAL DATASETS
# Use multimerge function
# n = 1210 w/436 variables
devtools::load_all()

nla17_v1<-multimerge("dat17_merge")

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


#######################
#### MANUALLY MERGE LANDSCAPE DATA
# w/LakeCat in 200m buffer (n = 1210 w/295 variables)
nla17_v2<-left_join(nla17_v1,nla17_lakecat_200_red,
                    by="SITE_ID")

# w/LakeCat reduced
nla17_v3<-left_join(nla17_v2,nla17_lakecat_red,
                    by="SITE_ID")

nla17_v3<-nla17_v3%>%
  mutate(IrrAg_pct = IrrAg*100)%>%
  mutate(AgDrain_pct = AgDrain*100)


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
  select(SITE_ID, VISIT_NO, LAT_DD83,LON_DD83,MaxElev,ELEV_use, Elev_Pt, ELEVATION, elev_relief_m)

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

#######################
# NABD DAM DATA FROM MARC 11/1/2023 n = 278
nabd<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_LakeCAT/Marc_NLA1722/nla17_detailed_daminfo.csv")

# OBSERVATIONS w/MORE THAN ONE DAM PER LAKE n= 66 observations (27 unique SITE_ID -some lakes are resampled between years tho)
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
single_all<-single_all%>%
  mutate(multi_dams= "n")

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

##############
# WRITE DATASET WITH TRANSFORMATIONS
#
##############
## FULL DATASET NLA 17 >=1ha RESAMPLED n = 1210 obs w/470 vars (VISIT_NO 1= 1113; VISIT_NO 2 = 97)
write_csv(nla17_v4, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Processed_17/nla17_proc.csv")

nla17_v4<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Processed_17/nla17_proc.csv")

##########
## PROCESS DATA TO MATCH NLA07 + 12
nla17_v5<-nla17_v4%>%
  mutate(lk_hydro_iso=cut(E_I,breaks=c(-Inf,0.39,0.99,Inf),
                        labels=c("Flow_through","Restricted","Closed")))%>%
  mutate(WALA=WSAREASQKM/LkArea_km2,
         L_WALA=log10(WALA),
         bffFlat_grad=BFFFLAT+BFFGRADUAL)%>%
  mutate(ECOP5_2015 = case_when(
    (AG_ECO9 %in% c("WMT","XER")) ~ "WEST",
    (AG_ECO9 %in% c("NAP","SAP")) ~ "APPS",
    (AG_ECO9 %in% c("NPL","SPL","TPL"))~"CENPL",
    (AG_ECO9 %in% c("CPL"))~"CPL",
    (AG_ECO9 %in% c("UMW"))~"UMW"))

table(nla17_v5$lk_hydro_iso)
summary(nla17_v5$WALA)
summary(nla17_v5$bffFlat_grad)

##############
## READ HYDRAP DATA n=1210 w/274 variables
hydrap<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/NLA17_HydAP.csv")

hydrap_red<-hydrap%>%
  select(SITE_ID,VISIT_NO,hap_rank_9)

## JOIN DATA n = 1210 w/471 variables
nla17_full<-left_join(nla17_v5,hydrap_red,
                    by=c("SITE_ID","VISIT_NO"))#"VISIT_NO"

################
## WRITE PROCESSED DATA
write_csv(nla17_full,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Processed_17/nla17_full.csv")

# COLUMN NAMES
var_names<-data.frame(colnames(nla17_v5))
write_csv(var_names,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Processed_17/nla17_vars.csv")
