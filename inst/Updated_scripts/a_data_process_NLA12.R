#################
## PROCESS NLA 2012
##  EFFECTS OF LAKE HYDRO ON LAKE ECOLOGY PROJECT
##  3/4/2024
#################

rm(list=ls())

library(tidyverse)
library(ggplot2)

library(devtools)
library(SEMNLA)

##################
## LOAD DATA
# On OneDrive C:\Users\efergus\OneDrive - Environmental Protection Agency (EPA)\a_NLA_MMI_project\Data\
# Key variable n = 1138
key.vars<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_key_variables.csv") #WGT_ALL - adjusted weight?

# Site information n = 2764
site<- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_Site_information.csv")

# water chemistry n = 1230
chem <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_water_chemistry.csv")

# chl-a n = 1230
chla<- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_chlorophyll.csv")

# toxin n = 1230
toxin <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_toxin.csv")

# secchi n = 1221
secchi<- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_secchi.csv")

#phab n = 1244
phab<-read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_phab_metrics.csv") # there are 1018 VISIT=1 = maybe merge later

###########################
## 12/11/19 UPDATED Phys Habitat index and condition metrics from Phil's email
##    INCLUDES 2007 + 2012 Data -
##    Habitat index and condition metrics were adjusted for the revised 2015 ecoregions
habitat_b<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/a_Habitat/NLA0712PHab_20150511x.csv")

# condition n = 1038
condition<- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_condition.csv")

# visual n = 2524
visual <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_wide_assement_visual.csv")

# landscape basin NLCD 2006 that is included in NLA 2012 n = 1723
landscape<- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLA2012_wide_landscape.csv")

# isotope n = 2492
isodata<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Original_07/a_nla0712_isodrawdown_20160219.csv")

## NOTE: water chemistry, chla are missing SITE_ID
ID12<-site[,c(1:4,68,78)] # SITE_ID and UID

# merge SITE_ID to water chem and chla datasets
chem.12<- dplyr :: inner_join(chem, ID12, by ="UID")

chla.12<- dplyr :: inner_join(chla, ID12, by = "UID")

########################
## CLEAN UP 2012 ISOTOPE DATASET
## Manually enter isotope and d-excess for two lakes that were missing isotope info
########################
## 8/31/18 ## ADD Water Isotope and d-excess values to lakes - from Renee's email 8/8/18
#############
# NLA 2012 isotope n = 1240
isodata_12<-isodata%>%
  filter(YEAR==2012)%>%
  select(-c(E_I))

# Create dataset of lakes that were missing isotope values
z<-subset(isodata_12,(SITE_ID %in% c("NLA12_KS-114","NLA12_WI-129")))
head(z[,c(1:9)])
head(isodata_12[,c(1:9)])

# MANUALLY ENTERING WATER ISOTOPE DATA FOR two lakes
# Update date, dD, d18O, and d-excess
dates<- c("5/22/2012","8/7/2012")
DATE_COL_iso <-as.Date(dates,
                       format = "%m/%d/%Y")
z$DATE_COL_iso <- DATE_COL_iso
# Hydrogen
dD <-c(-17.43,-46.72)
z$dD <-dD
# Oxygen
d18O <-c(-1.87,-5.75)
z$d18O<-d18O
# d-excess
d_excess <- c(-2.47, -0.75)
z$d_excess<-d_excess

head(z[,c(1:9)])

###########
## Create dataset of all other lakes in the 2012 isotope dataset (n=1238 vs 1240)
other<-isodata_12[!(isodata_12$SITE_ID %in% c("NLA12_KS-114","NLA12_WI-129")),]
# FORMAT data column
other$DATE_COL_iso<- as.Date(other$DATE_COL_iso,
                             format="%m/%d/%Y")
head(other[,c(1:9)])

#############
## Bind two dataframes together
isodata_12_b<- bind_rows(other,z)
#head(isodata_12_b[,c(1:9)])

# Order by SITE_ID
isodata_12_b <-isodata_12_b[order(isodata_12_b$SITE_ID),]
tail(isodata_12_b[,c(1:9)])

#########
## WRITE ISOTOPE DATA
write_csv(isodata_12_b,"C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/a_lk_iso12.csv")


#######################
## PROCESS ORIGINAL DATA
#######################
# SITE
site.vars <- c("SITE_ID","VISIT_NO","DATE_COL","AGGR_ECO3_2015","AGGR_ECO9_2015", #"UID",
               "AREA_HA","BORD_LAKE","CAT_UNIQUE","CH0712_CAT","CH0712_USE","CH0712_WGT","COMID2007","COMID2012","COMIDS2007",
               "ELEVATION","EPA_REG","EVALSTAT","HUC2","HUC8","LAKE_ORIGIN",
               "LAKE_ORIGIN12","LAT_DD83","LON_DD83","EVAL_NAME","NARS_NAME","PERIM_KM","RCHCODE","EVALSTAT","STATUS","RT_NLA12",
               "SITEID_07","SITETYPE","SIZE_CLASS","STATE","STRATUM","WGT_ALL","WGT_CAT","XCOORD","YCOORD","REF_NLA12_NUTR","RT_NLA12_BENT","RT_NLA12_ZOOP",
               "TNT")
site.red<-site%>%
  select(site.vars)%>%
  distinct(SITE_ID,VISIT_NO, .keep_all = T)

## KEY variables - need to get depth at sample location (which I think is the approximate max depth of the lake)
vars.k <- c("SITE_ID","VISIT_NO","INDEX_SITE_DEPTH")
key.red <-key.vars[vars.k]

## WATER CHEMISTRY - Used the merged data (chem.12) that has SITE_ID and VISIT_NO
# Identify chemistry units
units<-chem.12%>%
  select("AMMONIA_N_UNITS","ANC_UNITS","CALCIUM_UNITS","CHLORIDE_UNITS","COLOR_UNITS",
         "COND_UNITS","DOC_UNITS","MAGNESIUM_UNITS","NITRATE_N_UNITS","NITRATE_NITRITE_N_UNITS",
         "NITRITE_N_UNITS","NTL_UNITS","PH_UNITS","POTASSIUM_UNITS","PTL_UNITS",
         "SILICA_UNITS","SODIUM_UNITS","SULFATE_UNITS","TOC_UNITS","TSS_UNITS","TURB_UNITS")
head(units)

chem.vars<- c("SITE_ID","VISIT_NO",
              "ALUMINUM_RESULT","AMMONIA_N_RESULT","ANC_RESULT","CALCIUM_RESULT",
              "CHLORIDE_RESULT","COLOR_RESULT","COND_RESULT","DOC_RESULT","MAGNESIUM_RESULT",
              "NITRATE_N_RESULT","NITRATE_NITRITE_N_RESULT","NITRITE_N_RESULT","NTL_RESULT",
              "PH_RESULT","POTASSIUM_RESULT","PTL_RESULT","SILICA_RESULT","SODIUM_RESULT",
              "SULFATE_RESULT","TOC_RESULT","TSS_RESULT","TURB_RESULT")

chem.red<-chem.12[chem.vars]

##########
## Chla - - Used the merged data (chla.12) that has SITE_ID and VISIT_NO
# dropped units and changed variable heading to indicate units of measurement
chla.vars<-c("SITE_ID","VISIT_NO",
             "CHLL_RESULT","CHLX_RESULT")

chla.red <- chla.12%>%
  select(all_of(chla.vars))%>%
  rename("CHLL_RESULT_ugL"="CHLL_RESULT",
         "CHLX_RESULT_ugL"="CHLX_RESULT")

##########
## Toxin
## dropped units and changed variable heading to indicate units of measurement
toxin.vars<-c("SITE_ID","VISIT_NO",
              "MICL_RESULT")

toxin.red <- toxin%>%
  select(all_of(toxin.vars))%>%
  rename("MICL_RESULT_ppb"="MICL_RESULT")

#########
## SECCHI
secchi.vars<- c("SITE_ID","VISIT_NO",
                "SECCHI","CLEAR_TO_BOTTOM")

secchi.red<- secchi%>%
  select(all_of(secchi.vars))%>%
  rename("SECCHI_m"="SECCHI")

#############
## Physical habitat
phab.vars<-c("SITE_ID","VISIT_NO",#"DATE_COL",
             "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
             "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE","BFXHORIZDIST","BFXHORIZDIST_DD","BFXVERTHEIGHT","BFXVERTHEIGHT_DD",
             "BSFCBEDROCK","BSFCBOULDERS","BSFCCOBBLE","BSFCGRAVEL","BSFCORGANIC","BSFCSAND","BSFCSILT","BSFCWOOD",
             "BSISITEVARIETY","BSISTAVARIETY","BSVLDIA","BSXLDIA",
             "FCFCAQUATIC","FCFCAQUATIC_DD","FCFCAQUATIC_LIT","FCFCAQUATIC_SIM",
             "FCFCBOULDERS","FCFCBOULDERS_DD","FCFCBOULDERS_LIT","FCFCBOULDERS_SIM",
             "FCFCBRUSH","FCFCBRUSH_DD","FCFCBRUSH_LIT","FCFCBRUSH_SIM",
             "FCFCLEDGES","FCFCLEDGES_DD","FCFCLEDGES_LIT","FCFCLEDGES_SIM",
             "FCFCLIVETREES","FCFCLIVETREES_DD","FCFCLIVETREES_LIT","FCFCLIVETREES_SIM",
             "FCFCOVERHANG","FCFCOVERHANG_DD","FCFCOVERHANG_LIT","FCFCOVERHANG_SIM",
             "FCFCSNAG","FCFCSNAGS_DD","FCFCSNAGS_LIT","FCFCSNAGS_SIM",
             "FCFCSTRUCTURES","FCFCSTRUCTURES_DD","FCFCSTRUCTURES_LIT","FCFCSTRUCTURES_SIM",
             "FCFPALL","FCFPALL_DD","FCFPALL_LIT","FCFPALL_SIM",
             "HIFPANY_SYN","HIFPANYCIRCA_SYN","HIIAG_SYN","HIIAGCIRCA_SYN","HIIALL_SYN","HIIALLCIRCA_SYN","HIINONAG_SYN","HIINONAGCIRCA_SYN",
             "HIPWAG_SYN","HIPWALL_SYN","HIPWBUILDINGS_SYN","HIPWCOMMERCIAL_SYN","HIPWCROPS_SYN","HIPWDOCKS_SYN","HIPWLANDFILL_SYN","HIPWLAWN_SYN","HIPWNONAG_SYN","HIPWORCHARD_SYN","HIPWOTHER_SYN","HIPWPARK_SYN","HIPWPASTURE_SYN","HIPWPOWERLINES_SYN","HIPWROADS_SYN","HIPWWALLS_SYN",
             "HORIZDD","VERTDD",
             "LitCvrQc3OE","LitRipCvrQc3OE","RDis_IX","RVegQc3OE",
             "RVFCCANBIG","RVFCCANBIG_DD","RVFCCANBIG_RIP","RVFCCANBIG_SYN",
             "RVFCCANSMALL","RVFCCANSMALL_DD","RVFCCANSMALL_RIP","RVFCCANSMALL_SYN",
             "RVFCGNDBARE","RVFCGNDBARE_DD","RVFCGNDBARE_RIP","RVFCGNDBARE_SYN",
             "RVFCGNDINUNDATED","RVFCGNDINUNDATED_DD","RVFCGNDINUNDATED_RIP","RVFCGNDINUNDATED_SYN",
             "RVFCGNDNONW","RVFCGNDNONW_DD","RVFCGNDNONW_RIP","RVFCGNDNONW_SYN",
             "RVFCGNDWOODY","RVFCGNDWOODY_DD","RVFCGNDWOODY_RIP","RVFCGNDWOODY_SYN",
             "RVFCUNDNONW","RVFCUNDNONW_DD","RVFCUNDNONW_RIP","RVFCUNDNONW_SYN",
             "RVFCUNDWOODY","RVFCUNDWOODY_DD","RVFCUNDWOODY_RIP","RVFCUNDWOODY_SYN",
             "RVICANOPY","RVICANOPY_DD","RVICANOPY_RIP","RVICANOPY_SYN",
             "RVICANUND","RVICANUND_DD","RVICANUND_RIP","RVICANUND_SYN",
             "RVIGROUND","RVIGROUND_DD","RVIGROUND_RIP","RVIGROUND_SYN",
             "RVIHERBS", "RVIHERBS_DD","RVIHERBS_RIP","RVIHERBS_SYN",
             "RVITALLWOOD","RVITALLWOOD_DD","RVITALLWOOD_RIP","RVITALLWOOD_SYN",
             "RVITOTALVEG","RVITOTALVEG_DD","RVITOTALVEG_RIP","RVITOTALVEG_SYN",
             "RVIUNDERSTORY","RVIUNDERSTORY_DD","RVIUNDERSTORY_RIP","RVIUNDERSTORY_SYN",
             "RVIWOODY","RVIWOODY_DD","RVIWOODY_RIP","RVIWOODY_SYN",
             "SIVDEPTH","SIXDEPTH",
             "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",
             "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA")

phab.red<- phab%>%
  select(all_of(phab.vars))%>%
  mutate(bffFlat_grad = BFFFLAT + BFFGRADUAL)

#######
# PHAB C - Reduce number of HABITAT VARIABLES (PHIL EMAILED 12/11/19)
myvars <- c("SITE_ID","VISIT_NO",#"UID","PRISTINE","APPEALNG","RECREATIONAL_VALUE",
            "RDisIn","RDisInAg","RDisInNonAG","RDisIXAgAdj5",
            "L_RVegQ","L_LitCvrQ","L_LitRipCvQ",
            "RVegQc15","LitCvrQc15","LitRipCvrQc15",
            "L_RVegQc15","L_LitCvrQc15","L_LitRipCvrQc15",
            "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
            "RVeg_CONDus15","LitCvr_CONDus15","LitRipCvr_CONDus15","RDIS_CONDus15")

hab_b_red <- habitat_b%>%
  rename(SITE_ID=site_id)%>%
  filter(YEAR==2012)%>%
  select(all_of(myvars))

###########
## Visual
visual.vars<- c("SITE_ID","VISIT_NO","DATE_COL",
                "AGR_CROPLAND","AGR_FEEDLOT","AGR_LIVESTOCK","AGR_ORCHARDS","AGR_PASTURE","AGR_POULTRY","AGR_SCORE","AGR_STRING","AGR_WITHDRAWAL",
                "APPEALING","BIOTIC_INTEGRITY",
                "IND_COMMERCIAL","IND_FIRE","IND_INDUSTRIAL","IND_LOGGING","IND_MINES","IND_ODORS","IND_OIL","IND_POWER","IND_SCORE","IND_STRING",
                "HYDRO_TYPE","LAKE_LEVEL","LEVEL_CHANGE_M","LEVEL_CHANGES",
                "MAN_DRINKING_WATER","MAN_LEVEL_FLUCTUATIONS","MAN_SCORE","OUTLET_DAMS",
                "PRISTINE","RCH_AGRICULTURE","RCH_BARE_GROUND","RCH_DEVELOPMENT","RCH_FOREST", "RCH_GRASS","RCH_SHOREMODS","RCH_SHRUB","RCH_WETLAND",
                "REC_FILMS","REC_MARINAS","REC_PARKS","REC_PRIMITIVE","REC_RESORTS","REC_SCORE","REC_STRING","REC_TRAILS","REC_TRASH",
                "RECREATIONAL_ACTIVITY","RECREATIONAL_VALUE","RES_BRIDGES","RES_CONSTRUCTION","RES_DUMPING","RES_LAWNS","RES_PIPES","RES_RESIDENCES","RES_ROADS","RES_SCORE",
                "RES_SEWAGE","RES_STRING","SWIMMABILITY","TROPHIC_STATE")

visual.red<- visual[visual.vars]

###########
## Condition - but this is a smaller dataset than the others so may want to join later
cond.vars<-c("SITE_ID",#"VISIT_NO","LAKE_ORIGIN12",
             "BENT_COND","CHLA_COND","NTL_COND","PTL_COND","DRAWDOWN_COND",
             "LITCVR_COND","LITRIPCVR_COND","RDIS_COND","RVEG_COND",
             "SEDHG_MTH_TOP_COND","SEDHG_TOT_BTM_COND","SEDHG_TOT_TOP_COND",
             "CHLL_REC","CYNL_REC","MICL_REC","WSA9_LO")

cond.red <- condition[cond.vars]

#############
## Landscape variables provided in NLA12 - only one obs per lake so merge after
#2/7/18 - reduced just elevation
landscape.vars <- c("SITE_ID","DOMGEOL_BSN", "DAMCNT_BSN","DAMDEN_BSN",
               "ELEV_SITE","ELEVMAX_BSN","ELEVMEAN_BSN","ELEVMIN_BSN",
               "GNEISSAREA_BSN","GNEISSPCT_BSN","GRANITICAREA_BSN","GRANITICPCT_BSN",
               "HOUSEDEN_BSN","MAFULAREA_BSN","MAFULPCT_BSN",
               "PERMRATE_BSN","PERMRATE_SITE",
               "POPDEN_BSN","PSUM6M_BSN","PSUMPY_BSN","QTRNRYAREA_BSN","QTRNRYPCT_BSN","ROADDEN_BSN",
               "SANDPCT_BSN","SITE_GEOLOGY",
               "PMEAN_BSN","PMIN_BSN","PMAX_BSN","PIP_BSN",
               "TIP_BSN",
               "TMEAN_BSN","TMAX_BSN","TMIN_BSN",
               "TMEANSD_BSN","TMINSD_BSN","TMAXSD_BSN",
               "PMIN_PT","PMAX_PT","PMEAN_PT","PIP_PT",
               "TIP_PT","TMIN_PT","TMAX_PT","TMEAN_PT","TMEANPW_PT","TMEANPY_PT")#,)

landscape.red <- landscape[landscape.vars]

############
###  Derived E:I estimates UPDATED 9/10/17
# 8/2/18 - Updated E:I & RT values
# 9/10/17 - Updated E:I values and lake volume
# 9/12/18 - Updated E:I values and lake volume
# 6/24/19 - Updated lake volume and WRT
# 7/4/19  - Updated lake volume and WRT for 3 lakes with zmax from literature OLD E_I_nla12_19JUN19.csv
EI<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Derived_datasets/E_I_nla12_04JUL19.csv")
# n=1237 obs; 16 variables

# Reduce dataset to just important variables
myvars<- c("SITE_ID", "VISIT_NO","UID","E_I","Water_yield_m","Modeled_Water_yield_m",
           "RT_iso","RT_modeled","Volume_Corrected_m3","Lake_precip_m","Lake_evap_m",
           "P_E_deficit_m","Wyield_P_E_m")
EI_red2 <-EI[myvars]

## MERGE EI dataset with Site info dataset (n=2770 obs w/54 variables)
EI_site<-left_join(site.red,EI_red2,
                   by=c("SITE_ID","VISIT_NO"))
head(EI_site[c(1:3,26,46:49)])

####################
# CLEAN UP E:I estimates
#   Put NAs where values are not reliable
#     for E:I, RT_iso, Modeled_Water_yield,Water_yield_m
### Observations with negative E:I
#GA-101 (VISIT_NO=1) - negative E:I
z<-EI_site%>%
  filter(UID==6293)%>%
  select("SITE_ID","VISIT_NO","UID","E_I","RT_iso")

#WA-141 (VISIT_NO=1) - negative E:I
z<-EI_site%>%
  filter(UID==7148)%>%
  select("SITE_ID","VISIT_NO","UID","E_I","RT_iso")

### Observations with really high E:I
#  FOR WA-101 VISIT_NO = 2
z<-EI_site%>%
  filter(UID==8157)%>%
  select("SITE_ID","VISIT_NO","UID","E_I","RT_iso")

#  FOR VA-R18 VISIT_NO = 1 - E:I > 4
z<-EI_site%>%
  filter(UID==7738)%>%
  select("SITE_ID","VISIT_NO","UID","E_I","RT_iso")

#######################
# List of UID with suspicious E:I
uid<-c(6293,7148,8157,7738)
EI_red<-EI_red2%>%
  mutate(E_I = if_else(UID%in%uid,NA,E_I),
         RT_iso = if_else(UID%in%uid,NA,RT_iso),
         Modeled_Water_yield_m =if_else(UID%in%uid,NA,Modeled_Water_yield_m))
check<-EI_red%>%
  filter(UID%in%uid)%>%
  select("SITE_ID","VISIT_NO","UID","E_I","RT_iso","Modeled_Water_yield_m")
summary(EI_red$E_I) # NA=6
# n = 1237
summary(EI_red$RT_iso)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.0000  0.2681  0.5188  0.8112  0.9415 27.3213      33

###########
# Lake morpho output - 9/11/17
###########
# Lake volume and other morpho metrics - 8/30/17
vol <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Stable_isotope_calculations/2012/Processed/NLA12_ordered_SITEID_VOLUME.csv")

# Reorder variables
myvars<- c("SITE_ID","VISIT_NO","Volume_m3","maxDepth_derived_m","meanDepth_m","surfaceArea_sqm",
           "shorelineLength_m","shorelineDevelopment","fetch_m")
vol<-vol[myvars]
names(vol)


####################
## WRITE REDUCED DATASETS TO MERGE
write_csv(isodata_12_b,"dat12_merge/a_lk_iso12.csv")
write_csv(site.red, "dat12_merge/a_site_info.csv")
write_csv(chem.red,"dat12_merge/b_chem.csv")
write_csv(chla.red,"dat12_merge/c_chla.csv")
write_csv(toxin.red,"dat12_merge/d_toxin.csv")
write_csv(secchi.red,"dat12_merge/e_secchi.csv")
write_csv(phab.red,"dat12_merge/f_phab.csv")
write_csv(hab_b_red,"dat12_merge/f_phab_b.csv")
write_csv(visual.red,"dat12_merge/g_visual.csv" )
write_csv(EI_red,"dat12_merge/h_EI_24JUN19.csv") # I copied the old one and pasted one folder back (in NLA_2012 "h_EI.csv")
write_csv(vol, "dat12_merge/i_lake_morpho_ordered.csv")
write_csv(key.red, "dat12_merge/j_key_zmax.csv")

write_csv(cond.red,"data_processed/nla12_proc/lk_condition.csv")
write_csv(landscape.red,"data_processed/nla12_proc/f_landscape.csv" )

################
## MERGE DATA
##  use multi-merge function
#  from https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
#  from http://www.talkstats.com/showthread.php/22305-Merging-Multiple-Data-Frames
#  Merging based on SITE_ID and VISIT_NO
devtools::load_all()
##############
# MERGE USING multimerge fxn with processed data in dat12_merge folder
#  n = 1269w/358 vars
nla12_v1<- multimerge("dat12_merge")

table(nla12_v1$VISIT_NO)
#  1    2
#1166  103

## DROP DUPLICATE SITE_ID, VISIT_NO (n = 1238 from 1269)
nla12_v2<-nla12_v1%>%
  group_by(SITE_ID, VISIT_NO)%>%
  distinct(SITE_ID, VISIT_NO,.keep_all=T)%>%
  ungroup()

#### MANUALLY MERGE LAKE CONDITION
nla12_v3<-left_join(nla12_v2,cond.red,
                    by="SITE_ID")

##########
## RENAME LAKE ORIGIN to match 2007
#########
# LAKE ORIGIN
nla12_v3<-nla12_v3%>%
  mutate(Lake_Origin_use = recode(Lake_Origin_use, "MAN-MADE"="MAN_MADE",
                                  "NATURAL" = "NATURAL"))
table(nla12_v3$Lake_Origin_use)

################
## WRITE NLA12 MERGED DATA - 1238 observations and 379 variables
write_csv(nla12_v3, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_merge1.csv")

##############
## LOAD Additional DATA
##############
# Tabulated NLCD 2011 Land use/land cover
nlcd11<- read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/NLCD2011_NLA2012_basins_sqkm_area.csv")

# Climate - these are the (PRISM 30 yr avg?) values (not for 2011-2012 water year)
climate<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/Avg_climate_lk_pt.csv")

# Climate 2011-2012 dataset
climate12_all <- read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/climate_2012_all.csv")

######
## PROCESS
# NLCD 2011 - relabel land use/cover classes
names(nlcd11)
# NEED TO MAKE SURE ORDER IS THE SAME
names(nlcd11) <- c("SITE_ID", "BASINAreaSqKM", "OPEN_11_km2","ICE_11_km2",
                   "DEVOPEN_11_km2","DEVLOW_11_km2","DEVMED_11_km2","DEVHIGH_11_km2",
                   "BARREN_11_km2","DECID_11_km2","CONIF_11_km2", "MIXED_11_km2",
                   "SHRUBLAND_11_km2","GRASS_11_km2", "PASTURE_11_km2","CROPS_11_km2",
                   "WDYWET_11_km2","EMHERBWET_11_km2")

write_csv(nlcd11,"data_processed/nla12_proc/c_NLCD2011_NLA2012_basins_sqkm_area.csv")

## CLIMATE 2011-12
#  Calculate climate values https://stackoverflow.com/questions/27354734/dplyr-mutate-rowsums-calculations-or-custom-functions
climate12.red<-climate12_all%>%
  mutate(E_mm = rowSums(select(.,pet_201110:pet_201209)),
         E_m = E_mm/1000,
         E_avg_11_12 = (rowMeans(select(.,pet_201110:pet_201209)))/1000,
         P_WY_mm = rowSums(select(.,precip_201110:precip_201209)),
         P_WY_m = P_WY_mm/1000,
         temp_degC_summer = rowMeans(select(.,tmean_201206:tmean_201209)),
         temp_degC_winter = rowMeans(select(.,tmean_201112:tmean_201202)),
         temp_degC_spring = rowMeans(select(.,tmean_201203:tmean_201205)),
         precip_mm_summer = rowSums(select(.,precip_201206:precip_201209)),
         precip_mm_winter = rowSums(select(.,precip_201112:precip_201202)),
         precip_mm_spring = rowSums(select(.,precip_201203:precip_201205)))%>%
  rename("Precip_mm_avg_yr"="Precip_PT_avg_11_12",
         "Precip_mm_total_yr"="Precip_WY_11_12",
         "Temp_degC_avg_yr"="TMEAN_PT_avg_11_12")%>%
  select(SITE_ID,Precip_mm_avg_yr,Precip_mm_total_yr,Temp_degC_avg_yr,P_WY_m,
         E_m,E_avg_11_12,RH_PT_avg_11_12,temp_degC_summer,temp_degC_winter,temp_degC_spring,
         precip_mm_summer,precip_mm_winter,precip_mm_spring)

write_csv(climate12.red,"data_processed/nla12_proc/e_climate12_red.csv")

## Landscape info - convert elevation from cm to m
landscape.red<-landscape.red%>%
  mutate(ELEV_SITE_m = ELEV_SITE*0.01,
         ELEVMAX_BSN_m = ELEVMAX_BSN*0.01,
         ELEVMEAN_BSN_m = ELEVMEAN_BSN*0.01,
         ELEVMIN_BSN_m = ELEVMIN_BSN*0.01)

write_csv(landscape.red,"data_processed/nla12_proc/f_landscape_red.csv")

#################
## MERGE ALL DATA
nla12_v4<-left_join(nla12_v3,nlcd11,
                    by="SITE_ID")

nla12_v5<-left_join(nla12_v4,landscape.red,
                    by="SITE_ID")

#nla12_v6<-left_join(nla12_v5,climate,
#                    by="SITE_ID")

nla12_v6<-left_join(nla12_v5,climate12.red,
                    by="SITE_ID")

########
## RENAME COLMUNS
nla12_v6<- nla12_v6%>%
  rename("VOL_m3_OLD"="Volume_m3")

###########
#  Manually ADD Hollister lake morpho output
#  LAKE VOLUME VARIABLE using derived max depth
#  11/15/17
# 7/26/18 - added more lakes
###########
# Read in morph dataset
morph<-read.csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Original12/g_morpho_hollister_23JUL18.csv")
morph.red<-morph%>%
  rename("VolumeDerivedZmax"="VolumeCorrect",
         "MaxDepthDerived"="MaxDepthCorrect",
         "MeanDepthDerived"="MeanDepthCorrect")%>%
  select(-c(UID,COMID,nlaSITE_ID))

nla12_v6<-left_join(nla12_v6,morph.red,
                by=c("SITE_ID","VISIT_NO"))
nla12_v6<-nla12_v6%>%
  distinct(SITE_ID,VISIT_NO, .keep_all = T)
table(nla12_v6$VISIT_NO) # Good there are 1135 unique SITE_ID
# 1    2
#1135  103

############
## WRITE MERGED DATA BEFORE PROCESSING FURTHER n = 1238 obs w/460 vars
write_csv(nla12_v6,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_merge2.csv")

##################
## PROCESS DATA
#########
## Grouping lakes by E:I flow-through
# based on Brooks et al. 2014 - Wolfe et al. 2007
#########
##  Flowthrough E:I <0.4
##  Restricted drainage 0.4 >= E:I <1
##  Closed-basin E:I >= 1

## CALCULATE LANDUSE PROPORTIONS CLASSES
## Add level to Trophic status https://stackoverflow.com/questions/42936369/recode-character-vector-with-some-empty-strings

nla12_proc<-nla12_v6%>%
  #filter(!is.na(DpthMx_use))%>% # drop obs missing lake depth (n=1)
  mutate(lk_hydro_iso=cut(E_I,breaks=c(-Inf,0.39,0.99,Inf),
                          labels=c("Flow_through","Restricted","Closed")))%>%
  mutate(PCT_OPEN11_BSN = OPEN_11_km2/BASINAreaSqKM,
         PCT_ICE11_BSN = ICE_11_km2/BASINAreaSqKM,
         PCT_DEVOPEN11_BSN = DEVOPEN_11_km2/BASINAreaSqKM,
         PCT_DEVLOW11_BSN = DEVLOW_11_km2/BASINAreaSqKM,
         PCT_DEVMED11_BSN = DEVMED_11_km2/BASINAreaSqKM,
         PCT_DEVHIGH11_BSN = DEVHIGH_11_km2/BASINAreaSqKM,
         PCT_BARREN11_BSN = BARREN_11_km2/BASINAreaSqKM,
         PCT_DECID11_BSN = DECID_11_km2/BASINAreaSqKM,
         PCT_CONIF11_BSN = CONIF_11_km2/BASINAreaSqKM,
         PCT_MIXED11_BSN = MIXED_11_km2/BASINAreaSqKM,
         PCT_SHRUB11_BSN = SHRUBLAND_11_km2/BASINAreaSqKM,
         PCT_GRASS11_BSN = GRASS_11_km2/BASINAreaSqKM,
         PCT_PASTURE11_BSN = PASTURE_11_km2/BASINAreaSqKM,
         PCT_CROPS11_BSN = CROPS_11_km2/BASINAreaSqKM,
         PCT_WDYWET11_BSN = WDYWET_11_km2/BASINAreaSqKM,
         PCT_EMHERBWET11_BSN = EMHERBWET_11_km2/BASINAreaSqKM,
         PCT_DEVELOPED11_BSN = PCT_DEVOPEN11_BSN+PCT_DEVLOW11_BSN+PCT_DEVMED11_BSN+PCT_DEVHIGH11_BSN,
         PCT_FOREST11_BSN = PCT_DECID11_BSN+PCT_CONIF11_BSN+PCT_MIXED11_BSN,
         PCT_AGR11_BSN = PCT_PASTURE11_BSN+PCT_CROPS11_BSN,
         PCT_WET11_BSN = PCT_WDYWET11_BSN+PCT_EMHERBWET11_BSN)%>%
  mutate(TROPHIC_STATE = recode(na_if(TROPHIC_STATE,""),
                                "EUTROPHIC"="EUTROPHIC",
                                "HYPEREUTROPHIC"="HYPEREUTROPHIC",
                                "MESOTROPHIC"="MESOTROPHIC",
                                "OLIGOTROPHIC"="OLIGOTROPHIC",
                                .missing="not_assessed"))
table(nla12_proc$TROPHIC_STATE)
#EUTROPHIC HYPEREUTROPHIC    MESOTROPHIC   not_assessed   OLIGOTROPHIC
#474             88            518             45            113

################
# MAX DEPTH Decision - 9/26/17 - Populate underestimated max depth with lit values

# Create dataset of lakes with underestimated max depth - see excel spreadsheet "NLA12_lakes_35m_maxdepth.xlsx"
#   Order by alphabetized SITE_ID n = 29
z<-nla12_proc%>%
  filter(SITE_ID %in% c("NLA12_WA-151","NLA12_CA-206","NLA12_MT-136",
                        "NLA12_OR-109","NLA12_NV-109","NLA12_NY-0101",
                        "NLA12_NC-105","NLA12_MT-111"))
#z[,c(1:3,22)]

# Entering max depth from lit - make sure put depth in twice for lakes with revists
depth_lit<- c(158,71,119,76,109,57,57,130,200)
z$DpthMx_mod <-depth_lit
#head(z[,c(1,22,482)])

# Create column indicating source of max depth
z$Zmax_source <- "LIT"
head(z[,c(1,22,482,483)])

# Create dataset of all other lakes
other<-nla12_proc%>%
  filter(!SITE_ID %in% c("NLA12_WA-151","NLA12_CA-206","NLA12_MT-136",
                         "NLA12_OR-109","NLA12_NV-109","NLA12_NY-0101",
                         "NLA12_NC-105","NLA12_MT-111"))
# Create column that populates the updated depth
other$DpthMx_mod <-other$DpthMx_use

# Create column that indicates source of max depth
other$Zmax_source <-"NLA"
head(other[,c(1,22,482,483)])

## Bind two dataframes together
nla12_bind<-bind_rows(other,z)
nla12_bind<-nla12_bind%>%
  arrange(SITE_ID)%>%
  mutate(L_DpthMx_mod = log10(DpthMx_mod),
         LKAREA_KM2_mod = AREA_HA*0.01,
         L_LKAREA_KM2_mod = log10(LKAREA_KM2_mod),
         DDVrtDix_sc_MOD = VertDD_use/DpthMx_mod,
         L_DDVrtDix_sc_MOD = log10(DDVrtDix_sc_MOD+0.01))

##############
## ADD PALMER HYDROLOGIC DROUGHT INDEX - Average in Water Year
##  See Palmer_data_29MAY18.R for script working with original data layers
##############
# LOAD processed PHDI data
phdi_12<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Climate data/Palmer_Drought_Data/NLA12/PHDI_NLA12.csv")
phdi_12<- phdi_12%>%
  distinct(SITE_ID,.keep_all = T)

###########################
## LakeCAT VARIABLES for 2012 lakes
lkcat12<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/LakeCat/merged_vars_nla12.csv")
vars_del<-c("X.1","X.x","X.y","X.x.1","X.y.1","X.x.2","X.y.2","X.x.3",
            "X.y.3","X.x.4","X.y.4","X",
            "COMID","VISIT_NO","LAT_DD83","LON_DD83")
#,"Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws",,"LATdd_use","LONdd_use"

lkcat12<-lkcat12%>%
  select(-c(vars_del))

####################
## ADD MODIFIED DRAWDOWN MEASURES
## Phil emailed 2/11/19
##  Processed in Data>Additional_NLA_data>From_Phil_11FEB19>Scripts>NLA0712_create_datasets_modified_DD_12FEB19.R
# Modified Drawdown for 2012 lakes (n= 1252 obs w/19 vars)
dd<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Additional_NLA_data/From_Phil_11FEB19/NLA12_modDD_only.csv")
dd<-dd%>%
  select(c(2:19))

####################
## ADD UPDATED POPULATION WEIGHTS 6/26/19
####################
## NLA POPULATION WEIGHTS - UPDATED 6/24/19 - processed in Analysis>NLA_weighted_calculations>Scripts>a_UPDATED_NLA_POP_WGTS_24JUN19.R
# n=1038 with 9 variables
nla12wt <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Additional_NLA_data/From_Tony_42JUN19/NLA12_popwts_25JUN19.csv")
nla12wt<-nla12wt%>%
  select(-c("X","SITETYPE","WGT_CAT"))

##################
## MERGE ADDITIONAL DATASETS WITH PROCESSED NLA07
nla12_rev<-left_join(nla12_bind,phdi_12,
                      by="SITE_ID")

nla12_rev2<-left_join(nla12_rev,lkcat12,
                      by="SITE_ID")

nla12_rev3<-left_join(nla12_rev2,dd,
                      by=c("SITE_ID","VISIT_NO"))

nla12_rev4<-left_join(nla12_rev3,nla12wt,
                      by=c("SITE_ID"))

#n = 1238 obs w/ 601 vars

###############
## WRITE PROCESSED NLA 2012 data (keeping observations) - need to drop non-target lakes and zero weights
write_csv(nla12_rev4,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_merge3.csv")

###################
## REDUCE TO TARGET LAKES WITH WEIGHTS n = 1038 unique sites
nla12_rev4<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_merge3.csv")

nla12_rev5 <- nla12_rev4%>%
  filter(WGT_ALL>0)%>%
  filter(!is.na(WSA9_LO))
length(unique(nla12_rev5$SITE_ID))

###########
## READ HYDRAP DATA (07 + 12) n = 2066 w.473 vars
hydrap0712<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Data/NLA0712_1ha_opt1_HydAP_v9_RESAMPLED.csv")
#2007 2012
#1028 1038
hydrap12<-hydrap0712%>%
  filter(YEAR==2012)%>%
  select(SITE_ID,OUTLET_DAMS_red2,elev_relief_m,
         multi_dam,dam_ht_m,damht_zmax_full,
         PCT_AG_URB_BSN,PCT_AG_URB_CAT,PctDEVELOPED_Cat,
         PctIrrigated.AgLandCat,PctAgDrainageCat,
         hap_rank_9)

# Join with NLA07 processed data
nla12_full<-left_join(nla12_rev5,hydrap12,
                      by=c("SITE_ID")) #,VISIT_NO,YEAR
###############
## WRITE FULL NLA 2012 data (keeping observations)n=1138 w/631 vars
write_csv(nla12_full,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_full.csv")

# Variables Names
nla12_vars<-as.data.frame(names(nla12_full))
write_csv(nla12_vars,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_vars.csv")
