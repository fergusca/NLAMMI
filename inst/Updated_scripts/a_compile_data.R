#################
## COMPILE NLA SURVEY DATA
##  2007, 2012, 2017
##
##  3/15/2024
##################
rm(list=ls())

library(tidyverse)
library(ggplot2)

library(devtools)
library(SEMNLA)

##########
## NLA 07 n=1123
##  Processed in a_data_process_NLA07.R
nla07_full<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA07/Processed_07/nla07_full.csv")

## NLA 2012 n=1138
nla12_full<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA12/Processed_12/nla12_full.csv")

## NLA 2017 n=1210
nla17_full<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Data/NLA17/Processed_17/nla17_full.csv")

# Benthic and zooplankton MMIs NLA 07-17 from Alan (email 2/19/24)
#  n = 3687 w/15 variables
mmi<-read_csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/Additional_NLA_data/From_Alan_2024_0219/NLA_3yr_bio_out.csv")
table(mmi$YEAR)
#2007 2012 2017
#1247 1230 1210

#####################
## DATA PREP
##############
## REDUCE NUMBER OF VARIABLES TO KEEP IN DATASETS FOR MERGING
## MERGE DATA BY CREATING SAME COLUMNS AND THEN BINDING ROWS
## RENAME COLUMN NAMES TO MATCH BTW SURVEYS
###################

#######
# MMI - Reduce variables
myvars_mmi<- c("SITE_ID","VISIT_NO","YEAR",
               "RT_NLA17","ZOOP_COND_2020","BENT_COND_2020",
               "PTL_COND","NTL_COND","MMI_BENT","MMI_ZOOP_NLA6","TOTLNIND_BENT")
mmi_red<-mmi%>%
  select(all_of(myvars_mmi))

########
## NLA07
nla07_mod<-nla07_full%>%
  rename(PCT_OPENH2O_BSN=PCT06_OPENH2O_BSN,
         PCT_ICESNOW_BSN=PCT06_ICESNOW_BSN,
         PCT_DEVOPEN_BSN=PCT06_DEVOPEN_BSN,
         PCT_DEVLOW_BSN=PCT06_DEVLOW_BSN,
         PCT_DEVMED_BSN=PCT06_DEVMED_BSN,
         PCT_DEVHIGH_BSN=PCT06_DEVHIGH_BSN,
         PCT_BARREN_BSN=PCT06_BARREN_BSN,
         PCT_DECID_BSN=PCT06_DECID_BSN,
         PCT_CONIF_BSN=PCT06_CONIF_BSN,
         PCT_MIXED_BSN=PCT06_MIXED_BSN,
         PCT_SHRUBLAND_BSN=PCT06_SHRUBLAND_BSN,
         PCT_GRASS_BSN=PCT06_GRASS_BSN,
         PCT_PASTURE_BSN=PCT06_PASTURE_BSN,
         PCT_CROPS_BSN=PCT06_CROPS_BSN,
         PCT_WDYWET_BSN=PCT06_WDYWET_BSN,
         PCT_EMHERBWET_BSN=PCT06_EMHERBWET_BSN,
         PCT_FOREST_BSN=PCT06_FOREST_BSN,
         PCT_DEVELOPED_BSN=PCT06_DEVELOPED_BSN,
         PCT_AGRIC_BSN=PCT06_AGRIC_BSN,
         PCT_WETLAND_BSN=PCT06_WETLAND_BSN)%>%
  mutate(L_PTL=log10(PTL),
         L_NTL=log10(NTL),
         L_CHLA=log10(CHLA),
         WALA=WsAreaSqKm/LkArea_km2,
         L_WALA=log10(WALA),
         bffFlat_grad=bffFlat+bffGradual,
         L_DpthMx_mod=log10(DpthMx_mod))%>%
  mutate(DATE_COL_iso=mdy(DATE_COL_iso))

myvars07<-c("SITE_ID","VISIT_NO","YEAR","UID","UNIQUE_ID","DATE_COL_iso",
            "dD","d18O","d_excess","E_I","RT_iso",
            "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use",
            "LkArea_km2","L_LkAreakm2","DpthMx_mod","L_DpthMx_mod",
            "sixDepth",
            "LAKEPERIM","Lake_Vol_m3","ELEV_use","L_ELEV_use",
            "LATdd_use","LONdd_use", "ALBERS_X","ALBERS_Y",
            "ST","URBAN","Lake_Origin_use","lk_hydro_iso",
            "LAKENAME","ECOWSA9_2015",
            "ECOP5_2015","ECOP6_2015","SITETYPE",
            #"RT_NLA12_2015",
            "NH4N_PPM","ANC","CA_PPM","CL_PPM","COLOR","COND","DOC",
            "MG_PPM","NO3N_PPM","NO3_NO2","NTL","PH_LAB","K_PPM",
            "PTL","SIO2","NA_PPM","SO4_PPM","TURB",
            "CHLA","SECMEAN","L_NTL","L_PTL","L_CHLA",
            "amfcAll","amfcEmergent","amfcFloating","amfcSubmergent",
            "bffFlat","bffGradual","bffSteep","bffVertical","bfoAngle",
            "ssfcBedrock","ssfcBoulders","ssfcCobble","ssfcGravel","ssfcOrganic","ssfcOther","ssfcSand","ssfcSilt",
            "ssfcWood","ssiSiteVariety","ssiStaVariety","ssvLdia","ssxLdia",
            "RVegQc15","LitCvrQc15","LitRipCvrQc15",
            "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
            "WsAreaSqKm",
            "PCT_OPENH2O_BSN","PCT_ICESNOW_BSN","PCT_DEVOPEN_BSN","PCT_DEVLOW_BSN","PCT_DEVMED_BSN","PCT_DEVHIGH_BSN",
            "PCT_BARREN_BSN","PCT_DECID_BSN","PCT_CONIF_BSN","PCT_MIXED_BSN","PCT_SHRUBLAND_BSN","PCT_GRASS_BSN",
            "PCT_PASTURE_BSN","PCT_CROPS_BSN","PCT_WDYWET_BSN","PCT_EMHERBWET_BSN",
            "PCT_FOREST_BSN","PCT_GRASS_BSN","PCT_WETLAND_BSN","PCT_AGRIC_BSN","PCT_DEVELOPED_BSN",
            "Max_WSelev","POP_DEN","OUTLET_DAMS",
            "BFIWs","OmWs","PermWs","RckdepWs","WtDepWs",
            "AvgWetIndxWs","DamNrmStorWs","DamNIDStorWs","RdDensWs",
            "Precip8110Ws","Tmean8110Ws","Tmax8110Ws","Tmin8110Ws",
            "WALA","L_WALA","bffFlat_grad",
            "OUTLET_DAMS_red2","elev_relief_m",
            "multi_dam","dam_ht_m","damht_zmax_full",
            "PCT_AG_URB_BSN","PCT_AG_URB_CAT","PctDEVELOPED_Cat",
            "PctIrrigated.AgLandCat","PctAgDrainageCat",
            "hap_rank_9")

nla07_red<-nla07_mod%>%
  select(all_of(myvars07))
#n = 1123 w/141 vars

##########
## NLA 2012
nla12_mod<-nla12_full%>%
  rename(AMMONIA_N_RESULT_mgL=AMMONIA_N_RESULT,
         ANC_RESULT_ueqL=ANC_RESULT,
         CALCIUM_RESULT_mgL=CALCIUM_RESULT,
         CHLORIDE_RESULT_mgL=CHLORIDE_RESULT,
         COLOR_RESULT_PtCo=COLOR_RESULT,
         COND_RESULT_uscm=COND_RESULT,
         DOC_RESULT_mgL=DOC_RESULT,
         MAGNESIUM_RESULT_mgL=MAGNESIUM_RESULT,
         NITRATE_N_RESULT_mgL=NITRATE_N_RESULT,
         NITRATE_NITRITE_N_RESULT_mgL=NITRATE_NITRITE_N_RESULT,
         NTL_RESULT_mgL=NTL_RESULT,
         POTASSIUM_RESULT_mgL=POTASSIUM_RESULT,
         PTL_RESULT_ugL=PTL_RESULT,
         SILICA_RESULT_mgL=SILICA_RESULT,
         SODIUM_RESULT_mgL=SODIUM_RESULT,
         SULFATE_RESULT_mgL=SULFATE_RESULT,
         TOC_RESULT_mgL=TOC_RESULT,
         TURB_RESULT_NTU=TURB_RESULT)%>%
  mutate(L_NTL=log10(NTL_RESULT_mgL),
         L_PTL=log10(PTL_RESULT_ugL),
         L_CHLA=log10(CHLL_RESULT_ugL),
         WALA=BASINAreaSqKM/LKAREA_KM2_mod,
         L_WALA=log10(WALA))

myvars12<-c("SITE_ID","VISIT_NO","YEAR","UID","UNIQUE_ID","DATE_COL_iso",
            "dD","d18O","d_excess","E_I","RT_iso",
            "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use",
            "LKAREA_KM2_mod","L_LKAREA_KM2_mod","DpthMx_mod","L_DpthMx_mod",
            "SIXDEPTH",#"DpthMx_use","L_DpthMx_use",
            "PERIM_KM","Volume_Corrected_m3","ELEV_use","L_ELEV_use",
            "LATdd_use","LONdd_use", "XCOORD","YCOORD",
            "STATE","URBAN","Lake_Origin_use","lk_hydro_iso",
            "NARS_NAME","ECOWSA9_2015",
            "ECOP5_2015","ECOP6_2015","SITETYPE",
            #"RT_NLA12_2015",
            "AMMONIA_N_RESULT_mgL","ANC_RESULT_ueqL",
            "CALCIUM_RESULT_mgL","CHLORIDE_RESULT_mgL","COLOR_RESULT_PtCo","COND_RESULT_uscm",
            "DOC_RESULT_mgL","MAGNESIUM_RESULT_mgL","NITRATE_N_RESULT_mgL","NITRATE_NITRITE_N_RESULT_mgL","NTL_RESULT_mgL", # NOTE - using the transformed value
            "PH_RESULT","POTASSIUM_RESULT_mgL","PTL_RESULT_ugL","SILICA_RESULT_mgL","SODIUM_RESULT_mgL","SULFATE_RESULT_mgL",
            "TURB_RESULT_NTU", #"TOC_RESULT_mgL",
            "CHLL_RESULT_ugL","SECCHI_m","L_NTL","L_PTL","L_CHLA",
            "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
            "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE",
            "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",
            "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA",
            "RVegQc15","LitCvrQc15","LitRipCvrQc15",
            "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
            "BASINAreaSqKM",
            "PCT_OPEN11_BSN","PCT_ICE11_BSN","PCT_DEVOPEN11_BSN","PCT_DEVLOW11_BSN","PCT_DEVMED11_BSN","PCT_DEVHIGH11_BSN",
            "PCT_BARREN11_BSN","PCT_DECID11_BSN","PCT_CONIF11_BSN","PCT_MIXED11_BSN","PCT_SHRUB11_BSN","PCT_GRASS11_BSN","PCT_PASTURE11_BSN","PCT_CROPS11_BSN","PCT_WDYWET11_BSN","PCT_EMHERBWET11_BSN",
            "PCT_FOREST11_BSN","PCT_WET11_BSN","PCT_AGR11_BSN","PCT_DEVELOPED11_BSN",
            "ELEVMAX_BSN_m","POPDEN_BSN","OUTLET_DAMS",
            "BFIWs","OmWs","PermWs","RckdepWs","WtDepWs",
            "AvgWetIndxWs","DamNrmStorWs","DamNIDStorWs","RdDensWs",
            "Precip8110Ws","Tmean8110Ws","Tmax8110Ws","Tmin8110Ws",
            "WALA","L_WALA","bffFlat_grad",
            "OUTLET_DAMS_red2","elev_relief_m",
            "multi_dam","dam_ht_m","damht_zmax_full",
            "PCT_AG_URB_BSN","PCT_AG_URB_CAT","PctDEVELOPED_Cat",
            "PctIrrigated.AgLandCat","PctAgDrainageCat",
            "hap_rank_9")

nla12_red<-nla12_mod%>%
  select(all_of(myvars12))


##########
## NLA 2017
nla17_mod<-nla17_full%>%
  mutate(L_NTL=log10(NTL_mgL),
         L_PTL=log10(PTL_ugL),
         L_CHLA=log10(CHLA_ugL),
         PCTFOREST2011=PCTCONIF2011+PCTDECID2011+PCTMXFST2011,
         PCTWETLAND2011=PCTWDWET2011+PCTHBWET2011,
         PCTAGR2011=PCTCROP2011+PCTHAY2011,
         PCTDEVELOPED2011=PCTURBOP2011+PCTURBLO2011+PCTURBMD2011+PCTURBHI2011)%>%
  mutate(DATE_COL=mdy(DATE_COL))%>%
  mutate(YEAR=case_when(!is.na(YEAR)~YEAR,
                        TRUE~2017))

str(nla17_mod$DATE_COL)

myvars17<-c("SITE_ID","VISIT_NO","YEAR","UID","UNIQUE_ID","DATE_COL",
            "dD","d18O","d_excess","E_I","RT_iso",
            "VERTDD_use","HORIZDD_use","L_VERTDD_use","L_HORIZDD_use",
            "LkArea_km2","L_LkAreakm2",
            "DpthMx_use","L_DpthMx_use","SIXDEPTH",
            "PERIM_KM","Lk_vol_m3","ELEV_use","L_ELEV_use",
            "LATdd_use","LONdd_use", "XCOORD","YCOORD",
            "STATE","URBN_NLA17","Lake_Origin_use","lk_hydro_iso",
            "NARS_NAME","ECOWSA9_2015",
            "ECOP5_2015","ECOP6_2015","SITETYPE",
            #"RT_NLA17",
            "AMMONIA_N_mgL","ANC_ueqL",
            "CALCIUM_mgL","CHLORIDE_mgL","COLOR_ALPHA","COND_USCM",
            "DOC_mgL","MAGNESIUM_mgL","NITRATE_N_mgL","NITRATE_NITRITE_N_mgL","NTL_mgL", # NOTE - using the transformed value
            "PH","POTASSIUM_mgL","PTL_ugL","SILICA_mgL","SODIUM_mgL","SULFATE_mgL",
            "TURB_NTL",#"TURB_NTU"
            "CHLA_ugL","SECMEAN","L_NTL","L_PTL","L_CHLA",
            "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
            "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE",
            "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",
            "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA",
            "RVegQc15","LitCvrQc15","LitRipCvrQc15",
            "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
            "WSAREASQKM",
            "PCTOW2011","PCTICE2011","PCTURBOP2011","PCTURBLO2011","PCTURBMD2011","PCTURBHI2011",
            "PCTBL2011","PCTDECID2011","PCTCONIF2011","PCTMXFST2011","PCTSHRB2011","PCTGRS2011","PCTHAY2011","PCTCROP2011","PCTWDWET2011","PCTHBWET2011",
            "PCTFOREST2011","PCTWETLAND2011","PCTAGR2011","PCTDEVELOPED2011",
            "MaxElev","POPDEN2010","OUTLET_DAMS",
            "BFI","OM","PERM","RCKDEP","WTDEP",
            "WETINDEX","NrmStorM3","NIDStorM3","RDDENS",
            "PRECIP8110","TMEAN8110","TMAX8110","TMIN8110",
            "WALA","L_WALA","bffFlat_grad",
            "OUTLET_DAMS_red2","elev_relief_m",
            "multi_dams","NID_height","damht_zmax_full",
            "PCT_AG_URB_BSN","PCT_AG_URB_200","PCT_DEVELOPED_200", #"PCT_AG_URB_CAT",
            "IrrAg_pct","AgDrain_pct",
            "hap_rank_9")

nla17_red<-nla17_mod%>%
  select(all_of(myvars17))

################
## CHECK COLUMN NAMES ACROSS NLA SURVEYS
# Check order
var07<-as.data.frame(names(nla07_red))
var12<-as.data.frame(names(nla12_red))
var17<-as.data.frame(names(nla17_red))

write_csv(var07, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/var07_check.csv")
write_csv(var12, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/var12_check.csv")
write_csv(var17, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/var17_check.csv")

###############
## RELABEL COLUMN NAMES TO MATCH
###
# SUBSET DATA
# Variables to relabel based on nla2017 names
nla07_red2<-nla07_red%>%
  select(c(1:89,111:139))
nla12_red2<-nla12_red%>%
  select(c(1:89,111:139))
nla17_red2<-nla17_red%>%
  select(c(1:89,111:139))

# Rename to 2017 names
names(nla07_red2)<- names(nla17_red2)
names(nla12_red2)<- names(nla17_red2)

#######
# Rename NLCD vars to 2007 names
# Select NLCD vars
nla07_red3<-nla07_red%>%
  select(c(90:110))
nla12_red3<-nla12_red%>%
  select(c(90:110))
nla17_red3<-nla17_red%>%
  select(c(90:110))

# Rename to 2007 NLCD names
names(nla12_red3)<- names(nla07_red3)
names(nla17_red3)<- names(nla07_red3)

######################
## BIND COLUMNS BY NLA SURVEY
nla07_bind<-bind_cols(nla07_red2,nla07_red3)
nla12_bind<-bind_cols(nla12_red2,nla12_red3)
nla17_bind<-bind_cols(nla17_red2,nla17_red3)

################
## BIND ROWS TO COMBINE ALL THREE NLA SURVEYS
#  n=3471 obs w/140 variables
nla_bind<-bind_rows(nla07_bind,nla12_bind,nla17_bind)

table(nla_bind$YEAR)
#2007 2012 2017
#1123 1138 1210


##############
## MERGE PROCESSED NLA with MMI
nla_mmi<-left_join(nla_bind,mmi_red,
                   by=c("SITE_ID","VISIT_NO","YEAR"))

################
# CLEAN DATA
# Change lake type to be consistent
table(nla_mmi$Lake_Origin_use)
nla_mmi<-nla_mmi%>%
  mutate(Lake_Origin_use = case_when(Lake_Origin_use %in% c("MAN-MADE")~"MAN_MADE",
                                     Lake_Origin_use %in% c("NATURAL")~"NATURAL",
                                     TRUE ~ Lake_Origin_use))
#MAN_MADE  NATURAL
#1907     1529

####################
## WRITE COMPILED NLA 07,12,17 data
####################
write_csv(nla_bind,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/nla071217_resampled.csv")

# COLUMN NAMES
var_names<-data.frame(colnames(nla_bind))
write_csv(var_names,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_NLA_MMI_project/Processed_data/nla071217_vars.csv")


#################
## CHECK DATA
# Number of unique observations by survey
check<-nla_mmi%>%
  group_by(YEAR)%>%
  mutate(uniq_sites=length(unique(SITE_ID)))%>%
  select(YEAR,uniq_sites)
table(check$uniq_sites,check$YEAR)
#     2007 2012 2017
#1028 1123    0    0
#1038    0 1138    0
#1113    0    0 1210

#####
table(nla_mmi$Lake_Origin_use)
# Need to relabel MAN_MADE

## Check chemistry ranges
chem<-nla_mmi%>%
  group_by(YEAR)%>%
  filter(!is.na(NTL_mgL))%>%
  filter(!is.na(PTL_ugL))%>%
  summarise(meanTN=mean(NTL_mgL),
            minTN=min(NTL_mgL),
            maxTN=max(NTL_mgL),
            meanTP=mean(PTL_ugL),
            maxTP=max(PTL_ugL),
            minTP=min(PTL_ugL))

# Look for NAs
################
## COUNT NUMBER OF MISSING OBS
na_count <-nla_mmi %>%
  summarise(across(everything(),~sum(is.na(.))))

t<- as.data.frame(t(na_count))
na_count_long<-t%>%
  filter(V1>0)
na_count_long

# Look for zero values
zero_count<-sapply(nla_mmi, function(y)sum(length(which(y==0))))
zero_count<-data.frame(zero_count)

##
missing_vert<-nla_mmi%>%
  filter(is.na(L_VERTDD_use))%>%
  filter(YEAR==2017)%>%
  select(SITE_ID,VISIT_NO,YEAR,VERTDD_use,L_VERTDD_use)

# There are 598 obs L_VERTDD_use as NA
# 498 of these observations are from 2017 and have VERTDD_use=0
#  Need to ask Phil if we should treat these as 0 - can we add +0.1 and log10 transform?

#Missing E:I
missing_EI<-nla_mmi%>%
  filter(is.na(E_I))%>%
  select(SITE_ID,VISIT_NO,YEAR,DpthMx_use,dD,E_I)

# Missing HydrAP
missing_hydrap<-nla_mmi%>%
  filter(is.na(hap_rank_9))%>%
  select(SITE_ID,VISIT_NO,YEAR,DpthMx_use,damht_zmax_full,NID_height,hap_rank_9)
table(missing_hydrap$YEAR)
#2007 2012 2017
#129  170    3

# Missing MMI_BENT
missing_mmib<-nla_mmi%>%
  filter(is.na(MMI_BENT))%>%
  select(SITE_ID,VISIT_NO,YEAR,MMI_BENT)

table(missing_mmib$YEAR)
#2007 2012 2017
#187  130   79
