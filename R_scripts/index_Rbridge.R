############################################

#QUANTITATIVE INDICES

################## MANAGEMENT BENEFIT ##################

### Step 1: Load and initialize the arcgisbinding

library(arcgisbinding)
arc.check_product()


### Step 2: Load packages into the R workspace

library(sp)
library(dplyr)
library(tidyverse)
#library(readr)
library(sf)

### Step 3: Set working directory

setwd("G:\\fnaExternal")

### Step 4: Load the datasets of interest

# gis_data <- arc.open(path = '    ')
gis_huc <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\huc_proj')
huc_r <- arc.select(gis_huc)
# huc is our R dataframe of the huc_proj arc gdb feature class 

################### MANAGEMENT BENEFIT INDEX ###############################

d1 <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\firedepVeg_evt_huc_table')
d2 <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\firedepVeg_evt_huc_coa_table')

### Step 5: Translate arc file into R file. Can select a subset data from the loaded dataset

#r_data <- arc.select(gis_data, fields, SQL, spatial ref)

ch <- arc.select(d1)
chc <- arc.select(d2, where_clause = "gridcode <> 0 AND FID_Conservation_Opportunity_Areas__Terrestrial_and_Lake_ <> -1")  
chc <- chc %>% filter(gridcode != 9202 & gridcode != 9203 & gridcode != 9101 & gridcode != 9100 & gridcode != 9119 & gridcode != 9115 & gridcode != 9064 & gridcode != 9113)

# Summarize EVT within HUC 

ch <- rename(ch, Area_Within_Huc = Evt_Area)
ch <- ch %>% drop_na(Area_Statewide) 
ch <- within(ch, Prop_Huc <- as.numeric(Area_Within_Huc/Area_Statewide))
head(ch)

ch$Id <- paste(ch$HUC12Num, ch$gridcode, sep = "_") #creates index HUc_Evt, use as join column


# Set up EVT within HUC X COA dataset ("chc")


chc$Id_2 <- paste(chc$HUC12Num, chc$gridcode, sep = "_") #creates index HUc_CommunityGroup, use a join column

chcId <- chc %>% 
  group_by(Id_2) %>% 
  summarise(Area_Coa_Sum = sum(Area_Within_Coa)) %>%   
  arrange(desc(Area_Coa_Sum))

#chcGroup10 <- chcId %>% slice (1:10)  # preview top ten values
#print(chcGroup10)

# Join datasets using Id = Id_2. (each row represents a unique HUC x EVT combination)

# left join keeps all rows from x (ch), and all columns from x and y (chc)
join <- ch %>% left_join(chcId, by = c("Id" = "Id_2"))
# drop un-needed fields
join <- select (join,-c(FREQUENCY))


# calculate benefit index

join <- within(join, Prop_Coa <- as.numeric(Area_Coa_Sum/Area_Within_Huc))
join <- join %>% replace(is.na(.), 0) # replace NA with 0

join <- within(join, bIndex <- as.numeric((Prop_Coa+1)*Prop_Huc))

# calculate ecological benefit

d <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\naturalVeg_table')
attr <- arc.select(d)  
rarity <- attr %>% select(EVT_VALUE,Rarity)

joinBenefit <- join %>% left_join(rarity, by = c("gridcode" = "EVT_VALUE"))  

hucBen <- joinBenefit %>% 
  group_by(HUC12Num) %>% 
  summarise(Area_Natural = sum(Area_Within_Huc)) %>%  
  arrange((Area_Natural))

joinBenefit <- joinBenefit %>% left_join(hucBen, by = c("HUC12Num" = "HUC12Num")) # sum of natural area
joinBenefit <- within(joinBenefit, Weight <- as.numeric(Area_Within_Huc/Area_Natural))
joinBenefit <- within(joinBenefit, benefit_weight <- as.numeric(Weight*bIndex))
joinBenefit <- within(joinBenefit, rarity_weight <- as.numeric(Weight*Rarity))

# summarize by HUC  (combines all natural community groups), ecol scale 1-25 (each index is 5)

benefit_HUC <- joinBenefit %>%
  group_by(HUC12Num) %>% 
  summarise(benefit_step = (sum(benefit_weight, na.rm=TRUE)), Rarity_sum = sum(rarity_weight), Area_Fire_Sum = sum(Area_Within_Huc), Area_Coa_TotalSum = sum(Area_Coa_Sum), Weight_total = sum(Weight)) %>% 
  arrange(desc(benefit_step))
benefit_HUC <- within(benefit_HUC, benefit_Index <- as.numeric(benefit_step/max(benefit_step)*5))
benefit_HUC <- within(benefit_HUC, ecological_Index <- as.numeric(benefit_Index*Rarity_sum))


############# CREATE HUC INDEX SUMMARIES FEATURE CLASS ##############


# EXPORT FILE GBD TO ARCGIS

# 1 -- Join huc with management benefit index
tempJoin <- huc_r %>% full_join(benefit_HUC, by = c("HUC12Num" = "HUC12Num")) 

#if not already loaded above.. 
gis_huc <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\huc_proj')
huc_r <- arc.select(gis_huc)

# huc is our R dataframe of the huc_proj arc gdb feature class 

arc.shapeinfo(arc.shape(huc_r))
#geometry type   : Polygon
#WKT             : PROJCS["NAD_1983_Contiguous_USA_Albers",GEOGCS["GCS_North_Am...
#WKID            : 5070 

#arc.write (path, R_dataframe, shapeinfo = arc.shapeinfo(arc_dataset))
#arc.write('G:/fnaExternal/fna.gdb/wi_benefit_index', data = tempJoin, shape_info = arc.shapeinfo(gis_huc))   # gis_huc is arc.dataset class object of original huc_proj gdb fc

#write as geodatabase feature class
benefit.sf <- arc.data2sf(tempJoin)
out_fc = paste0('G:\\fnaExternal\\fna.gdb\\index_benefit')
arc.write(out_fc, benefit.sf,  overwrite=TRUE) 

# To write as a table:
#arc.write(path, data, {coords}, {shape_info}, {validate}, overwrite = FALSE)
arc.write(path = 'G:\\fnaExternal\\fna.gdb\\index_benefit_ecol', data = benefit_HUC, overwrite=TRUE)


#or...
#write_csv(benefit_HUC, "benefit_index_HUC.csv")




#################### MANAGEMENT EFFORT INDEX  ###################################


# use dataframe ch from benefit index. otherwise

#d1 <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\firedepVeg_evt_huc_table')
#ch <- arc.select(d1)

# import dataframe of just hiFire needs areas (mFRI <34)
hiFire_gis <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\huc_34')
hiFire_r <- arc.select(hiFire_gis)

# All natural area: 
chEffort <- select (ch,-c(FREQUENCY,Area_Statewide,Prop_Huc))

# Join to acquire FRI attributes. Join to acquire total Huc area --

# use attr dataframe from above. otherwise: 
#d <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\naturalVeg_table')
#attr <- arc.select(d)  # warning: contains all attributes 

# use dataframe huc_r from benefit index. otherwise
#gis_huc <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\huc_proj')  # should have from above
#huc_r <- arc.select(gis_huc)

attrSub <- attr %>% select(EVT_NAME,EVT_VALUE,WI_Nat_Comm,Rarity,BPS_NAME,FRI_ALLFIR) # subset of attributes for workability. 
attrSub$FRI_ALLFIR <- as.numeric(attrSub$FRI_ALLFIR)  # default from landfire attribute table is text. be careful to check data type! 

joinEffort <- chEffort %>% left_join(attrSub, by = c("gridcode" = "EVT_VALUE"))  
Effort <- joinEffort %>% filter(FRI_ALLFIR>0)  #remove areas where FRI = 0 

hiEffort <- Effort # save for below

hucEffort <- Effort %>% 
  group_by(HUC12Num) %>% 
  summarise(Area_Natural = sum(Area_Within_Huc)) %>%  
  arrange((Area_Natural))

Effort <- Effort %>% left_join(hucEffort, by = c("HUC12Num" = "HUC12Num")) # sum of natural area
Effort <- Effort %>% left_join(huc_r, by = c("HUC12Num" = "HUC12Num")) # total huc area

# calculate weighted average mFRI. (sum next step)

Effort <- within(Effort, Weight <- as.numeric(Area_Within_Huc/Area_Natural))
Effort <- within(Effort, FRI_weight <- as.numeric(Weight*FRI_ALLFIR))

# summarize by HUC

effort_HUC <- Effort %>%
  group_by(HUC12Num) %>% 
  summarise(FRI_Huc = sum(FRI_weight), all_Fire_Area = sum(Area_Within_Huc)) %>%  # FRI_huc = mFRIk for k HUC unit
  arrange(desc(FRI_Huc))
effort_HUC <- within(effort_HUC, Fire_Need <- as.numeric(all_Fire_Area/(FRI_Huc)))  # essentially Area of Fire-Dependent Veg * 1/mFRIk for k HUc unit = annual fire need 
effort_HUC <- within(effort_HUC, Fire_Need_Ac <- as.numeric(Fire_Need/4046.86))

# create subset of layer identifying high fire needs areas (e.g. FRI < 35)

hiEffort <- hiEffort %>% filter(FRI_ALLFIR<35)  # also, FRI_ALLFIR>0

# summarize to calculate hi-fire-dep area
hucHiEffort <- hiEffort %>% 
  group_by(HUC12Num) %>% 
  summarise(hi_Fire_Area = sum(Area_Within_Huc)) %>%  
  arrange((hi_Fire_Area))
hiEffort <- hiEffort %>% left_join(hucHiEffort, by = c("HUC12Num" = "HUC12Num")) # sum of fire-dep area
hiEffort <- hiEffort %>% left_join(huc_r, by = c("HUC12Num" = "HUC12Num")) # total huc area

# calculate weighted average mFRI. (sum next step)

hiEffort <- within(hiEffort, Weight <- as.numeric(Area_Within_Huc/hi_Fire_Area)) #proportion of hi-fire area
hiEffort <- within(hiEffort, FRI_weight <- as.numeric(Weight*FRI_ALLFIR)) #weighted fri, hi-fire-dep only

# summarize by HUC

hiEffort_HUC <- hiEffort %>%
  group_by(HUC12Num) %>% 
  summarise(hiFRI_Huc = sum(FRI_weight), hiFire_Area = sum(Area_Within_Huc)) %>%  # FRI_HUC = mFRIk for k HUC unit
  arrange(desc(hiFRI_Huc))
hiEffort_HUC <- within(hiEffort_HUC, hiFire_Need <- as.numeric(hiFire_Area/(hiFRI_Huc)))  # essentially Area of hi-Fire-Dependent Veg * 1/mFRIk for k HUc unit = annual fire need 

# join with external dataset hiFire_r to gain patch number per huc
hiEffort_HUC <- hiEffort_HUC %>% left_join(hiFire_r, by = c("HUC12Num" = "HUC12Num")) # hiFire_r has Huc_Area
hiEffort_HUC <- within(hiEffort_HUC, effort_index1 <- as.numeric(Join_Count/Huc_Area)) # patches per unit area
hiEffort_HUC <- within(hiEffort_HUC, effort_index2 <- as.numeric(effort_index1/FRI_Huc)) #hiFRI_HUC
hiEffort_HUC <- within(hiEffort_HUC, effort_index <- as.numeric(effort_index2/max(effort_index2)*5)) # normalize, scale 1-5
hiEffort_HUC <- select (hiEffort_HUC,-c(OBJECTID,effort_index1, effort_index2))

################################################
######## MESSY       HI FIRE ECOLOGICAL BENEFIT        #####################
####################################################


# hiEffort, calculate hiFire ecological index
b <- joinBenefit %>% select(bIndex, Area_Coa_Sum, Id)
hiFire <- hiEffort%>% left_join(b, by = c("Id" = "Id"))
#Weight ~ proportion of hi-fire area
hiFire<- within(hiFire, benefit_weight <- as.numeric(Weight*bIndex))
hiFire<- within(hiFire, rarity_weight <- as.numeric(Weight*Rarity))

# summarize by HUC  (combines all natural community groups), ecol scale 1-25 (each index is 5)

rx_benefit_HUC <- hiFire %>%
  group_by(HUC12Num) %>% 
  summarise(benefit_step = (sum(benefit_weight, na.rm=TRUE)), Rarity_sum = sum(rarity_weight), Area_Fire_Sum = sum(Area_Within_Huc), Area_Coa_TotalSum = sum(Area_Coa_Sum), Weight_total = sum(Weight)) %>% 
  arrange(desc(benefit_step))
rx_benefit_HUC <- within(rx_benefit_HUC, rx_benefit_Index <- as.numeric(benefit_step/max(benefit_step)*5))
rx_benefit_HUC <- within(rx_benefit_HUC, rx_ecological_Index <- as.numeric(rx_benefit_Index*Rarity_sum))

############# UPDATE HUC INDEX SUMMARIES FEATURE CLASS ##############


# 2 -- Join huc with management effort index
tempJoin <- tempJoin %>% full_join(effort_HUC, by = c("HUC12Num" = "HUC12Num")) 
tempJoin <- tempJoin %>% full_join(hiEffort_HUC, by = c("HUC12Num" = "HUC12Num")) 
tempJoin <- tempJoin %>% full_join(rx_benefit_HUC, by = c("HUC12Num" = "HUC12Num")) 
eff_huc <- tempJoin

#write as geodatabase feature class
effort.sf <- arc.data2sf(eff_huc)
out_fc = paste0('G:\\fnaExternal\\fna.gdb\\index_effort')
arc.write(out_fc, effort.sf, overwrite=TRUE)  #effort.sf

#write as table
effort.df <- effort.sf %>% st_drop_geometry()
arc.write(path = 'G:\\fnaExternal\\fna.gdb\\index_effort', data = effort_HUC, overwrite = TRUE)


################### MANAGEMENT FEASIBILITY ##############################


# Use previously calculated summaries for fire-dependent vegetation

head(tempJoin) # Area_Fire_Sum = area of fire-dep veg within a given huc

# bring in evt-huc-wui table, output from union
d <- arc.open(path = 'G:\\fnaExternal\\fna.gdb\\firedepVeg_evt_huc_wui_table')
cw <- arc.select(d, where_clause = "gridcode <> 0 AND FID_wi_wui10_only <> -1")  # select only areas that are fire dependent vegetation and overlap with wui


# Set up EVT within HUC X WUI dataset ("cw")

#group by HUC units
hucFeas<- cw %>% 
  group_by(HUC12Num) %>% 
  summarise(Area_Wui_Sum = sum(Area_Within_Wui)) %>%   
  arrange(desc(Area_Wui_Sum))

# Join datasets using HUC12Num. 

feas_HUC <- tempJoin %>% left_join(hucFeas, by = c("HUC12Num" = "HUC12Num"))

feas_HUC <- within(feas_HUC, Prop_Wui <- as.numeric(Area_Wui_Sum/all_Fire_Area)) # proportion of fire-dep veg that overlaps wui
feas_HUC <- feas_HUC %>% replace(is.na(.), 0)

# calculate feasibility index 

feas_HUC <- within(feas_HUC, feas_Index <- as.numeric(1-(Prop_Wui*0.5)))
feas_HUC <- within(feas_HUC, feas_Index <- feas_Index*5) # scale 1-5

#write as geodatabase feature class
feas.sf <- arc.data2sf(feas_HUC) # feas_HUC is arc.data which is unusable. convert to simple feature, sf, which will retain geom
out_fc = paste0('G:\\fnaExternal\\fna.gdb\\index_feasibility')
arc.write(out_fc, feas.sf, overwrite=TRUE)

#write as table
feas.df <- feas.sf %>% st_drop_geometry()   # convert sf to dataframe to export as table
arc.write(path = 'G:\\fnaExternal\\fna.gdb\\index_feasibility', data = feas.df, overwrite=TRUE)

############# UPDATE HUC INDEX SUMMARIES FEATURE CLASS ##############


# 3 -- Join huc with management feasibility index
tempJoin <- feas_HUC



################################ CALCULATE COMPREHENSIVE PRIORITIZATION ############################################

############ Ecological Benefit = Management Benefit * Rarity [calculated with benefit_Index]
############ Maximum Ecological Benefit with Minimum Effort ## 

tempJoin$effort_index <- replace(tempJoin$effort_index, tempJoin$effort_index == 0, NA)
tempJoin <- within(tempJoin, benefit_effort_Index <- as.numeric(ecological_Index/effort_index)) # NA values where HUCs have no natural area
tempJoin <- within(tempJoin, rx_benefit_effort_Index <- as.numeric(rx_ecological_Index/effort_index)) # NA values where HUCs have no natural area
tempJoin <- within(tempJoin, Prop_hiFire <- as.numeric(hiFire_Area/Huc_Area.y))
tempJoin <- within(tempJoin, rx_be_weighted_index <- as.numeric((rx_ecological_Index/effort_index)*Prop_hiFire))
tempJoin <- within(tempJoin, ecol_over_fri <- as.numeric(ecological_Index/FRI_Huc))
tempJoin <- within(tempJoin, Prop_Natural <- as.numeric(all_Fire_Area/Huc_Area.y))
tempJoin <- within(tempJoin, ecol_weighted_index <- as.numeric(ecological_Index*Prop_Natural))

########### Comprehensive Prioritization 

Indices <- within(tempJoin, Comp <- as.numeric(feas_Index*ecological_Index/effort_index))
Indices <- within(tempJoin, Comp_rx <- as.numeric(feas_Index*rx_ecological_Index/effort_index)) 





write_csv(Indices, "wi_indices_nov8.csv")


################################# JOIN FIELDS, EXPORT AS GEODATABASE FEATURE CLASS ##################################


#confirm datatype. does the datatype have spatial geometry?

class(Indices)
#[1] "arc.data"   "data.frame"

arc.shapeinfo(arc.shape(Indices))    #arc.shapeinfo will work with an arc.dataset class, but not with arc.data class <- think of arc.dataset as an ArcGIS file, arc.data is a intermediate between Arc and R. Needs to be converted to other datatypes to use
#geometry type   : Polygon 
#WKT             : PROJCS["NAD_1983_Contiguous_USA_Albers",GEOGCS["GCS_North_Am...
#WKID            : 5070 


# write to new gdb feature class
indices.sf <- arc.data2sf(Indices)
out_fc = paste0('G:/fnaExternal/fna.gdb/wi_indices_nov8')
arc.write(out_fc, indices.sf, overwrite=TRUE)

#write as table
indices.df <- indices.sf %>% st_drop_geometry()   # convert sf to dataframe to export as table
arc.write(path = 'G:\\fnaExternal\\fna.gdb\\wi_indices_table_nov8', data = indices.df, overwrite = TRUE)




