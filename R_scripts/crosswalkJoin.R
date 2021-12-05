library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

# JOIN EVT ATTRIBUTES TO CROSSWALK DOC

evtAtt <- read_csv("G:\\fnaExternal\\wiEVT.csv") 
#cross <- read_csv("G:\\fnaExternal\\crosswalkA.csv")  # use final crosswalk file with the following columns: EVT_NAME (model output), EVT_VALUE (model output), natVeg (manually assigned 0, 1), WI_Nat_Comm (manually assigned), Rarity (manually calculated), BPS_NAME (manually paired)
cross <- read_csv("G:\\fnaExternal\\crosswalkB.csv")  # I ran multiple iterations of a crosswalk for model robustness
#cross <- read_csv("G:\\fnaExternal\\crosswalkC.csv") 
#cross <- read_csv("G:\\fnaExternal\\crosswalkD.csv") 

# left join keeps all rows from x (crosswalk), and all columns from x and y (evt attributes)
# join on evt numerical code (VALUE, EVT_VALUE)


join <- cross %>% left_join(evtAtt, by = c("EVT_VALUE" = "VALUE"))

# Drop unwanted rows

join <- select (join,-c(OBJECTID,VALUE_1,EVT_NAME.y,COUNT,R,G,B,RED,GREEN,BLUE))
#join <- join %>% .[1:108,]  # odd excel bug: spreadsheet has 1000 rows; most empty
print(join)


# Select bps name row with the highest count; use for FRI information ** So, using most abundant bps--zone row for FRI. This is preferable to averaging or selecting just a single zone
# bpsFRI <- bps %>% 
#   group_by(BPS_NAME) %>% 
#   slice(which.max(COUNT))
# print(bpsFRI)
# write_csv(bpsFRI, "d:\\wibpsFRI.csv")

# JOIN BPS ATTRIBUTES

bpsFRI <- read_csv("G:\\fnaExternal\\wibpsFRI.csv") 

# join bpsFRI data to evt table. bpsFRI dataset has a single row per BPS_NAME 
# left join keeps all rows from x (evt), and all columns from x and y (bps)


join2 <- join %>% left_join(bpsFRI, by = c("BPS_NAME" = "BPS_NAME"))

# Drop unwanted rows

join2 <- select (join2,-c(OBJECTID,VALUE,COUNT,OID_1,VALUE_1,R,G,B,RED,GREEN,BLUE,sum,p))
join2 <- join2 %>% .[1:108,]  # remove extra rows (excel bug)
join2 <- rename(join2, EVT_NAME = EVT_NAME.x)  # ditch join appendage 
print(join2)

# FRI will exactly match BpS descriptions with no modification 
# DIRECTLY MODIFIED dataframe to adjust FRI_ALLFIR values as desired.  

#write_csv(join2, "G:\\fnaExternal\\crosswalkJoinA.csv")
write_csv(join2, "G:\\fnaExternal\\crosswalkJoinB.csv")

# csv output has all EVT200 and BPS200 attributes excluding duplicate attributes, count, oid, objectid, and color scheme (Most information is not needed)

