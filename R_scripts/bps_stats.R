# BpS summaries
# Our analysis used BpS LANDFIRE 2016, version 2.0.0 (i.e. 200)

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

bps <- read_csv("G:\\fnaExternal\\wiBPS.csv") # csv is exported attribute table from the wisconsin BpS raster layer

# Part 1 - Area summaries and historical fire regimes

# create variable p as percent of total count, per row.  sum = total count everything combined
bps$sum <- sum(bps$COUNT)
print(bps$sum)

bps <- within(bps, p <- as.numeric(COUNT/bps$sum*100))
head(bps)
head(bps$p)

# Sort data table
# Because our dataset crosses model zones, there are on occasion multiple rows for a BpS for each zone. We want to take the attributes from the BpS-Zone most common in our project extent.
bpsOrder <- bps %>% 
  arrange(desc(BPS_CODE), desc(COUNT))
print(bpsOrder)

# Calculate counts by BpS using our sorted dataset
# bpsClean <- bpsOrder %>% 
#   group_by(BPS_NAME) %>% 
#   summarise(BPS_CODE = first(BPS_CODE), COUNT = sum(COUNT), area = sum(COUNT)*30*30, acres = sum(COUNT)*30*30/4046.86, perArea = sum(p), FRI_ALLFIR = first(FRI_ALLFIR), annual = acres/FRI_ALLFIR, PRC_REPLAC = first(PRC_REPLAC), annRep = annual*PRC_REPLAC, GROUPVEG = first(GROUPVEG)) %>%   ## 30x30 M resolution. 1 acre = 4046.86 sqm Output = acres
#   arrange(desc(area))
# bpsClean <- bpsClean %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>% replace(is.na(.), 0) # get rid of values calculated as Inf (value/0 = Inf)
# print(bpsClean)
# write_csv(bpsClean, "d:\\wiBpsSummarized.csv")

# Select bps name row with the highest count; use for FRI information ** So, using most abundant bps--zone row for FRI. This is preferable to averaging or selecting just a single zone
bpsFRI <- bps %>% 
  group_by(BPS_NAME) %>% 
  slice(which.max(COUNT))
print(bpsFRI)
write_csv(bpsFRI, "G:\\fnaExternal\\wibpsFRI.csv")

# Take a look at just the top 10 most common BpS's i.e. historical vegetation conditions 
bps10 <- bpsClean %>% slice (1:10) %>% 
  arrange(desc(area))
print(bps10)

# plot 
p <- ggplot(bps10) +  geom_bar(aes(x = reorder(BPS_NAME, perArea), y = perArea), stat = 'identity') + scale_y_continuous(limits=c(0,30))
p + coord_flip()

# Export plot and edit in Adobe Illustrator to match colors of map output 

# Part 1B:
# Take a look at where fire occurred most frequently, by major vegetation group
bpsFireGroup <- subset(bpsClean, GROUPVEG %in% c("Conifer", "Grassland", "Hardwood", "Hardwood-Conifer", "Riparian")) %>% 
  group_by(GROUPVEG) %>% 
  summarise(annual = sum(annual)) %>% 
  arrange(desc(annual))  # annual = annual acres burned
print(bpsFireGroup)

p <- ggplot(bpsFireGroup) +  geom_bar(aes(x = reorder(GROUPVEG, annual), y = annual), stat = 'identity') + scale_y_continuous(limits=c(0,2000000))
p + coord_flip()

# Take a look at where fire occurred most frequently, by BpS
bpsFire <- subset(bpsClean, GROUPVEG %in% c("Conifer", "Grassland", "Hardwood", "Hardwood-Conifer", "Riparian")) %>% 
  group_by(BPS_NAME) %>% 
  summarise(annual = sum(annual)) %>% 
  arrange(desc(annual)) # annual = annual acres burned
print(bpsFire)

bpsFire10 <- bpsFire %>% slice (1:10) %>% 
  arrange(desc(annual))  

p <- ggplot(bpsFire10) +  geom_bar(aes(x = reorder(BPS_NAME, annual), y = annual), stat = 'identity') + scale_y_continuous(limits=c(0,2000000))
p + coord_flip()

# Export plot and edit in Adobe Illustrator to match colors of map output 

# Part 2 - Summarize historic fire Severity

# Recode fire regime groups
bps$sev <- recode(as.character(bps$FRG_NEW),
                  "I-A" = "Low/mixed",
                  "I-B" = "Low/mixed",
                  "I-C" = "Low/mixed",
                  "II-A" = "Replacement",
                  "II-B" = "Replacement",
                  "II-C" = "Replacement",
                  "III-A" = "Low/mixed",
                  "III-B" = "Low/mixed",
                  "III-C" = "Low/mixed",
                  "IV-A" = "Replacement",
                  "IV-B" = "Replacement",
                  "IV-C" = "Replacement",
                  "V-A" = "Any/Replacement",
                  "V-B" = "Any/Replacement")

head(bps$sev)


# Percent replacement by veg group - categorical, stacked bar
bpsSev <- bps %>% 
  group_by(GROUPVEG, sev) %>% 
  summarise(perCount = sum(p)) %>% 
  arrange(GROUPVEG)
print(bpsSev, n=nrow(bpsSev))

# subset to remove open water and shrubland which as < 0.1%, and open water which is NA
p <- ggplot(subset(bpsSev, GROUPVEG %in% c("Conifer", "Grassland", "Hardwood", "Hardwood-Conifer", "Riparian"))) +  
  aes(fill = sev, x = reorder(GROUPVEG, perCount), y = perCount) + 
  geom_bar(position="stack", stat = "identity") 
p + coord_flip()

# Export plot; edit colors in Adobe Illustrator to match map output 