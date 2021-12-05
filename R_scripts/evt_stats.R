# EVT summaries
# Our analysis used EVT LANDFIRE 2016, version 2.0.0 (i.e. 200)

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

evt <- read_csv("G:\\fnaExternal\\wiModEvtD.csv") # csv is exported attribute table from the modified wisconsin EVT raster layer

# create variable p as percent of total count, per row
evt$sum <- sum(evt$Count)
print(evt$sum)

evt <- within(evt, p <- as.numeric(Count/evt$sum*100))
head(evt)
head(evt$p)

# Primary summaries of interest are the most abundant evts and evt vegetation groups present on today's landscape.
# In Wisconsin, for example, much of the landscape is represented by Developed, Agricultural, Ruderal, or Exotic communities

# Summarize by most common evt's
evtClean <- evt %>% 
  group_by(EVT_NAME) %>% 
  summarise(area = sum(Count)*30*30, acres = sum(Count)*30*30/4046.86, perArea = sum(p), EVT_PHYS) %>%   ## 30x30 M resolution. 1 acre = 4046.86 sqm Output = acres
  arrange(desc(area))
print(evtClean)

evt10 <- evtClean %>% slice (1:10)
print(evt10)

p <- ggplot(evt10) +  geom_bar(aes(x = reorder(EVT_NAME, perArea), y = perArea), stat = 'identity') + scale_y_continuous(limits=c(0,20))
p + coord_flip()

# Summarize by evt vegetation groups (e.g. developed, agricultural, exotic, riparian, etc.)
evtGroup <- evt %>%  
  select (everything()) %>%
  mutate(Group = case_when (
    EVT_PHYS == "Agricultural" ~ "Agricultural",
    EVT_PHYS == "Riparian" ~ "Riparian",
    EVT_PHYS == "Hardwood" ~ "Hardwood",
    EVT_PHYS == "Developed" ~ "Ruderal",  # see EVT descriptions.  "Developed" includes vegetated communities with no historical reference, e.g. ruderal
    EVT_PHYS == "Conifer" ~ "Conifer",
    EVT_PHYS == "Conifer-Hardwood" ~ "Conifer-Hardwood",
    EVT_PHYS == "Open Water" ~ "Open Water",
    EVT_PHYS == "Developed-Roads" ~ "Developed",
    EVT_PHYS == "Exotic Herbaceous" ~ "Exotic",
    EVT_PHYS == "Developed-Low Intensity" ~ "Developed",
    EVT_PHYS == "Developed-Medium Intensity" ~ "Developed",
    EVT_PHYS == "Developed-High Intensity" ~ "Developed",
    EVT_PHYS == "Grassland" ~ "Grassland",
    EVT_PHYS == "Exotic Tree-Shrub" ~ "Exotic",
    EVT_PHYS == "Shrubland" ~ "Shrubland",
    EVT_PHYS == "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads" ~ "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads",
    EVT_PHYS == "Sparsely Vegetated" ~ "Sparsely Vegetated"
    ))

evtGroup <- evtGroup %>% 
  group_by(Group) %>% 
  summarise(area = sum(Count)*30*30, acres = sum(Count)*30*30/4046.86, perArea = sum(p)) %>%   ## 30x30 M resolution. 1 acre = 4046.86 sqm Output = acres
  arrange(desc(area))
print(evtGroup)
write_csv(evtGroup, "d:\\evtAgroup.csv")

evt10 <- evtGroup %>% slice (1:10)

p <- ggplot(evt10) +  geom_bar(aes(x = reorder(Group, perArea), y = perArea), stat = 'identity') + scale_y_continuous(limits=c(0,35))
p + coord_flip()

# Export plot and edit in Adobe Illustrator

