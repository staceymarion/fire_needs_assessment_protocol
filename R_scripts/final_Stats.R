# Fire regime summaries

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

final <- read_csv("d:\\wiModifiedEvt.csv") # final fire dependent vegetation areas  (joinAug10.csv = export from raster.  new evt classification, joined with all evt, bps attributes + count data)

# create variable p as percent of total count, per row
final$sum <- sum(final$Count)
print(final$sum)

final <- within(final, p <- as.numeric(Count/final$sum*100))
head(final)
head(final$p)

# Calculate annual burning needs

# annualAll ~ based on mean FRI (FRI_ALLFIR), expected annual burning needs (units = acres)

finalFRI <- final %>% 
  group_by(EVT_NAME) %>% 
  summarise(WI_Nat_Comm, area = sum(Count)*30*30, acres = sum(Count)*30*30/4046.86, FRI_ALLFIR_1, annualALL = (acres/FRI_ALLFIR_1), FRI_REPLAC_1, PRC_REPLAC_1, annualREPLAC = (annualALL*PRC_REPLAC_1/100), FRI_MIXED_1, PRC_MIXED_1, annualMIXED = (annualALL*PRC_MIXED_1/100), FRI_SURFAC_1, PRC_SURFAC_1, annualSURFAC = (annualALL*PRC_SURFAC_1/100)) %>%   
  arrange(desc(annualALL))
finalFRI <- finalFRI %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>% replace(is.na(.), 0) # remove Inf (0/Value; replace with 0)
finalFRI <- finalFRI %>% arrange(desc(annualALL)) # re-sort 
print(finalFRI)


write_csv(finalFRI, "d:\\finalFRIAug15.csv")

final10 <- finalFRI %>% slice (1:10) %>% 
  arrange(desc(annualALL))

p <- ggplot(final10) +  geom_bar(aes(x = reorder(EVT_NAME, annualALL), y = annualALL), stat = 'identity') + scale_y_continuous(limits=c(0,250000))
p + coord_flip()
