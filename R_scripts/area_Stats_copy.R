# Fire regime summaries

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

final <- read_csv("G:\\fnaExternal\\wiModEvtD.csv") # ungeneralized fire dependent vegetation areas  

# create variable p as percent of total count, per row
final$sum <- sum(final$Count)
print(final$sum)

final <- within(final, p <- as.numeric(Count/final$sum*100))
head(final)
head(final$p)

# Calculate annual burning needs

# annualAll ~ based on mean FRI (FRI_ALLFIR), expected annual burning needs (units = acres). note = no generalization

finalFRI <- final %>% mutate(area_un = Count*30*30, acres_un = Count*30*30/4046.86, annualALL_un = (acres_un/FRI_rest), annualREPLAC = (annualALL_un*PRC_REPLAC/100), annualMIXED = (annualALL_un*PRC_MIXED/100), annualSURFAC = (annualALL_un*PRC_SURFAC/100)) 

# finalFRI <- final %>% 
#   group_by(EVT_NAME) %>% 
#   summarise(WI_Nat_Com, area = sum(Count)*30*30, acres_un = sum(Count)*30*30/4046.86, acres_mod = sum(firedepVeg_spc_Shape_Area), FRI_ALLFIR, annualALL_un = (acres_un/FRI_ALLFIR), annualALL_un = (acres_mod/FRI_ALLFIR), FRI_REPLAC, PRC_REPLAC, annualREPLAC = (annualALL_un*PRC_REPLAC/100), FRI_MIXED_, PRC_MIXED_, annualMIXED = (annualALL_un*PRC_MIXED_/100), FRI_SURFAC, PRC_SURFAC, annualSURFAC = (annualALL_un*PRC_SURFAC/100), p = sum(p)) %>%   
#   arrange(desc(annualALL))
finalFRI <- finalFRI %>% mutate_if(is.numeric, list(~na_if(., Inf))) 
finalFRI <- finalFRI %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) #replace(is.na(.), 0) # remove Inf (0/Value; replace with 0)
finalFRI <- finalFRI %>% arrange(desc(annualALL_un)) # re-sort 

#write_csv(finalFRI, "d:\\finalFRIAug15.csv")
write_csv(finalFRI, "G:\\fnaExternal\\wifna_FireNeeds_D.csv")

final10 <- finalFRI %>% slice (1:10) %>% 
  arrange(desc(annualALL_un))

p <- ggplot(final10) +  geom_bar(aes(x = reorder(EVT_NAME, annualALL_un), y = annualALL_un), stat = 'identity') + scale_y_continuous(limits=c(0,700000))
p + coord_flip()