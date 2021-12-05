# Fire regime summaries

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

#final <- read_csv("G:\\fnaExternal\\wiModifiedEvt_1.csv") # unmodified fire dependent vegetation areas 
#modcsv <- read.csv ("G:\\fnaExternal\\firedepVegBThird.csv")
modcsv <- read.csv ("G:\\fnaExternal\\wifna_modarea10ac.csv") # model B

#final <- final %>% left_join(mod, by = c("EVT_VALUE" = "Value"))


# create variable p as percent of total count, per row
# final$sum <- sum(final$Count)
# print(final$sum)
# 
# final <- within(final, p <- as.numeric(Count/final$sum*100))
# head(final)
# head(final$p)

# Calculate annual burning needs

# annualAll ~ based on mean FRI (FRI_ALLFIR), expected annual burning needs (units = acres). note = modified!

finalFRI <- modcsv %>% mutate(area_mod = Count*30*30, acres_mod = Count*30*30/4046.86, annualALL_mod = (acres_mod/FRI_ALLFIR)) 

# finalFRI <- finalFRI %>% 
#    group_by(EVT_NAME) %>% 
#    summarise(WI_Nat_Com, Area_mod = sum(area_mod), Acres_mod = sum(acres_mod), FRI_ALLFIR, AnnualALL_mod = sum(annualALL_mod)) %>%   
#    arrange(desc(annualALL_mod))

finalFRI <- finalFRI %>% mutate_if(is.numeric, list(~na_if(., Inf))) 
finalFRI <- finalFRI %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) #replace(is.na(.), 0) # remove Inf (0/Value; replace with 0)
finalFRI <- finalFRI %>% arrange(desc(annualALL_mod)) # re-sort 
print(finalFRI)


# modified top third + original model B
#finalFRi is top third
mod <- select (finalFRI, c(Value, area_mod, acres_mod, annualALL_mod))
og <- read.csv ("G:\\fnaExternal\\wifna_FireNeeds_ALLarea.csv")
final <- og %>% left_join(mod, by = c("EVT_VALUE" = "Value"))
write_csv(final, "G:\\fnaExternal\\wiFNA_10ac_area.csv")

#write_csv(finalFRI, "d:\\finalFRIAug15.csv")
#write_csv(finalFRI, "G:\\fnaExternal\\wiFNA_model1_AREA.csv")

final10 <- finalFRI %>% slice (1:10) %>% 
  arrange(desc(annualALL_mod))

p <- ggplot(final10, aes(y=annualALL_un, x=reorder(EVT_NAME, annualALL_un), fill=factor(annualALL_mod))) + geom_bar(stat = 'identity')
p + coord_flip()


p <- ggplot(final10) +  geom_bar(aes(x = reorder(EVT_NAME, annualALL_mod), y = annualALL_mod), stat = 'identity') + scale_y_continuous(limits=c(0,250000))
p + coord_flip()