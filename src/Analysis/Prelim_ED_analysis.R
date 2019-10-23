#Preliminary analyses of ED
#________________________________

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, visdat)

#Get the abundance data
source(here("src/Data_wrangling", "Abundance_for_analysis.R"))
glimpse(all_dat)

#Extract just the soils that were used in the experiment

soils_ED <- all_dat %>% filter(plot == c("L30", "L40",
                                         "Q20", "R50",
                                         "S10", "T35",
                                         "U50", "U0",
                                         "V30", "E5",
                                         "D15", "E40",
                                         "C40", "A30",
                                         "A35", "C30",
                                         "I40"))








soils_ED <- all_dat %>% filter(plot == "L30" |
                               plot == "L40"|
                               plot == "Q20" |
                               plot == "R50 " |
                               plot == "S10" |
                               plot == "T35" |
                               plot == "U50" |
                               plot == "V10" |
                               plot == "V30" |
                               plot == "E5" |
                               plot == "D15" |
                               plot == "E40" |
                               plot == "C40" |
                               plot == "A30" |
                               plot == "A35" |
                               plot == "C30" |
                               plot == "I40" |
                               plot == "AE50"|
                               plot == "AG40" |
                               plot == "AA0" |
                               plot == "AC30" |
                               plot == "G40" |
                               plot == "AA30" |
                               plot == "AC50" |
                               plot == "AK10" |
                               plot == "AI25" |
                               plot == "AI15" |
                               plot == "AI40" |
                               plot == "R0 " |
                               plot == "AK20" 
)

glimpse(soils_ED)
soils_ED$Soil <- seq(1, 30, 1) #Add column of the soil codes for easy plotting

plot(soils_ED$R_comptonii ~ soils_ED$Soil)

