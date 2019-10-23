#Preliminary analyses of ED
#________________________________

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, visdat)

#Get the abundance data
source(here("src/Data_wrangling", "Abundance_for_analysis.R"))
glimpse(all_dat)

#Get ED data
ED_dat <- read.csv(here("Raw/Unprocessed", "ED_growth_metrics_23Oct_full.csv"), sep = ";" )

#Extract just the soils that were used in the experiment

soils_field <- all_dat %>% filter(plot == "L30" |
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

glimpse(soils_field)
soils_field$Soil <- seq(1, 30, 1) #Add column of the soil codes for easy plotting

plot(soils_field$Oophytum_sp ~ soils_field$Soil)

#Wrangling ED data ####

ED_dat <- ED_dat %>% filter(!notes == "wrong_id")


