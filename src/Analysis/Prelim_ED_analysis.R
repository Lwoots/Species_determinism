#Preliminary analyses of ED
#________________________________

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, visdat)

#Get the abundance data
source(here("src/Data_wrangling", "Abundance_for_analysis.R"))
glimpse(all_dat)

#Get ED data
ED_dat <- read.csv(here("Raw/Unprocessed", "ED_growth_metrics_23Oct_edited.csv"), sep = ";" )

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

ED_dat <- ED_dat %>% filter(!notes == "wrong_id") #Exclude misidentified individuals

filtered_ED_dat <- ED_dat %>% filter(!(is.na(Height_1) &
                                           is.na(Height_2) &
                                           is.na(Height_3))) #Removing rows where there was never an individual, besides those pots where nothing sprouted

#Replace NAs with 0 to show mortality
filtered_ED_dat$Height_1[is.na(filtered_ED_dat$Height_1)] <- 0
filtered_ED_dat$Height_2[is.na(filtered_ED_dat$Height_2)] <- 0
filtered_ED_dat$Height_3[is.na(filtered_ED_dat$Height_3)] <- 0

mean_ED_dat <- filtered_ED_dat %>% 
                   group_by(Soil, Species) %>% 
                   summarise(H1 = mean(Height_1),
                             H2 = mean(Height_2),
                             H3 = mean(Height_3))
glimpse(mean_ED_dat)

hist(mean_ED_dat$H1[mean_ED_dat$Species == "A"],
     xlim = c(0,40),
     breaks = seq(0,40,2))
hist(mean_ED_dat$H2[mean_ED_dat$Species == "A"], 
     add = T,
     col = "grey",
     xlim = c(0,40),
     breaks = seq(0,40,2))
hist(mean_ED_dat$H3[mean_ED_dat$Species == "A"], 
     add = T,
     col = "black",
     xlim = c(0,40),
     breaks = seq(0,40,2))

hist(mean_ED_dat$H1[mean_ED_dat$Species == "B"],
     xlim = c(0,15),
     breaks = seq(0,40,1))
hist(mean_ED_dat$H2[mean_ED_dat$Species == "B"], 
     add = T,
     col = "grey",
     xlim = c(0,15),
     breaks = seq(0,40,1))
hist(mean_ED_dat$H3[mean_ED_dat$Species == "B"], 
     add = T,
     col = "black",
     xlim = c(0,15),
     breaks = seq(0,40,1))
