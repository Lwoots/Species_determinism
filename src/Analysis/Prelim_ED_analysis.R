#Preliminary analyses of ED
#________________________________

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, visdat)

#Get the abundance data
source(here("src/Data_wrangling", "Abundance_for_analysis.R"))
glimpse(all_dat)

#Get ED data
ED_dat <- read.csv(here("Raw/Unprocessed", "ED_growth_metrics_23Oct_edited.csv"), sep = ";" )

#Get soil and species codes

codes <- read.csv(here("Raw/Tidy", "ED_species_soil_codes.csv"), sep = ";" )

#Extract just the soils that were used in the experiment from the field data

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

names(codes)[2] <- "plot"
combined <- left_join(soils_field, codes, by = "plot") #Add column of the soil codes for easy plotting
combined <- combined[,-c(29:30)]

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

rburt <- mean_ED_dat[mean_ED_dat$Species == "A",]
ggplot() +
    #geom_histogram(data = rburt, aes(H2), binwidth = 2.5, alpha = 0.5, fill = "blue") +
    geom_histogram(data = rburt, aes(H1), binwidth = 2.5, alpha = 0.5) +
    geom_histogram(data = rburt, aes(H3), binwidth = 2.5, alpha = 0.5, fill = "red")
ggplot() +
    geom_histogram(data = mean_ED_dat[mean_ED_dat$Species == "C",], aes(H2), binwidth = 2.5, alpha = 0.5, fill = "blue") +
    geom_histogram(data = mean_ED_dat[mean_ED_dat$Species == "C",], aes(H1), binwidth = 2.5, alpha = 0.5) +
    geom_histogram(data = mean_ED_dat[mean_ED_dat$Species == "C",], aes(H3), binwidth = 2.5, alpha = 0.5, fill = "red")

ggplot() +
    #geom_histogram(data = mean_ED_dat[mean_ED_dat$Species == "J",], aes(H2), binwidth = 2, alpha = 0.5, fill = "blue") +
    geom_histogram(data = mean_ED_dat[mean_ED_dat$Species == "J",], aes(H1), binwidth = 2, alpha = 0.5) +
    geom_histogram(data = mean_ED_dat[mean_ED_dat$Species == "J",], aes(H3), binwidth = 2, alpha = 0.5, fill = "red")

ggplot() +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "A",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "A",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "A",], aes(H3),  alpha = 0.4, fill = "red")

combined <- left_join(soils_field, mean_ED_dat, by = "Soil")

fissum <- combined[combined$Species =="B",]
plot(fissum$A_fissum, fissum$H3)

spissum <- combined[combined$Species =="J",]
plot(spissum$C_spissum, spissum$H3)

framesii <- combined[combined$Species =="I",]
plot(framesii$A_framesii, framesii$H3)

comp <- combined[combined$Species =="E",]
plot(comp$R_comptonii, comp$H3)

burt <- combined[combined$Species =="A",]
plot(burt$R_burtoniae, burt$H3)

stam <- combined[combined$Species =="D",]
plot(stam$C_staminodiosum, stam$H3)

species <- combined[combined$Species == "A",]

pca <- prcomp(species[,c(13:16, 21, 22, 24,25)], scale. = T)
plot(pca$x[,1], pca$x[,2])



plot(combined[combined$Species == "I",]$Clay, combined[combined$Species == "I",]$H3)
plot(combined[combined$Species == "A",]$Na, pca$x[,1])
plot(pca$x[,1], combined[combined$Species == "B",]$H3)

m1 <- lm(combined[combined$Species == "I",]$H3 ~ combined[combined$Species == "I",]$ph_kcl)
summary(m1)

ggplot(combined[combined$Species == "B",], aes(ph_kcl, Na, colour = H3)) +
    geom_point(size = 3)

species <- combined[combined$Species == "J",]

pca <- prcomp(species[,c(13:16, 21, 22, 24,25)], scale. = T)

pca_dat <- cbind(as.data.frame(pca$x), species$H3)
ggplot(pca_dat, aes(PC1, PC2, color = species$H3) ) +
    geom_point(size = 4)


#Models

mburt <- lm(burt$H3 ~ burt$ph_kcl + burt$Na)
summary(mburt)
plot(burt$H3 ~ burt$ph_kcl)
plot(burt$H3 ~ burt$Na)

mstam <- lm(stam$H3 ~ stam$ph_kcl + stam$Na)
summary(mstam)

mcomp <- lm(comp$H3 ~  comp$ph_kcl + comp$Na )
summary(mcomp)
plot(comp$Na, comp$H3)
plot(comp$ph_kcl, comp$H3)

mfram <- lm(framesii$H3 ~ framesii$Clay + framesii$N_perc)
mfram <- lm(framesii$H3 ~ framesii$ph_kcl + framesii$Na)
summary(mfram)
plot(framesii$Clay, framesii$H3)
