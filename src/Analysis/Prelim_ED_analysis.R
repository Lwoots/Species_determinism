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

#Combine with field data
names(mean_ED_dat)[1] <- "Soil_code"

combined <- left_join(combined, mean_ED_dat, by = "Soil_code")


#Let's see how height distributions have changed over time

#Rburt
ggplot() + labs(x = "R burt") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "A",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "A",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "A",], aes(H3),  alpha = 0.4, fill = "red")

#Afissum

ggplot() + labs(x = "A fissum") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "B",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "B",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "B",], aes(H3),  alpha = 0.4, fill = "red")

#Ddiv

ggplot() + labs(x = "D div") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "C",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "C",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "C",], aes(H3),  alpha = 0.4, fill = "red")

#Cstam

ggplot() + labs(x = "C stam") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "D",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "D",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "D",], aes(H3),  alpha = 0.4, fill = "red")

#Rcomp

ggplot() + labs(x = "R comp") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "E",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "E",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "E",], aes(H3),  alpha = 0.4, fill = "red")

#Adel

ggplot() + labs(x = "A del") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "G",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "G",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "G",], aes(H3),  alpha = 0.4, fill = "red")

#Ooph

ggplot() + labs(x = "Oophytum") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "H",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "H",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "H",], aes(H3),  alpha = 0.4, fill = "red")

#Aframesii

ggplot() + labs(x = "A framesii") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "I",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "I",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "I",], aes(H3),  alpha = 0.4, fill = "red")

#CCspissum

ggplot() + labs(x = "C spissum") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "J",], aes(H2), alpha = 0.4, fill = "blue") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "J",], aes(H1),  alpha = 0.4, fill = "grey") +
    geom_density(data = mean_ED_dat[mean_ED_dat$Species == "J",], aes(H3),  alpha = 0.4, fill = "red")

#Make growth curves ####

#Subset data

burt <- combined[combined$Species =="A",]
fissum <- combined[combined$Species =="B",]
div <- combined[combined$Species =="C",]
stam <- combined[combined$Species =="D",]
comp <- combined[combined$Species =="E",]
del <- combined[combined$Species =="G",]
ooph <-  combined[combined$Species =="H",]
fram <- combined[combined$Species =="I",]
spissum <- combined[combined$Species =="J",]

#curves

#Rburt
plot(burt$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = burt$Soil_code,
     pch = 19)
points(burt$H2 ~ rep(2, 30),
     xlim = c(1,3),
     col = burt$Soil_code,
     pch = 19)
points(burt$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = burt$Soil_code,
       pch = 19)
segments(rep(1,30), burt$H1, rep(2, 30), burt$H2)
segments(rep(2,30), burt$H2, rep(3, 30), burt$H3)

#Afissum

plot(fissum$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = fissum$Soil_code,
     pch = 19)
points(fissum$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = fissum$Soil_code,
       pch = 19)
points(fissum$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = fissum$Soil_code,
       pch = 19)
segments(rep(1,30), fissum$H1, rep(2, 30), fissum$H2)
segments(rep(2,30), fissum$H2, rep(3, 30), fissum$H3)

#Ddiv

plot(div$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = div$Soil_code,
     pch = 19)
points(div$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = div$Soil_code,
       pch = 19)
points(div$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = div$Soil_code,
       pch = 19)
segments(rep(1,30), div$H1, rep(2, 30), div$H2)
segments(rep(2,30), div$H2, rep(3, 30), div$H3)

#Cstam

plot(stam$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = stam$Soil_code,
     pch = 19)
points(stam$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = stam$Soil_code,
       pch = 19)
points(stam$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = stam$Soil_code,
       pch = 19)
segments(rep(1,30), stam$H1, rep(2, 30), stam$H2)
segments(rep(2,30), stam$H2, rep(3, 30), stam$H3)

#Rcomp

plot(comp$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = comp$Soil_code,
     pch = 19)
points(comp$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = comp$Soil_code,
       pch = 19)
points(comp$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = comp$Soil_code,
       pch = 19)
segments(rep(1,30), comp$H1, rep(2, 30), comp$H2)
segments(rep(2,30), comp$H2, rep(3, 30), comp$H3)

#Adel

plot(del$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = del$Soil_code,
     pch = 19)
points(del$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = del$Soil_code,
       pch = 19)
points(del$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = del$Soil_code,
       pch = 19)
segments(rep(1,30), del$H1, rep(2, 30), del$H2)
segments(rep(2,30), del$H2, rep(3, 30), del$H3)

#Ooph

plot(ooph$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = ooph$Soil_code,
     pch = 19)
points(ooph$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = ooph$Soil_code,
       pch = 19)
points(ooph$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = ooph$Soil_code,
       pch = 19)
segments(rep(1,30), ooph$H1, rep(2, 30), ooph$H2)
segments(rep(2,30), ooph$H2, rep(3, 30), ooph$H3)

#framesii

plot(fram$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = fram$Soil_code,
     pch = 19)
points(fram$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = fram$Soil_code,
       pch = 19)
points(fram$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = fram$Soil_code,
       pch = 19)
segments(rep(1,30), fram$H1, rep(2, 30), fram$H2)
segments(rep(2,30), fram$H2, rep(3, 30), fram$H3)

#Cspissum

plot(spissum$H3 ~ rep(3, 30),
     xlim = c(1,3),
     col = spissum$Soil_code,
     pch = 19)
points(spissum$H2 ~ rep(2, 30),
       xlim = c(1,3),
       col = spissum$Soil_code,
       pch = 19)
points(spissum$H1 ~ rep(1, 30),
       xlim = c(1,3),
       col = spissum$Soil_code,
       pch = 19)
segments(rep(1,30), spissum$H1, rep(2, 30), spissum$H2)
segments(rep(2,30), spissum$H2, rep(3, 30), spissum$H3)

#Is there a relationship between H2 and H3?

plot(burt$H2, burt$H3, pch = 19)
plot(fissum$H2, fissum$H3, pch = 19)
plot(div$H2, div$H3, pch = 19)
plot(stam$H2, stam$H3, pch = 19)
plot(comp$H2, comp$H3, pch = 19)
plot(del$H2, del$H3, pch = 19)
plot(ooph$H2, ooph$H3, pch = 19)
plot(fram$H2, fram$H3, pch = 19)
plot(spissum$H2, spissum$H3, pch = 19)


#How does height relate to field abundance?


plot(burt$R_burtoniae, burt$H3)
plot(fissum$A_fissum, fissum$H3)
plot(div$D_diversifolium, div$H3)
plot(stam$C_staminodiosum, stam$H3)
plot(comp$R_comptonii, comp$H3)
plot(del$A_delaetii, del$H3)
plot(ooph$Oophytum_sp, ooph$H3)
plot(fram$A_framesii, fram$H3)
plot(spissum$C_spissum, spissum$H3)

#Height and ph?

plot(burt$ph_kcl, burt$H3)
plot(fissum$ph_kcl, fissum$H3)
plot(div$ph_kcl, div$H3)
plot(stam$ph_kcl, stam$H3)
plot(comp$ph_kcl, comp$H3)
plot(del$ph_kcl, del$H3)
plot(ooph$ph_kcl, ooph$H2)
plot(fram$ph_kcl, fram$H3)
plot(spissum$ph_kcl, spissum$H3)

#Height and Na?

plot(burt$Na, burt$H3)
plot(fissum$Na, fissum$H3)
plot(div$Na, div$H3)
plot(stam$Na, stam$H3)
plot(comp$Na, comp$H3)
plot(del$Na, del$H3)
plot(ooph$Na, ooph$H3)
plot(fram$Na, fram$H3)
plot(spissum$Na, spissum$H3)


plot(log(burt$P), burt$H3)
plot(fissum$P, fissum$H3)
plot(div$P, div$H3)
plot(stam$P, stam$H3)
plot(comp$P, comp$H3)
plot(del$P, del$H3)
plot(ooph$P, ooph$H3)
plot(log(fram$P), fram$H3)
plot(spissum$P, spissum$H3)




#PCA

#Burt
burt_pca <- prcomp(burt[,c(13:16, 21, 22, 24,25)], scale. = T)


burt_pca <- burt_pca$x
ggplot(as.data.frame(dat_pca), aes(PC1, PC2, color = burt$H3) ) +
    geom_point(size = 4)

burt_pca <- as.data.frame(burt_pca)
plot(burt_pca$PC1, burt$H3)
plot(burt_pca$PC2, burt$H3)
plot(burt_pca$PC3, burt$H3)

mbpca <- lm(burt$H3 ~ burt_pca$PC2 + burt_pca$PC1 + burt_pca$PC3)
summary(mbpca)

#Fissum

fis_pca <- prcomp(fissum[,c(13:16, 21, 22, 24,25)], scale. = T)

fis_pca <- fis_pca$x
ggplot(as.data.frame(fis_pca), aes(PC1, PC2, color = fissum$H3) ) +
    geom_point(size = 4)

fis_pca <- as.data.frame(fis_pca)

plot(fis_pca$PC1, fissum$H3)
plot(fis_pca$PC2, fissum$H3)
plot(fis_pca$PC3, fissum$H3)

mfispca <- lm(fissum$H3 ~ fis_pca$PC1 + fis_pca$PC2 + fis_pca$PC3)
summary(mfispca)

#D div

div_pca <- prcomp(div[,c(13:16, 21, 22, 24,25)], scale. = T)

div_pca <- as.data.frame(div_pca$x)
ggplot(as.data.frame(div_pca), aes(PC1, PC2, color = div$H3) ) +
    geom_point(size = 4)


plot(div_pca$PC1, div$H3)
plot(div_pca$PC2, div$H3)
plot(div_pca$PC3, div$H3)

mdivpca <- lm(div$H3 ~ div_pca$PC2 + div_pca$PC1 + div_pca$PC3)
summary(mdivpca)

#Stam

stam_pca <- prcomp(stam[,c(13:16, 21, 22, 24,25)], scale. = T)

stam_pca <- as.data.frame(stam_pca$x)
ggplot(stam_pca, aes(PC1, PC2, color = stam$H3) ) +
    geom_point(size = 4)

plot(stam$H3 ~ stam_pca$PC1)
plot(stam$H3 ~ stam_pca$PC2)
plot(stam$H3 ~ stam_pca$PC3)

mstpca <- lm(stam$H3 ~ stam_pca$PC3 + stam_pca$PC1 + stam_pca$PC2)
summary(mstpca)

#Comp
comp_pca <- prcomp(comp[,c(13:16, 21, 22, 24,25)], scale. = T)

comp_pca <- as.data.frame(comp_pca$x)
ggplot(as.data.frame(comp_pca), aes(PC1, PC2, color = comp$H3) ) +
    geom_point(size = 4)

plot(dat$PC1, comp$H3)
plot(dat$PC2, comp$H3)
plot(dat$PC3, comp$H3)


#Ooph

ooph_pca <- prcomp(ooph[,c(13:16, 21, 22, 24,25)], scale. = T)
ooph_pca <- as.data.frame(ooph_pca$x)

ggplot(as.data.frame(ooph_pca), aes(PC1, PC2, color = ooph$H3) ) +
    geom_point(size = 4)

plot(ooph$H3 ~ ooph_pca$PC3)








species <- combined[combined$Species == "A",]

dat_pca <- prcomp(species[,c(13:16, 21, 22, 24,25)], scale. = T)






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
ggplot(dat_pca, aes(PC1, PC2, color = species$H3) ) +
    geom_point(size = 4)


#Models

mburt <- lm(burt$H3 ~ burt$ph_kcl + burt$Na + burt$C_N_ratio + burt$Ca + burt$P + burt$N_perc + burt$Clay + burt$corr_dN)
summary(mburt)

mfis <- lm(H3 ~ ph_kcl + Na + C_N_ratio + Ca + P + N_perc + Clay + corr_dN, data = fissum)
summary(mfis)

mdiv <- lm(H3 ~ ph_kcl + Na + C_N_ratio + Ca + P + N_perc + Clay + corr_dN, data = div)
summary(mdiv)


mstam <- lm(H3 ~ ph_kcl + Na + C_N_ratio + Ca + P + N_perc + Clay + corr_dN, data = stam)
summary(mstam)


mcomp <- lm(H3 ~ ph_kcl + Na + C_N_ratio + Ca + P + N_perc + Clay + corr_dN, data = comp)
summary(mcomp)










mcomp <- lm(comp$H3 ~  comp$ph_kcl + comp$Na )
summary(mcomp)
plot(comp$Na, comp$H3)
plot(comp$ph_kcl, comp$H3)

mfram <- lm(framesii$H3 ~ framesii$Clay + framesii$N_perc)
mfram <- lm(framesii$H3 ~ framesii$ph_kcl + framesii$Na)
summary(mfram)
plot(framesii$Clay, framesii$H3)
