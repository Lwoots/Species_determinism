#Knersvlakte ED data analysis
#7 Feb 2020
###############################################

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, ggfortify, visreg)

#Read in experimental data
source(here("src/Data_wrangling", "ED_weight_wrangling.R"))


#Create PCA of the soil variables. ####

soil_pca <- prcomp(final_ED_dat[, c(21:24, 29:33)], scale. = T)
plot(soil_pca$x[,1], soil_pca$x[,2])
plot(soil_pca$x[,1], soil_pca$x[,3])

autoplot(soil_pca,
         data = final_ED_dat)

#Add to dataframe

final_ED_dat <- data.frame(final_ED_dat, 
                           PCA1 = soil_pca$x[,1], 
                           PCA2 = soil_pca$x[,2], 
                           PCA3 = soil_pca$x[,3])

#How does the mass data relate to the pca? ####


#R. burtoniae
plot(final_ED_dat$Mass[final_ED_dat$Species == "R_burtoniae"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "R_burtoniae"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "R_burtoniae"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "R_burtoniae"], pch = 19)

m_burt <- lm(final_ED_dat$Mass[final_ED_dat$Species == "R_burtoniae"] ~ 
                 final_ED_dat$PCA1[final_ED_dat$Species == "R_burtoniae"] + 
                 final_ED_dat$PCA2[final_ED_dat$Species == "R_burtoniae"] +
                 final_ED_dat$PCA3[final_ED_dat$Species == "R_burtoniae"])
summary(m_burt)

#R. comptonii
plot(final_ED_dat$Mass[final_ED_dat$Species == "R_comptonii"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "R_comptonii"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "R_comptonii"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "R_comptonii"], pch = 19)

m_comp <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "R_comptonii"] ~ 
                           final_ED_dat$PCA1[final_ED_dat$Species == "R_comptonii"] + 
                           final_ED_dat$PCA2[final_ED_dat$Species == "R_comptonii"] +
                           final_ED_dat$PCA3[final_ED_dat$Species == "R_comptonii"])
summary(m_comp)


#D.diversifolium

plot(final_ED_dat$Mass[final_ED_dat$Species == "D_diversifolium"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "D_diversifolium"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "D_diversifolium"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "D_diversifolium"], pch = 19)

m_div <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "D_diversifolium"] ~ 
                  final_ED_dat$PCA1[final_ED_dat$Species == "D_diversifolium"] + 
                  final_ED_dat$PCA2[final_ED_dat$Species == "D_diversifolium"] +
                  final_ED_dat$PCA3[final_ED_dat$Species == "D_diversifolium"])
summary(m_div)

m_div2 <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "D_diversifolium"] ~ 
                 final_ED_dat$PCA1[final_ED_dat$Species == "D_diversifolium"] + 
                 I(final_ED_dat$PCA1[final_ED_dat$Species == "D_diversifolium"]^2))
summary(m_div2)
m_div3 <- lm(final_ED_dat$Mass[final_ED_dat$Species == "D_diversifolium"] ~ 
    final_ED_dat$PCA1[final_ED_dat$Species == "D_diversifolium"])
summary(m_div3)

#A. fissum

plot(final_ED_dat$Mass[final_ED_dat$Species == "A_fissum"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "A_fissum"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "A_fissum"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "A_fissum"], pch = 19)

m_fis <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "A_fissum"] ~ 
                 final_ED_dat$PCA1[final_ED_dat$Species == "A_fissum"] + 
                 final_ED_dat$PCA2[final_ED_dat$Species == "A_fissum"] +
                 final_ED_dat$PCA3[final_ED_dat$Species == "A_fissum"])

summary(m_fis)

#A. framesii

plot(final_ED_dat$Mass[final_ED_dat$Species == "A_framesii"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "A_framesii"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "A_framesii"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "A_framesii"], pch = 19)

m_fram <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "A_framesii"] ~ 
                 final_ED_dat$PCA1[final_ED_dat$Species == "A_framesii"] + 
                 final_ED_dat$PCA2[final_ED_dat$Species == "A_framesii"] +
                 final_ED_dat$PCA3[final_ED_dat$Species == "A_framesii"])
summary(m_fram)

#A. delaetii

plot(final_ED_dat$Mass[final_ED_dat$Species == "A_delaetii"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "A_delaetii"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "A_delaetii"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "A_delaetii"], pch = 19)

m_del <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "A_delaetii"] ~ 
                  final_ED_dat$PCA1[final_ED_dat$Species == "A_delaetii"] + 
                  final_ED_dat$PCA2[final_ED_dat$Species == "A_delaetii"] +
                  final_ED_dat$PCA3[final_ED_dat$Species == "A_delaetii"])
summary(m_del)

#Oophytum

plot(final_ED_dat$Mass[final_ED_dat$Species == "Oophytum_sp"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "Oophytum_sp"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "Oophytum_sp"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "Oophytum_sp"], pch = 19)

m_ooph <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "Oophytum_sp"] ~ 
                 final_ED_dat$PCA1[final_ED_dat$Species == "Oophytum_sp"] + 
                 final_ED_dat$PCA2[final_ED_dat$Species == "Oophytum_sp"] +
                 final_ED_dat$PCA3[final_ED_dat$Species == "Oophytum_sp"])
summary(m_ooph)

m_ooph1 <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "Oophytum_sp"] ~ 
                  final_ED_dat$PCA1[final_ED_dat$Species == "Oophytum_sp"])
summary(m_ooph1)

#C. spissum

plot(final_ED_dat$Mass[final_ED_dat$Species == "C_spissum"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "C_spissum"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "C_spissum"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "C_spissum"], pch = 19)


m_spis <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "C_spissum"] ~ 
                  final_ED_dat$PCA1[final_ED_dat$Species == "C_spissum"] + 
                  final_ED_dat$PCA2[final_ED_dat$Species == "C_spissum"] +
                  final_ED_dat$PCA3[final_ED_dat$Species == "C_spissum"])
summary(m_spis)

#C. stam

plot(final_ED_dat$Mass[final_ED_dat$Species == "C_staminodiosum"] ~ final_ED_dat$PCA1[final_ED_dat$Species == "C_staminodiosum"], pch = 19)
plot(final_ED_dat$Mass[final_ED_dat$Species == "C_staminodiosum"] ~ final_ED_dat$PCA2[final_ED_dat$Species == "C_staminodiosum"], pch = 19)


m_stam <-  lm(final_ED_dat$Mass[final_ED_dat$Species == "C_staminodiosum"] ~ 
                  final_ED_dat$PCA1[final_ED_dat$Species == "C_staminodiosum"] + 
                  final_ED_dat$PCA2[final_ED_dat$Species == "C_staminodiosum"] +
                  final_ED_dat$PCA3[final_ED_dat$Species == "C_staminodiosum"])
summary(m_stam)




#See which soil types have all 9 spp. growing in them  ####

grow <- final_ED_dat %>% 
    filter(!is.na(Mass)) %>% 
    group_by(Soil_code) %>% 
    summarise(count = length(Soil_code)) 

#Filter out soils where everything grows. Pull converts dataframe to vector
good_soil_list <- grow %>% filter(count > 8) %>% pull(Soil_code)

#Subset the original data by this list

good_soil_df <- final_ED_dat %>% filter(Soil_code %in% good_soil_list) 

burt <- good_soil_df %>% filter(Species == "R_burtoniae")
comp <- good_soil_df %>% filter(Species == "R_comptonii")
div <- good_soil_df %>% filter(Species == "D_diversifolium")
fram <- good_soil_df %>% filter(Species == "A_framesii")
fis <- good_soil_df %>% filter(Species == "A_fissum")
del <- good_soil_df %>% filter(Species == "A_delaetii")
spis <- good_soil_df %>% filter(Species == "C_spissum")
stam <- good_soil_df %>% filter(Species == "C_staminodiosum")
ooph <- good_soil_df %>% filter(Species == "Oophytum_sp")


burt <- final_ED_dat %>% filter(Species == "R_burtoniae")
comp <- final_ED_dat %>% filter(Species == "R_comptonii")
div <- final_ED_dat %>% filter(Species == "D_diversifolium")
fram <- final_ED_dat %>% filter(Species == "A_framesii")
fis <- final_ED_dat %>% filter(Species == "A_fissum")
del <- final_ED_dat %>% filter(Species == "A_delaetii")
spis <- final_ED_dat %>% filter(Species == "C_spissum")
stam <- final_ED_dat %>% filter(Species == "C_staminodiosum")
ooph <- final_ED_dat %>% filter(Species == "Oophytum_sp")

#Standardised plots ####
ggplot(burt, aes(Soil_code, rep(9, 30), colour = Height_t3/mean(Height_t3))) +
    geom_point(size = 7, shape = 15) +
    geom_point(data = comp, 
               aes(Soil_code, rep(8, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = div, 
               aes(Soil_code, rep(7, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = fram, 
               aes(Soil_code, rep(6, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = fis, 
               aes(Soil_code, rep(5, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = del, 
               aes(Soil_code, rep(4, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = spis, 
               aes(Soil_code, rep(3, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = stam, 
               aes(Soil_code, rep(2, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = ooph, 
               aes(Soil_code, rep(1, 30), colour = Height_t3/mean(Height_t3)), 
               size = 7,
               shape = 15)
    

ggplot(burt, aes(Soil_code, rep(9, 20), colour = Mass/mean(Mass))) +
    geom_point(size = 7, shape = 15) +
    geom_point(data = comp, 
               aes(Soil_code, rep(8, 20), colour = Mass/mean(Mass, na.rm = T)), 
               size = 7,
               shape = 15) +
    geom_point(data = div, 
               aes(Soil_code, rep(7, 20), colour = Mass/mean(Mass, na.rm = T)), 
               size = 7,
               shape = 15) +
    geom_point(data = fram, 
               aes(Soil_code, rep(6, 20), colour = Mass/mean(Mass)), 
               size = 7,
               shape = 15) +
    geom_point(data = fis, 
               aes(Soil_code, rep(5, 20), colour = Mass/mean(Mass)), 
               size = 7,
               shape = 15) +
    geom_point(data = del, 
               aes(Soil_code, rep(4, 20), colour = Mass/mean(Mass)), 
               size = 7,
               shape = 15) +
    geom_point(data = spis, 
               aes(Soil_code, rep(3, 20), colour = Mass/mean(Mass)), 
               size = 7,
               shape = 15) +
    geom_point(data = stam, 
               aes(Soil_code, rep(2, 20), colour = Mass/mean(Mass)), 
               size = 7,
               shape = 15) +
    geom_point(data = ooph, 
               aes(Soil_code, rep(1, 20), colour = Mass/mean(Mass)), 
               size = 7,
               shape = 15)

#CoV plots ####

ggplot(burt, aes(Soil_code, rep(9, 20), colour = sd(Height_t3)/mean(Height_t3))) +
    geom_point(size = 7, shape = 15) +
    geom_point(data = comp, 
               aes(Soil_code, rep(8, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = div, 
               aes(Soil_code, rep(7, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = fram, 
               aes(Soil_code, rep(6, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = fis, 
               aes(Soil_code, rep(5, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = del, 
               aes(Soil_code, rep(4, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = spis, 
               aes(Soil_code, rep(3, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = stam, 
               aes(Soil_code, rep(2, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15) +
    geom_point(data = ooph, 
               aes(Soil_code, rep(1, 20), colour = sd(Height_t3)/mean(Height_t3)), 
               size = 7,
               shape = 15)

#Coefficient of variation ####
#sd/mean

mass_cv <- final_ED_dat %>% group_by(Species) %>% summarise(cv = sd(Mass, na.rm = T)/mean(Mass, na.rm = T)) %>% as.data.frame()
height_cv <- final_ED_dat %>% group_by(Species) %>% summarise(cv = sd(Height_t3, na.rm = T)/mean(Height_t3, na.rm = T)) %>% as.data.frame()

plot(mass_cv[,2], height_cv[,2])

#selected soils
mass_cv <- good_soil_df %>% group_by(Species) %>% summarise(cv = sd(Mass, na.rm = T)/mean(Mass, na.rm = T)) %>% as.data.frame()
height_cv <- good_soil_df %>% group_by(Species) %>% summarise(cv = sd(Height_t3, na.rm = T)/mean(Height_t3, na.rm = T)) %>% as.data.frame()


#Looking at the data quality ####

raw_weights <- read.csv(here("Raw/Unprocessed", "ED_weight_data_David.csv"))
names(raw_weights)[2] <- "Mass"
summary(raw_weights)

#Replace 'empty' with NA.

raw_weights$Mass[raw_weights$Mass == "empty"] <- NA
raw_weights$Mass <- as.numeric(as.character(raw_weights$Mass)) #Convert to numeric

#Rename incorrect code name from I79 to I19

raw_weights$Code[510] <- "I19"

#There should be at max four of the same codes. But E25, B29, C30 have five or more.
#Looking at the data, there seems to be a few typos

raw_weights$Code[521] <- "E23"
raw_weights$Code[521] <- "B28"

#I think that for C30, he measured one sample twice. Not sure where the extra E25 is coming from. I'll remove it for now.

raw_weights <- raw_weights[-363,]

#Check for outliers
#hist(raw_weights$Mass) #seems okay

#Remove NAs
raw_weights <- na.omit(raw_weights)

#Break up code identity in soil and species
raw_weights$Species_code <- substr(raw_weights$Code, 0, 1)
raw_weights$Soil_code <- substr(raw_weights$Code, 2, 3)
raw_weights$Soil_code <- as.numeric(as.character(raw_weights$Soil_code))

no_per_soil <- raw_weights %>% group_by(Species_code, Soil_code) %>% summarise(number = n()) 
no_per_soil$number <- as.character(no_per_soil$number)

p1 <- ggplot(no_per_soil %>% filter(Species_code == "C"), aes(Soil_code, rep(8, 27), colour = number)) +
           geom_point(size = 10, shape = 15) +
           ylim(4,11) +
           scale_y_continuous(breaks = seq(5, 9, 0.5), labels = c("C_spis", "A_fram", "Ooph", "A_del", "R_comp", "C_stam", "D_div", "A_fis", "R_burt")) +
    geom_point(data = no_per_soil %>% filter(Species_code == "A"), aes(Soil_code, rep(9, 29), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil %>% filter(Species_code == "B"), aes(Soil_code, rep(8.5, 27), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil %>% filter(Species_code == "D"), aes(Soil_code, rep(7.5, 30), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil %>% filter(Species_code == "E"), aes(Soil_code, rep(7, 28), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil %>% filter(Species_code == "G"), aes(Soil_code, rep(6.5, 28), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil %>% filter(Species_code == "H"), aes(Soil_code, rep(6, 20), colour = number),
               size = 11, shape = 15)+
    geom_point(data = no_per_soil %>% filter(Species_code == "I"), aes(Soil_code, rep(5.5, 25), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil %>% filter(Species_code == "J"), aes(Soil_code, rep(5, 27), colour = number),
               size = 11, shape = 15)
       

#height data

ED_dat <- read.csv(here("Raw/Unprocessed", "ED_growth_metrics_23Oct_full.csv"), sep = ";" )

#Exclude misidentified individuals
ED_dat <- ED_dat %>% filter(!notes == "wrong_id")

#Exclude rows where individuals never sprouted

filtered_ED_dat <- ED_dat %>% filter(!(is.na(Height_1) &
                                           is.na(Height_2) &
                                           is.na(Height_3)
)
)

#Exclude notes, leaf width, and intermediate time intervals for now

cleaned_ED_dat <- filtered_ED_dat %>% select(-Height_1a, -Height_2a, -Time_1a, -Time_2a, -notes, -Leaf_3)
rm(filtered_ED_dat)

#Exclude rows containing dicrocaulon (too few to analyse)
cleaned_ED_dat <- cleaned_ED_dat[-c(548:551),]

#Add a missing individual number
cleaned_ED_dat$Individ[722] <- 5

#Replace NAs with 0
cleaned_ED_dat[is.na(cleaned_ED_dat)] <- 0

#For species measured under the microscope, divide by 10 to convert epu to mm.

cleaned_ED_dat[cleaned_ED_dat$Species == "G", c(8,10,12)] <- cleaned_ED_dat[cleaned_ED_dat$Species == "G", c(8,10,12)]/10
cleaned_ED_dat[cleaned_ED_dat$Species == "H", c(8,10,12)] <- cleaned_ED_dat[cleaned_ED_dat$Species == "H", c(8,10,12)]/10
cleaned_ED_dat[cleaned_ED_dat$Species == "I", c(8,10,12)] <- cleaned_ED_dat[cleaned_ED_dat$Species == "I", c(8,10,12)]/10

#Rename

names(cleaned_ED_dat)[1] <- "Code"

no_per_soil_weight <- cleaned_ED_dat %>% group_by(Species, Soil) %>% filter(Height_3>0) %>% summarise(number = n()) 
no_per_soil_weight$number <- as.character(no_per_soil_weight$number)
       

p2 <- ggplot(no_per_soil_weight %>% filter(Species == "C"), aes(Soil, rep(8, 27), colour = number)) +
    geom_point(size = 10, shape = 15) +
    ylim(4,11) +
    scale_y_continuous(breaks = seq(5, 9, 0.5), labels = c("C_spis", "A_fram", "Ooph", "A_del", "R_comp", "C_stam", "D_div", "A_fis", "R_burt")) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "A"), aes(Soil, rep(9, 29), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "B"), aes(Soil, rep(8.5, 27), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "D"), aes(Soil, rep(7.5, 30), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "E"), aes(Soil, rep(7, 28), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "G"), aes(Soil, rep(6.5, 27), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "H"), aes(Soil, rep(6, 20), colour = number),
               size = 11, shape = 15)+
    geom_point(data = no_per_soil_weight %>% filter(Species == "I"), aes(Soil, rep(5.5, 24), colour = number),
               size = 11, shape = 15) +
    geom_point(data = no_per_soil_weight %>% filter(Species == "J"), aes(Soil, rep(5, 29), colour = number),
               size = 11, shape = 15)

require(patchwork)

p1/p2

no_per_soil_weight %>%
    ungroup() %>%
    mutate(Species = case_when(
        Species == "A" ~ "C_spis",
        Species == "B" ~ "A_fram",
        Species == "C" ~ "Ooph",
        Species == "D" ~ "A_del",
        Species == "E" ~ "R_comp",
        Species == "G" ~ "C_stam",
        Species == "H" ~ "D_div",
        Species == "I" ~ "A_fis",
        Species == "J" ~ "R_burt"
    )) %>%
    ggplot() +
        aes(Soil, Species, fill = number) +
        geom_tile()


#Make auc dataframe

aucs <- data.frame( Species = 
                             c("A_delaetii",	
                             "A_fissum",
                             "A_framesii",
                             "C_spissum",
                             "C_staminodiosum",
                             "D_diversifolium",
                             "Oophytum_sp",	
                             "R_burtoniae",
                             "R_comptonii"),
                    AUC = c(0.78, 0.61, 0.73, 0.69, 0.79, 0.84, 0.88, 0.89, 0.91))
