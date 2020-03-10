#Knersvlakte ED data analysis
#7 Feb 2020
###############################################

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, ggfortify, visreg, patchwork)

#Read in experimental data
source(here("src/Data_wrangling", "ED_weight_wrangling.R"))


#Create PCA of the soil variables. ####

soil_pca <- prcomp(final_ED_dat[, c(21:24, 29:33)], scale. = T)
plot(soil_pca$x[,1], soil_pca$x[,2])

#Add to dataframe

final_ED_dat <- data.frame(final_ED_dat, 
                           PCA1 = soil_pca$x[,1], 
                           PCA2 = soil_pca$x[,2], 
                           PCA3 = soil_pca$x[,3])

#Scale mass and height variables for between species comparisions

final_ED_dat <- final_ED_dat %>% 
    group_by(Species) %>% 
    mutate(scaled_mass = scale(Mass))
final_ED_dat <- final_ED_dat %>% 
    group_by(Species) %>% 
    mutate(scaled_height = scale(Height_t3))

#Add growth rate parameters


final_ED_dat <- final_ED_dat %>% 
    mutate(full_growth_rate = Height_t3 - Height_t1)

final_ED_dat <- final_ED_dat %>% 
    mutate(half_growth_rate = Height_t2 - Height_t1)


#How does the mass and weight data relate to the pca? ####

ggplot(final_ED_dat, aes(PCA1, Mass)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")

ggplot(final_ED_dat, aes(PCA2, Mass)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y") 

ggplot(final_ED_dat, aes(PCA1, Height_t3)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")

ggplot(final_ED_dat, aes(PCA2, Height_t3)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")

ggplot(final_ED_dat, aes(PCA1, full_growth_rate)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")

ggplot(final_ED_dat, aes(PCA2, full_growth_rate)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")

ggplot(final_ED_dat, aes(PCA1, half_growth_rate)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")

ggplot(final_ED_dat, aes(PCA2, half_growth_rate)) +
    geom_point() +
    facet_wrap( ~ Species, ncol = 3, scales = "free_y")



#See which soil types have all 9 spp. growing in them  ####

grow <- final_ED_dat %>% 
    filter(!is.na(Mass)) %>% 
    group_by(Soil_code) %>% 
    summarise(count = length(Soil_code)) 

#Filter out soils where everything grows. Pull converts dataframe to vector
good_soil_list <- grow %>% filter(count > 8) %>% pull(Soil_code)

#Subset the original data by this list

good_soil_df <- final_ED_dat %>% filter(Soil_code %in% good_soil_list) 


#Standardised plots ####

ggplot(final_ED_dat, aes(Soil_code, Species, fill = scaled_mass)) +
    geom_tile()

ggplot(final_ED_dat, aes(Soil_code, Species, fill = scaled_height)) +
    geom_tile()
    

#Box plots showing variation on a soil ####

ggplot(final_ED_dat, aes(plot, Mass)) +
    geom_boxplot()

ggplot(final_ED_dat, aes(plot, Height_t3)) +
    geom_boxplot()

plots_by_PCA1 <- final_ED_dat %>%
    select(plot, PCA1) %>%
    distinct() %>%
    arrange(PCA1) %>%
    pull(plot)

final_ED_dat %>% 
    filter(!Species == Oophytum_sp) %>% 
    arrange(PCA1) %>%
    mutate(plot = factor(plot, levels = plots_by_PCA1)) %>%
    ggplot() +
    aes(plot, Mass) +
    geom_boxplot()
    
final_ED_dat %>% 
    filter(!Species == Oophytum_sp) %>%
    ggplot() +
    aes(ph_kcl, Height_t3-Height_t2, colour = Species) +
    geom_point()

final_ED_dat %>%
    filter(Species != "Oophytum_sp") %>%
    group_by(Species) %>%
    mutate(Height_t3 = scale(Height_t3-Height_t2)) %>%
    ggplot() +
    aes(P, Height_t3) +
    geom_point() +
    geom_smooth(method = lm)



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
raw_weights$Code[445] <- "J4"

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

#plot
p1 <- no_per_soil %>% 
    filter(!Species_code == "F") %>% 
    ungroup() %>%
    mutate(Species_code = case_when(
        Species_code == "J" ~ "C_spis",
        Species_code == "I" ~ "A_fram",
        Species_code == "H" ~ "Ooph",
        Species_code == "G" ~ "A_del",
        Species_code == "E" ~ "R_comp",
        Species_code == "D" ~ "C_stam",
        Species_code == "C" ~ "D_div",
        Species_code == "B" ~ "A_fis",
        Species_code == "A" ~ "R_burt"
    )) %>%
    ggplot() +
    aes(Soil_code, Species_code, fill = number) +
    geom_tile()


#height data

ED_dat <- read.csv(here("Raw/Unprocessed", "ED_growth_metrics_23Oct_for_analysis.csv") )

#Exclude misidentified individuals
ED_dat <- ED_dat %>% filter(!notes == "wrong_id")

summary(ED_dat)
#Exclude rows where individuals never sprouted

filtered_ED_dat <- ED_dat %>% filter(!(is.na(Height_1) &
                                           is.na(Height_2) &
                                           is.na(Height_3)
)
)

#Move incorrect data entries to correct column

filtered_ED_dat$Height_3[filtered_ED_dat$Both == "I28"] <- filtered_ED_dat$Leaf_3[filtered_ED_dat$Both == "I28"]
filtered_ED_dat$Height_3[filtered_ED_dat$Both == "I19"] <- filtered_ED_dat$Leaf_3[filtered_ED_dat$Both == "I19"]

#Exclude notes, leaf width, and intermediate time intervals for now

cleaned_ED_dat <- filtered_ED_dat %>% select(-Height_1a, -Height_2a, -Time_1a, -Time_2a, -notes, -Leaf_3)
rm(filtered_ED_dat)

#Exclude rows containing dicrocaulon (too few to analyse)
cleaned_ED_dat <- cleaned_ED_dat %>% 
                      filter(!Species == "F") %>% 
                      filter(!is.na(Soil))


#Add a missing individual number

#cleaned_ED_dat$Individ[722] <- 5
cleaned_ED_dat$Individ[is.na(cleaned_ED_dat$Individ)] <- 5


                      
                          


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
       

p2 <- no_per_soil_weight %>%
    ungroup() %>%
    mutate(Species = case_when(
        Species == "J" ~ "C_spis",
        Species == "I" ~ "A_fram",
        Species == "H" ~ "Ooph",
        Species == "G" ~ "A_del",
        Species == "E" ~ "R_comp",
        Species == "D" ~ "C_stam",
        Species == "C" ~ "D_div",
        Species == "B" ~ "A_fis",
        Species == "A" ~ "R_burt"
    )) %>%
    ggplot() +
        aes(Soil, Species, fill = number) +
        geom_tile() +
        xlab("Height")

p1/p2


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
