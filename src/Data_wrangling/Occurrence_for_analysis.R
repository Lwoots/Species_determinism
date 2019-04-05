#Converting the abundance data to occurrence data, selecting variables and filtering out unused plots
#31 March
#----------------------------------------------------------------------------------------

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(here, dplyr)

source(here("src/Data_wrangling", "Combine_data.R"))

soil_dat <- my_soil_df %>% 
    select(site, 
           ph_kcl,
           Na,
           C_N_ratio,
           Ca,
           Q_cover,
           elevation,
           percent_over1,
           percent_over2,
           P,
           N_perc,
           aspect,
           Clay,
           corr_dN,
           drainage,
           slope,
           type) %>% 
    filter(!is.na(type)) %>% 
    na.omit() %>% select(-type)

#Select study species (those that are present in ten or more plots) 
nb_sp <- species_df %>% select(site,
                               plot,
                               ruschia_burtoniae,
                               mesemb_1, 
                               drosanthemum_diversifolium,
                               a_delaetii,
                               a_fissum,
                               a_framesii,
                               c_spissum,
                               cephalophyllum_staminodiosum,
                               dicrocaulon,
                               oophytum) 


names(nb_sp) <- c("site", 
                  "plot",
                  "R_burtoniae", 
                  "R_comptonii", 
                  "D_diversifolium",
                  "A_delaetii",
                  "A_fissum",
                  "A_framesii",
                  "C_spissum",
                  "C_staminodiosum",
                  "Dicrocaulon_sp",
                  "Oophytum_sp")

nb_sp[is.na(nb_sp)] <- 0 #replace NA with 0

sp_occ <- nb_sp[3:12] %>% 
    apply(2, function(x) ifelse(x == 0, 0, 1)) %>% 
    as.data.frame() %>% 
    mutate(site = nb_sp$site) %>% 
    mutate(plot = nb_sp$plot) %>% 
    na.omit()

all_dat <- left_join(sp_occ, my_soil_df %>% select(colnames(soil_dat[2:15]), plot), by = "plot") %>% #excluding slope
    na.omit()

rm(nb_sp, sp_occ, soil_dat)
