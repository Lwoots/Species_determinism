#Generating joint species distribution models for occurrence data from the knersvlakte
# 24 July 2019
#---------------------------------------------------------------------------------------------------------

source(here("src/Data_wrangling", "Occurrence_for_analysis.R")) #get data
all_dat[,13:26] <- scale(all_dat[,13:26])

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)} #load packages
p_load(MASS, dplyr, boral, foreach, doParallel, rlist, vegan)

##### Joint species distribution model for occurrence data ####

#set up parallel

cl<-makeCluster(detectCores() - 1)
registerDoParallel(cl)

#create empty lists for aucs from loop

auc_burt <- list()
auc_comp <- list()
auc_div <- list()
auc_del <- list()
auc_fis <- list()
auc_fram <- list()
auc_spis <- list()
auc_stam <- list()
auc_dic <- list()
auc_ooph <- list()
sp_num <- list()