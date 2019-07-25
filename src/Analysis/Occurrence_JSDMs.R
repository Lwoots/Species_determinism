#Joint species distribution models for occurrence data, in addition to generating stachastic null models by permuting occurrences between plots
# 24 July 2019
#---------------------------------------------------------------------------------------------------------

source(here("src/Data_wrangling", "Occurrence_for_analysis.R")) #get data
all_dat[,13:26] <- scale(all_dat[,13:26])

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)} #load packages
p_load(MASS, dplyr, boral, foreach, doParallel, rlist, vegan)


#### Stochastic null model ####

#set up parallel

cl<-makeCluster(detectCores() - 1)
registerDoParallel(cl)


#create empty lists for aucs from loop

stoc_auc_burt <- list()
stoc_auc_comp <- list()
stoc_auc_div <- list()
stoc_auc_del <- list()
stoc_auc_fis <- list()
stoc_auc_fram <- list()
stoc_auc_spis <- list()
stoc_auc_stam <- list()
stoc_auc_dic <- list()
stoc_auc_ooph <- list()
stoc_sp_num <- list()

#Run loop

num = 1

repeat{
    
    #Shuffle species, keeping row and column sums equal to original data.
    rsp <- permatswap(all_dat[,1:10], 
                      times = 1, 
                      burnin = 5, 
                      fixedmar = "both", 
                      mtype = "prab")
    rand_dat <- cbind(rsp$perm, all_dat[,13:26])
    
    #Choose subset of plots
    set.seed(Sys.time())
    rplots <- sample(150, 100)
    train <- rand_dat[rplots,] #data to create jsdm
    test <- rand_dat[-c(rplots),] #data to test jsdm
    
    
    #Skip loop interation (this stops loop crashing when there's no occurrences in the test data set)
    
    if(train %>% summarise(C_staminodiosum = sum(C_staminodiosum)) == 10 |
       train %>% summarise(A_fissum = sum(A_fissum)) == 12 |
       train %>% summarise(A_framesii = sum(A_framesii)) == 12) {
        next
    }
    
    #Record the number of species used in each plot
    stoc_sp_num[[num]] <- train %>% summarise(R_burtoniae = sum(R_burtoniae),
                                              R_comptonii = sum(R_comptonii),
                                              D_diversifolium = sum(D_diversifolium),
                                              A_delaetii = sum(A_delaetii),
                                              A_fissum = sum(A_fissum),
                                              A_framesii = sum(A_framesii),
                                              C_spissum = sum(C_spissum),
                                              C_staminodiosum = sum(C_staminodiosum),
                                              Dicrocaulon_sp = sum(Dicrocaulon_sp),
                                              Oophytum_sp = sum(Oophytum_sp)
    )
    
    
    #Set up JSDM
    sp <- as.matrix(train[,1:10])
    covar <- as.matrix(train[,13:length(train)])
    
    mod <- boral(
        sp,
        covar,
        num.lv = 3,
        family = "binomial",
        #mcmc.control = example_mcmc_control,
        save.model = T
    )
    
    #extract enviro vars from the test data
    test_covar <-  as.matrix(test[,13:length(test)])
    
    #Predict occurrence in test data
    newpred <- predict.boral(mod, 
                             newX = test_covar, 
                             predict.type = "marginal",
                             est = "mean")
    
    #Calculate auc for each species
    stoc_auc_burt[[num]] <- pROC::roc(test$R_burtoniae, newpred$linpred[,1]) %>% pROC::auc()
    stoc_auc_comp[[num]] <- pROC::roc(test$R_comptonii, newpred$linpred[,2]) %>% pROC::auc()
    stoc_auc_div[[num]] <- pROC::roc(test$D_diversifolium, newpred$linpred[,3]) %>% pROC::auc()
    stoc_auc_del[[num]] <- pROC::roc(test$A_delaetii, newpred$linpred[,4]) %>% pROC::auc()
    stoc_auc_fis[[num]] <- pROC::roc(test$A_fissum, newpred$linpred[,5]) %>% pROC::auc()
    stoc_auc_fram[[num]] <- pROC::roc(test$A_framesii, newpred$linpred[,6]) %>% pROC::auc()
    stoc_auc_spis[[num]] <- pROC::roc(test$C_spissum, newpred$linpred[,7]) %>% pROC::auc()
    stoc_auc_stam[[num]] <- pROC::roc(test$C_staminodiosum, newpred$linpred[,8]) %>% pROC::auc()
    stoc_auc_dic[[num]] <- pROC::roc(test$Dicrocaulon_sp, newpred$linpred[,9]) %>% pROC::auc()
    stoc_auc_ooph[[num]] <- pROC::roc(test$Oophytum_sp, newpred$linpred[,10]) %>% pROC::auc()
    
    
    num <- num + 1
    if(num > 3) {
        break
    }
}

#Stop parallel
stopCluster(cl)
registerDoSEQ()
rm(cl)

#Add lists of aucs into a single object

stochastic_run <- list(stoc_auc_burt,
                                stoc_auc_comp,
                                stoc_auc_div ,
                                stoc_auc_del ,
                                stoc_auc_fis ,
                                stoc_auc_fram,
                                stoc_auc_spis,
                                stoc_auc_stam,
                                stoc_auc_dic ,
                                stoc_auc_ooph)

rm(stoc_auc_burt,
   stoc_auc_comp,
   stoc_auc_div ,
   stoc_auc_del ,
   stoc_auc_fis ,
   stoc_auc_fram,
   stoc_auc_spis,
   stoc_auc_stam,
   stoc_auc_dic ,
   stoc_auc_ooph,
   cl)

#Add auc from each iteration into a vector

stoc_R_burt <- c(unlist(stochastic_run[[1]]))
stoc_R_comp <- c(unlist(stochastic_run[[2]]))
stoc_D_div <- c(unlist(stochastic_run[[3]]))
stoc_A_del <- c(unlist(stochastic_run[[4]]))
stoc_A_fis <- c(unlist(stochastic_run[[5]]))
stoc_A_fra <- c(unlist(stochastic_run[[6]]))
stoc_C_spis <- c(unlist(stochastic_run[[7]]))
stoc_C_sta <- c(unlist(stochastic_run[[8]]))
stoc_Dicro <- c(unlist(stochastic_run[[9]]))
stoc_Ooph <- c(unlist(stochastic_run[[10]]))

