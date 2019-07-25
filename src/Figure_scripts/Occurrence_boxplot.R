#Making a boxplot of real vs stochastic data
#25 July 2019
#-----------------------------------------------------------------------------------

require(here)

#Load model runs

load(here("Results/Models", "stochastic_run.rda"))
load(here("Results/Models", "run.rda"))

#Extract aucs into a vector

#stochastic
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

#real data
R_burt <- c(unlist(run[[1]]))
R_comp <- c(unlist(run[[2]]))
D_div <-  c(unlist(run[[3]]))
A_del <-  c(unlist(run[[4]]))
A_fis <-  c(unlist(run[[5]]))
A_fra <-  c(unlist(run[[6]]))
C_spis <- c(unlist(run[[7]]))
C_sta <-  c(unlist(run[[8]]))
Dicro <-  c(unlist(run[[9]]))
Ooph <-   c(unlist(run[[10]]))

#Make the boxplot ####

par(mar = c(6, 4.5, 3, 1.5) + 0.1)
boxplot(R_comp, stoc_R_comp,
        R_burt, stoc_R_burt, 
        Ooph, stoc_Ooph,
        Dicro, stoc_Dicro,
        D_div, stoc_D_div,
        C_sta, stoc_C_sta,
        A_del, stoc_A_del,
        A_fra, stoc_A_fra,
        C_spis, stoc_C_spis,
        A_fis, stoc_A_fis,
        
        col = c("steelblue1", "grey"),
        whisklty = 1,
        staplelty = 0,
        at = c(1,2,4,5,7,8,10,11,13,14,16,17,19,20, 22,23,25,26,28,29),
        xaxt = "n",
        cex = 0.75,
        ylab = "AUC",
        cex.lab = 1.9,
        cex.axis = 1.3
)

axis(1, at = seq(1.5,28.5, 3),
     labels = F,
     tck = -0.01
)

labels <- c("R. comptonii",
            "R. burtoniae", 
            "Oophytum sp.",
            "Dicrocaulon sp.",
            "D. diversifolium",
            "C. staminodiosum",
            "A. delaetii",
            "A. framesii",
            "C. spissum",
            "A. fissum"
)

text(x = seq(1.5,28.5, 3), y = 0.315, srt = 35, adj= 1, xpd = TRUE, labels = labels, cex=1.35, font = 3)

#mtext(at = seq(1.5, 25.5, 3), 
      side = 3, 
      text = "*", 
      cex = 4)

points(x = seq(1,28,3),
       y = sort(c(mean(R_burt),mean(R_comp), 
                  mean(D_div), mean(A_del), 
                  mean(A_fis), mean(A_fra), 
                  mean(C_spis), mean(C_sta),
                  mean(Dicro), mean(Ooph)), decreasing = T),
       pch = 21,
       col = "black",
       bg = "#F6B540",
       cex = 1.8
)
