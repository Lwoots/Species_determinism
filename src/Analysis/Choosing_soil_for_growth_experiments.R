#Choosing soils to grow common garden experiments
#26 July
#---------------------------------------------------------------

require(here)
source(here("src/Data_wrangling", "Occurrence_for_analysis.R")) #get data
row.names(all_dat) <- all_dat$plot

#Conduct a pca on the soil data

soil_pca <- prcomp(all_dat[,c(13:26)], center = TRUE,scale. = TRUE)
summary(soil_pca)

library(devtools)
#install_github("vqv/ggbiplot")
require(ggbiplot)
library(ggplot2)

ggbiplot(soil_pca, labels = row.names(all_dat)) #PC1 and PC2 don't explain a lot of variation

ggbiplot(soil_pca, choices = c(3,4), labels = row.names(all_dat))

#But I want to find ten different soil types, so lets use a cluster analysis
#https://www.statmethods.net/advstats/cluster.html

all_dat[,c(13:26)] <- scale(all_dat[,c(13:26)])


wss <- (nrow(all_dat[,c(13:26)]) - 1) * sum(apply(all_dat[,c(13:26)], 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(all_dat[,c(13:26)], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(all_dat[,c(13:26)], 10)

require(cluster)
clusplot(all_dat[,c(13:26)], fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


d <- dist(all_dat[,c(13:26)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, cex = 0.7, cex.main = 1) # display dendogram
groups <- cutree(fit, k=10) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=10, border="red")

#Take for example A0, AC50, T40, N30, AA50, AA30, T20, AH50, AK40, E0

selected <- c("A0", "AC50", "T40", "N30", "AA50", "AA30", "T20", "AH50", "AK40", "E0")
grow_soils <- all_dat[all_dat$plot %in% selected,]

boxplot(all_dat[,c(13:26)], cex = 0.75, las = 2)

points(x = rep(1,10), y = grow_soils$ph_kcl, col = "red", pch = 4)
points(x = rep(2,10), y = grow_soils[,14], col = "red", pch = 4)
points(x = rep(3,10), y = grow_soils[,15], col = "red", pch = 4)
points(x = rep(4,10), y = grow_soils[,16], col = "red", pch = 4)
points(x = rep(5,10), y = grow_soils[,17], col = "red", pch = 4)
points(x = rep(6,10), y = grow_soils[,18], col = "red", pch = 4)
points(x = rep(7,10), y = grow_soils[,19], col = "red", pch = 4)
points(x = rep(8,10), y = grow_soils[,20], col = "red", pch = 4)
points(x = rep(9,10), y = grow_soils[,21], col = "red", pch = 4)
points(x = rep(10,10), y = grow_soils[,22], col = "red", pch = 4)
points(x = rep(11,10), y = grow_soils[,23], col = "red", pch = 4)
points(x = rep(12,10), y = grow_soils[,24], col = "red", pch = 4)
points(x = rep(13,10), y = grow_soils[,25], col = "red", pch = 4)
points(x = rep(14,10), y = grow_soils[,26], col = "red", pch = 4)


#Redo it without elevation, drainage, % over, aspect, as we no longer have those characters in our soil.

d <- dist(all_dat[,c(13:16, 21, 22, 24, 25)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, cex = 0.7) # display dendogram
groups <- cutree(fit, k=10) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=10, border="red")


selected <- c("Q35", "O10", "A10", "AK10", "AI10", "AA50", "G50", "Q20", "AH50", "C10")
grow_soils <- all_dat[all_dat$plot %in% selected,]

boxplot(all_dat[,c(13:16, 21, 22, 24, 25)])

points(x = rep(1,10), y = grow_soils$ph_kcl, col = "red")
points(x = rep(2,10), y = grow_soils[,14], col = "red")
points(x = rep(3,10), y = grow_soils[,15], col = "red")
points(x = rep(4,10), y = grow_soils[,16], col = "red")
points(x = rep(5,10), y = grow_soils[,21], col = "red")
points(x = rep(6,10), y = grow_soils[,22], col = "red")
points(x = rep(7,10), y = grow_soils[,24], col = "red")
points(x = rep(8,10), y = grow_soils[,25], col = "red")

text(x = rep(1,10), y = grow_soils$ph_kcl, labels = grow_soils$plot)
