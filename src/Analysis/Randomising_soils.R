#Experimental design for the growth experiments
#Randomising soils and seeds to pots.
#5 August 2019
#---------------------------------------------------------------

#Create vector of the 30 different soil types

#New approach with a bit of excel cheating
#library(here, ggplot2, RColorBrewer)
dat <- read.csv("~/Documents/Species_determinism/Raw/Tidy/Randomisation_ED.csv", sep = ";")
names(dat)[1] <- "Both"

#Create vector to draw rows out of
set.seed(540)
all_combos <- 1:300

rcombos <- sample(all_combos, 300)

draw1 <- rcombos[1:60]
draw2 <- rcombos[61:120]
draw3 <- rcombos[121:180]
draw4 <- rcombos[181:240]
draw5 <- rcombos[241:300]

tray1 <- dat[draw1 ,] %>% mutate( 
    x = rep(c(1:6), 10), 
    y = c(rep(10, 6), rep(9, 6), rep(8, 6), rep(7, 6), rep(6, 6), rep(5, 6), rep(4, 6), rep(3, 6), rep(2, 6), rep(1,6))
    )
tray2 <- dat[draw2 ,] %>% mutate( 
    x = rep(c(1:6), 10), 
    y = c(rep(10, 6), rep(9, 6), rep(8, 6), rep(7, 6), rep(6, 6), rep(5, 6), rep(4, 6), rep(3, 6), rep(2, 6), rep(1,6))
    )
tray3 <- dat[draw3 ,] %>% mutate( 
    x = rep(c(1:6), 10), 
    y = c(rep(10, 6), rep(9, 6), rep(8, 6), rep(7, 6), rep(6, 6), rep(5, 6), rep(4, 6), rep(3, 6), rep(2, 6), rep(1,6))
    )

tray4 <- dat[draw4 ,] %>% mutate( 
    x = rep(c(1:6), 10), 
    y = c(rep(10, 6), rep(9, 6), rep(8, 6), rep(7, 6), rep(6, 6), rep(5, 6), rep(4, 6), rep(3, 6), rep(2, 6), rep(1,6))
    )
tray5 <- dat[draw5 ,] %>% mutate( 
    x = rep(c(1:6), 10), 
    y = c(rep(10, 6), rep(9, 6), rep(8, 6), rep(7, 6), rep(6, 6), rep(5, 6), rep(4, 6), rep(3, 6), rep(2, 6), rep(1,6))
    )


#Graphs of soil ####
library(cowplot)

legend <- cowplot::get_legend( ggplot(tray5, 
       aes(as.factor(x), y, colour = as.factor(Soil), drop = F)) +
    geom_point(size = 12, aes(shape = as.factor(Soil))) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_blank()) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1"), "grey22"),times=5),
                        limits = levels(as.factor(dat$Soil)),
                        drop = F) +
    scale_shape_manual(values=rep(c(15, 17, 19, 18, 8),each=6),
                       limits = levels(as.factor(dat$Soil)),
                       drop = F))


p1 <- ggplot(tray1, 
       aes(as.factor(x), y, colour = as.factor(Soil), drop = F)) +
    geom_point(size = 12, aes(shape = as.factor(Soil))) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1"), "grey22"),times=5),
                        limits = levels(as.factor(dat$Soil)),
                        drop = F) +
    scale_shape_manual(values=rep(c(15, 17, 19, 18, 8),each=6),
                       limits = levels(as.factor(dat$Soil)),
                       drop = F) +
    xlab("Tray 1")

p2 <- ggplot(tray2, 
             aes(as.factor(x), y, colour = as.factor(Soil), drop = F)) +
    geom_point(size = 12, aes(shape = as.factor(Soil))) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1"), "grey22"),times=5),
                        limits = levels(as.factor(dat$Soil))
                        ) +
    scale_shape_manual(values=rep(c(15, 17, 19, 18, 8),each=6),
                       limits = levels(as.factor(dat$Soil))
                       )+
    xlab("Tray 2")

p3 <- ggplot(tray3, 
             aes(as.factor(x), y, colour = as.factor(Soil), drop = F)) +
    geom_point(size = 12, aes(shape = as.factor(Soil))) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1"), "grey22"),times=5),
                        limits = levels(as.factor(dat$Soil))
    ) +
    scale_shape_manual(values=rep(c(15, 17, 19, 18, 8),each=6),
                       limits = levels(as.factor(dat$Soil))
    )+
    xlab("Tray 3")

p4 <- ggplot(tray4, 
             aes(as.factor(x), y, colour = as.factor(Soil), drop = F)) +
    geom_point(size = 12, aes(shape = as.factor(Soil))) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1"), "grey22"),times=5),
                        limits = levels(as.factor(dat$Soil))
    ) +
    scale_shape_manual(values=rep(c(15, 17, 19, 18, 8),each=6),
                       limits = levels(as.factor(dat$Soil))
    )+
    xlab("Tray 4")

p5 <- ggplot(tray5, 
             aes(as.factor(x), y, colour = as.factor(Soil), drop = F)) +
    geom_point(size = 12, aes(shape = as.factor(Soil))) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1"), "grey22"),times=5),
                        limits = levels(as.factor(dat$Soil))
    ) +
    scale_shape_manual(values=rep(c(15, 17, 19, 18, 8),each=6),
                       limits = levels(as.factor(dat$Soil))
    )+
    xlab("Tray 5")

plot_grid(p1, p2, p3, legend, nrow = 1, rel_widths = c(1,1,1,1))

plot_grid(p4, p5, legend, nrow = 1, rel_widths = c(1,1, 2))


#Graphs of species

slegend <- get_legend( ggplot(tray1, aes(as.factor(x), y, group = Species)) +
    geom_point(size = 12, aes(shape = Species, colour = Species)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1")),times=2),
                        limits = levels(dat$Species)
    ) +
    scale_shape_manual(values=rep(c(19, 17),each=5)
    )+
    xlab("Tray 1")
)


s1 <- ggplot(tray1, aes(as.factor(x), y, group = Species)) +
    geom_point(size = 12, aes(shape = Species, colour = Species)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1")),times=2),
                        limits = levels(dat$Species)
    ) +
    scale_shape_manual(values=rep(c(19, 17),each=5)
    )+
    xlab("Tray 1")

s2 <- ggplot(tray2, aes(as.factor(x), y, group = Species)) +
    geom_point(size = 12, aes(shape = Species, colour = Species)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1")),times=2),
                        limits = levels(dat$Species)
    ) +
    scale_shape_manual(values=rep(c(19, 17),each=5)
    )+
    xlab("Tray 2")

s3 <- ggplot(tray3, aes(as.factor(x), y, group = Species)) +
    geom_point(size = 12, aes(shape = Species, colour = Species)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1")),times=2),
                        limits = levels(dat$Species)
    ) +
    scale_shape_manual(values=rep(c(19, 17),each=5)
    )+
    xlab("Tray 3")

s4 <- ggplot(tray4, aes(as.factor(x), y, group = Species)) +
    geom_point(size = 12, aes(shape = Species, colour = Species)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1")),times=2),
                        limits = levels(dat$Species)
    ) +
    scale_shape_manual(values=rep(c(19, 17),each=5)
    )+
    xlab("Tray 4")

s5 <- ggplot(tray5, aes(as.factor(x), y, group = Species)) +
    geom_point(size = 12, aes(shape = Species, colour = Species)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    scale_colour_manual(values=rep(c(brewer.pal(5,"Set1")),times=2),
                        limits = levels(dat$Species)
    ) +
    scale_shape_manual(values=rep(c(19, 17),each=5)
    )+
    xlab("Tray 5")


plot_grid(s1, s2, s3, slegend, nrow = 1, rel_widths = c(1,1,1,1))
plot_grid(s4, s5, slegend, nrow = 1, rel_widths = c(1,1,2))


#Graphs of both

t1 <- ggplot(tray1, aes(as.factor(x), y)) +
    geom_text(label = tray1$Both) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    xlab("Tray 1") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank()) 

t2 <- ggplot(tray2, aes(as.factor(x), y)) +
    geom_text(label = tray2$Both) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    xlab("Tray 2") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank()) 

t3 <- ggplot(tray3, aes(as.factor(x), y)) +
    geom_text(label = tray3$Both) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    xlab("Tray 3") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank()) 

t4 <- ggplot(tray4, aes(as.factor(x), y)) +
    geom_text(label = tray4$Both) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    xlab("Tray 4") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank()) 

t5 <- ggplot(tray5, aes(as.factor(x), y)) +
    geom_text(label = tray5$Both) +
    scale_y_reverse(breaks = 1:10) +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F"), breaks = 1:6, position = "top", drop = F) +
    xlab("Tray 5") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank()) 
plot_grid(t1, t2, t3, t4, t5, nrow = 1)

