#Extrapolating gps data from corner points to get coordinates for every plot 

#29 March
#----------------------------------------------------------------------

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(dplyr, plotKML, sp, gstat, here)

waypts <- readGPX(here("Raw/Unprocessed", "Waypoints_01-MAY-18.gpx"))

my_points <- data.frame(waypts$waypoints)


#Extrapolate coords along the edge plots
left_sides_lat <- c(seq(my_points$lat[8], my_points$lat[7], length.out = 11),
                    seq(my_points$lat[2], my_points$lat[1], length.out = 11),
                    seq(my_points$lat[13], my_points$lat[14], length.out = 11))
right_sides_lat <- c(seq(my_points$lat[9], my_points$lat[10], length.out = 11),
                     seq(my_points$lat[5], my_points$lat[4], length.out = 11),
                     seq(my_points$lat[12], my_points$lat[11], length.out = 11))

extrap_lat <- data.frame(left_sides_lat, right_sides_lat)

#Extrapolate along rows
row_lat <- apply(extrap_lat, 
                 1, 
                 function(x) seq(x[1], x[2], length.out = 11))

#Repeat for lon

#Extrapolate coords along the edge plots
left_sides_lon <- c(seq(my_points$lon[8], my_points$lon[7], length.out = 11),
                    seq(my_points$lon[2], my_points$lon[1], length.out = 11),
                    seq(my_points$lon[13], my_points$lon[14], length.out = 11))
                    
                    
right_sides_lon <- c(seq(my_points$lon[9], my_points$lon[10], length.out = 11),
                     seq(my_points$lon[5], my_points$lon[4], length.out = 11),
                     seq(my_points$lon[12], my_points$lon[11], length.out = 11))

extrap_lon <- data.frame(left_sides_lon, right_sides_lon)

#Extrapolate along rows
row_lon <- apply(extrap_lon, 
                 1, 
                 function(x) seq(x[1], x[2], length.out = 11))

#Create dataset with coords in long format 
all_coords <- data.frame(lon = as.vector(row_lon), 
                        lat = as.vector(row_lat))

#Read in plots that have been reordered to match coords

plots <- read.csv(here("Raw/Tidy", "reordered2_plots.csv"))

geo <- data.frame( plots, 
                   all_coords,
                   site = c(rep("site1", 121), rep("site2", 121), rep("site3", 121)))

#Write to csv
write.csv(geo, 
          here("Results/Data", "geo_ref_plots.csv"), 
          row.names = F)
