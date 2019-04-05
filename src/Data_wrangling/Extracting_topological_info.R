#Extracting topographical data from drone rasters
#31 March 2019
#---------------------------------------------

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(dplyr, plotKML, sp, raster, rgdal, here)

#Read in drone image
dem50 <- raster(here("Raw/Unprocessed", "Quartz_survey_dem_50.tif"))

#Read in geo refed plots
my_coords <- read.csv(here("Results/Data", "geo_ref_plots.csv")) 

#Set coords
coordinates(my_coords) <- c("lon", "lat")

#Aggregate raster pixels to res of 100cm
dem100 <- aggregate(dem50, fact = 2)

#Extract elevation of each plot
elev <- extract(dem100, my_coords)

#Create slope raster and extract values at each of the plots
slope_raster <- terrain(dem100, "slope")
slope <- extract(slope_raster, my_coords)

#Repeat for aspect and catchment area
aspect_raster <- terrain(dem100, "aspect")
aspect <- extract(aspect_raster, my_coords)

drain_raster <- raster(here("Raw/Unprocessed", "Catchment_area_dem100_14June.tif"))
drain <- extract(drain_raster, my_coords)                       

coords <- as.data.frame(my_coords)
data <- data.frame(coords, elevation = elev, slope = slope, aspect = aspect, drainage = drain)


write.csv(data, 
          here("Results/Data", "topographic_data.csv"), 
          row.names = F)
