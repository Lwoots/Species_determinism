#Tidying experimental design weight data measured by undergraduate David Klopp.
#Note: empty = samples lost during weighing process. All B7 samples seem to be lost this way.
# 1 February 2020
#-------------------------------------------------

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(tidyverse, here, visdat)

raw_weights <- read.csv(here("Raw/Unprocessed", "ED_weight_data_David.csv"))
names(raw_weights)[2] <- "Mass"
summary(raw_weights)

#Replace 'empty' with NA.

raw_weights$Mass[raw_weights$Mass == "empty"] <- NA
raw_weights$Mass <- as.numeric(as.character(raw_weights$Mass)) #Convert to numeric

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

#Average by code

avg_mass <- raw_weights %>% group_by(Code) %>% summarise(Mass = mean(Mass))

