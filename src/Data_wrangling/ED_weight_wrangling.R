#Weight data ####
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



#### Growth data ####

#Get ED data

ED_dat <- read.csv(here("Raw/Unprocessed", "ED_growth_metrics_23Oct_full.csv"), sep = ";" )

#Exclude misidentified individuals
ED_dat <- ED_dat %>% filter(!notes == "wrong_id")

#Exclude rows where individuals never sprouted

filtered_ED_dat <- ED_dat %>% filter(!(is.na(Height_1) &
                                       is.na(Height_2) &
                                       is.na(Height_3)
                                       )
                                     )

#Exclude notes, leaf width, and intermediate time intervals for now

cleaned_ED_dat <- filtered_ED_dat %>% select(-Height_1a, -Height_2a, -Time_1a, -Time_2a, -notes, -Leaf_3)
rm(filtered_ED_dat)

#Exclude rows containing dicrocaulon (too few to analyse)
cleaned_ED_dat <- cleaned_ED_dat[-c(548:551),]

#Add a missing individual number
cleaned_ED_dat$Individ[722] <- 5

#Replace NAs with 0
cleaned_ED_dat[is.na(cleaned_ED_dat)] <- 0

#For species measured under the microscope, divide by 10 to convert epu to mm.

cleaned_ED_dat[cleaned_ED_dat$Species == "G", c(8,10,12)] <- cleaned_ED_dat[cleaned_ED_dat$Species == "G", c(8,10,12)]/10
cleaned_ED_dat[cleaned_ED_dat$Species == "H", c(8,10,12)] <- cleaned_ED_dat[cleaned_ED_dat$Species == "H", c(8,10,12)]/10
cleaned_ED_dat[cleaned_ED_dat$Species == "I", c(8,10,12)] <- cleaned_ED_dat[cleaned_ED_dat$Species == "I", c(8,10,12)]/10

#Rename

names(cleaned_ED_dat)[1] <- "Code"


#I want to average height by group, not including individuals of zero height.
#Need to write a function for this
zero_free_mean <- function(x) {
    zvals <- x==0
    if (all(zvals)) 0 else mean(x[!zvals])
}

#Test it

#foo <- c(4, 5, 6)
#goo <- c(4, 5, 6, 0)
#hoo <- c(0, 0, 0)
#
#mean(foo)
#mean(goo)
#zero_free_mean(goo)
#zero_free_mean(hoo)

avg_height <- cleaned_ED_dat %>% group_by(Code) %>% summarise(Height_t1 = zero_free_mean(Height_1),
                                                              Height_t2 = zero_free_mean(Height_2),
                                                              Height_t3 = zero_free_mean(Height_3))


#Combining datasets ####

full_ED_data <- left_join(avg_height, avg_mass, by = "Code")
plot(full_ED_data$Mass, full_ED_data$Height_t3)
