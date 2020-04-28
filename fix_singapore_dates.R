#####################################################################################################################
###         This script is to address the date formating issues in the updated Singapore dataset                  ###
###                                                                                                               ###
###         Currently, the csv downloads from git with years that are not possible (e.g. 2007)                    ###
###         This is likely due to most date columns having TWO formats (visible on github)                        ###
###         Where one of the formats has a year that is only two digits (i.e. '20' instead of '2020')             ###
###         This is problematic to fix systematically in excel and when directly parsing during import into R     ###
###         So this script fixes this problem as re-saves the dataset as a new csv                                ###
###                                                                                                               ###
#####################################################################################################################

### 1. Load packages and dataset   --------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
sdates <- read_csv("data/COVID-19_Singapore_eg_update_edges.csv")
s_org <- read_csv("Initial_submission/data/COVID-19_Singapore.csv")


### 2. Define the scope of the issue   ----------------------------------------------------------------------------------------------------------------
sum(is.na(sdates$date_onset_symptoms))  #12 rows that do not have date of symptom onset
sum(is.na(s_org$date_onset_symptoms))

oops <- dmy(sdates$date_onset_symptoms) #Warning that 39 rows failed to parse...aka NOT NA or in the dd/mm/yyyy format
sum(is.na(oops)) #51; so these 39 non-parsing rows have also been turned into NAs by dmy() function

ok <- dmy(s_org$date_onset_symptoms)
sum(is.na(ok)) #no problems here

table(sdates$date_onset_symptoms) 
  #We clearly have some dates that have WRONG years (i.e. 2005) and these all have a day of '20'
  #My guess is that somehow the 'days' were turned into years for these cells
  #aka Feb 5, 2020 was written as 05-02-20, and then became 2005-02-20 in the .csv 

t <- sdates %>% separate(date_onset_symptoms,into=c("day","month","year"),sep="/|-")
print(fct_count(t$day), n = 50)
fct_count(t$month)
fct_count(t$year) #39 rows that have year indicated as 

o <- s_org %>% separate(date_onset_symptoms,into=c("day","month","year"),sep="/|-")
print(fct_count(o$day), n = 50) #only ONE row that has a day of '20' in the original dataset without parsing errors
fct_count(o$month)
fct_count(o$year)


#Is this an issue in any other date columns? 
table(sdates$presumed_infected_date)  #Yes
table(sdates$last_poss_exposure)      #Yes
table(sdates$symp_presumed_infector)  #Yes
table(sdates$date_quarantine)         #Yes
table(sdates$date_hospital)           #Yes
table(sdates$date_confirmation)       #yes
table(sdates$date_discharge)          #Yes

rm(oops, ok, t, o) 


### 3. Fix by splitting dates, removing the extra "2000" added to the 'day', and check dates match original dataset  ----------------------------------

# Make a vector of all the columns that have a date
date_cols <- c("presumed_infected_date", "last_poss_exposure", "symp_presumed_infector", "date_onset_symptoms",
               "date_quarantine","date_hospital", "date_confirmation", "date_discharge")

# Loop to fix dates in all date columns
for(i in seq_along(date_cols)) {
  
  #Split the dates
  sdates <- sdates %>% separate(date_cols[i],
                                into = c("day","month","year"),
                                sep = "/|-")
  
  sdates$year <- ifelse(is.na(sdates$year), NA, 2020) #now the whole column is just 2020, not a mix of 20 and 2020, but keep the NAs
  sdates$month <- as.numeric(sdates$month) #removes the issue of some months being represented as 02 or 2
  sdates$day <- as.numeric(sdates$day) #removes the issue of some days being represented as 03 or 3
  
  #Fix issue of days having been converted into years
    #If the day is larger than 31, then subtract 2000, else return the day already present
  sdates$day <- ifelse(sdates$day > 31, sdates$day - 2000, sdates$day)
  
  #Unite back into one column, but turns NAs into character strings
  sdates <- sdates %>% 
    unite(col = new_date, day, month, year, sep = "/") 
  
  #Replace the NA/NA/NA character strings back to NA
  sdates$new_date <- str_replace(sdates$new_date, pattern = "NA/NA/NA", replacement = NA_character_)
  
  #Turn column into a date
  sdates$new_date <- dmy(sdates$new_date)
  
  #Rename the column with the original column name
  names(sdates) <- str_replace(names(sdates), pattern = "new_date", replacement = date_cols[i])

  #Indicate where in the loop we are in case it fails silently
  print(paste(date_cols[i], "column had dates fixed", sep = " "))
}


### 4. Double check date columns are identical between original dataset and new dataset   -------------------------------------------------------------

# Ensure in same order, based on the CaseID number
sdates <- arrange(sdates, CaseID)
s_org <- arrange(s_org, CaseID)

# Turn original data into dates as well
s_org$presumed_infected_date <- dmy(s_org$presumed_infected_date)
s_org$last_poss_exposure <- dmy(s_org$last_poss_exposure)
s_org$symp_presumed_infector <- dmy(s_org$symp_presumed_infector)
s_org$date_onset_symptoms <- dmy(s_org$date_onset_symptoms)
s_org$date_quarantine <- dmy(s_org$date_quarantine)
s_org$date_hospital <- dmy(s_org$date_hospital)
s_org$date_confirmation <- dmy(s_org$date_confirmation)
s_org$date_discharge <- dmy(s_org$date_discharge)

# Are they the same? (Should all return TRUE)
identical(sdates$presumed_infected_date, s_org$presumed_infected_date)
identical(sdates$last_poss_exposure, s_org$last_poss_exposure)
identical(sdates$symp_presumed_infector, s_org$symp_presumed_infector)
identical(sdates$date_onset_symptoms, s_org$date_onset_symptoms)
identical(sdates$date_quarantine, s_org$date_quarantine)
identical(sdates$date_hospital, s_org$date_hospital)
identical(sdates$date_confirmation, s_org$date_confirmation)
identical(sdates$date_discharge, s_org$date_discharge)


### 5. Save as a new .csv file  -----------------------------------------------------------------------------------------------------------------------
# write.csv(sdates, file = "data/COVID-19_Singapore_formated_dates.csv")

