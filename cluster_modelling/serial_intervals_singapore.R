######################################################################################################
###         Script to determine Serial Intervals for Singapore COVID-19 outbreak                   ###
###       Data manually entered from government of Singapore online reports of cases               ###
######################################################################################################

###~~ 1. Load packages and data ~~###
library(tidyverse)
library(lubridate)
library(EpiEstim) #not sure if we will need this quite yet or not...

sp.cov <- read_csv("Clustering/data/COVID-19_Singapore.csv")

# Ensure proper import and quick explore
glimpse(sp.cov)
head(sp.cov)
tail(sp.cov)
colSums(is.na(sp.cov))
table(sp.cov$cluster)

# Rename columns 2, 3 and 4 so no spaces
sp.cov <- rename(sp.cov, related_cases = starts_with("Related"),
                         cluster_links = "Cluster links",
                         relationship_notes = starts_with("Relation"))

# Change date columns into date objects
sp.cov <- mutate(sp.cov, presumed_infected_date = dmy(presumed_infected_date),
                         date_onset_symptoms = dmy(date_onset_symptoms),
                         date_quarantine = dmy(date_quarantine),
                         date_hospital = dmy(date_hospital),
                         date_confirmation = dmy(date_confirmation),
                         date_discharge = dmy(date_discharge))

# make sure dates parsed properly
range(sp.cov$presumed_infected_date, na.rm = T)
range(sp.cov$date_onset_symptoms, na.rm = T)
range(sp.cov$date_quarantine, na.rm = T)
range(sp.cov$date_hospital, na.rm = T)
range(sp.cov$date_confirmation, na.rm = T)
range(sp.cov$date_discharge, na.rm = T)


###~~~ 2. Split presumed_infected_date into different columns depending on the reason ~~~###
  #Don't want one column with different types of data in it
table(sp.cov$presumed_reason)
  #date_of_assembly = date of a group gathering which is presumed source of infection
  #date_sympt_presumed_infector = date when presumed infector started showing symptoms
  #last_poss_exposure = date of flight in from Wuhan or other area of infection

# Turn 'presumed_reason' into lower case and get trim any whitespace so don't have issues with case sensitivity, etc
sp.cov$presumed_reason <- str_to_lower(sp.cov$presumed_reason)
sp.cov$presumed_reason <- str_trim(sp.cov$presumed_reason)

# Need a new column for possible sources of contacts??? Ie some of the "symtom onsets" list multiple contacts but only one date
t <- which(!is.na(str_match(sp.cov$presumed_reason, "symptom onset")))
t1 <- sp.cov[t, ]
t1$CaseID
t1$related_cases
t1$presumed_reason 
    #Hmmm - I think there is an error in our data
    #Row 3 (CaseID 72) has 59 and 79 listed as related cases but presumed reason is "via 59 (date of 59 symptom onset)" - ie no mention of 79 in presumed reason
    #While row 5 (CaseID 79) has only 72 listed as a related case but presumed reason is "via 72 via 59 (date of 59 symptom onset)" - ie despite 59 not being listed as a related case, that is the source for the presumed date of infection
  #I think the infection path goes 59 to 72 to 79; but then the presumed_infected_date is wrong for case 79...as it would be from case 72 NOT 59, as it has been used
  
#TODO: I think I need to re-make the presumed_infected_date completely instead of trying to split the dates we have been using
  #as I think the dates are not consistently used in that column due to the time crunch of the hackathon
  

# Make a new column where we group the 'presumed_reason' under a label (assembly, contacted_case, travel) for each of the above three groups
#TODO: use MUTATE and/or grep and/or str_match for this!!


# Make a loop where if 'presumed_reason' column contains a given reason, the date goes into the appropriate new column


###~~~ 3. Need to reshape data so that make links between caseID and each possible infector
    #i.e. split the related cases and cluster links CaseIDs into separate columns, so don't have multiple numbers in one column
    #TODO: use pivot??