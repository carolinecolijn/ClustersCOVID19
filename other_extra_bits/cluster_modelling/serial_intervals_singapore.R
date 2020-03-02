######################################################################################################
###         Script to determine Serial Intervals for Singapore COVID-19 outbreak                   ###
###       Data manually entered from government of Singapore online reports of cases               ###
######################################################################################################

###~~ 1. Load packages and data ~~###
library(tidyverse)
library(lubridate)
#library(EpiEstim) #not sure if we will need this quite yet or not...

#sp.cov <- read_csv("Clustering/data/COVID-19_Singapore.csv") #Hackathon dataset
sp.cov <- read_csv("Clustering/data/COVID-19_Singapore - singapore_ncov_2019_fix.csv")

# Ensure proper import and quick explore
glimpse(sp.cov)
head(sp.cov)
tail(sp.cov)
colSums(is.na(sp.cov))
table(sp.cov$cluster)
  #Hmmm, there are two people with both Wuhan and Life Church listed as thier cluster name

# Rename columns 2, 3 and 4 so no spaces
sp.cov <- rename(sp.cov, related_cases = starts_with("Related"),
                         cluster_links = "Cluster links",
                         relationship_notes = starts_with("Relation"))

# Change date columns into date objects
sp.cov <- mutate(sp.cov, presumed_infected_date = dmy(presumed_infected_date),
                         last_poss_exposure = dmy(last_poss_exposure),
                         symp_presumed_infector = dmy(symp_presumed_infector),
                         date_onset_symptoms = dmy(date_onset_symptoms),
                         date_quarantine = dmy(date_quarantine),
                         date_hospital = dmy(date_hospital),
                         date_confirmation = dmy(date_confirmation),
                         date_discharge = dmy(date_discharge))

# make sure dates parsed properly
range(sp.cov$presumed_infected_date, na.rm = T)
range(sp.cov$last_poss_exposure, na.rm = T)
range(sp.cov$symp_presumed_infector, na.rm = T)
range(sp.cov$date_onset_symptoms, na.rm = T)
range(sp.cov$date_quarantine, na.rm = T)
range(sp.cov$date_hospital, na.rm = T)
range(sp.cov$date_confirmation, na.rm = T)
range(sp.cov$date_discharge, na.rm = T)


###~~~ 2. Split presumed_infected_date into different columns depending on the reason ~~~###
  #Don't want one column with different types of data in it
table(sp.cov$presumed_reason)
  #date_of_gathering = date of a group gathering which is presumed source of infection
  #date_sympt_presumed_infector = date when presumed infector (with a direct relationship to infectee) started showing symptoms
  #date_last_poss_exposure = date of flight in from Wuhan or other area of infection

# Turn 'presumed_reason' into lower case and get trim any whitespace so don't have issues with case sensitivity, etc
sp.cov$presumed_reason <- str_to_lower(sp.cov$presumed_reason)
sp.cov$presumed_reason <- str_trim(sp.cov$presumed_reason)
table(sp.cov$presumed_reason)
sum(is.na(sp.cov$presumed_reason_group))

# Make a new column where we group the 'presumed_reason' under a label (known relationship, gathering, wuhan travel) for each of the above three groups
sp.cov <- mutate(sp.cov, presumed_reason_group = case_when(!is.na(str_match(presumed_reason, "symptom onset|via")) ~ "known relationship",
                                                           !is.na(str_match(presumed_reason, "grace|grand|life|seletar|yong")) ~ "gathering",
                                                           !is.na(str_match(presumed_reason, "wuhan|airport")) ~ "wuhan travel", #'airport' case (CaseID 17) does not have 'wuhan' in reason but does have it under 'Case' column that they are from Wuhan
                                                           is.na(presumed_reason) ~ NA_character_,
                                                           TRUE ~ "other")) #should not be any other, so is just a double check this has run correctly, especially as dataset grows
table(sp.cov$presumed_reason_group)
sum(is.na(sp.cov$presumed_reason_group))

# Split the "presumed_infected_date" into multiple columns, based on the grouping from the 'presumed_reason_group' 
  #so that how the date of presumed infection is derived consistently
sp.cov <- sp.cov %>% 
          mutate(date_of_gathering = case_when(presumed_reason_group == "gathering" ~ as.character(presumed_infected_date),
                                               TRUE ~ NA_character_),
                 date_sympt_presumed_infector = case_when(presumed_reason_group == "known relationship" ~ as.character(presumed_infected_date),
                                                          TRUE ~ NA_character_),
                 date_last_poss_exposure = case_when(presumed_reason_group == "wuhan travel" ~ as.character(presumed_infected_date),
                                                     TRUE ~ NA_character_)) %>% 
          mutate(date_of_gathering = ymd(date_of_gathering),
                 date_sympt_presumed_infector = ymd(date_sympt_presumed_infector),
                 date_last_poss_exposure =  ymd(date_last_poss_exposure))

glimpse(sp.cov)
sum(is.na(sp.cov$date_sympt_presumed_infector)) 

  #TODO: Change date_sympt_presumed_infector for case 79
    #CaseID 72 has 59 and 79 listed as related cases but presumed reason is "via 59 (date of 59 symptom onset)" - ie no mention of 79 in presumed reason
    #CaseID 79 has only 72 listed as a related case but presumed reason is "via 72 via 59 (date of 59 symptom onset)" - ie despite 59 not being listed as a related case, that is the source for the presumed date of infection
    #I think the infection path goes 59 to 72 to 79; but then the presumed_infected_date needs to be changed for case 79...to keep it consistent in the column, it would be from case 72 NOT 59, as it has been used
  #Maybe wait until look at case-pairs so I'm not making erroneous assumptions


###~~~ 3. Need to reshape data so that make links between caseID and each possible infector - USING ONLY 'KNOWN RELATIONSHIP' RELATED CASES ~~~###
# Select only individuals who are suspected to have acquired COVID-19 directly through a known close relationship with another case
related <- filter(sp.cov, presumed_reason_group == "known relationship")

# Make sure we have info on all the primary cases in here too
related$relationship_notes
related$related_cases
related$presumed_reason
related$CaseID

# Nope, missing all of the potential primary cases who are presumed infectors; based on 'presumed_reason' column
  #The primary cases that are missing are are CaseIDs 13, 26, 50, 55, 83, 91, 59, 41, [72 - have] , 82
primaries <- c(13, 26, 50, 55, 83, 91, 59, 41, 82)
m.related <- filter(sp.cov, CaseID %in% primaries)

related <-  bind_rows(related, m.related)

# Need to have a couple of columns that list the possible infectors; will need to do a hard code work-around for the time being

related <- mutate(related, poss_infectors = related_cases)
related$CaseID
related$presumed_reason

related[1, "poss_infectors"] <- c("13, 26")
related[2, "poss_infectors"] <- c("50, 55")
related[3, "poss_infectors"] <- c("83, 91")
related[4, "poss_infectors"] <- c("59")
related[5, "poss_infectors"] <- c("41")
related[6, "poss_infectors"] <- c("72") #Note that 59 is NOT listed as related to Case 79; CaseID 79 is a family member of 72; while CaseID 72 is a contact of CaseID 59
related[7, "poss_infectors"] <- c("82")
related[8:nrow(related), "poss_infectors"] <- NA

# Split into separate columns
related <- separate(related,
                    col = poss_infectors,
                    into = paste("contactID", 1:2, sep = "_"),
                    fill = "right")

# Turn into numeric values
related <- mutate(related, 
                    contactID_1 = as.numeric(contactID_1),
                    contactID_2 = as.numeric(contactID_2))


# Select down to critical columns for serial interval analysis
singapore <- select(related, c(CaseID, date_onset_symptoms, contactID_1, contactID_2, symp_presumed_infector,
                               presumed_infected_date, presumed_reason, relationship_notes, additional_information))
View(singapore) 
  #To see which cases are linked and to ensure the symp_presumed_infector is actually the date of the earliest onset 
    #of symptoms from listed possible infectors (initially as text under presumed_reason column)

# Fix discrepancies
## NOTE: can remove this section once have updated master google file ##
  #CaseID 66 changed to 2020-01-23 for date of onset of symptoms, based on earliest date from either of case 83 or 91; 
    #currently listed as 2020-01-25 which is mid-way bewteen dates of onset for both presumed infectors
  singapore$symp_presumed_infector[singapore$CaseID == 66] <- ymd("2020-01-23")
  
  #CaseID 79 changed to 2020-02-10, based on the earliest date of onset of symptoms from case 72; this was originally listed as date of onset of symptoms from case 52 (2020-02-07), 
    #but based on info provided, contact is between 72 (family member) and not directly to case 52
  singapore$symp_presumed_infector[singapore$CaseID == 79] <- ymd("2020-02-10")
  
  #CaseIDs 83 and 91 both have dates (2020-01-19) currently listed under date of onset of symptoms, but no caseID for presumed infector (any column)
    #I suspect this relates to the life church gathering, so symp_presumed_infector should be changed to NA
  
  # Case 83 and 89 attended Life Church and were likely infected by Case 8 and 9, hence the date of Jan 19
    #Not listed here - need to look under cases 8 and 9 to find that info, so I am probably missing other cases too!!
  
  #TODO: go through sheet manually to determine all the direct links
  
  
rm(primaries, m.related)


###~~~ 4. Save dataset for easier access for R0 and serial interval analyses ~~~###
write_csv(singapore, path = "Clustering/data/singapores_pairs_Feb25.csv")



######################## BELOW: Still a work in progress ######################################################################
###~~~ 3. Need to reshape data so that make links between caseID and each possible infector - USING ALL RELATED CASES ~~###
  #Although on further inspection, this includes cases related through multiple ways (ie some gatherings, some direct known contacts, but also misses some gatherings)
    #So probably need to inspect/clean this further (ie add all case IDs listed under 'related_cases' and 'cluster_links' columns)
# Subset data so that it contains only those with related cases
pairs <- filter(sp.cov, !is.na(related_cases))
glimpse(pairs)
head(pairs$related_cases, n = nrow(pairs)) 
  #have multiple related cases within one column, seperated by commas; need to put these all into separate columns to use
  #there's also one case that has a new line indicator in that column...either need to replace with appropriate data or get rid of this row

# Separate case IDs from 'related_cases' columns into one column for each related case
pairs <- separate(pairs, 
                  col = related_cases, 
                  into = paste("contactID", 1:7, sep = "_"),
                  fill = "right")

# Turn into numeric values; not actually sure if I need this but likely do for plotting later...
pairs <- mutate(pairs, 
                contactID_1 = as.numeric(contactID_1),
                contactID_2 = as.numeric(contactID_2),
                contactID_3 = as.numeric(contactID_3),
                contactID_4 = as.numeric(contactID_4),
                contactID_5 = as.numeric(contactID_5),
                contactID_6 = as.numeric(contactID_6),
                contactID_7 = as.numeric(contactID_7))

# Turn the dataset into a long format, so there is a row for each infectee (i.e. CaseID) paired with possible related infector case (i.e. contactID_# columns)
pairs <-  pairs %>% 
             pivot_longer(cols = starts_with("contactID"),
                          names_to = "n_contacts",
                          names_prefix = "contactID_",
                          names_ptypes = numeric(),
                          values_to = "contactID",
                          values_drop_na = T)

# Select down to critical columns for serial interval analysis
pairs2 <- select(pairs, c(CaseID, date_onset_symptoms, contactID, relationship_notes, additional_information))

# Add CaseIDs missing from the "related cases"
#NOTE: we seem to be missing some data in our "related_cases" column - Case 30 is listed as a relationship for CaseID 36, but is not included as a 'related case'
missing <- sp.cov %>% 
            filter(CaseID == 30) %>% 
            mutate(contactID = NA) %>% 
            select(c(CaseID, date_onset_symptoms, contactID, relationship_notes, additional_information))

pairs2 <- bind_rows(pairs2, missing)

# Add column for date of symptoms of each contact ID in the row for each Case
  #TODO: still not running quite correctly; seems to get stuck when missing rows where the CaseID for the potential infector is not in the dataset
    #See if adding missing data above helps or not...

caseids <- unique(pairs2$CaseID)
contacts.list <- list()
case.list <- list()

for(i in seq_along(caseids)){
  data <- filter(pairs2, CaseID == caseids[i])
  contactids <- unique(data$contactID)
  
  for(j in seq_along(contactids)){
    p <- filter(pairs2, CaseID == caseids[i] & contactID == contactids[j])
    p$contact_sympt_onset <- unique(pairs2$date_onset_symptoms[pairs$CaseID == contactids[j]])
    contacts.list[[j]] <- p
    q <- contacts.list
    print(paste("contact #", contactids[j], sep = " "))
  }
  
  case.list[[i]] <- q
  print(paste("case #", caseids[i], sep = " "))
}

# Turn the list back into a tibble / dataframe
pairs.list <-  list()

for(i in seq(caseids)){
  pairs.list[[i]] <- map_df(case.list[[i]],~.x)
  print(i)
}




rm(p, q, contactids, caseids)

######################## BELOW: To remove eventually???????? ######################################################################
### 4. Quick Exploration
# Summarize info for each CaseID 
caseID.summary <- pairs %>% 
                 group_by(CaseID) %>% 
                 summarise(n_contacts = n(),
                           date_sick = min(date_onset_symptoms, na.rm = T))

# Quick data explore to figure out how to impute presumed date of infection
hist(caseID.summary$n_contacts)
sum(is.na(pairs$date_sympt_presumed_infector)) #83 missing...hmmm... I think I will need to fill this column???; at least for those who are secondary cases
sum(is.na(pairs$date_onset_symptoms)) #9 individuals are missing date of symptom onset

pairs$fCaseID <- factor(pairs$CaseID)
ggplot(pairs, aes(x = CaseID, y = contactID, color = fCaseID)) +
  geom_point() + 
  theme_bw()
######################## ABOVE: To remove eventually???????? ######################################################################
