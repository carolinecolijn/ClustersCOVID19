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
  #Hmmm, there are two people with both Wuhan and Life Church listed as thier cluster name

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
  #date_of_gathering = date of a group gathering which is presumed source of infection
  #date_sympt_presumed_infector = date when presumed infector (with a direct relationship to infectee) started showing symptoms
  #date_last_poss_exposure = date of flight in from Wuhan or other area of infection

# Turn 'presumed_reason' into lower case and get trim any whitespace so don't have issues with case sensitivity, etc
sp.cov$presumed_reason <- str_to_lower(sp.cov$presumed_reason)
sp.cov$presumed_reason <- str_trim(sp.cov$presumed_reason)

# Make a new column where we group the 'presumed_reason' under a label (known relationship, gathering, wuhan travel) for each of the above three groups
sp.cov <- mutate(sp.cov, presumed_reason_group = case_when(!is.na(str_match(presumed_reason, "symptom onset")) ~ "known relationship",
                                                           !is.na(str_match(presumed_reason, "grace|grand|life|seletar|yong")) ~ "gathering",
                                                           !is.na(str_match(presumed_reason, "wuhan|airport")) ~ "wuhan travel", #'airport' case (CaseID 17) does not have 'wuhan' in reason but does have it under 'Case' column that they are from Wuhan
                                                           is.na(presumed_reason) ~ NA_character_,
                                                           TRUE ~ "other")) #should not be any other, so is just a double check this has run correctly, especially as dataset grows

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


###~~~ 3. Need to reshape data so that make links between caseID and each possible infector
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
# Select down to the most useful columns for this analysis to make it easier to read
pairs <- select(pairs, c(CaseID, contactID, date_sympt_presumed_infector, Case, 
                         relationship_notes, presumed_reason_group, date_onset_symptoms,
                         date_confirmation, age, sex, country, additional_information))


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
