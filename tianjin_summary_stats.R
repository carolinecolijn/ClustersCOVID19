#~~~ POSSIBLE SUMMARY MEASURES FOR STRATIFICATION FOR TIANJIN ~~~#

library(deSolve)
library(ggplot2)
library(tidyverse)

tj_data <-read_csv("Clusters/tianjin_data/Tianjin_125_cases_updated_Feb_17_csv.csv")
head(tj_data)
tail(tj_data)

glimpse(tj_data)
  #Note that for gender, 0 = female and 1 = male; as per notes column

#Change gender to a factor
tj_data$gender <- factor(tj_data$gender,
                         levels = c(0, 1),
                         labels = c("male", "female"))
summary(tj_data$gender)
table(tj_data$gender, tj_data$death)
  #Hmmm...will need to create a new column for death vs no death if want to use this further

tj_data %>% group_by(gender) %>% 
  summarize(n_gender = n(),
            prop_gender = n()/nrow(tj_data),
            n_deaths = sum(!is.na(death)),
            ave_age = mean(age))

table(tj_data$gender, tj_data$severity)
table(tj_data$gender, tj_data$source)
  #This also requires some cleaning to be able to use...but could be interesting!
    #Anything starting with TJ should be changed to "confirmed patient"
    #Hebei needs to changed to Hubei and Hubei; TJ45 needs to either be patient or Hubei
    #all the 'mall' references need to be grouped into one category
    #Travel and Train probably should be grouped (only one "travel" case), or travel should be "other travel"
    #Needto decide if Wuhan; TJ1 and Wuhan; train import should be Wuhan or the patient/train source
table(tj_data$source)
sum(is.na(tj_data$source))
      #9 NAs that should also be changed to "unknown"

# Data cleaning to group 'source' into Wuhan, other region, confirmed patient, mall, train and other travel mode, unknown
    #Decision rule is to prioritize region of travel first, then confirmed patient..
t <- tj_data
which(t$source == "Hubei")
t$source <- str_replace(t$source, "Hebei", "Hubei") # to fix spelling error
t$source <- str_replace(t$source, "mall", "mall") #to change all descriptions of mall to one label
t <- mutate(tj_data, source_group = case_when(source ))

#Age
sum(is.na(tj_data$age)) #0

mean(tj_data$age)
range(tj_data$age)
table(tj_data$age)

# To see how many cases of each age group
ggplot(tj_data, aes(x = age)) +
  geom_bar()
  #To me no obvious breaks in age groups

# To group into 10 age groups
ggplot(tj_data, aes(x = age)) +
  geom_histogram(bins = 10)

# To summarize, need to group manually into age categories
tj_data <- tj_data %>% mutate(age_group = case_when(age <10 ~"under10y",
                                         age >= 10 & age < 20 ~ "y10to19",
                                         age >= 20 & age < 30 ~ "y20to29",
                                         age >= 30 & age < 40 ~ "y30to39",
                                         age >= 40 & age < 50 ~ "y40to49",
                                         age >= 50 & age < 60 ~ "y50to59",
                                         age >= 60 & age < 70 ~ "y60to69",
                                         age >= 70 & age < 80 ~ "y70to79",
                                         age >= 80 & age < 89 ~ "y80to89",
                                         age >= 90 ~ "y90over"))

ggplot(tj_data, aes(x = age_group)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tj_data %>% group_by(age_group) %>% 
  summarize(n_age = n(),
            prop_age = n()/nrow(tj_data),
            n_deaths = sum(!is.na(death)))

table(tj_data$age_group, tj_data$severity)

ggplot(tj_data, aes(x = age_group)) +
  geom_bar(aes(fill = severity), alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(tj_data, aes(x = age, fill = severity, color = severity)) +
  geom_density(alpha = 0.7)
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #maybe more severe cases in older age groups??? I'm not super convinced by these graphs, 
    #but peak of density function is slightly older age for severe vs normal disease

table(tj_data$age_group, tj_data$source)
  #Really really need to group this variable to make any sense of this...