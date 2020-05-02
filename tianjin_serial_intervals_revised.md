---
title: "Tianjin serial intervals - Revisions"
author: "Caroline Colijn, Michelle Coombe, and Manu Saraswat"
date: "2020-05-02"
output: 
  html_document:  
    keep_md: TRUE
---



## Data 

Thanks to Dongxuan Chen and Louxin Zhang. These data are from three main sources:

* source1: http://wsjk.tj.gov.cn/col/col87/index.html#!uid=259&pageNum=1 (Tianjin health commission official website, for daily announcements)

* source2: https://weibo.com/u/2967529507 (Jinyun News, Tianjin offical local media weibo account, for patient symptom onset reference)

* source3: https://m.weibo.cn/status/IrrHI1FHm?jumpfrom=weibocom (another Tianjin local media weibo link, for mall cluster reference)


```r
tdata <- read.csv("data/Tianjin135casesFeb22.csv",na.strings = "", stringsAsFactors = F)

tdata$symptom_onset=as.Date(tdata$symptom_onset, format = "%d/%m/%Y")
tdata$start_source=as.Date(tdata$start_source, format = "%d/%m/%Y")
tdata$end_source=as.Date(tdata$end_source,format = "%d/%m/%Y" )
tdata$confirm_date=as.Date(tdata$confirm_date,format = "%d/%m/%Y" )

glimpse(tdata)
```

```
## Observations: 135
## Variables: 13
## $ case_id          <chr> "TJ1", "TJ2", "TJ3", "TJ4", "TJ5", "TJ6", "TJ7", "TJ…
## $ gender           <chr> "F", "M", "F", "M", "M", "M", "F", "M", "M", "M", "F…
## $ age              <int> 59, 57, 68, 40, 46, 56, 29, 39, 57, 30, 55, 79, 19, …
## $ symptom_onset    <date> 2020-01-14, 2020-01-18, 2020-01-14, 2020-01-14, 202…
## $ symptom_type     <chr> "NA", "NA", "NA", "NA", "sore throat", "fever", "fev…
## $ confirm_date     <date> 2020-01-21, 2020-01-21, 2020-01-21, 2020-01-21, 202…
## $ Infection_source <chr> "Wuhan", "Wuhan; train import", "Wuhan", "Wuhan", "t…
## $ start_source     <date> 2020-01-05, NA, NA, NA, NA, NA, NA, 2020-01-19, NA,…
## $ end_source       <date> 2020-01-14, 2020-01-18, 2020-01-14, 2020-01-14, 202…
## $ severity         <chr> "severe", "severe", "severe", "normal", "severe", "s…
## $ death            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ recorrection     <chr> "added end source", "added end source", NA, NA, NA, …
## $ notes            <chr> "sometimes Tianjin didn't mention the patient's seve…
```


## Estimates of serial interval from Tianjin data (without imputation)
We will estimate the serial interval using the 'interval case to case' approach given in Vink et al (https://academic.oup.com/aje/article/180/9/865/2739204). 

Note that we are removing cases that do NOT have a date of symptom onset to determine the ICC. There are 10 cases without symptom onset, but all have a confirmation date. We will re-run the estimates with imputed data a little later on. 

```r
# Make a copy of the original dataset for use later on in imputed serial interval estimates
tdata_org <- tdata

# Remove any cases that are missing data for date of symptom onset
tdata <- tdata[which(!is.na(tdata$symptom_onset)),]  #removes 10 observations
```

Now let's make a column which groups all the information in the 'Infection_source' column into categories. Because the infection_source column can contain multiple possible sources of infection (for a handful of cases), it is important to consistently apply a decision rule for when each case would be assigned to a particular infection source group. Here we are emphasizing the Wuhan/Hubei and other known mall outbreak clusters over known interpersonal relationships, as it seems to best represent the introduction of the outbreak. **These groups are NOT used in the estimation of the serial intervals - only to help visualize the network graph.**

Decision rule applied to source_group label classification:

1. Known outbreak cluster locations (e.g. Wuhan/Hubei, mall, or church) *highest priority*

2. Known close relationship to another case (i. family; ii. work; iii. other known direct contact)

3. Known travel history to non-outbreak locations or unclear destinations

4. Any other listed associations (e.g. being part of a particular at-risk group such as airport worker)

5. No known source of possible viral exposure *lowest priority*


```r
# Make a column for sensible groupings for Tianjin, based on the reason why the case was exposed
# Turn 'Infection_source' into lower case and get trim any whitespace so don't have issues with case sensitivity, etc
tdata$Infection_source <- str_to_lower(tdata$Infection_source)
tdata$Infection_source <- str_trim(tdata$Infection_source)
table(tdata$Infection_source)
```

```
## 
##                      appeared in workplaces of tj37/tj43 
##                                                        1 
##                                         business partner 
##                                                        1 
##                                     clos contact of tj68 
##                                                        1 
##                       close contacts of cases tj64; tj99 
##                                                        1 
##                         colleague of cases tj34 and tj37 
##                                                        1 
##                                         colleague of tj6 
##                                                        1 
##                                        confirmed patient 
##                                                        3 
##                     confirmed patient and family of tj39 
##                                                        2 
##        contact with shoppers living in the same viliiage 
##                                                        1 
##                          coworker of a person from wuhan 
##                                                        1 
##                                          coworker of tj5 
##                                                        1 
##                              coworkers of cases tj2; tj6 
##                                                        1 
##                                        dshopping in mall 
##                                                        1 
##                                         family of  tj124 
##                                                        1 
##                                  family of tj14 and tj25 
##                                                        1 
##                                           family of tj17 
##                                                        1 
##                                           family of tj20 
##                                                        1 
##                                           family of tj34 
##                                                        1 
##                                            family of tj9 
##                                                        1 
##                                                    hubei 
##                                                        3 
##                                              hubei; tj45 
##                                                        1 
##                                         husband of tj119 
##                                                        1 
##                                          living in hubei 
##                                                        1 
##                             living in the mall proximity 
##                                                        3 
##                                                     mall 
##                                                        2 
##                     mother of tj115, travel to huhehaote 
##                                                        1 
##                             relative of cases tj20, tj35 
##                                                        1 
##                                         relative of tj53 
##                                                        1 
##                                      shooper in the mall 
##                                                        1 
##                                      shopper at the mall 
##                                                        4 
##                                      shopper in the mall 
##                                                        7 
##                                         shopping in mall 
##                                                        2 
##                                       shuperstore worker 
##                                                        1 
##                                          sister of tj109 
##                                                        1 
##                               son of cases  tj119; tj121 
##                                                        1 
##                                                    tj100 
##                                                        1 
##                                            tj100， tj103 
##                                                        1 
##                                      tj100, tj103, tj108 
##                                                        1 
##                                                    tj102 
##                                                        1 
##                                                    tj103 
##                                                        1 
##                                                    tj114 
##                                                        1 
##                                                     tj27 
##                                                        1 
##                                                     tj38 
##                                                        1 
##                                                     tj43 
##                                                        1 
##                                             tj5 (family) 
##                                                        2 
##                                              tj5(family) 
##                                                        1 
##                                             tj50  family 
##                                                        1 
##                                                     tj51 
##                                                        1 
##                                             tj6 (family) 
##                                                        1 
##                                                      tj7 
##                                                        1 
##                                                     tj70 
##                                                        1 
##                                                     tj72 
##                                                        2 
##                                               tj72, tj85 
##                                                        1 
##                                                     tj75 
##                                                        1 
##                                               tj75; tj87 
##                                                        1 
##                                                     tj82 
##                                                        2 
##                                                     tj87 
##                                                        1 
##                                                tj9; tj21 
##                                                        1 
##                                                     tj93 
##                                                        1 
##                                    tj93, living in baodi 
##                                                        1 
##                              tj93, tj96, living in baodi 
##                                                        1 
##                                               tj93, tj97 
##                                                        1 
##                                              tj93; tj107 
##                                                        1 
##                                                     tj95 
##                                                        1 
##                                                    train 
##                                                        1 
##                                               train crew 
##                                                        1 
##                                             train import 
##                                                        2 
##                                                   travel 
##                                                        1 
##                                          travle to wuhan 
##                                                        1 
##                                           trip to dalian 
##                                                        1 
##                                            trip to hebei 
##                                                        1 
##                                                  unclear 
##                                                        1 
##                                                  unknown 
##                                                        4 
##           wife of tj110 shopping at the baodi superstore 
##                                                        1 
##        wife of tj117;living at the proximity of the mall 
##                                                        1 
## wife of tj82, living at proximity of baodi shopping mall 
##                                                        1 
##                                       worker in the mall 
##                                                        1 
##                             worker in the train compoany 
##                                                        1 
##                                                    wuhan 
##                                                       16 
##                                               wuhan; tj1 
##                                                        1 
##                                      wuhan; train import 
##                                                        2
```

```r
sum(is.na(tdata$Infection_source)) #3 NAs
```

```
## [1] 3
```

```r
#Note that the order the data are selected in is VERY important to which case goes into which source_group category
  #For those that meet multiple criteria (e.g. wuhan; tj1), the str_match which is highest in the case_when call (i.e. "wuhan|hubei") will have priority over those matching later 
  #so that the 'source' column contain "wuhan; tj1" would be labelled as infection from a "wuhan" rather than from a "known relationship" origin 

#See what happens when we emphasize the wuhan and travel cases over known relationships
  #This seems more logical, given that the epicenter of the outbreak was Wuhan
tdata <- mutate(tdata, source_group = case_when(!is.na(str_match(Infection_source, "wuhan|hubei")) ~ "Wuhan and Hubei", #Priority 1
                                                  !is.na(str_match(Infection_source, "mall|store|shopper")) ~ "Mall", #Priority 1
                                                  !is.na(str_match(Infection_source, "family|relative|wife|mother|son|sister|husband")) ~ "Relative", #Priority 2
                                                  !is.na(str_match(Infection_source, "coworker|business|workplace|colleague")) ~ "Coworker", #Priority 2
                                                  !is.na(str_match(Infection_source, "tj|patient")) ~ "Known relationship", #Priority 2
                                                  !is.na(str_match(Infection_source, "train|travel|trip|hebei|dalian")) ~ "Other travel", #Priority 3
                                                  !is.na(str_match(Infection_source, "unknown|unclear")) ~ "Unknown", #Priority 5
                                                  is.na(Infection_source) ~ "Unknown", #Priority 5
                                                  T ~ "other")) #there should be none of these, so this is just a sanity check!  
table(tdata$source_group) 
```

```
## 
##           Coworker Known relationship               Mall       Other travel 
##                  6                 32                 26                  8 
##           Relative            Unknown    Wuhan and Hubei 
##                 19                  8                 26
```

The dataset has quite a few instances where a putative infector or contact is known. These are listed in the 'Infection_source' column. We first make a graph in which nodes are individuals and edges are present from cases listed as possible sources, to the cases for whom they are possible sources. These are extracted regardless of which infection source group label has been applied.

```r
mynodes <- tdata$case_id

#Transform everything to lower case to make sure there aren't any issues with matching due to case inconsistencies
mynodes <- str_to_lower(mynodes) 
tdata$case_id <- str_to_lower(tdata$case_id)

edges = data.frame(from=mynodes[9],to=mynodes[21],stringsAsFactors = F ) # i read this one manually 

for (id in 1:nrow(tdata)) {
tonode=tdata$case_id[id]
fromnodes=str_extract_all(tdata$Infection_source[id], "tj\\d+", simplify = T) #in lower case due to above early/late split on infection source
  if (length(fromnodes)>0) {
    for (k in 1:length(fromnodes)) {
      edges=rbind(edges, c(fromnodes[k], tonode))
    }
  }
}
head(edges)
```

```
##   from   to
## 1  tj9 tj21
## 2  tj6 tj11
## 3  tj5 tj12
## 4  tj5 tj13
## 5  tj5 tj16
## 6  tj5 tj17
```

```r
edges=edges[-1,] #Remove the initial relationship we gave so it isn't duplicated
edges=edges[-which(is.na(edges[,1])),] # NAs arose from a few empty entries for Infection_source 
```

We need to make sure the cases labelled as "from" (aka the infectors) actually got the virus prior to those in the "to" column (aka the infectees). It is reasonable to assume that cases that were infected first will show signs of infection first, so within case-pairs we will assign the case with the earliest date of symptom onset as the "from" (infector) case and the case with the later date of symptom onset as "to".

To do this, let's start by making a new dataset from our case pairs ('undir') that contains date of symptom onset for each case. We will make a few new columns, both for determining which case has the earliest date of symptom onset, as well as to plot serial intervals over time later on. 

```r
# Make a smaller dataset of original spdata that contains only the CaseID and date of symptom onset
tdata_sympt <- select(tdata, case_id, symptom_onset)

# Add the date of symptom onset -for the caseID of the 'from' case - to the case pairs dataset (edges)
  #Do some renaming so the join is based on the caseID in the from column and that name of date column reflects this
names(tdata_sympt) <- str_replace(names(tdata_sympt), "case_id", "from")
undir_tdates <- left_join(edges, tdata_sympt, by = "from")
names(undir_tdates) <- str_replace(names(undir_tdates), "symptom_onset", "from_sympt_date")

# Repeat, but add the date of symptom onset for the caseID of the 'to' case
names(tdata_sympt) <- str_replace(names(tdata_sympt), "from", "to")
undir_tdates <- left_join(undir_tdates, tdata_sympt, by = "to")
names(undir_tdates) <- str_replace(names(undir_tdates), "symptom_onset", "to_sympt_date")

# Now add some extra columns which give us the raw serial interval (i.e. number of days between symptom onset in infector-infectee pairs)
  #As well as the absolute value of the serial interval (as some cases in the "from" and "to" columns should be switched around!)
  #And finally a 'direction' column in case we need to sort out which directions the arrows should be going in for a network graph and where we have missing dates
undir_tdates <- mutate(undir_tdates, earliest_sympt_onset = pmin(to_sympt_date, from_sympt_date, na.rm = T), 
                                   raw_serial_interval = to_sympt_date - from_sympt_date,   
                                   abs_serial_interval = abs(raw_serial_interval))
```

Now we need to split the dataset apart so that we can switch around the directionality of presumed transmission for case-pairs where the serial interval is negative. Easiest way to do this is to rename columns and then join back to the other parts of the dataset, based on the column names. This actually doesn't affect the serial inteval estimates, but makes the directionality on the network graph more likely to reflect the biology.

```r
# Split dataset into positive (or 0) serial interval vs. negative vs. NA 
  #A negative serial interval means our "to" and "from" cases are mixed up
pos <- filter(undir_tdates, raw_serial_interval >= 0)
neg <- filter(undir_tdates, raw_serial_interval < 0)
onlyone <- filter(undir_tdates, is.na(raw_serial_interval)) #3 NAs where date of symptom onset is not known

# Negative dataset needs the column headers changed to reflect that the 'from' and 'to' columns are backwards
  #as we are assuming that the 'case with the earliest onset of symptoms would have been infected first, 
  #and passed on the infection to the other case in the pair
names(neg)
```

```
## [1] "from"                 "to"                   "from_sympt_date"     
## [4] "to_sympt_date"        "earliest_sympt_onset" "raw_serial_interval" 
## [7] "abs_serial_interval"
```

```r
names(neg)[1] <- "to"
names(neg)[2] <- "from"
names(neg)[3] <- "to_sympt_date"
names(neg)[4] <- "from_sympt_date"
names(neg)
```

```
## [1] "to"                   "from"                 "to_sympt_date"       
## [4] "from_sympt_date"      "earliest_sympt_onset" "raw_serial_interval" 
## [7] "abs_serial_interval"
```

```r
# Now bind the rows of the seperated datasets back together based on column names
  #Must use dplyr::bind_rows to bind based on column name rather than position
undir_tdates <- bind_rows(pos, neg, onlyone)

# For plotting - Add a column with padded to and from caseID numbers so they print in numerical order
  #Add a zero on the left of the number so all numbers have three digits  
undir_tdates$pto <- str_replace(undir_tdates$to, pattern = "tj", replacement = "")
undir_tdates$pto <- str_pad(undir_tdates$pto, width = 3, side = "left", pad = "0")

undir_tdates$pfrom <- str_replace(undir_tdates$from, pattern = "tj", replacement = "")
undir_tdates$pfrom <- str_pad(undir_tdates$pfrom, width = 3, side = "left", pad = "0")

# For plotting - Make a new column with case pair ID
undir_tdates <- mutate(undir_tdates, pairID = factor(paste("tj", pfrom, "-", "tj", pto, sep = "")))

rm(pos, neg, onlyone)
```

From the edge list we can use visNetwork to visualise the graph. Colours are from the infection source group column. 

```r
# Make data frame of edges, where the cases as the 'earliest' date of symptom onset are labeled as the "from" cases
tedges <- select(undir_tdates, from, to)
tedges$arrows <- "to" 

# Select down to only the unique cases-pairs 
  #Note that if you keep the negative serial intervals as negatives, there are no duplicates; 
  #But instead there are two instances where you get a loop (tj114-tj115 and tj96-tj97)
  #ex) tj114 is supposed to infect tj115 and tj115 is supposed to infect tj114
tedges <- distinct(tedges)

# Make a data frames of nodes
nodes = data.frame(id=tdata$case_id, 
                   label=tdata$case_id,
                   group=tdata$source_group)

# Plot network graph
visNetwork(nodes, tedges) %>% visLegend()
```

<!--html_preserve--><div id="htmlwidget-1aee73cbc26e64cda35b" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-1aee73cbc26e64cda35b">{"x":{"nodes":{"id":["tj1","tj2","tj3","tj4","tj5","tj6","tj7","tj8","tj9","tj10","tj11","tj12","tj13","tj14","tj15","tj16","tj17","tj18","tj19","tj20","tj21","tj22","tj23","tj24","tj25","tj26","tj27","tj28","tj29","tj30","tj31","tj32","tj33","tj34","tj35","tj36","tj37","tj39","tj40","tj41","tj42","tj43","tj44","tj45","tj47","tj48","tj49","tj50","tj51","tj52","tj53","tj54","tj55","tj56","tj57","tj58","tj59","tj60","tj61","tj62","tj63","tj64","tj65","tj66","tj67","tj68","tj69","tj70","tj71","tj72","tj73","tj74","tj75","tj76","tj77","tj78","tj79","tj80","tj81","tj82","tj83","tj84","tj85","tj86","tj87","tj88","tj89","tj90","tj91","tj92","tj93","tj94","tj95","tj96","tj97","tj98","tj100","tj103","tj105","tj106","tj107","tj108","tj109","tj110","tj111","tj112","tj113","tj114","tj115","tj116","tj117","tj118","tj119","tj120","tj121","tj122","tj124","tj125","tj126","tj128","tj130","tj131","tj132","tj133","tj134"],"label":["tj1","tj2","tj3","tj4","tj5","tj6","tj7","tj8","tj9","tj10","tj11","tj12","tj13","tj14","tj15","tj16","tj17","tj18","tj19","tj20","tj21","tj22","tj23","tj24","tj25","tj26","tj27","tj28","tj29","tj30","tj31","tj32","tj33","tj34","tj35","tj36","tj37","tj39","tj40","tj41","tj42","tj43","tj44","tj45","tj47","tj48","tj49","tj50","tj51","tj52","tj53","tj54","tj55","tj56","tj57","tj58","tj59","tj60","tj61","tj62","tj63","tj64","tj65","tj66","tj67","tj68","tj69","tj70","tj71","tj72","tj73","tj74","tj75","tj76","tj77","tj78","tj79","tj80","tj81","tj82","tj83","tj84","tj85","tj86","tj87","tj88","tj89","tj90","tj91","tj92","tj93","tj94","tj95","tj96","tj97","tj98","tj100","tj103","tj105","tj106","tj107","tj108","tj109","tj110","tj111","tj112","tj113","tj114","tj115","tj116","tj117","tj118","tj119","tj120","tj121","tj122","tj124","tj125","tj126","tj128","tj130","tj131","tj132","tj133","tj134"],"group":["Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Other travel","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Relative","Relative","Relative","Wuhan and Hubei","Wuhan and Hubei","Relative","Coworker","Wuhan and Hubei","Wuhan and Hubei","Coworker","Relative","Other travel","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Relative","Mall","Relative","Relative","Coworker","Known relationship","Relative","Relative","Unknown","Coworker","Relative","Wuhan and Hubei","Coworker","Coworker","Wuhan and Hubei","Mall","Mall","Relative","Mall","Relative","Known relationship","Known relationship","Mall","Mall","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Known relationship","Known relationship","Other travel","Mall","Wuhan and Hubei","Mall","Relative","Unknown","Mall","Unknown","Mall","Unknown","Known relationship","Known relationship","Unknown","Mall","Known relationship","Mall","Known relationship","Mall","Mall","Known relationship","Known relationship","Known relationship","Mall","Known relationship","Mall","Mall","Known relationship","Mall","Mall","Known relationship","Unknown","Known relationship","Known relationship","Known relationship","Unknown","Known relationship","Known relationship","Other travel","Known relationship","Known relationship","Known relationship","Unknown","Mall","Known relationship","Known relationship","Relative","Known relationship","Known relationship","Known relationship","Known relationship","Known relationship","Relative","Relative","Mall","Mall","Relative","Known relationship","Relative","Known relationship","Mall","Mall","Mall","Mall"]},"edges":{"from":["tj6","tj5","tj5","tj5","tj5","tj6","tj9","tj17","tj20","tj34","tj39","tj39","tj20","tj35","tj2","tj6","tj37","tj43","tj14","tj25","tj9","tj21","tj1","tj45","tj27","tj51","tj53","tj87","tj43","tj85","tj70","tj82","tj75","tj93","tj93","tj96","tj100","tj103","tj93","tj100","tj7","tj100","tj103","tj108","tj115","tj95","tj93","tj75","tj87","tj93","tj107","tj109","tj119","tj119","tj68","tj64","tj82","tj117","tj43","tj43","tj52","tj81","tj85","tj86","tj98","tj108","tj122","tj125","tj128","tj38","tj102","tj99"],"to":["tj11","tj12","tj13","tj16","tj17","tj20","tj21","tj33","tj35","tj36","tj40","tj41","tj44","tj44","tj47","tj47","tj48","tj48","tj54","tj54","tj55","tj55","tj60","tj61","tj63","tj64","tj69","tj75","tj84","tj86","tj88","tj91","tj94","tj96","tj97","tj97","tj103","tj105","tj107","tj108","tj112","tj113","tj113","tj113","tj114","tj116","tj117","tj118","tj118","tj119","tj119","tj120","tj121","tj125","tj126","tj130","tj132","tj134","tj34","tj37","tj50","tj72","tj72","tj72","tj82","tj103","tj110","tj121","tj124","tj56","tj109","tj130"],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":["Wuhan and Hubei","Other travel","Relative","Coworker","Mall","Known relationship","Unknown"],"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","legend":{"width":0.2,"useGroups":true,"position":"left","ncol":1,"stepX":100,"stepY":100,"zoom":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

The interval case to case (ICC) data are the times between the (presumed) index case for a small cluster and the other cases in the cluster. The Vink et al approach allows these intervals to be one of 4 types, and estimates the serial interval and the probability of each type. To extract ICC intervals, we let the clusters be the components of the graph, and we let the presumed index case be the first to develop symptoms. For each cluster, we subtract the index cases' symptom time from the symtom times of the rest of the cluster (or just the first few; it turns out that the estimate is not sensitive to this). This results in a list of time intervals between symptom onset in presumed index cases and symptom onset in other cases in the same cluster (graph component). 


First construct the graph

```r
tgraph = graph_from_edgelist(as.matrix(edges[,1:2]), directed = FALSE)
ccs=components(tgraph)

tdata$component=vapply(tdata$case_id, function(x)
  { if (x %in% names(ccs$membership)) { return(ccs$membership[match(x, names(ccs$membership))])
  } else { 
    return(NA)}}, FUN.VALUE = 3)
```


Extract ICC interval data: a function 

```r
 getICCs <- function(thisdata, ccs, K, orderby= "onset" ) {
  iccs=1
    for (n in 1:max(ccs$membership)) {
      mycases  = which(thisdata$component==n)
      if (orderby == "onset")
          {  myonsets = sort(thisdata$symptom_onset[mycases])[1:min(K, length(mycases))]}
        if (orderby == "exposure") {
          myonsets =thisdata$symptom_onset[mycases][order(thisdata$end_source[mycases])][1:min(K,length(mycases))]
          }
      iccs =c(iccs, myonsets[-1]-myonsets[1])
        }
    return(iccs[-1]) 
    }
```


Note that we are removing cases that do NOT have a date of symptom onset to determine the ICC. There are 10 cases without symptom onset, but all have a confirmation date.

```r
icc3 = getICCs(tdata,ccs,3)
icc4 = getICCs(tdata,ccs,4)
icc5 = getICCs(tdata,ccs,5)
icc6 = getICCs(tdata,ccs,6)
icc_expose = getICCs(tdata, ccs, 4, orderby ="exposure")
```


Perform the estimate using the Vink et al method, and display the result:


```r
source("TianjinSI_VinkWallinga_CC.R")
myest3 = serial_mix_est(data=icc3, N=100, startmu=10, startsig =4)
```

```
## [1] 6.44 2.90
## [1] 5.2 2.4
## [1] 4.55 1.90
## [1] 4.22 1.49
## [1] 4.10 1.24
## [1] 4.06 1.13
## [1] 4.04 1.08
## [1] 4.04 1.06
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
## [1] 4.03 1.05
```

```r
myest4 = serial_mix_est(data=icc4, N=100, startmu=10, startsig =4)
```

```
## [1] 6.74 3.02
## [1] 5.55 2.47
## [1] 4.93 2.04
## [1] 4.55 1.63
## [1] 4.36 1.31
## [1] 4.28 1.12
## [1] 4.24 1.03
## [1] 4.230 0.997
## [1] 4.225 0.985
## [1] 4.22 0.98
## [1] 4.223 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
## [1] 4.222 0.978
```

```r
myest5 = serial_mix_est(data=icc5, N=100, startmu=10, startsig =4)
```

```
## [1] 7.16 2.99
## [1] 6.17 2.61
## [1] 5.59 2.34
## [1] 5.18 2.07
## [1] 4.89 1.80
## [1] 4.67 1.55
## [1] 4.53 1.34
## [1] 4.43 1.19
## [1] 4.38 1.10
## [1] 4.35 1.06
## [1] 4.34 1.04
## [1] 4.33 1.03
## [1] 4.33 1.03
## [1] 4.33 1.03
## [1] 4.33 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
## [1] 4.32 1.03
```

```r
myest6 = serial_mix_est(data=icc6, N=100, startmu=10, startsig =4)
```

```
## [1] 7.69 3.38
## [1] 6.52 2.91
## [1] 5.86 2.56
## [1] 5.42 2.25
## [1] 5.10 1.96
## [1] 4.86 1.69
## [1] 4.69 1.46
## [1] 4.58 1.29
## [1] 4.51 1.18
## [1] 4.48 1.12
## [1] 4.46 1.09
## [1] 4.44 1.08
## [1] 4.44 1.08
## [1] 4.44 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
## [1] 4.43 1.07
```

```r
myest_exp= serial_mix_est(data=icc_expose, N=100, startmu=10, startsig =4)
```

```
## [1] 6.69 3.44
## [1] 5.35 3.14
## [1] 4.65 2.86
## [1] 4.29 2.62
## [1] 4.09 2.42
## [1] 3.98 2.25
## [1] 3.92 2.12
## [1] 3.90 2.01
## [1] 3.90 1.92
## [1] 3.92 1.85
## [1] 3.95 1.79
## [1] 3.99 1.73
## [1] 4.03 1.68
## [1] 4.08 1.62
## [1] 4.14 1.57
## [1] 4.19 1.51
## [1] 4.25 1.45
## [1] 4.31 1.39
## [1] 4.36 1.34
## [1] 4.41 1.29
## [1] 4.46 1.24
## [1] 4.49 1.21
## [1] 4.52 1.18
## [1] 4.55 1.16
## [1] 4.57 1.15
## [1] 4.58 1.14
## [1] 4.59 1.13
## [1] 4.59 1.13
## [1] 4.60 1.13
## [1] 4.60 1.13
## [1] 4.60 1.12
## [1] 4.60 1.12
## [1] 4.60 1.12
## [1] 4.60 1.12
## [1] 4.60 1.12
## [1] 4.60 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
## [1] 4.61 1.12
```

```r
mm=rbind(myest3, myest4, myest5,myest6, myest_exp)
colnames(mm)=c("mu","sig")
mm=as.data.frame(mm)
mm$NumCasesPerCluster=c( 3, 4, 5, 6, 4) 
mm$ordering = c("Onset","Onset","Onset","Onset","LastExposure")
print(mm[,c(4,3,1,2)]) 
```

```
##               ordering NumCasesPerCluster   mu   sig
## myest3           Onset                  3 4.03 1.049
## myest4           Onset                  4 4.22 0.978
## myest5           Onset                  5 4.32 1.028
## myest6           Onset                  6 4.43 1.071
## myest_exp LastExposure                  4 4.61 1.123
```

The mean SI is 4.222. The standard deviation of the serial intervals is 0.978.


```r
### Make a density plot of the ICC estimate
days = seq(from=0, to=10, by=0.1) 
sp.density= dnorm(days, mean = myest4[1], sd = myest4[2])

ggplot(data=data.frame(days=days, density=sp.density), aes(x=days,y=density)) +
    geom_line() + 
    ggtitle("ICC estimate of the Tianjin cluster serial interval")
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
# ggsave(file="final_figures/tianjin_serialint.pdf", height = 4, width = 6)
```

We need CIs for the mean. For this we use bootstrapping. 

Bootstrap analysis code - have left it set to eval=FALSE in the Rmd because it takes time. Bootstraps are saved in the data folder and named "tianjin_bootstraps_100.Rdata". 

```r
# bootstrap analysis
Nboot=100
bestimates_tj = myest4 
for (kk in 1:Nboot) {
  bdata = sample(x=icc4, size = length(icc4), replace = T)
  bestimates_tj = rbind(bestimates_tj, serial_mix_est(data=bdata, N=100, startmu=10, startsig =4))
  print(paste("loop iteration #", kk, sep = ": "))
}

bestimates_tj <- bestimates_tj[-1, ] #Remove the non-bootstrapped row (i.e. the myest4 object)
# save(bestimates_tj, file = "data/tianjin_boots_100.Rdata")
```


```r
load("data/tianjin_boots_100.Rdata")
mean(bestimates_tj[,1]) # mean of the mean serial intervals
```

```
## [1] 4.18
```

```r
median(bestimates_tj[,1])
```

```
## [1] 4.21
```

```r
mean(bestimates_tj[,2]) # sd of the sd serial intervals 
```

```
## [1] 0.979
```

```r
sd(bestimates_tj[,1]) # sd of the MEAN serial intervals 
```

```
## [1] 0.389
```

The 95% range for the mean serial interval is (3.46, 4.984).

The following makes a histogram of the bootstrapped mean serial interval (using a cluster size of 4).

```r
hist(bestimates_tj[,1],breaks = 10)
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
bootdf=data.frame(mu=bestimates_tj[,1], sig=bestimates_tj[,2])

ggplot(bootdf, aes(x=mu, y=sig)) + geom_point()
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
ggplot(bootdf, aes(x=mu)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

```r
# ggsave(file = "final_figures/FigS1_bootst_SI_tianjin.pdf", width = 6, height = 4)
```

## Effect of time on serial interval estimates
To see the effects of the passage of time on the raw serial intervals, we will plot all possible infector-infectee pairs and the difference in their dates of symptom onset. To do this, we need a dataframe that has (1) case pairs (= 'undir'), (2) dates of symptom onset for both individuals in that pair, and (3) difference in days between those pairs. This has been done above and is in the 'undir_tdates' object.

#### Dotplot of raw serial intervals
Now let's turn this into a dot plot and a bar chart so we can see if and how serial interval changes over time. The dates on the x-axis are the earliest date of symptom onset from each infected pair. 

```r
###  Cleveland dotplot of raw serial intervals per possible case pair 

# We want to exclude any case-pairs that have NA for length of serial interval
  #i.e. one of the pair does not have a date of symptom onset
undir_tdates_org <- undir_tdates  #Just in case....
undir_tdates <- filter(undir_tdates, !is.na(raw_serial_interval))

#Pivot the to/from dates of symptom onset column to a long format, so that can make a legend based on this variable
undir_dotplot <- pivot_longer(undir_tdates, 
                              cols = contains("sympt_date"),
                              names_to = "pair_member",
                              values_to = "onset_date")

#Let's rename the values so it makes more sense in the legend
undir_dotplot$pair_member <- str_replace(undir_dotplot$pair_member, pattern = "from_sympt_date", replacement = "Presumed infector")
undir_dotplot$pair_member <- str_replace(undir_dotplot$pair_member, pattern = "to_sympt_date", replacement = "Presumed infectee")

#Make the Cleaveland dotplot
p <- ggplot(undir_dotplot, aes(y = reorder(pairID, earliest_sympt_onset))) +
          geom_segment(aes(x = earliest_sympt_onset, xend = earliest_sympt_onset + abs_serial_interval, yend = pairID), 
                       color = "#404788FF") +
          geom_point(aes(x = onset_date, color = pair_member, fill = pair_member, shape = pair_member)) +
          scale_x_date(date_breaks = "1 day") +
          scale_color_manual(name = "Pair member for \ndate of symptom onset", values = c("#D44842FF", "#FAC127FF")) +
          scale_fill_manual(name = "Pair member for \ndate of symptom onset", values = c("#D44842FF", "#FAC127FF")) +
          scale_shape_manual(name = "Pair member for \ndate of symptom onset", values = c(23, 21)) +
          theme(axis.text.x = element_text(angle = 60, hjust = 1),
                axis.ticks.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(colour = "grey80", linetype = "dashed"),
                panel.background = element_rect(fill = "white")) +
          labs(title = "Serial intervals of possible case pairs from Tianjin, over time",
               x = "Date of symptom onset",
               y = "Case pairs")
p
```

![](tianjin_serial_intervals_revised_files/figure-html/Cleveland dotplot-1.png)<!-- -->

```r
# Write to PDF
# pdf("final_figures/Dotplot_raw_serial_intervals_Tianjin.pdf", 
     #family = "Times", 
#     width = 8.5, height = 11)

# p

# dev.off()
```

Notice that there are 3 possible case-pairs where the serial interval is zero; these show up with a single point/diamond and no solid line.

#### Mean and Median of half the case pairs
To determine the effect of time on the raw serial intevals, we will split the case-pairs in half, and find the median, mean, and standard deviation for those pairs in the half with the earlier first date of symptom onset ('earliest_sympt_onset') vs. those in the half with the later dates of first symtom onset ('earliest_sympt_onset'). Note that splitting the case-pairs in half does arbitarily split some pairs that have the same date of earliest_sympt_onset into the two different datasets. 

```r
### Add a column that specifies if case-pairs are part of "early onset" or "late onset"
  #based on which half of the case-pairs they fall into 
# Need to arrange the dataset by the date of earliest symptom onset for each case pair
undir_tdates <- arrange(undir_tdates, earliest_sympt_onset)

# Define half of the dataset
half <- nrow(undir_tdates) / 2 

# Define which pairID fall into the early and later half of the dataset
  #***NOTE THAT THIS DOES ARBITARILY SPLIT SOME PAIRS ON THE SAME DAY OF EARLIEST SYMPT ONSET INTO DIFFERENT HALVES***
early_half <- undir_tdates$pairID[1:floor(half)] #use floor to round 'half' down
early_half <- droplevels(early_half)

late_half <- undir_tdates$pairID[ceiling(half):nrow(undir_tdates)] #use ceiling to round 'half' up
late_half <- droplevels(late_half)

# Make new column indicating which case-pair belongs to which half of the dataset
undir_tdates <- mutate(undir_tdates, portion_of_pairs = case_when(pairID %in% early_half ~ "early half",
                                                                  pairID %in% late_half ~"late half",
                                                                  T ~ "other")) #Sanity check
undir_tdates$portion_of_pairs <- factor(undir_tdates$portion_of_pairs, 
                                        levels = c("late half", "early half"))

### Calculate the mean and median for each of the two halves
e <- undir_tdates %>% 
        filter(portion_of_pairs == "early half") %>% 
        summarize(mean_early = mean(abs_serial_interval),
                  median_early = median(abs_serial_interval),
                  sd_early = sd(abs_serial_interval))

l <- undir_tdates %>% 
        filter(portion_of_pairs == "late half") %>% 
        summarize(mean_late = mean(abs_serial_interval),
                  median_late = median(abs_serial_interval),
                  sd_late = sd(abs_serial_interval))
```

The mean serial interval for the **early** half of the case-pairs is 5.429, with a standard deviation of 3.032, and a median of 5. The mean serial interval for the **late** half of the case-pairs is 4.111, with a standard deviation of 3.37, and a median of 3.

This version of the Cleveland dotplot shows which half of the case-pairs was used to calculate each summary statistic.

```r
###  Cleveland dotplot of raw serial intervals, facetted by portion of dataset used to do mean, median and sd calculations
#Pivot the to/from dates of symptom onset column to a long format, so that can make a legend based on this variable
undir_dotplot2 <- pivot_longer(undir_tdates, 
                              cols = contains("sympt_date"),
                              names_to = "pair_member",
                              values_to = "onset_date")

#Let's rename the values so it makes more sense in the legend
undir_dotplot2$pair_member <- str_replace(undir_dotplot2$pair_member, pattern = "from_sympt_date", replacement = "Presumed infector")
undir_dotplot2$pair_member <- str_replace(undir_dotplot2$pair_member, pattern = "to_sympt_date", replacement = "Presumed infectee")

#Make the Cleaveland dotplot; this time FACET by portion_of_pairs
p2 <- ggplot(undir_dotplot2, aes(y = reorder(pairID, earliest_sympt_onset))) +
          geom_segment(aes(x = earliest_sympt_onset, xend = earliest_sympt_onset + abs_serial_interval, yend = pairID), 
                       color = "#404788FF") +
          geom_point(aes(x = onset_date, color = pair_member, fill = pair_member, shape = pair_member)) +
          facet_grid(portion_of_pairs ~ .,
                     scales = "free_y", space = "free_y") +
          scale_x_date(date_breaks = "1 day") +
          scale_color_manual(name = "Pair member for \ndate of symptom onset", values = c("#D44842FF", "#FAC127FF")) +
          scale_fill_manual(name = "Pair member for \ndate of symptom onset", values = c("#D44842FF", "#FAC127FF")) +
          scale_shape_manual(name = "Pair member for \ndate of symptom onset", values = c(23, 21)) +
          theme(axis.text.x = element_text(angle = 60, hjust = 1),
                axis.ticks.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(colour = "grey80", linetype = "dashed"),
                panel.background = element_rect(fill = "white")) +
          labs(title = "Serial intervals of possible case pairs from Tianjin, over time",
               x = "Date of symptom onset",
               y = "Case pairs")
p2
```

![](tianjin_serial_intervals_revised_files/figure-html/Cleveland dotplot facetted by half for mean calculations-1.png)<!-- -->

```r
# Write to PDF
# pdf("final_figures/Dotplot_raw_serial_intervals_Tianjin_facetted_in_half.pdf", 
     #family = "Times", 
#     width = 8.5, height = 11)

# p2

# dev.off()
```

## Imputataion of missing data
We want to see what the effect of cases with missing dates of symptom onset has on our estimates of serial intervals. To do this, we will impute the missing data by:
missing date of symptom onset = confirmation date - mean(confirmation data- symptom onset date)...where the mean is taken over the cases that do have a symptom onset date present. We will use the copy made of the original data so that we can repeat the full estimation process using the data with imputed dates of symptom onset.

```r
# Figure out which cases are missing date of symptom onset
no_date <- which(is.na(tdata_org$symptom_onset))
tdata_org$case_id[no_date]
```

```
##  [1] "TJ38"  "TJ46"  "TJ99"  "TJ101" "TJ102" "TJ104" "TJ123" "TJ127" "TJ129"
## [10] "TJ135"
```

```r
#Do all of the cases missing date of symptom onset have a date of confirmation? 
sum(is.na(tdata_org$confirm_date[no_date])) #Yes! 
```

```
## [1] 0
```

```r
# Figure out the mean(confirmation date - symptom onset date) for cases that HAVE a symptom onset date
avg_date_diff <- tdata_org %>% 
                    filter(!is.na(symptom_onset)) %>% 
                    select(case_id, confirm_date, symptom_onset) %>% 
                    summarise(mean(confirm_date - symptom_onset)) %>% 
                    pluck(1) 
avg_date_diff   #Notice that this is a 'difftime' class variable; may become important later?? 
```

```
## Time difference of 5.25 days
```

```r
# Impute the missing date of symptom onset values by each cases' date of confirmation - avg_date_diff
imp_tdata <- tdata_org
imp_tdata$dso_imputed = if_else(is.na(imp_tdata$symptom_onset),
                                     imp_tdata$confirm_date - avg_date_diff,
                                     imp_tdata$symptom_onset) 
```

Now we can re-run the serial estimates based on the imputed date of symptom onset column (dso_imputed).

Step 1: make the source_group column where we group data in the infection source columns into categories. This categorization is based on the same decision rules as above and is not used in the actual calculation of the serial interval estimates.

```r
# Make a column for sensible groupings for Tianjin, based on the reason why the case was exposed
# Turn 'Infection_source' into lower case and get trim any whitespace so don't have issues with case sensitivity, etc
imp_tdata$Infection_source <- str_to_lower(imp_tdata$Infection_source)
imp_tdata$Infection_source <- str_trim(imp_tdata$Infection_source)
table(imp_tdata$Infection_source)
```

```
## 
##                          appeared in workplaces of tj37/tj43 
##                                                            1 
##                                             business partner 
##                                                            1 
##                                         clos contact of tj68 
##                                                            1 
##                    close contact of cases tj72 tj85 and tj86 
##                                                            1 
##                close contact of tj93, tj107, tj119 and tj121 
##                                                            1 
##                           close contacts of cases tj64; tj99 
##                                                            1 
##                             colleague of cases tj34 and tj37 
##                                                            1 
##                                             colleague of tj6 
##                                                            1 
##                                            confirmed patient 
##                                                            3 
##                         confirmed patient and family of tj39 
##                                                            2 
##            contact with shoppers living in the same viliiage 
##                                                            1 
##                              coworker of a person from wuhan 
##                                                            1 
##                                              coworker of tj5 
##                                                            1 
##                                  coworkers of cases tj2; tj6 
##                                                            1 
##                                            dshopping in mall 
##                                                            1 
##                           family member of cases tj95; tj116 
##                                                            1 
##                                             family of  tj124 
##                                                            1 
##                                      family of tj14 and tj25 
##                                                            1 
##                                               family of tj17 
##                                                            1 
##                                               family of tj20 
##                                                            1 
##                                               family of tj32 
##                                                            1 
##                                               family of tj34 
##                                                            1 
##                                                family of tj9 
##                                                            1 
##                                                        hubei 
##                                                            3 
##                                                  hubei; tj45 
##                                                            1 
##                                             husband of tj119 
##                                                            1 
## living at the proximity of the mail, wife shoped at the mall 
##                                                            1 
##                                              living in hubei 
##                                                            1 
##                                 living in the mall proximity 
##                                                            3 
##                                                         mall 
##                                                            2 
##                         mother of tj115, travel to huhehaote 
##                                                            1 
##                                 relative of cases tj20, tj35 
##                                                            1 
##                                             relative of tj32 
##                                                            1 
##                                             relative of tj53 
##                                                            1 
##                                          shooper in the mall 
##                                                            1 
##                                          shopper at the mall 
##                                                            4 
##                                          shopper in the mall 
##                                                            7 
##                                    shopper in the mall, tj64 
##                                                            1 
##                                             shopping in mall 
##                                                            2 
##                                           shuperstore worker 
##                                                            1 
##                                              sister of tj109 
##                                                            1 
##                                   son of cases  tj119; tj121 
##                                                            1 
##                                                        tj100 
##                                                            1 
##                                                tj100， tj103 
##                                                            1 
##                                          tj100, tj103, tj108 
##                                                            1 
##                                                        tj102 
##                                                            1 
##                                                        tj103 
##                                                            1 
##                                                        tj114 
##                                                            1 
##                                                         tj27 
##                                                            1 
##                                                         tj38 
##                                                            1 
##                                                         tj43 
##                                                            1 
##                                                 tj5 (family) 
##                                                            2 
##                                                  tj5(family) 
##                                                            1 
##                                                 tj50  family 
##                                                            1 
##                                                         tj51 
##                                                            1 
##                                                 tj6 (family) 
##                                                            1 
##                                                          tj7 
##                                                            1 
##                                                         tj70 
##                                                            1 
##                                                         tj72 
##                                                            2 
##                                                   tj72, tj85 
##                                                            1 
##                                                         tj75 
##                                                            1 
##                                                   tj75; tj87 
##                                                            1 
##                                                         tj82 
##                                                            2 
##                                                         tj87 
##                                                            1 
##                                                    tj9; tj21 
##                                                            1 
##                                                         tj93 
##                                                            1 
##                                        tj93, living in baodi 
##                                                            1 
##                                  tj93, tj96, living in baodi 
##                                                            1 
##                                                   tj93, tj97 
##                                                            1 
##                                                  tj93; tj107 
##                                                            1 
##                                                         tj95 
##                                                            1 
##                                                  tj95, tj101 
##                                                            1 
##                                                        train 
##                                                            1 
##                                                   train crew 
##                                                            1 
##                                                 train import 
##                                                            2 
##                                                       travel 
##                                                            1 
##                                              travle to wuhan 
##                                                            1 
##                                               trip to dalian 
##                                                            1 
##                                                trip to hebei 
##                                                            1 
##                                                      unclear 
##                                                            1 
##                                                      unknown 
##                                                            5 
##               wife of tj110 shopping at the baodi superstore 
##                                                            1 
##            wife of tj117;living at the proximity of the mall 
##                                                            1 
##     wife of tj82, living at proximity of baodi shopping mall 
##                                                            1 
##                                                 wife of tj95 
##                                                            1 
##                                           worker in the mall 
##                                                            1 
##                                 worker in the train compoany 
##                                                            1 
##                                                        wuhan 
##                                                           16 
##                                                   wuhan; tj1 
##                                                            1 
##                                          wuhan; train import 
##                                                            2
```

```r
sum(is.na(imp_tdata$Infection_source)) #3 NAs
```

```
## [1] 3
```

```r
#Note that the order the data are selected in is VERY important to which case goes into which source_group category
  #For those that meet multiple criteria (e.g. wuhan; tj1), the str_match which is highest in the case_when call (i.e. "wuhan|hubei") will have priority over those matching later 
  #so that the 'source' column contain "wuhan; tj1" would be labelled as infection from a "wuhan" rather than from a "known relationship" origin 

#See what happens when we emphasize the wuhan and travel cases over known relationships
  #This seems more logical, given that the epicenter of the outbreak was Wuhan
imp_tdata <- mutate(imp_tdata, source_group = case_when(!is.na(str_match(Infection_source, "wuhan|hubei")) ~ "Wuhan and Hubei", #Priority 1
                                                  !is.na(str_match(Infection_source, "mall|store|shopper")) ~ "Mall", #Priority 1
                                                  !is.na(str_match(Infection_source, "family|relative|wife|mother|son|sister|husband")) ~ "Relative", #Priority 2
                                                  !is.na(str_match(Infection_source, "coworker|business|workplace|colleague")) ~ "Coworker", #Priority 2
                                                  !is.na(str_match(Infection_source, "tj|patient")) ~ "Known relationship", #Priority 2
                                                  !is.na(str_match(Infection_source, "train|travel|trip|hebei|dalian")) ~ "Other travel", #Priority 3
                                                  !is.na(str_match(Infection_source, "unknown|unclear")) ~ "Unknown", #Priority 5
                                                  is.na(Infection_source) ~ "Unknown", #Priority 5
                                                  T ~ "other")) #there should be none of these, so this is just a sanity check!  
table(imp_tdata$source_group) 
```

```
## 
##           Coworker Known relationship               Mall       Other travel 
##                  6                 35                 28                  8 
##           Relative            Unknown    Wuhan and Hubei 
##                 23                  9                 26
```


Step 2: split related cases column so that we can obtain nodes and edges for clusters.

```r
mynodes_i <- imp_tdata$case_id

#Transform everything to lower case to make sure there aren't any issues with matching due to case inconsistencies
mynodes_i <- str_to_lower(mynodes_i) 
imp_tdata$case_id <- str_to_lower(imp_tdata$case_id)

edges_i = data.frame(from=mynodes_i[9],to=mynodes_i[21],stringsAsFactors = F ) # i read this one manually 

for (id in 1:nrow(imp_tdata)) {
tonode=imp_tdata$case_id[id]
fromnodes=str_extract_all(imp_tdata$Infection_source[id], "tj\\d+", simplify = T) #in lower case due to above early/late split on infection source
  if (length(fromnodes)>0) {
    for (k in 1:length(fromnodes)) {
      edges_i=rbind(edges_i, c(fromnodes[k], tonode))
    }
  }
}
head(edges_i)
```

```
##   from   to
## 1  tj9 tj21
## 2  tj6 tj11
## 3  tj5 tj12
## 4  tj5 tj13
## 5  tj5 tj16
## 6  tj5 tj17
```

```r
edges_i=edges_i[-1,] #Remove the initial relationship we gave so it isn't duplicated
edges_i=edges_i[-which(is.na(edges_i[,1])),] # NAs arose from a few empty entries for Infection_source 
```

Step 3: ensure that the "from" cases have the earliest date of symptom onset and the "to" cases have the later date of symptom onset.

```r
# Make a smaller dataset of original spdata that contains only the CaseID and date of symptom onset
imp_tdata_sympt <- select(imp_tdata, case_id, symptom_onset)

# Add the date of symptom onset -for the caseID of the 'from' case - to the case pairs dataset (edges_i)
  #Do some renaming so the join is based on the caseID in the from column and that name of date column reflects this
names(imp_tdata_sympt) <- str_replace(names(imp_tdata_sympt), "case_id", "from")
undir_timp <- left_join(edges_i, imp_tdata_sympt, by = "from")
names(undir_timp) <- str_replace(names(undir_timp), "symptom_onset", "from_sympt_date")

# Repeat, but add the date of symptom onset for the caseID of the 'to' case
names(imp_tdata_sympt) <- str_replace(names(imp_tdata_sympt), "from", "to")
undir_timp <- left_join(undir_timp, imp_tdata_sympt, by = "to")
names(undir_timp) <- str_replace(names(undir_timp), "symptom_onset", "to_sympt_date")

# Now add some extra columns which give us the raw serial interval (i.e. number of days between symptom onset in infector-infectee pairs)
  #As well as the absolute value of the serial interval (as some cases in the "from" and "to" columns should be switched around!)
  #And finally a 'direction' column in case we need to sort out which directions the arrows should be going in for a network graph and where we have missing dates
undir_timp <- mutate(undir_timp, earliest_sympt_onset = pmin(to_sympt_date, from_sympt_date, na.rm = T), 
                                   raw_serial_interval = to_sympt_date - from_sympt_date,   
                                   abs_serial_interval = abs(raw_serial_interval))

# Split dataset into positive (or 0) serial interval vs. negative vs. NA 
  #A negative serial interval means our "to" and "from" cases are mixed up
pos <- filter(undir_timp, raw_serial_interval >= 0)
neg <- filter(undir_timp, raw_serial_interval < 0)
onlyone <- filter(undir_timp, is.na(raw_serial_interval)) #3 NAs where date of symptom onset is not known

# Negative dataset needs the column headers changed to reflect that the 'from' and 'to' columns are backwards
  #as we are assuming that the 'case with the earliest onset of symptoms would have been infected first, 
  #and passed on the infection to the other case in the pair
names(neg)
```

```
## [1] "from"                 "to"                   "from_sympt_date"     
## [4] "to_sympt_date"        "earliest_sympt_onset" "raw_serial_interval" 
## [7] "abs_serial_interval"
```

```r
names(neg)[1] <- "to"
names(neg)[2] <- "from"
names(neg)[3] <- "to_sympt_date"
names(neg)[4] <- "from_sympt_date"
names(neg)
```

```
## [1] "to"                   "from"                 "to_sympt_date"       
## [4] "from_sympt_date"      "earliest_sympt_onset" "raw_serial_interval" 
## [7] "abs_serial_interval"
```

```r
# Now bind the rows of the seperated datasets back together based on column names
  #Must use dplyr::bind_rows to bind based on column name rather than position
undir_timp <- bind_rows(pos, neg, onlyone)

# For plotting - Add a column with padded to and from caseID numbers so they print in numerical order
  #Add a zero on the left of the number so all numbers have three digits  
undir_timp$pto <- str_replace(undir_timp$to, pattern = "tj", replacement = "")
undir_timp$pto <- str_pad(undir_timp$pto, width = 3, side = "left", pad = "0")

undir_timp$pfrom <- str_replace(undir_timp$from, pattern = "tj", replacement = "")
undir_timp$pfrom <- str_pad(undir_timp$pfrom, width = 3, side = "left", pad = "0")

# For plotting - Make a new column with case pair ID
undir_timp <- mutate(undir_timp, pairID = factor(paste("tj", pfrom, "-", "tj", pto, sep = "")))

rm(pos, neg, onlyone)
```

Step 4: make a network diagram. This won't be our manuscript figure (it's not as pretty...) but gives us that picture here without copying that script here too. 

```r
# Make data frame of edges_i, where the cases as the 'earliest' date of symptom onset are labeled as the "from" cases
tedges_i <- select(undir_timp, from, to)
tedges_i$arrows <- "to" 

# Select down to only the unique cases-pairs 
  #Note that if you keep the negative serial intervals as negatives, there are no duplicates; 
  #But instead there are two instances where you get a loop (tj114-tj115 and tj96-tj97)
  #ex) tj114 is supposed to infect tj115 and tj115 is supposed to infect tj114
tedges_i <- distinct(tedges_i)

# Make a data frames of nodes
nodes = data.frame(id=imp_tdata$case_id, 
                   label=imp_tdata$case_id,
                   group=imp_tdata$source_group)

# Plot network graph
visNetwork(nodes, tedges_i) %>% visLegend()
```

<!--html_preserve--><div id="htmlwidget-8cfbe20b64abf60107c7" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-8cfbe20b64abf60107c7">{"x":{"nodes":{"id":["tj1","tj2","tj3","tj4","tj5","tj6","tj7","tj8","tj9","tj10","tj11","tj12","tj13","tj14","tj15","tj16","tj17","tj18","tj19","tj20","tj21","tj22","tj23","tj24","tj25","tj26","tj27","tj28","tj29","tj30","tj31","tj32","tj33","tj34","tj35","tj36","tj37","tj38","tj39","tj40","tj41","tj42","tj43","tj44","tj45","tj46","tj47","tj48","tj49","tj50","tj51","tj52","tj53","tj54","tj55","tj56","tj57","tj58","tj59","tj60","tj61","tj62","tj63","tj64","tj65","tj66","tj67","tj68","tj69","tj70","tj71","tj72","tj73","tj74","tj75","tj76","tj77","tj78","tj79","tj80","tj81","tj82","tj83","tj84","tj85","tj86","tj87","tj88","tj89","tj90","tj91","tj92","tj93","tj94","tj95","tj96","tj97","tj98","tj99","tj100","tj101","tj102","tj103","tj104","tj105","tj106","tj107","tj108","tj109","tj110","tj111","tj112","tj113","tj114","tj115","tj116","tj117","tj118","tj119","tj120","tj121","tj122","tj123","tj124","tj125","tj126","tj127","tj128","tj129","tj130","tj131","tj132","tj133","tj134","tj135"],"label":["tj1","tj2","tj3","tj4","tj5","tj6","tj7","tj8","tj9","tj10","tj11","tj12","tj13","tj14","tj15","tj16","tj17","tj18","tj19","tj20","tj21","tj22","tj23","tj24","tj25","tj26","tj27","tj28","tj29","tj30","tj31","tj32","tj33","tj34","tj35","tj36","tj37","tj38","tj39","tj40","tj41","tj42","tj43","tj44","tj45","tj46","tj47","tj48","tj49","tj50","tj51","tj52","tj53","tj54","tj55","tj56","tj57","tj58","tj59","tj60","tj61","tj62","tj63","tj64","tj65","tj66","tj67","tj68","tj69","tj70","tj71","tj72","tj73","tj74","tj75","tj76","tj77","tj78","tj79","tj80","tj81","tj82","tj83","tj84","tj85","tj86","tj87","tj88","tj89","tj90","tj91","tj92","tj93","tj94","tj95","tj96","tj97","tj98","tj99","tj100","tj101","tj102","tj103","tj104","tj105","tj106","tj107","tj108","tj109","tj110","tj111","tj112","tj113","tj114","tj115","tj116","tj117","tj118","tj119","tj120","tj121","tj122","tj123","tj124","tj125","tj126","tj127","tj128","tj129","tj130","tj131","tj132","tj133","tj134","tj135"],"group":["Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Other travel","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Relative","Relative","Relative","Wuhan and Hubei","Wuhan and Hubei","Relative","Coworker","Wuhan and Hubei","Wuhan and Hubei","Coworker","Relative","Other travel","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Relative","Mall","Relative","Relative","Coworker","Relative","Known relationship","Relative","Relative","Unknown","Coworker","Relative","Wuhan and Hubei","Relative","Coworker","Coworker","Wuhan and Hubei","Mall","Mall","Relative","Mall","Relative","Known relationship","Known relationship","Mall","Mall","Wuhan and Hubei","Wuhan and Hubei","Wuhan and Hubei","Other travel","Known relationship","Known relationship","Other travel","Mall","Wuhan and Hubei","Mall","Relative","Unknown","Mall","Unknown","Mall","Unknown","Known relationship","Known relationship","Unknown","Mall","Known relationship","Mall","Known relationship","Mall","Mall","Known relationship","Known relationship","Known relationship","Mall","Known relationship","Mall","Mall","Known relationship","Mall","Mall","Known relationship","Unknown","Known relationship","Known relationship","Known relationship","Mall","Unknown","Relative","Unknown","Known relationship","Known relationship","Known relationship","Other travel","Known relationship","Known relationship","Known relationship","Unknown","Mall","Known relationship","Known relationship","Relative","Known relationship","Known relationship","Known relationship","Known relationship","Known relationship","Relative","Relative","Mall","Relative","Mall","Relative","Known relationship","Known relationship","Relative","Known relationship","Known relationship","Mall","Mall","Mall","Mall","Mall"]},"edges":{"from":["tj6","tj5","tj5","tj5","tj5","tj6","tj9","tj17","tj20","tj34","tj39","tj39","tj20","tj35","tj2","tj6","tj37","tj43","tj14","tj25","tj9","tj21","tj1","tj45","tj27","tj51","tj53","tj87","tj43","tj85","tj70","tj82","tj75","tj93","tj93","tj96","tj100","tj103","tj93","tj100","tj7","tj100","tj103","tj108","tj115","tj95","tj93","tj75","tj87","tj93","tj107","tj109","tj119","tj119","tj68","tj64","tj82","tj117","tj43","tj43","tj52","tj81","tj85","tj86","tj98","tj108","tj122","tj125","tj128","tj32","tj32","tj38","tj64","tj95","tj95","tj101","tj102","tj95","tj116","tj93","tj107","tj119","tj121","tj72","tj85","tj86","tj99"],"to":["tj11","tj12","tj13","tj16","tj17","tj20","tj21","tj33","tj35","tj36","tj40","tj41","tj44","tj44","tj47","tj47","tj48","tj48","tj54","tj54","tj55","tj55","tj60","tj61","tj63","tj64","tj69","tj75","tj84","tj86","tj88","tj91","tj94","tj96","tj97","tj97","tj103","tj105","tj107","tj108","tj112","tj113","tj113","tj113","tj114","tj116","tj117","tj118","tj118","tj119","tj119","tj120","tj121","tj125","tj126","tj130","tj132","tj134","tj34","tj37","tj50","tj72","tj72","tj72","tj82","tj103","tj110","tj121","tj124","tj38","tj46","tj56","tj99","tj101","tj104","tj104","tj109","tj123","tj123","tj127","tj127","tj127","tj127","tj129","tj129","tj129","tj130"],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":["Wuhan and Hubei","Other travel","Relative","Coworker","Mall","Known relationship","Unknown"],"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","legend":{"width":0.2,"useGroups":true,"position":"left","ncol":1,"stepX":100,"stepY":100,"zoom":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Step 5: determine the ICC intervals by extracting the components of the network graph list. Need to update the 'getICC' function defined earlier so it uses the column with imputed date of symptom onset (or else get NAs!).
First construct the graph

```r
tgraph_i = graph_from_edgelist(as.matrix(tedges_i[,1:2]), directed = FALSE)
ccs_i=components(tgraph_i)

imp_tdata$component=vapply(imp_tdata$case_id, function(x)
  { if (x %in% names(ccs_i$membership)) { return(ccs_i$membership[match(x, names(ccs_i$membership))])
  } else { 
    return(NA)}}, FUN.VALUE = 3)
```

Extract ICC interval data: a function 

```r
 getICC_i <- function(thisdata, ccs, K, orderby= "onset" ) {
  iccs=1
    for (n in 1:max(ccs$membership)) {
      mycases  = which(thisdata$component==n)
      if (orderby == "onset")
          {  myonsets = sort(thisdata$dso_imputed[mycases])[1:min(K, length(mycases))]}
        if (orderby == "exposure") {
          myonsets =thisdata$dso_imputed[mycases][order(thisdata$end_source[mycases])][1:min(K,length(mycases))]
          }
      iccs =c(iccs, myonsets[-1]-myonsets[1])
        }
    return(iccs[-1]) 
    }
```


Note that this time we are using **imputed data** for 10 cases that are missing a date of symptom onset in order to determine the ICC. 

```r
icc3_i = getICC_i(imp_tdata,ccs_i,3)
icc4_i = getICC_i(imp_tdata,ccs_i,4)
icc5_i = getICC_i(imp_tdata,ccs_i,5)
icc6_i = getICC_i(imp_tdata,ccs_i,6)
icc_expose_i = getICC_i(imp_tdata, ccs_i, 4, orderby ="exposure")
```


Step 6: determine the serial interval estimates by using the method from Vink et al. Use the 'serial_mix_est' function sourced earlier.

```r
#source("TianjinSI_VinkWallinga_CC.R")
myest3_i = serial_mix_est(data=icc3_i, N=100, startmu=10, startsig =4)
```

```
## [1] 6.08 2.63
## [1] 5.00 1.95
## [1] 4.58 1.45
## [1] 4.42 1.16
## [1] 4.36 1.03
## [1] 4.336 0.974
## [1] 4.324 0.957
## [1] 4.320 0.951
## [1] 4.317 0.949
## [1] 4.316 0.949
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
## [1] 4.316 0.948
```

```r
myest4_i = serial_mix_est(data=icc4_i, N=100, startmu=10, startsig =4)
```

```
## [1] 6.77 2.84
## [1] 5.77 2.34
## [1] 5.23 1.98
## [1] 4.87 1.63
## [1] 4.64 1.30
## [1] 4.51 1.08
## [1] 4.439 0.973
## [1] 4.404 0.928
## [1] 4.388 0.911
## [1] 4.380 0.905
## [1] 4.377 0.903
## [1] 4.375 0.902
## [1] 4.375 0.902
## [1] 4.374 0.902
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
## [1] 4.374 0.901
```

```r
myest5_i = serial_mix_est(data=icc5_i, N=100, startmu=10, startsig =4)
```

```
## [1] 7.29 2.97
## [1] 6.40 2.54
## [1] 5.9 2.3
## [1] 5.55 2.09
## [1] 5.27 1.88
## [1] 5.03 1.65
## [1] 4.84 1.43
## [1] 4.69 1.23
## [1] 4.59 1.09
## [1] 4.53 1.01
## [1] 4.491 0.977
## [1] 4.473 0.961
## [1] 4.463 0.954
## [1] 4.46 0.95
## [1] 4.456 0.949
## [1] 4.455 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
## [1] 4.454 0.948
```

```r
myest6_i = serial_mix_est(data=icc6_i, N=100, startmu=10, startsig =4)
```

```
## [1] 7.72 3.29
## [1] 6.66 2.77
## [1] 6.10 2.45
## [1] 5.73 2.21
## [1] 5.43 1.98
## [1] 5.19 1.75
## [1] 4.99 1.53
## [1] 4.82 1.32
## [1] 4.71 1.17
## [1] 4.63 1.08
## [1] 4.59 1.03
## [1] 4.56 1.01
## [1] 4.550 0.997
## [1] 4.543 0.993
## [1] 4.54 0.99
## [1] 4.538 0.989
## [1] 4.537 0.989
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
## [1] 4.536 0.988
```

```r
myest_exp_i= serial_mix_est(data=icc_expose_i, N=100, startmu=10, startsig =4)
```

```
## [1] 7.13 3.21
## [1] 6.06 2.84
## [1] 5.50 2.54
## [1] 5.18 2.27
## [1] 4.98 2.02
## [1] 4.86 1.78
## [1] 4.80 1.55
## [1] 4.77 1.34
## [1] 4.75 1.17
## [1] 4.74 1.05
## [1] 4.726 0.999
## [1] 4.721 0.976
## [1] 4.717 0.967
## [1] 4.716 0.963
## [1] 4.715 0.961
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
## [1] 4.71 0.96
```

```r
mm=rbind(myest3_i, myest4_i, myest5_i,myest6_i, myest_exp_i)
colnames(mm)=c("mu","sig")
mm=as.data.frame(mm)
mm$NumCasesPerCluster=c( 3, 4, 5, 6, 4) 
mm$ordering = c("Onset","Onset","Onset","Onset","LastExposure")
print(mm[,c(4,3,1,2)]) 
```

```
##                 ordering NumCasesPerCluster   mu   sig
## myest3_i           Onset                  3 4.32 0.948
## myest4_i           Onset                  4 4.37 0.901
## myest5_i           Onset                  5 4.45 0.948
## myest6_i           Onset                  6 4.54 0.988
## myest_exp_i LastExposure                  4 4.71 0.960
```

The mean SI is 4.374. The standard deviation of the serial intervals is 0.901.

```r
### Make a density plot of the ICC estimate
days = seq(from=0, to=10, by=0.1) 
sp.density_i= dnorm(days, mean = myest4_i[1], sd = myest4_i[2])

ggplot(data=data.frame(days=days, density=sp.density_i), aes(x=days,y=density)) +
    geom_line() + 
    ggtitle("ICC estimate of the Tianjin cluster serial interval \nwith imputed data")
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
# ggsave(file="final_figures/tianjin_serialint_imputed.pdf", height = 4, width = 6)
```


Step 7: determine the 95% confidence intervals for the mean serial estimate through bootstrapping.

```r
# bootstrap analysis
Nboot=100
tbestimates_i = myest4_i 
for (kk in 1:Nboot) {
  bdata = sample(x=icc4_i, size = length(icc4_i), replace = T)
  tbestimates_i = rbind(tbestimates_i, serial_mix_est(data=bdata, N=100, startmu=10, startsig =4))
  print(paste("loop iteration #", kk, sep = ": "))
}

tbestimates_i <- tbestimates_i[-1, ] #Remove the non-bootstrapped row (i.e. the myest4_i object)
# save(tbestimates_i, file = "data/tianjin_boots_100_imputed.Rdata")
```


```r
load("data/tianjin_boots_100_imputed.Rdata")
mean(tbestimates_i[,1]) # mean of the mean serial intervals
```

```
## [1] 4.43
```

```r
median(tbestimates_i[,1])
```

```
## [1] 4.37
```

```r
mean(tbestimates_i[,2]) # sd of the sd serial intervals 
```

```
## [1] 0.922
```

```r
sd(tbestimates_i[,1]) # sd of the MEAN serial intervals 
```

```
## [1] 0.407
```

The 95% range for the mean serial interval is (3.576, 5.02).

The following makes a histogram of the bootstrapped mean serial interval (using a cluster size of 4).

```r
hist(tbestimates_i[,1],breaks = 10)
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```r
bootdf_i=data.frame(mu=tbestimates_i[,1], sig=tbestimates_i[,2])

ggplot(bootdf_i, aes(x=mu, y=sig)) + geom_point()
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-28-2.png)<!-- -->

```r
ggplot(bootdf_i, aes(x=mu)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](tianjin_serial_intervals_revised_files/figure-html/unnamed-chunk-28-3.png)<!-- -->

```r
# ggsave(file = "final_figures/bootst_SI_tianjin_imputed.pdf", width = 6, height = 4)
```





