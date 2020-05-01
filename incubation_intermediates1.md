---
title: "Testing incubation with intermediate"
author: "Caroline Colijn"
date: "2020-04-27"
output: 
  html_document:
    keep_md: TRUE
---
  


## Data 

Thanks to EpiCoronaHack Cluster team. These data are manually entered from postings from the Government of Singapore website: [website](https://www.moh.gov.sg/covid-19).
  

```r
spdata <- read_csv("data/COVID-19_Singapore_eg_update_edges.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   CaseID = col_double(),
##   age = col_double()
## )
```

```
## See spec(...) for full column specifications.
```

```r
# Ensure properly imported
glimpse(spdata)
```

```
## Observations: 93
## Variables: 23
## $ CaseID                 <dbl> 48, 49, 51, 53, 54, 57, 58, 60, 61, 62,...
## $ `Related cases`        <chr> NA, NA, NA, "\n", "57,58", "54,58", "54...
## $ `Cluster links`        <chr> "49,51,53,54,57,58,60,61,62,63,66,67,68...
## $ `Relationship notes`   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ Case                   <chr> "Case 48, 34M, Grace Assembly of God", ...
## $ age                    <dbl> 34, 46, 48, 54, 54, 26, 55, 51, 57, 44,...
## $ sex                    <chr> "M", "M", "M", "M", "F", "M", "M", "F",...
## $ country                <chr> "Singapore", "Singapore", "Singapore", ...
## $ hospital               <chr> "National Centre for Infectious Disease...
## $ presumed_infected_date <chr> "29/01/2020", "29/01/2020", "29/01/2020...
## $ presumed_reason        <chr> "Grace Assembly of God", "Grace Assembl...
## $ last_poss_exposure     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ symp_presumed_infector <chr> "29/01/2020", "29/01/2020", "29/01/2020...
## $ date_onset_symptoms    <chr> "1/2/20", "3/2/20", "4/2/20", "10/2/20"...
## $ date_quarantine        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ date_hospital          <chr> "10/2/20", "10/2/20", "11/2/20", "12/2/...
## $ date_confirmation      <chr> "12/2/20", "12/2/20", "13/02/2020", "13...
## $ outcome                <chr> "Discharged", "Discharged", "Discharged...
## $ date_discharge         <chr> "17/02/2020", "26/02/2020", "21/02/2020...
## $ travel_history         <chr> "Malaysia", "not to China", "not to Chi...
## $ additional_information <chr> NA, NA, NA, NA, NA, NA, NA, NA, "Family...
## $ cluster                <chr> "Grace Assembly of God", "Grace Assembl...
## $ citizenship            <chr> "Singapore", "Singapore", "Singapore", ...
```

```r
table(spdata$`Related cases`) # There is one cell with "\n", needs to be changed to 'NA'
```

```
## 
##                     \n                    1,2                    1,3 
##                      1                      1                      1 
##                     11                     12                     13 
##                      1                      1                      1 
##                  13,26                     18                     19 
##                      1                      1                      1 
##                  19,20               19,20,25                  19,27 
##                      2                      1                      1 
##                    2,3   20,21,24,27,28,34,40                     22 
##                      1                      1                      1 
##                     23                     24                  24,19 
##                      1                      1                      1 
##                  26, 2                  28,19                  30,39 
##                      1                      1                      1 
##                      4                     41            42,47,52,56 
##                      1                      1                      1 
##                     50                  50,55                     51 
##                      2                      1                      1 
##                  54,57                  54,58               55,65,77 
##                      1                      1                      1 
##                  57,58                 59, 79                     61 
##                      1                      1                      1 
##                     66     66, 68, 70, 71, 80         66, 68, 71, 80 
##                      4                      1                      1 
##         66, 70, 71, 80                     67 68, 70, 71, 80, 83, 91 
##                      1                      1                      1 
##                     72                  72,79                     76 
##                      1                      1                      1 
##                      8                     82                     86 
##                      1                      1                      1 
##    9,83,91,90,38,33,31 
##                      1
```

```r
spdata$`Related cases`[which(spdata$`Related cases` == "\n")] <- NA
colSums(is.na(spdata))
```

```
##                 CaseID          Related cases          Cluster links 
##                      0                     43                     88 
##     Relationship notes                   Case                    age 
##                     64                      0                      0 
##                    sex                country               hospital 
##                      0                      0                      0 
## presumed_infected_date        presumed_reason     last_poss_exposure 
##                     16                     16                     65 
## symp_presumed_infector    date_onset_symptoms        date_quarantine 
##                     42                     12                     80 
##          date_hospital      date_confirmation                outcome 
##                      0                      0                     31 
##         date_discharge         travel_history additional_information 
##                     31                      0                     55 
##                cluster            citizenship 
##                     23                      0
```

```r
# Rename columns 2, 3 and 4 so no spaces
spdata <- rename(spdata, related_cases = starts_with("Related"),
                 cluster_links = "Cluster links",
                 relationship_notes = starts_with("Relation"))
# Change date columns into date objects
spdata <- mutate(spdata, presumed_infected_date = dmy(presumed_infected_date),
                 last_poss_exposure = dmy(last_poss_exposure),
                 symp_presumed_infector = dmy(symp_presumed_infector),
                 date_onset_symptoms = dmy(date_onset_symptoms),
                 date_quarantine = dmy(date_quarantine),
                 date_hospital = dmy(date_hospital),
                 date_confirmation = dmy(date_confirmation),
                 date_discharge = dmy(date_discharge))

# make sure dates parsed properly
range(spdata$presumed_infected_date, na.rm = T)
```

```
## [1] "2020-01-18" "2020-02-10"
```

```r
range(spdata$last_poss_exposure, na.rm = T)
```

```
## [1] "2020-01-18" "2020-02-09"
```

```r
range(spdata$symp_presumed_infector, na.rm = T)
```

```
## [1] "2020-01-19" "2020-02-09"
```

```r
range(spdata$date_onset_symptoms, na.rm = T)
```

```
## [1] "2020-01-20" "2020-02-16"
```

```r
range(spdata$date_quarantine, na.rm = T)
```

```
## [1] "2020-01-26" "2020-02-15"
```

```r
range(spdata$date_hospital, na.rm = T)
```

```
## [1] "2020-01-22" "2020-02-25"
```

```r
range(spdata$date_confirmation, na.rm = T)
```

```
## [1] "2020-01-23" "2020-02-26"
```

```r
range(spdata$date_discharge, na.rm = T)
```

```
## [1] "2020-02-04" "2020-02-26"
```

```r
# Note that case 36 is listed has having symptoms 16 days AFTER being hospitalized; suspect a typo in the month, fixing: 
# spdata$date_onset_symptoms[spdata$CaseID==36] <- ymd("2020-01-24")
# Note that the date of symp_presumed_infector for CaseID 79 changed was originally listed as 2020-02-07 (based on online visualizations) but was changed to 2020-02-10, due to Feb 10, 2020 being on the earliest date of onset of symptoms from case 72, as from online info provided, presumed infective contact for CaseID 79 is from 72 (family member), rather than directly from case 52
spdata$symp_presumed_infector[spdata$CaseID == 79] <- ymd("2020-02-10")
# Change symp_presumed_infector to Feb 10, 2020 (date of symptom onset from caseID 72, the presumed infector)

#TODO: Make a new script with imputed data for all those that do not have info on data of symptom onset
spdata <- filter(spdata, !is.na(date_onset_symptoms)) #Remove all the cases that do not have info on date of symptom onset 
# NOTE NOTE 12 of these, but they have a date of confiramation and dates of presumed infection - COULD FIX 
```



## Incubation period

The incubation period is the time between exposure and the onset of symptoms. We estimate this directly from the stated start and end times for cases' exposure windows. These are explicitly listed for the Tianjin dataset but in Singapore they are approximated using contact tracing and the route by which a case was exposed. Because it is explicitly about the symptom onset, we removed those who don't have symptom onset defined. (These are a small minority of 12 cases and the alternative would be to impute their symptom onset time using the others' delay to confirmation time. For now, we remove them).   

Then, if no other end time for the exposure is given or if the end of the exposure time is after the time of symptom onset, set the last exposure time to the symptom onset time. This is because they must have been exposed before symptom onset. We use four ideas to set the end time for the exposure window: 

* 1: the end source is last possible exposure, if this is given 

* 2:  if it is not given, then we set the end of the exposure window to the time of symptoms of the presumed infector plus a noise term epsilon (eps)

* 3: and if neither the last possible expsure or the symptom time of the presumed infector are given, the last exposure time is set to the time of symptom onset. 

* 4 Finally, we do not let the last possible exposure time be later than the time of symptom onset 


```r
spdata$end_source = spdata$last_poss_exposure # 1 above 
(method1 <- sum(!is.na(spdata$end_source))) #20 cases can have date of last possible exposure provided by known end of exposure window
```

```
## [1] 20
```

```r
eps=4
hasPresInf = which(is.na(spdata$last_poss_exposure) & !(is.na(spdata$symp_presumed_infector))) # 2 above 
spdata$end_source[hasPresInf] = spdata$presumed_infected_date[hasPresInf]+eps
length(hasPresInf) #47 cases have date of last possible exposure estimated by method #2
```

```
## [1] 47
```

```r
hasNone = which(is.na(spdata$last_poss_exposure) & is.na(spdata$symp_presumed_infector)) # 3 above 
spdata$end_source[hasNone] = spdata$date_onset_symptoms[hasNone]
length(hasNone) #14 cases have date of last possible exposure estimated by method #3
```

```
## [1] 14
```

```r
spdata$end_source = pmin(spdata$end_source, spdata$date_onset_symptoms) # 4
nrow(spdata) - method1 - length(hasPresInf) - length(hasNone) #0 cases  have date of last possible exposure estimated by method #4
```

```
## [1] 0
```

Model the start source 

* 1 if the time of presumed infector is given, use that - epsilon 

* If it is not given use symptom onset minus say 20 days, based on prior 
knowledge 


```r
spdata$start_source = spdata$presumed_infected_date - eps # 1
spdata$start_source[is.na(spdata$presumed_infected_date)] = spdata$date_onset_symptoms[is.na(spdata$presumed_infected_date)]-20
```

Define the maximum and minimum exposure times based on these assumptions. These are the times $t_{min}^i$ and $t_{max}^i$ in the notation. 



```r
spdata$minIncTimes <- spdata$date_onset_symptoms - spdata$end_source
spdata$maxIncTimes <- spdata$date_onset_symptoms - spdata$start_source
```


From here this file diverges from the ..wtables Rmd files .

First define the relevant times for truncation $T_i$


```r
spdata$Ti = as.numeric(ymd("2020-02-27")-spdata$start_source)
```

Specify some fixed and initial parameters.  
In the paper on medrxiv our estimates for the incubation period were shape: 3.36 (2.09, 4.28) and scale: 2.11 (1.32,2.46). 

Here we will have a shape $a_g$ for the generation time and $a_i$ for the incubation period, and the same scale $b$ for both. 


```r
b=2.1 # common scale parameter 
ai=3.4 # shape for incubation period ,as estimated in first round. true value less than this? 
ag = 3 #starting point for shape for generation time. 
n=3  # max number of intermediate cases 
r = 0.1 # add on average 1 intermediate per 10 days? who knows. must look at sensitivity to this parameter 
```

Functions


```r
# CDF of convolution of k gen times and an incubation period 
Fk <- function(t, ninters, genshape, incshape , comscale) {
  return(pgamma(q = t, 
                shape = ninters*genshape+incshape, 
                scale = comscale))}

#Li under right trunctation with k intermediates (Lirt k from eq 1 in C's notes) 
lirtk <- function(maxtime, mintime,k, rtTime, genshape, incshape, comscale) {
  Top1 = Fk(maxtime, ninters = k, genshape, incshape, comscale)
  Top2 = Fk(mintime,  ninters = k, genshape, incshape, comscale)
  Bottom = Fk(rtTime, ninters = k, genshape, incshape, comscale) 
  return((Top1-Top2)/Bottom)
}

# probability of k intermediates , eq 4 in C's notes 
pk <- function(k,maxinters=n, rate=r, maxtime, mintime) {
 if (k > maxinters)  {return(0)}
  if (k <= maxinters) {
    lam = 0.5*rate*(maxtime+mintime)
    return( dpois(k, lam)/ppois(maxinters, lam))
  }
}

# log of sum over k of pk*lirtk to get log(lirt) (eq 2 of C's notes) 
lirt = function(maxtime, mintime, rtTime, maxinters=n, rate=r,genshape = ag, 
                incshape=ai, comscale = b) {
  pks = vapply(0:maxinters, 
               function(x) pk(x, maxinters = maxinters, rate=rate,
              maxtime=maxtime,mintime=mintime),
               FUN.VALUE = 0)
 
   lirtks = vapply(0:maxinters,
                  function(x) lirtk(maxtime, mintime,k =x, rtTime,
                                    genshape,   incshape, comscale), 
                  FUN.VALUE = 0)
  
  return(log(sum(pks*lirtks)))
}
```


These functions seem to work. Yay! Now we have to set up the relevant data inputs, and maximize the likelihood to solve for several parameters. 

We have spdata's minIncTimes and maxIncTimes, which are the 'mintime' and 'maxtime' inputs. We already computed Ti which are the rtTime input. 


```r
# negative log likelihood function for optim 
l_optim <- function( twopars, allmaxtimes, allmintimes, allrtTimes, 
                     maxinters=n, rate=r, comscale=b) {
  gs=twopars[1] # gen time scale parameter
  is=twopars[2] # incubation period scale parameter
  Ncases = length(allmaxtimes) 
  # now compute lirt for each case i 
  indelikes = vapply(1:Ncases, 
                     function(ind) lirt(maxtime=allmaxtimes[ind],
                                        mintime=allmintimes[ind],
                                        rtTime=allrtTimes[ind],
                                        maxinters=maxinters, rate=rate, 
                                        genshape = gs, incshape=is, 
                                        comscale=comscale),
                     FUN.VALUE = 1)
  # the product is the likelihood. the negative sum is the negative log likelihood
  return(-sum(indelikes))
}
```



Testing: seems to work 


```r
l_optim(c(3,4), allmaxtimes = spdata$maxIncTimes, 
        allmintime=spdata$minIncTimes,allrtTimes = spdata$Ti, 
         maxinters=n, rate=r, comscale=b)
```

```
## [1] 84.1
```

So now let's optimize! 


