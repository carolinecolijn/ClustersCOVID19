# Analysis of novel Coronavirus Disease (COVID-19) Singapore and Tianjin outbreak clusters

## Transmission interval estimates suggest pre-symptomatic spread of COVID-19

This repo contains data and code for estimates of incubation period and serial interval in each of two transmission clusters: Tianjin, China and Singapore. We also estimate R0 and the portino of transmission that is pre-symptomatic. This work is currently available as a pre-print at medrxiv.org and has been submitted to a journal. 

[Link to preprint](https://medrxiv.org/cgi/content/short/2020.03.03.20029983v1)
[Link to manuscript draft](Initial_submission/COVID_19_Singapore_Tianjin_analysisSUPP-joined.pdf). 

**Abstract**: 

**Background**: As the COVID-19 epidemic is spreading, incoming data allows us to quantify values of key variables that determine the transmission and the effort required to control the epidemic. We determine the incubation period and serial interval distribution for transmission clusters in Singapore and in Tianjin. We infer the basic reproduction number and identify the extent of pre-symptomatic transmission.

**Methods**: We collected outbreak information from Singapore and Tianjin, China, reported from Jan.19-Feb.26 and Jan.21-Feb.22, respectively. We estimated incubation periods and serial intervals in both populations.

**Results**: The mean incubation period was 7.1 (6.13, 8.25) days for Singapore and 9 (7.92, 10.2) days for Tianjin. Both datasets had shorter incubation periods for earlier-occurring cases. The mean serial interval was 4.56 (2.69, 6.42) days for Singapore and 4.22 (3.43, 5.01) for Tianjin. We inferred that early in the outbreaks, infection was transmitted on average 2.55 and 2.89 days before symptom onset (Singapore, Tianjin). The estimated basic reproduction number for Singapore was 1.97 (1.45, 2.48) secondary cases per infective; for Tianjin it was 1.87 (1.65, 2.09) secondary cases per infective.

**Conclusions**: Estimated serial intervals are shorter than incubation periods in both Singapore and Tianjin, suggesting that pre-symptomatic transmission is occurring. Shorter serial intervals lead to lower estimates of R0, which suggest that half of all secondary infections should be prevented to control spread.

## Authors and contact information
* [Lauren Tindale](https://github.com/ltindale) at tindale13@gmail.com
* [Michelle Coombe](https://github.com/mkc030) at michelle.coombe.vet@gmail.com
* [Emma Garlock](https://github.com/esgarlock) at egarlock@sfu.ca
* [Venus Lau](https://github.com/vlauu) at Venusl@sfu.ca
* [Jessica Stockdale](https://github.com/jessicastockdale) at jessica_stockdale@sfu.ca
* [Y. Brian Lee](https://github.com/yxblee) at yblee@sfu.ca
* [Manu Saraswat](https://github.com/saraswatmanu) at msaraswat@cmmt.ubc.ca
* Louxin Zhang at matzlx@nus.edu.sg
* Dongxuan Chen at dongxuan_chen@outlook.com
* Jacco Wallinga at jacco.wallinga@rivm.nl
* [Caroline Colijn](https://github.com/carolinecolijn), who is corresponding author at ccolijn@sfu.ca

## Summary of work
The novel Coronavirus Disease, COVID-19, was first identified in Wuhan, Hubei Province, China in December 2019 and has since spread around the globe. It is crucial to identify accurate estimates of parameters that describe the SARS-CoV-2 virus' transmission patterns to understand and control this new pathogen. New, distinct outbreak clusters that report detailed case information (e.g. exposure contact networks and timing of symptom onset) are ideal for understanding how COVID-19 can spread through a population with no prior exposure to the virus. In our analysis we estimate the serial interval (i.e. the time between symptom onset in a primary case to symptom onset in a successive case in the chain of transmission) and incubation period (i.e. the time between infection and symptom onset) from two COVID-19 outbreak locations: Singapore and Tianjin, China. Using these parameters we are able to infer the basic reproductive number (R0) and identify the extent of the pre-symptomatic transmission (i.e. transmission that occurs prior to a patient displaying clinical symptoms). Our analysis suggest that there is substantial pre-symptomatic transmission, as the serial interval is shorter than incubation period by 2-4 days, and that stopping half the transmission events may be sufficient to control outbreaks, as R0 is approximately 2 in both populations.

It is also important to note that the true source of infection is often unknowable, and that the relationships used between cases in infection clusters are "best guesses". How changing these relationship estimates affects the epidemiological parameters is a topic the team aims to investigate further in the future.

This analysis originated from work begun at EpiCoronaHack which took place at Simon Fraser University, BC, Canada on Feb 18-19, 2020. The work began by compiling data on Singapore and Tianjin COVID-19 cases into a matrix, and has resulted in a Shiny App ([go to Shiny App folder](/Shiny)), which uses a heatmap to visualize the disease progression timeline of cases in each population, as well as a manuscript detailing results of statistical analyses. 


## Summary of data sources
### Singapore Cluster
This dataset [link to copy of dataset](/data/COVID-19_Singapore.csv) includes the 93 cases confirmed between Jan 19 to Feb 26, 2020. The data was obtained from press releases from the Ministry of Health Singapore [website](https://www.moh.gov.sg/covid-19). Close contacts and a few transmission clusters were identified for many of the Singapore cases. The dataset includes the following information:

* CaseID = case identification number

* Related cases = caseID of other confirmed cases who had direct known contact with this patient

* Cluster links = caseID of other confirmed cases that are linked together through an identified cluster event

* Relationship notes = available notes on the relationship between the case and other confirmed cases identified in previous columns

* Case = further demographic information on the case

* Age 

* Sex

* Country

* Hospital

* Presumed_infected_date = earliest known date of exposure, based on the reason provided in 'presumed reason'

* Presumed_reason  = reason why date for presumed_infected_date was used

* last_poss_exposure = subclassification of 'presumed_infected_date', representing the last date that the case could have been infected (which is the date of arrival in Singapore for those cases that travelled from Wuhan)

* symp_presumed_infector = subclassification of 'presumed_infected_date', representing the date when the case was likely infected during a local SARS-CoV-2 transmission event in Singapore

* date_onset_symptoms = the date of the onset of symptoms for the case

* date_quarantine = date of quarantine for the case

* date_hospital = date of hospitalization

* date_confirmation = date of offical COVID-19 confirmation

* outcome = outcome of the infection, either 'discharged' if the case has recovered and was discharged from hospital, or NA if case has not recovered yet. Note there have been no COVID-19 mortalities in Singapore between Jan 19 to Feb 26, 2020

* date_discharge = date of discharge, if applicable

* travel_history = any noted travel history

* additional_information = additional information provided on the case that was not capture in any prior columns

* cluster = the Ministry of Health Singapore's classification of cases into transmission cluster events

* citizenship


### Tianjin Cluster
The Tianjin dataset [link to copy of dataset](/data/Tianjin135casesFeb22.csv) was compiled by Dongxuan Chen and Louxin Zhang, and includes 135 confirmed cases between Jan 21 to Feb 27, 2020. The case information was obtained from online press releases, as well as local media sources including Zounai Jianjin and Jinyun News on Weibo. The three primary sources for the dataset are the following: 

* source1: Tianjin health commission official website, for daily announcements [link for source1](http://wsjk.tj.gov.cn/col/col87/index.html#!uid=259&pageNum=1)

* source2: Jinyun News, Tianjin offical local media weibo account, for patient symptom onset reference [link for source2](https://weibo.com/u/2967529507) 

* source3: another Tianjin local media weibo link, for mall cluster reference [link for source3](https://m.weibo.cn/status/IrrHI1FHm?jumpfrom=weibocom) 

The dataset includes the following information:

* case_id = case identification number, including the TJ prefix

* gender

* age

* symptom_onset = date of case's onset of clinical symptoms; not reported for a few patients who did not have symptoms before being diagnosed at quarantine center

* symptom_type = any reported clinical symptoms

* confirm_date = date of official COVID-19 confirmation

* Infection_source = probably source of infection, as reported in media sources

* start_source = start of exposure window, based on provided travel or exposure history and contact information

* end_source = end of exposure window, based on provided travel or exposure history and contact information

* severity = severity of disease, labelled as either minor, normal, severe, or unclassified (and missing in a small number of cases)

* death = date of death

* recorrection = additional notes on data and cases

* notes = additional notes on data and cases

## Summary of file locations on repo
This repo provides code for figures, descriptive and statistical analyses generated for the manuscript. The files to generate the initial manuscript are located under the 'Intial_submission' folder [link to intial submission files](/Initial_submission). The files in the main repo are used to generate the revised version of the manuscript.
Both Tianjin and Singapore datasets are in the data folder [link to data folder](/data). PDF versions of the final figures are availble under the folder 'final_figures' [link to final figures](/final_figures). There are multiple folders ending in '_file' which containing graphs generated by making the .hmtl of the associated file. Lastly, the remaining files in the main repo folder are the .rmd files (and associated .html and .md files) for the following:

* Tianjin_wtables and singapore_wtables contain the statistical models used to determine serial intervals, incubation period, and R0 estimates, which were reported in the manuscript and associated supplementary information. (Tianjin, tianjin_JS, and singapore are earlier versions of the respective files and are located in the 'old_code' folder).

* portion_presympt contains the statistical models used to estimate the proportion of pre-symptomatic transmission

* Fig 1a and 2a rmds produce plots of hospitalized case counts and cumulative recovery and mortality incidence (Singapore and Tianjin respectively) 

* Fig 1b and 1b rmds produce plots of daily incidence plots, grouped by probably source of infection (Singapore and Tianjin respectively) 

* Fig 1c and 2c rmds produce the static plot versions of the Shiny App heatmap (Singapore and Tianjin respectively) 

* Fig 3 and Fig S1 are produced by Tianjin_wtables and singapore_wtables (older figure versions are in singapore_incubation_analysis and Tianjin_incubation_analysis, located in the 'old_code' folder)

* Fig 4 rmd produces the network diagrams for Tianjin and Singapore

* Fig S2 panels are produced in portion_presympt

For those new to Git - you can click on the .md file to see the code and associated results without having to download and run it yourself.
