# Analysis of novel Coronavirus Disease (COVID-19) Singapore and Tianjin outbreak clusters

## Evidence for transmission of COVID-19 prior to symptom onset

This repo contains data and code for estimates of incubation period and serial interval in each of two transmission clusters: Tianjin, China and Singapore. We use these estimates to identify the extent of pre-symptomatic transmission (i.e., the amount of transmission that is occurring before someone is showing signs of being sick). 

<!--[Link to preprint](https://medrxiv.org/cgi/content/short/2020.03.03.20029983v1)-->
<!--[Link to manuscript draft](Initial_submission/COVID_19_Singapore_Tianjin_analysisSUPP-joined.pdf)-->
<!--[Link to final paper]-->

#### Summary of work
The novel Coronavirus Disease, COVID-19, was first identified in Wuhan, Hubei Province, China in December 2019 and has since spread around the globe. It is crucial to identify accurate estimates of parameters that describe the SARS-CoV-2 virus' transmission patterns to understand and control this new pathogen. New, distinct outbreak clusters that report detailed case information (e.g. exposure contact networks and timing of symptom onset) are ideal for understanding how COVID-19 can spread through a population with no prior exposure to the virus. 

We collated contact tracing data from COVID-19 clusters in Singapore and Tianjin, China and estimated the extent of pre-symptomatic transmission by estimating incubation periods and serial intervals. The mean incubation periods accounting for intermediate cases were 4.91 days (95%CI  4.35, 5.69) and 7.54 (95%CI  6.76, 8.56) days for Singapore and Tianjin, respectively. The mean serial interval was 4.17 (95%CI  2.44, 5.89) and 4.31 (95%CI 2.91, 5.72) days (Singapore, Tianjin). The serial intervals are shorter than incubation periods, suggesting that pre-symptomatic transmission may occur in a large proportion of transmission events (0.4-0.5 in Singapore and 0.6-0.8 in Tianjin, in our analysis with intermediate cases, and more without intermediates).  Given the evidence for pre-symptomatic transmission it is vital that even individuals who appear healthy abide by public health measures to control COVID-19.

This analysis originated from work begun at EpiCoronaHack which took place at Simon Fraser University, BC, Canada on Feb 18-19, 2020. The work began by compiling data on Singapore and Tianjin COVID-19 cases into a matrix and resulted in a manuscript detailing results of descriptive and statistical analyses. We would like to thank members of the EpiCoronaHack Cluster team for their help in curating the initial datasets and visualizations. 


## Authors and contact information
* [Lauren Tindale](https://github.com/ltindale)[^1] at tindale13@gmail.com
* [Jessica Stockdale](https://github.com/jessicastockdale)[^1] at jessica_stockdale@sfu.ca
* [Michelle Coombe](https://github.com/mkc030) at michelle.coombe.vet@gmail.com
* [Emma Garlock](https://github.com/esgarlock) at egarlock@sfu.ca
* [Wing Yin Venus Lau](https://github.com/vlauu) at Venusl@sfu.ca
* [Manu Saraswat](https://github.com/saraswatmanu) at msaraswat@cmmt.ubc.ca
* Louxin Zhang at matzlx@nus.edu.sg
* Dongxuan Chen at dongxuan_chen@outlook.com
* Jacco Wallinga at jacco.wallinga@rivm.nl
* [Caroline Colijn](https://github.com/carolinecolijn)[^2] at ccolijn@sfu.ca

[^1]: co-first authors
[^2]: corresponding author at ccolijn@sfu.ca

Please cite: Tindale LC*, Stockdale JE*, Coombe M, , Garlock E, Lau WYV, Saraswat M, Lee YHB, Zhang L, Chen D, Wallinga J, and Colijn C. Evidence for transmission of COVID-19 prior to symptom onset. eLife, 2020; 9:e57149. DOI: 10.7554/eLife.57149.

## Summary of data sources
### Singapore Cluster
This dataset [link to copy of dataset](/data/COVID-19_Singapore.csv) includes the 93 cases confirmed between Jan 19 to Feb 26, 2020. The data was obtained from press releases from the Ministry of Health Singapore [website](https://www.moh.gov.sg/covid-19). Close contacts and a few transmission clusters were identified for many of the Singapore cases. The dataset includes the following information:

* CaseID = case identification number

* Related cases = caseID of other confirmed cases who had known direct contact with this patient

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

* start_source = earliest possible date of exposure. Specifically, we assume that those travelling from Wuhan were exposed before travel (due to evidence of lack of community transmission in Singapore at the time), and that those cases associated with a particular event or location (e.g., Grace Assembly gatherings, the visit to the Yong Thai store by a tour group from Wuhan) were not exposed prior to that event. For cluster cases thought to originate from a particular index case but lacking information on dates of contact, 'start_source' is set to the first symptom onset in the cluster - 7 days. In the absence of other information, we set the 'start_source' of a case to their symptom onset date - 20 days (to allow for a wide range of epidemiologically feasible incubation periods).

* end_source = latest possible date of exposure. Specifically, for cases belonging to an identified cluster, we set 'end_source' to the date of the event + 4 days, to allow for some uncertainty and the possibility of an intermediate infector. The 'end_source' is set assuming that once a case in a cluster was identified, people were well aware of this and ceased mixing within the group; thus 'end_source' is the minimum of the earliest quarantine, hospitalization or symptom onset in the cluster, and the symptom onset date of the case in question. In the absence of other information, we set the 'end_source' to their symptom onset (since all cases must be exposed before they show symptoms).


### Tianjin Cluster
The Tianjin dataset [link to copy of dataset](/data/Tianjin135casesFeb22.csv) was compiled by Dongxuan Chen and Louxin Zhang, and includes 135 confirmed cases between Jan 21 to Feb 27, 2020. The case information was obtained from online press releases, as well as local media sources including Zounai Jianjin and Jinyun News on Weibo. The three primary sources for the dataset are the following: 

* source1: Tianjin health commission official website, for daily announcements [link for source1](http://wsjk.tj.gov.cn/col/col87/index.html#!uid=259&pageNum=1)
* source2: Jinyun News, Tianjin offical local media weibo account, for patient symptom onset reference [link for source2](https://www.weibo.com/u/2967529507?is_all=1) 
* source3: another Tianjin local media weibo link, for mall cluster reference [link for source3](https://m.weibo.cn/status/IrrHI1FHm?jumpfrom=weibocom) 

The dataset includes the following information:

* case_id = case identification number, including the TJ prefix

* gender

* age

* symptom_onset = date of case's onset of clinical symptoms; not reported for a few patients who did not have symptoms before being diagnosed at quarantine center

* symptom_type = any reported clinical symptoms

* confirm_date = date of official COVID-19 confirmation

* Infection_source = probably source of infection, as reported in media sources

* start_source = start of exposure window, based on provided travel or exposure history and contact information. These dates were defined in a similar manner as described for the Singapore dataset above. In most cases, the start and end times of exposure in Tianjin were defined by known windows of exposure; however, in the absence of other information, we set the 'start_source' of a case to their symptom onset date - 20 days.

* end_source = end of exposure window, based on provided travel or exposure history and contact information. Again, this column was defined in a similar manner as described in the Singapore dataset and—in the absence of other information—we set the 'end_source' to their symptom onset.

* severity = severity of disease, labelled as either minor, normal, severe, or unclassified (and missing in a small number of cases)

* death = date of death

* recorrection = additional notes on data and cases

* notes = additional notes on data and cases

