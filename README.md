# Analysis of novel Coronavirus Disease (COVID-19) Singapore and Tianjin outbreak clusters

## Authors and contact information
* [Lauren Tindale](https://github.com/ltindale)
* Michelle Coombe at michelle.coombe.vet@gmail.com
* [Emma Garlock](https://github.com/esgarlock)
* [Venus Lau](https://github.com/vlauu)
* Jessica Stockdale
* [Y. Brian Lee](https://github.com/yxblee)
* Manu Saraswat
* Louxin Zhang
* Dongxuan Chen
* Jacco Wallinga
* [Caroline Colijn](https://github.com/carolinecolijn)

## Summary of analysis and link to publication
The novel Coronavirus Disease, COVID-19, was first identified in Wuhan, Hubei Province, China in December 2019 and has since spread around the globe. It is crucial to identify accurate estimates of parameters that describe the SARS-CoV-2 virus' transmission patterns to understand and control this new pathogen. New, distinct outbreak clusters that report detailed case information (e.g. exposure contact networks and timing of symptom onset) are ideal for understanding how COVID-19 can spread through a population with no prior exposure to the virus. In our analysis we estimate the serial interval (i.e. the time between symptom onset in a primary case to symptom onset in a successive case in the chain of transmission) and incubation period (i.e. the time between infection and symptom onset) from two COVID-19 outbreak locations: Singapore and Tianjin, China. Using these parameters we are able to infer the basic reproductive number (R0) and identify the extent of the pre-symptomatic transmission (i.e. transmission that occurs prior to a patient displaying clinical symptoms). Our analysis suggest that there is substantial pre-symptomatic transmission, as the serial interval is shorter than incubation period by 2-4 days, and that stopping half the transmission events may be sufficient to control outbreaks, as R0 is approximately 2 in both populations.

This analysis originated from work begun at EpiCoronaHack 2020 which took place at Simon Fraser University, BC, Canada on Feb 18-19, 2020. This work has also resulted in a Shiny App, available here [Relative link to Shiny App](../Data/nCov_Singapore_2019.csv). \, to visualize the disease progression timeline of cases in each population.  


## Summary of data sources
### Singapore Cluster
84 confirmed cases as of February 19, 2019. The data was obtained from press releases from the Ministry of Health Singapore [website](https://www.moh.gov.sg/covid-19). 


### Tianjin Cluster
### Curation of Singapore Cases
Centralize all details of the Singapore cases into an analysis-friendly text file. [Relative link to copy of dataset](../Data/nCov_Singapore_2019.csv). \
These include:
- Relevant dates: presumed infection, symptom onset, hospitalization, case confirmation, hospital discharge
- Travel history
- Citizenship
- Links to other cases: family, travel companion, clusters at specific locations



