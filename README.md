# EpiCoronaHack Team Cluster

## Members 
* [Brian Lee](https://github.com/yxblee)
* Delin Chen
* [Emma Garlock](https://github.com/esgarlock)
* [Lauren Tindale](https://github.com/ltindale)
* Manu Saraswat
* Michelle Coombe
* [Michelle Kang](https://github.com/ymkng)
* [Sebastian Bugal](https://github.com/zorenum)
* [Venus Lau](https://github.com/vlauu)

## Singapore Cluster
84 confirmed cases as of February 19, 2019. The data was scraped from press releases from the Ministry of Health Singapore [website](https://www.moh.gov.sg/covid-19). 

### Curation of Singapore Cases
Centralize all details of the Singapore cases into an analysis-friendly text file. 
These includes
- Relevant dates: presumed infection, symptom onset, hospitalization, case confirmation, hospital discharge
- Travel history
- Citizenship
- Links to other cases: family, travel companion, clusters at specific locations

### Interactive contact network map via R Shiny
The R code to generate the network diagram can be found [here](https://github.com/yxblee/EpiCoronaHack_Cluster/blob/master/Clustering/network_diagram/network_diagram.html) using the data found [here](https://github.com/yxblee/EpiCoronaHack_Cluster/blob/master/Clustering/data/singapore_ncov_2019.csv).

The diagram will be clustered based on the clusters determined by the Singapore government. Any edges in the figure are depicted to show other relationships (such as known interaction or family members) 

### Disease timeline for all cases
We created a heat map to show the disease progression timeline for each patient. 
The script used can be found [here](https://github.com/yxblee/EpiCoronaHack_Cluster/blob/master/Clustering/heatmap/heatmap_v1.R), using the data found [here](https://github.com/yxblee/EpiCoronaHack_Cluster/blob/master/Clustering/heatmap/heatmap_long_v3.csv). 
## Tianjin Cluster

## Disease modelling
