library(tidyverse)


data<-read.table("nCov_Singapore_ 2019_heatmap_datatable_20202020.tsv", header = TRUE, sep = "\t")

data_long <- data %>% gather(key=date, value=status, X1.18.2020:X2.19.2020)

data_long$date <- gsub('X', '0', data_long$date)
data_long$date <- gsub('\\.', '\\/', data_long$date)


write.csv(data_long, "heatmap_long_20202020.csv")