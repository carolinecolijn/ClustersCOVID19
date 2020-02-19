library(ggplot2)
library(viridis)
library(plotly)

data1 <- read.csv("heatmap/heatmap_long_v3.csv")
data1$date <- factor(data1$date,levels=unique(data1$date))
data1$case <- factor(data1$case,levels=unique(data1$case))


data1$status_word=ifelse(data1$status==0,"Unexposed",
                         ifelse(data1$status==1,"Exposed",
                                ifelse(data1$status==2,"Symptomatic",
                                       ifelse(data1$status==3,"Hospitalized","Discharged"))))

p1 <- ggplot(data1, aes(x = date, y = case, fill = status_word,
                        text = paste("Case: ", case,
                                     "<br>Date: ", date,
                                     "<br>Status: ", status,
                                     "<br>Cluster: ", cluster,
                                     "<br>Citizenship: ", citizenship))) +
  geom_tile() +
  xlab(label = "Date") +
  ylab(label = "Cases") +
  ggtitle("COVID-19 progression among Singapore cases") +
  labs(fill = "Status") + #tile fill legend label
  theme(plot.title = element_text(hjust = 0.5)) + #centre main title
  theme(axis.text.x = element_text(angle =60, hjust = 0.6, size = 8),
        axis.ticks.x = element_blank(), #remove x axis ticks
        axis.ticks.y = element_blank()) + #remove y axis ticks
  scale_fill_viridis_d(direction = -1) +
  theme(panel.background = element_rect(fill = "white"))

ggplotly(p1,tooltip = 'text')
 