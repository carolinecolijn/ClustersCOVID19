

library(shiny)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggnetwork)
library(viridis)


# Define UI for app that draws a histogram ----
ui <- shinyUI(
  navbarPage(
    # App title ---
    title = "COVID-19 Clustering (Singapore)",
    
    tabPanel("Network Diagram", 
             plotOutput(outputId = "networkPlot", width = "100%", height = "800"),
             selectInput(inputId = "subset", 
                         label = "Choose an attribute to colour the group by:",
                         choices = c("age", "cluster", "hospital"),
                         selected = "age"),
    ),
    
    tabPanel("Heatmap",
             plotlyOutput(outputId = "heatmap", width = "100%", height = "1000"),
    ),
    
    # Footer link ---
    footer = HTML("<a href=\"https://github.com/yxblee/EpiCoronaHack_Cluster/blob/master/Data/Singapore/COVID-19_Singapore.csv\" style=\"font-size:24px\">
                  Singapore COVID-19 Dataset (Updated as of February 26, 2020)</a>")
    
  )
    
    
    # fluidPage(
    # 
    # titlePanel("Singapore nCov 2019 -- Network Diagram"),
    # 
    # 
    # plotOutput(outputId = "networkPlot", width = "100%", height = "800"),
    # selectInput(inputId = "subset", 
    #             label = "Choose an attribute to colour the group by:",
    #             choices = c("age", "cluster",
    #                         "hospital"),
    #             selected = "age")
    #     
    #   
    #   # Main panel for displaying outputs ---
    #   mainPanel(
    #     
    #     # Output: Plot ---
    #     plotOutput(outputId = "plot",  width = "100%")
    #     
    #   )
    # )
    
)


# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  # This expression that generates a plot is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$networkPlot <- renderPlot({
    
    varNet = read.csv("COVID-19_Singapore_Network_plot.csv")
    
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # varNet$age = as.character(varNet$age)
    # varNet$age = as.integer(varNet$age)
    # varNet$age_bin = bin(varNet$age,nbins=5)
    
    subset <- if(input$subset=="age") "age_bin" else input$subset
    
    cov_net = ggplot(varNet, aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(aes(), subset(varNet, Related.cases != ""), linetype="solid") +
      geom_nodes(aes(color=varNet[[subset]]), size=14) +
      geom_text(aes(label=name), check_overlap = TRUE) +
      ggtitle("COVID-19 Singapore Cluster Network") +
      guides(colour = guide_legend(title=subset, title.position="top", title.hjust = 0.5, override.aes = list(size=6))) +
      scale_color_viridis_d(na.value="grey50") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) + #centre main title
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.text=element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text=element_text(size=12))
    
    cov_net

  })

  
  output$heatmap <- renderPlotly({
    
    varHeatmap = read.csv("COVID-19_Singapore_Heatmap_plot.csv")
    
    varHeatmap$date <- factor(varHeatmap$date, levels=unique(varHeatmap$date))
    varHeatmap$case <- factor(varHeatmap$case, levels=unique(varHeatmap$case))
    
    heatmap <- ggplot(
      varHeatmap, 
      # aes(x = date, y = case, fill = status_word,
      aes(x = date, y = case, fill = status,
          text = paste("Case: ", case_detailed,
                       "<br>Date: ", date,
                       "<br>Status: ", status_word,
                       "<br>Cluster: ", cluster,
                       "<br>Citizenship: ", citizenship))) +
      geom_tile() +
      xlab(label = "Date") +
      ylab(label = "Cases") +
      ggtitle("COVID-19 Progression Amongst Singapore Cases") +
      labs(fill = "Status") + #tile fill legend label
      theme(plot.title = element_text(hjust = 0.5)) + #centre main title
      theme(axis.text.x = element_text(angle = 60, hjust = 0.6, size = 8),
            axis.ticks.x = element_blank(), #remove x axis ticks
            axis.ticks.y = element_blank()) + #remove y axis ticks
      # scale_fill_viridis_d(direction = -1) +
      scale_fill_viridis_c(direction = 1) +
      theme(panel.background = element_rect(fill = "white"))
    
    ggplotly(heatmap, tooltip = 'text')
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

