

library(shiny)
library(ggplot2)
library(viridis)

# Define UI for app that draws a histogram ----
ui <- navbarPage(
  # App title ----
  title="nCov 2019 Clustering (Singapore)",
  tabPanel("Network Diagram", 
           plotOutput(outputId = "networkPlot", width = "100%", height = "800"),
           selectInput(inputId = "subset", 
                       label = "Choose an attribute to colour the group by:",
                       choices = c("age", "cluster",
                                   "hospital"),
                       selected = "age"),),
  tabPanel("Heatmap",
           plotOutput(outputId = "heatmap", width = "100%", height = "800"),
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
  # # Sidebar layout with input and output definitions ----
  # sidebarLayout(
  # #   
  #   # Sidebar panel for inputs ----
  #   sidebarPanel(
  #     # We can subset by 
  #     # * age (binned)
  #     # * cluster 
  #     # * hospital 
  #     # * outcomes 
  #     # * travel_history_location 
  #     selectInput(inputId = "subset", 
  #                 label = "Choose an attribute to colour the group by:",
  #                 choices = c("age", "cluster",
  #                             "hospital"),
  #                 selected = "age"),
  # #     
  # #     # Input: Slider for the number of bins ----
  # #     sliderInput(inputId = "bins",
  # #                 label = "Number of bins:",
  # #                 min = 1,
  # #                 max = 50,
  # #                 value = 30)
  # #     
  #   ),
  #   
  #   # Main panel for displaying outputs ----
  #   mainPanel(
  #     
  #     # Output: Plot ----
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
    
    varNet = read.csv("data/good_coords.csv")
    
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    varNet$age = as.character(varNet$age)
    varNet$age = as.integer(varNet$age)
    varNet$age_bin = bin(varNet$age,nbins=5)
    
    subset <- if(input$subset=="age") "age_bin" else input$subset
    
    cov_net=ggplot(varNet,aes(x = x, y = y, xend = xend, yend = yend))+
      geom_edges(aes(),subset(varNet,Related.cases!=""),linetype="solid")+
      geom_nodes(aes(x = x, y = y,color=varNet[[subset]]),size=14)+
      geom_text(aes(x = x, y = y,label=name),check_overlap = TRUE)+
      guides(colour = guide_legend(title=subset, title.position="top",title.hjust = 0.5,override.aes = list(size=6)))+
      scale_color_viridis_d(na.value="grey50")+
      theme_bw()+
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.text=element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text=element_text(size=12)
      )
    
    cov_net

  })

  
  output$heatmap <- renderPlot({
    
    varHeatmap = read.csv("data/heatmap_plot.csv")
    
    heatmap <- ggplot(varHeatmap, aes(x = date, y = case, fill = status)) +
      geom_tile() +
      xlab(label = "Date") +
      ylab(label = "Cases") +
      ggtitle("COVID-19 progression among Singapore cases") +
      labs(fill = "Status") + #tile fill legend label
      theme(plot.title = element_text(hjust = 0.5)) + #centre main title
      theme(axis.text.x = element_text(angle =60, hjust = 0.6, size = 8),
            axis.ticks.x = element_blank(), #remove x axis ticks
            axis.ticks.y = element_blank()) + #remove y axis ticks
      scale_fill_viridis() +
      theme(panel.background = element_rect(fill = "white"))
    
    heatmap
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

