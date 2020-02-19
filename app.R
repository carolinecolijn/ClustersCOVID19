

library(shiny)
library(ggplot2)
library(viridis)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Singapore nCov 2019 -- Network Diagram"),
  
  plotOutput(outputId = "plot", width = "100%", height = "800"),
  
  selectInput(inputId = "subset", 
              label = "Choose an attribute to colour the group by:",
              choices = c("age", "cluster",
                          "hospital"),
              selected = "age"),
  
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
  output$plot <- renderPlot({
    
    n=read.csv("data/good_coords.csv")
    
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    n$age=as.character(n$age)
    n$age=as.integer(n$age)
    n$age_bin=bin(n$age,nbins=5)
    
    subset <- if(input$subset=="age") "age_bin" else input$subset
    
    cov_net=ggplot(n,aes(x = x, y = y, xend = xend, yend = yend))+
      geom_edges(aes(),subset(n,Related.cases!=""),linetype="solid")+
      geom_nodes(aes(x = x, y = y,color=n[[subset]]),size=14)+
      geom_text(aes(x = x, y = y,label=subset),check_overlap = TRUE)+
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
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

