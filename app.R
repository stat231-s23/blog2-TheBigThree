library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(RSQLite)
library(tidyverse)
library(igraph)
library(ggraph)
library(dplyr)
library(ggnetwork)
library(tidygraph)

###############
# import data #
###############

# load("LauwrangledData.RData")
# load("MuhammadAhsanTahir.RData")
# load("MichaelPerales.RData")
DataWrangling<-load("/Users/lautencate/Documents/GitHub/blog2-TheBigThree/DataWrangling.RData")


###############################################
# ADDITIONAL WRANGLING INCORPORATING FEEDBACK #
###############################################



#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets

team_names <- unique(finalTable$name1)

############
#    ui    #
############

# Create UI
ui <- navbarPage(
  title = "Head to Head Premier League Games",
  theme = shinytheme("flatly"),
  
  ##create network graph tab (Lau)
  tabPanel(
    title = "XXXX",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "netvar",
          label = "Choose a team in the Premier League to choose:",
          choices = team_names,
          selected = "Manchester United"
        )
      ),
      mainPanel(
        plotOutput(outputId = "net"),
        "XXX"
      )
    )
  ),
  
)

############
# server   #
############
  server <- function(input, output, session) {
    
  output$net <- renderPlot({
    set.seed(123)
    finalTablesolo <- finalTable %>%
      filter(name1 == input$netvar)
    solo <- graph_from_data_frame(finalTablesolo, directed = FALSE)
    ggraph(solo, layout = "fr") +
      # ggnetwork() %>%
      # ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edge_link(aes(width = abs(totalGoalDifference), color = Goals),
                     arrow = arrow(type = "closed", length = unit(8, "pt"))) +
      scale_edge_width(name = "Goal Difference", range = c(0.5, 5), guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
      scale_color_manual(name = "", values = c("Positive Goal Difference" = "green", "Negative Goal Difference" = "red")) +
      geom_node_point() +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
    
  })
    
   
  }

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

