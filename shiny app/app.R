library(shiny)
library(shinythemes)
library(tidyverse)
library(igraph)
library(ggraph)

###############
# import data #
###############
load("LauwrangledData.RData")
load("MuhammadAhsanTahir.RData")
load("MichaelPerales.RData")
DataWrangling<-load("DataWrangling.RData")


z <- c(finalTable$name1, finalTable$name2)

team_names <- unique(z)

############
#    ui    #
############

# Create UI
ui <- navbarPage(
  title = "Head to Head Premier League Games",
  theme = shinytheme("flatly"),
  
  ##create network graph tab
  tabPanel(
    title = "Network Of Premier League",
    sidebarLayout(
      sidebarPanel(
        
        selectInput(
          inputId = "netvar",
          label = "Choose a team in the Premier League to choose:",
          choices = team_names,
          selected = "Manchester United"
        ),
        radioButtons(inputId = "netType"
                     ,label = "Choose Type Of Network Graph"
                     ,choices = c("Goals", "Wins")
                     ,selected = "Goals")
      ),
      mainPanel(
        plotOutput(outputId = "net")
      )
    )
  )
)

############
# server   #
############
server <- function(input, output, session) {
  
  getNetGraph <- reactive({
    finalTablesolo <- finalTable %>%
      filter(name1 == input$netvar | name2 == input$netvar) %>%
      mutate(subjectTeam = input$netvar,
             objectTeam = ifelse(name1 == input$netvar, name2, name1),
             totalGoalDifference = ifelse(name1 == input$netvar, totalGoalDifference, -totalGoalDifference),
             winDifference = ifelse(name1 == input$netvar, winDifference, -winDifference),
             Goals = ifelse(totalGoalDifference > 0, "Positive Goal Difference",ifelse(totalGoalDifference == 0,"No Goal Difference","Negative Goal Difference")),
             Wins = ifelse(winDifference > 0, "Positive Win Difference",ifelse(winDifference == 0,"No Win Difference","Negative Win Difference")),
             absGoalDiff = abs(totalGoalDifference),
             absWinDiff = abs(winDifference)) %>%
      select(subjectTeam, objectTeam, absGoalDiff, absWinDiff, Goals, Wins)
    solo <- graph_from_data_frame(finalTablesolo, directed = FALSE)
    return (solo)
  })
  
  getSize <- reactive({
    ifelse(input$netType == "Goals", "absGoalDiff", "absWinDiff")
  })
  
  ##NETWORK GRAPH OUTPUT  
  output$net <- renderPlot({
    set.seed(123)
    solo <- getNetGraph()
    ggraph(solo, layout = "fr") +
      geom_edge_link(aes_string(width = getSize(), color = input$netType),
                     arrow = arrow(type = "open", length = unit(8, "pt"))) +
      scale_edge_width(name = paste("Size of ", input$netType, " Difference"), range = c(0.5, 5), guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
      geom_node_point() +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void() +
      labs(color = paste(input$netType, " Difference"), width = paste("Size of ", input$netType, " Difference"),
           title = "Graph For last 10 years premier league Head to Head")
    
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
