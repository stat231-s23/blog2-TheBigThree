library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)

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
  
  ##create table tab
  tabPanel(
    title = "Table for the last 10 years",
    sidebarLayout(
      sidebarPanel(
        
        selectInput(
          inputId = "tablevar",
          label = "Choose a team in the Premier League to view:",
          choices = team_names,
          selected = "Manchester United"
        )
      ),
      mainPanel(
        DTOutput(outputId = "premTable")
      )
    )
  )
)

############
# server   #
############
server <- function(input, output, session) {
  
  getPremTable <- reactive({
    finalTableSolo <- finalTable %>%
      filter(name1 == input$tablevar | name2 == input$tablevar) %>%
      mutate(subjectTeam = input$tablevar,
             objectTeam = ifelse(name1 == input$tablevar, name2, name1),
             totalGoalDifference = ifelse(name1 == input$tablevar, totalGoalDifference, -totalGoalDifference),
             winDifference = ifelse(name1 == input$tablevar, winDifference, -winDifference),
             absGoalDiff = abs(totalGoalDifference),
             absWinDiff = abs(winDifference)) %>%
      select(subjectTeam, objectTeam, absGoalDiff, absWinDiff)
    
    return(finalTableSolo)
  })
  
  ##TABLE OUTPUT  
  output$premTable <- renderDT({
    datatable(getPremTable(),
              options = list(
                pageLength = 25
              ))
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
