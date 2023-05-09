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

#Adding Clean Sheets Column to the attacking_table
attacking_table <- attacking_table %>%
  #clean_sheets_table has the Clean Sheets data
  inner_join(clean_sheets_table) %>%
  #Removing Extra Column
  select(-`Matches Played`)


choicesForYear <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

# Create a named list of all team names
squad_choice_values <- setNames(unique(attacking_table$Squad), unique(attacking_table$Squad))


############
#    ui    #
############

# Create UI
ui <- navbarPage(
  title = "Table For Last 10 Years stats",
  sidebarLayout(
    sidebarPanel(
      # Create radio buttons for choosing between a year and a squad
      radioButtons(inputId = "typeTable"
                   ,label = "Choose Type Of Table"
                   ,choices = c("Year", "Squad")
                   ,selected = "Year"),
      # Create a selectize input for choosing a year or a squad depending on the selected radio button
      selectizeInput(inputId = "tableDropDownMenu"
                     , label = "Choose a Year"
                     , choices = choicesForYear
                     , selected = "2013"
                     , multiple = FALSE),
      "Some Squads might not have data for all years because they might have been relegated from the premier league for some years."
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "table")
    )
  )
)

############
# server   #
############
server <- function(input, output, session) {
  
  #Gives choices available based on what type of table user wants
  new_choices <- reactive({
    ifelse(input$typeTable == "Year", return(choicesForYear), return(squad_choice_values))
  })
  
  #Gives a label based on what type of table user wants
  new_labels <- reactive({
    ifelse(input$typeTable == "Year", "Choose a Year", "Choose a Squad")
  })
  
  #Gives default selected item based on what type of table user wants
  new_selected <- reactive({
    ifelse(input$typeTable == "Year", "2013", "Manchester City")
  })
  
  # Update selectizeInput in Table Tab when choice for typeTable changes
  observeEvent(new_choices(), {
    updateSelectizeInput(session, "tableDropDownMenu", choices = new_choices(), label = new_labels(), selected = new_selected())
  },
  ignoreInit = TRUE, # ignore the initial value of new_choices
  priority = 100  # set a high priority to ensure it runs before other observers
  )
  
  data_for_table <- reactive({
    data <- attacking_table %>%
      #Renaming the column names
      rename(Rank = Rk,
             `Matches Played` = MP,
             Wins = W,
             Draws = D,
             Losses = L,
             `Goals Scored` = GF,
             `Goals Conceded` = GA,
             `Goals Difference` = GD,
             Points = Pts) %>%
      #Filtering based on what was user selected for their choice of squad or year.
      filter(Year == input$tableDropDownMenu | Squad == input$tableDropDownMenu) %>%
      #Selecting only the relevant columns based on what type of table the user wants
      select(ifelse(input$typeTable == "Year", "Rank", "Year"),
             ifelse(input$typeTable == "Year", "Squad", "Rank"),
             `Matches Played`,
             Wins,
             Draws,
             Losses,
             `Goals Scored`,
             `Goals Conceded`,
             `Goals Difference`,
             `Clean Sheets`,
             Points) 
    
  })
  
  
  output$table <- DT::renderDataTable({ 
    data_for_table() 
  }, rownames = FALSE
  , caption = paste("Premier League Table For ", input$tableDropDownMenu)) #Output based on what user selected for squad or year.
  ##    END OF AHSAN'S WORk          ##
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
