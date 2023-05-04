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

load("LauwrangledData.RData")
load("MuhammadAhsanTahir.RData")
load("MichaelPerales.RData")
DataWrangling<-load("DataWrangling.RData")


###############################################
# ADDITIONAL WRANGLING INCORPORATING FEEDBACK #
###############################################


#Adding Clean Sheets Column to the attacking_table
attacking_table <- attacking_table %>%
  #clean_sheets_table has the Clean Sheets data
  inner_join(clean_sheets_table) %>%
  #Removing Extra Column
  select(-`Matches Played`)

#Adding whether or not a team won a particular season.
clean_sheets_table <- clean_sheets_table %>%
  #league_winners_last_10_years has the relevant data
  inner_join(league_winners_last_10_years) %>%
  #We do not need points
  select(- Points) %>%
  #Fixing the name of Manchester United
  mutate(Squad = ifelse(Squad == "Manchester Utd", "Manchester United", Squad)) %>%
  #Adding a new column for whether or not that team won
  mutate(didWin = ifelse(Squad == Winner, "Winning Team", "Other Teams"))

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)
choicesForYear <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

# Create a named list of all team names
squad_choice_values <- setNames(unique(attacking_table$Squad), unique(attacking_table$Squad))


# Create a named list of team names for winners
bar_choice_values <- setNames(unique(league_winners_last_10_years$Winner), unique(league_winners_last_10_years$Winner))

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
        plotOutput(outputId = "net"),
        ""
      )
    )
  ),
  
  ##create bar graph tab (Lau)
  tabPanel(
    title = "Bar Graph of Winners of last 10 years",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "barvar",
          label = "Choose a winner of the Premier League to highlight:",
          choices = bar_choice_values,
          selected = bar_choice_values
        )
      ),
      mainPanel(
        plotOutput(outputId = "bar"),
        "Points in the Premier League are a way of measuring a team's success over the course of a season, with 3 points awarded for a win, 1 point for a draw, and 0 points for a loss."
      )
    )
  ),
  ##TAB FOR AHSAN'S WORK: TABLE##
  tabPanel(
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
  ),
  
  # TAB FOR MICHAEL'S WORK
  tabPanel(
    title = "Bar Graph of Clean Sheets per Team",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "yearCS"
                       , label = "Choose a year"
                       , choices = choicesForYear
                       , selected = "2013"
                       , multiple = FALSE)
      ),
      mainPanel(
        plotOutput(outputId = "clean_sheets_bar"),
        "Clean Sheets in the Premier League refer to the games in which a team's goalkeeper has not conceded a goal, resulting in a shutout victory or a draw."
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
      # ggnetwork() %>%
      # ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edge_link(aes_string(width = getSize(), color = input$netType),
                     arrow = arrow(type = "open", length = unit(8, "pt"))) +
      scale_edge_width(name = paste("Size of ", input$netType, " Difference"), range = c(0.5, 5), guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
      geom_node_point() +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void() +
      labs(color = paste(input$netType, " Difference"), width = paste("Size of ", input$netType, " Difference"),
           title = "Graph For last 10 years premier league Head to Head")
    
  })
  
  ##TAB FOR Bar Graph Of Winners (Lau)
  
  # Mutate the data such that ind = "Highlighted Team" for the selected team.
  selected_team <- reactive({ 
    league_winners_last_10_years <- mutate(league_winners_last_10_years,
                                           # Create a variable to highlight the winner
                                           ind = ifelse(Winner == input$barvar, "Highlighted Team", "Regular Teams"))
  })
  # Create the Bar Graph plot
  output$bar <- renderPlot({
    ggplot(selected_team(), aes(x = as.factor(Year), y = as.numeric(Points), color = as.factor(ind), fill = as.factor(Winner))) +
      geom_bar(stat = "Identity", size=2) +
      #create a color scale for the bar chart for each team
      scale_fill_manual(values = c("Manchester United" = "red",
                                   "Liverpool" = "pink",
                                   "Manchester City" = "lightblue",
                                   "Chelsea" = "blue",
                                   "Leicester City" = "orange")) +
      #create gold highlight for selected team
      scale_color_manual(values = c("Highlighted Team" = "gold",
                                    "Regular Teams" = "black")) +
      labs(x = "Season", y = "Points", title = "Points of Premier League winners in last 10 years", fill = "Teams",
           color = "")
  })
  
  ## TAB FOR TABLE(Ahsan's Work) ##
  
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
  
  
  
  ##michael's work
  
  
  filteredData <- reactive({
    clean_sheets_table <- clean_sheets_table %>%
      #filtering only the relevant year
      filter(Year == input$yearCS)
  })
  
  output$clean_sheets_bar <- renderPlot({
    filtered_data <- filteredData()
    
    # Create the Bar Graph
    ggplot(filtered_data, aes(x = reorder(Squad, `Clean Sheets`), y = `Clean Sheets`, fill = as.factor(didWin))) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #Adding different fills for Winning Team VS Other Teams
      scale_fill_manual(values = c("Winning Team" = "gold",
                                   "Other Teams" = "steelblue")) +
      labs(title = "Bar Graph of Clean Sheets per Team",
           x = "Team",
           y = "Number of Clean Sheets",
           fill = "") +
      coord_flip()
  })
  
  
  
    
   
  }

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

