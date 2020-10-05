# Shinydashboard test
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
source("dashboardFunctions.R")

ui <- tagList(dashboardPagePlus(
  useShinyjs(),
  
  ## Define dashboard header: title, icon for right sidebar, etc.
  header = dashboardHeaderPlus(
    title = "YGDP Data Explorer",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "sliders"
  ),
  
  ## Define left sidebar and menu options
  sidebar = dashboardSidebar(
    leftSidebarScroll, # CSS to make the sidebar scroll. See dashboardFunctions.R
    sidebarMenu(
      id = "leftSidebar", # name of the left sidebar
      menuItem(expandedName = "mapView", text = "Map view",  icon = icon("map"), startExpanded = TRUE,
               # Title
               h4("Select sentences"),
               # Inputs
               div(style = reduceSpacing,
                   selectInput("survey", "Survey", 
                               choices = c("5", "5b", "6", "6b", "7", "8", "9", "11", "12"),
                               selected = "11",
                               multiple = FALSE)),
               ### Sentence selector
               div(style = reduceSpacing,
                   selectizeInput("sentence1", "Sentence 1:",
                                  choices = list(
                                    `After perfect` = c("I'm after eating ice cream.",
                                                        "She's just after telling me that she got the promotion."),
                                    `Needs washed` = c("Most babies like cuddled.",
                                                       "My car needs washed.")
                                  ), multiple = F)),
               ### Ratings selector
               checkboxGroupButtons("ratingsSentence1", "Rated:",
                                    choiceNames = c("1", "2", "3", "4", "5"),
                                    choiceValues = 1:5,
                                    selected = 1:5),
               ## CSS formatting for coloring the buttons. See dashboardFunctions.R
               tags$script(formatButtons(1)[1]),
               tags$script(formatButtons(1)[2]),
               tags$script(formatButtons(1)[3]),
               tags$script(formatButtons(1)[4]),
               tags$script(formatButtons(1)[5]),
               actionButton("sentence1Apply", "Apply"), # apply selections/ratings for sentence 1
               hr(),
               uiOutput("sentence2Controls"), # controls for the second sentence
               uiOutput("sentence3Controls"), # controls for the third sentence
               actionButton("addSentence", "+ Add a sentence") # button to add the second sentence
      ),
      menuItem(text = "Social variables", expandedName = "socialVariables", icon = icon("map"), startExpanded = FALSE,
               div(style = reduceSpacing,
                   selectInput("test", "Test", 
                               choices = c("a", "b", "c", "d", "e"),
                               selected = "a",
                               multiple = FALSE)))
    )
  ),
  
  ## Define dashboard body (maps/graphs)
  body = dashboardBody(
    shinyDashboardThemes( # not sure why we define the theme in the body as opposed to at the beginning of the UI, but okay.
      theme = "grey_dark"
    ),
    tabItems( # different outputs to be shown depending on which menu item is selected in the lefthand menu
      tabItem(
        tabName = "map",
        p("[Insert map here]")),
      tabItem(
        tabName = "socialCharts",
        p("[Insert charts here]"))
    )
  ),
  rightsidebar = rightSidebar(
    background = "dark",
    uiOutput("rightSidebar"), # We'll define this dynamically using renderUI in the server
    title = "Right Sidebar"
  )
),
tags$footer("Created by Kaija Gahm for the YGDP, October 2020. Code at https://github.com/kaijagahm/ygdpDashboard.", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:18px;
              color: #e6e6e6;
              padding: 2px;
              background-color: #46505a;
              z-index: 500;
              font-size:75%") # z index governs what goes in front/in back. The left sidebar is apparently 810. I don't know what the right sidebar is. 500 seems to get it behind both.
)

server <- function(input, output) {
  # Define a reactiveVal to store the current number of sentences being used in the map view
  nSentences <- reactiveVal(1)
  
  observeEvent(input$addSentence, { # when they click the addSentence button
    if(is.null(input$sentence2)){ # if sentence 2 hasn't been selected yet, render sentence 2 selector
      output$sentence2Controls <- renderUI({
        tagList(
          div(style = reduceSpacing,
              selectizeInput("sentence2", "Sentence 2:",
                             choices = list(
                               `After perfect` = c("I'm after eating ice cream.", 
                                                   "She's just after telling me that she got the promotion."),
                               `Needs washed` = c("Most babies like cuddled.",
                                                  "My car needs washed.")
                             ),
                             multiple = F)),
          checkboxGroupButtons("ratingsSentence2", "Rated:",
                               choiceNames = c("1", "2", "3", "4", "5"),
                               choiceValues = 1:5,
                               selected = 1:5),
          tags$script(formatButtons(2)[1]), # nicely formatted rating selector buttons for sentence 2
          tags$script(formatButtons(2)[2]),
          tags$script(formatButtons(2)[3]),
          tags$script(formatButtons(2)[4]),
          tags$script(formatButtons(2)[5]),
          radioGroupButtons("joinType2", label = "How joined:", # create the joinType2 selector
                            choices = c("AND", "OR", "NOT"),
                            selected = "AND",
                            status = "info"),
          actionButton("sentence2Apply", "Apply"), # apply selections/ratings for sentence 2
          hr() # horizontal bar
        )
      })
    }else if(!is.null(input$sentence2)){ # if sentence 2 has already been selected
      output$sentence3Controls <- renderUI({
        tagList(
          div(style = reduceSpacing,
              selectizeInput("sentence3", "Sentence 3:",
                             choices = list(
                               `After perfect` = c("I'm after eating ice cream.",
                                                   "She's just after telling me that she got the promotion."),
                               `Needs washed` = c("Most babies like cuddled.",
                                                  "My car needs washed.")
                             ),
                             multiple = F),
              checkboxGroupButtons("ratingsSentence3", "Rated:",
                                   choiceNames = c("1", "2", "3", "4", "5"),
                                   choiceValues = 1:5,
                                   selected = 1:5),
              tags$script(formatButtons(3)[1]),
              tags$script(formatButtons(3)[2]),
              tags$script(formatButtons(3)[3]),
              tags$script(formatButtons(3)[4]),
              tags$script(formatButtons(3)[5]),
              radioGroupButtons("joinType3", label = "How Joined:",
                                choices = c("AND", "OR", "NOT"),
                                selected = "AND",
                                status = "info"),
              actionButton("sentence3Apply", "Apply"), # apply selection/ratings for sentence 1
              hr() # horizontal bar
          )
        )
      })
      removeUI(
        selector = "#addSentence"
      )
    }
  })
  
  ## Determine what shows up in the right menu bar, and when it opens/closes
  observeEvent(input$sidebarItemExpanded, {
    if(req(input$sidebarItemExpanded) == "mapView"){
      message("Map view has been selected.")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open") #open the "control sidebar" (righthand sidebar) when the menu tab is selected
      output$rightSidebar <- renderUI({
        rightSidebar(
          rightSidebarTabContent(
            title = "Demographic filters",
            id = "mapDemoFilters",
            active = T,
            icon = "sliders",
            sliderInput("age", "Age:", min = 18, max = 100, value = c(18, 100)),
            pickerInput("gender", "Gender:", choices = c("Male", "Female", "Nonbinary/Other"),
                        selected = c("Male", "Female", "Nonbinary/Other"), options = list(`actions-box` = TRUE), multiple = T),
            pickerInput("education", "Education level:", choices = c("Some HS", "High school diploma", "Some college", "Bachelor's degree", "Associate degree", "Graduate degree"), selected = c("Some HS", "High school diploma", "Some college", "Bachelor's degree", "Associate degree", "Graduate degree"), options = list(`actions-box` = TRUE), multiple = T),
            div(style="display:inline-block", actionButton("mapFiltersReset", "Reset", style = "background-color: #4AA8F7")),
            div(style="display:inline-block", actionButton("mapFiltersApply", "Apply", style = "background-color: #A8F74A"))
          ),
          rightSidebarTabContent(
            title = "Display settings",
            id = "mapDisplaySettings",
            icon = "gears",
            checkboxInput("showCriteria", 
                          label = "Show points that don't match criteria?", 
                          value = T),
            radioButtons("colorCriteria",
                         label = "Color points by:",
                         choices = c("Selected criteria",
                                     "Sentence 1 ratings",
                                     "Sentence 2 ratings",
                                     "Sentence 3 ratings")),
            actionButton("displaySettingsApply", "Apply",
                         style = "background-color: #A8F74A")
          )
        )
      })
    }else if(req(input$sidebarItemExpanded) == "socialVariables"){
      message("Social variables view has been selected.")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
      output$rightSidebar <- renderUI({
        rightSidebarTabContent(
          id = "socialVariablesFilters",
          title = "Filters for social variables view",
          p("Some filters relevant to the social variables view")
        )
      })
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)