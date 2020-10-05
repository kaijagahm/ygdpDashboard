# Shinydashboard test

# Load the functions and libraries
source("dashboardFunctions.R")

# Load the different parts of the UI, which I've separated out into separate scripts to make them cleaner.
source("header.R")
source("leftSidebar.R")
source("body.R")
source("rightSidebar.R")
source("footer.R")

ui <- tagList(dashboardPagePlus(
  useShinyjs(),
  
  ## Dashboard header (defined in header.R)
  header = HEADER,
  
  ## Left sidebar and menu options (defined in leftSidebar.R)
  sidebar = LEFTSIDEBAR,
  
  ## Body (defined in body.R)
  body = BODY,
  
  ## Right sidebar (defined in rightSidebar.R)
  rightsidebar = RIGHTSIDEBAR
),

## Footer (defined in footer.R)
FOOTER

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
                               choiceNames = ratingChoiceNames,
                               choiceValues = ratingChoiceValues,
                               selected = ratingChoiceValues),
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
                                   choiceNames = ratingChoiceNames,
                                   choiceValues = ratingChoiceValues,
                                   selected = ratingChoiceValues),
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