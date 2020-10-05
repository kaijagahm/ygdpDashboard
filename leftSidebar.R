# Dashboard left sidebar
source("dashboardFunctions.R")

LEFTSIDEBAR <- dashboardSidebar(
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
             prettyRatingSelector(sentenceNum = 1), # defined in dashboardFunctions.R
             hr(),
             uiOutput("sentence2Controls"), # controls for the second sentence
             uiOutput("sentence3Controls"), # controls for the third sentence
             br(),
             actionButton("addSentence", "+ Add a sentence"), # button to add the second sentence
             div(style="display:inline-block", actionButton("sentencesReset", "Reset all", style = "background-color: #4AA8F7")),
             div(style="display:inline-block", actionButton("sentencesApply", "Apply", style = "background-color: #A8F74A"))
    ),
    menuItem(text = "Social variables", expandedName = "socialVariables", icon = icon("map"), startExpanded = FALSE,
             div(style = reduceSpacing,
                 selectInput("test", "Test", 
                             choices = c("a", "b", "c", "d", "e"),
                             selected = "a",
                             multiple = FALSE)))
  )
)