# Dashboard left sidebar
source("dashboardFunctions.R")
load("data/points/snl.Rda")

leftSidebarWidth <- 250

LEFTSIDEBAR <- dashboardSidebar(
  leftSidebarScroll, # CSS to make the sidebar scroll. See dashboardFunctions.R
  width = leftSidebarWidth,
  sidebarMenu(
    id = "leftSidebar", # name of the left sidebar
    
    # POINTS MODE -----------------------------------------------------------
    menuItem(expandedName = "pointMaps", text = "Point maps",  icon = icon("map"), 
             startExpanded = TRUE,
             # Title
             h4("Select sentences"),
             # Inputs
             div(style = reduceSpacing,
                 selectInput("survey", "Survey", 
                             choices = str_replace(names(snl), "^S", ""),
                             selected = str_replace(names(snl), "^S", "")[1],
                             multiple = FALSE)),
             ### Sentence selector
             div(id = "sentence1controls", div(style = reduceSpacing,
                 selectizeInput("sentence1", "Sentence 1:",
                                choices = names(snl[[1]]),
                                selected = names(snl[[1]]), 
                                multiple = F)),
             prettyRatingSelector(sentenceNum = 1), # defined in dashboardFunctions.R
             hr(),
             br()
             ), # end of div "sentence1controls"
             actionButton("addSentence", "+ Add a sentence"), # button to add the second sentence
             div(style="display:inline-block", actionButton("sentencesReset", "Reset all", style = "background-color: #4AA8F7")),
             div(style="display:inline-block", actionButton("sentencesApply", "Apply", style = "background-color: #A8F74A"))
    ),

    # INTERPOLATION MODE ------------------------------------------------------
    menuItem(text = "Interpolation maps", expandedName = "interpolationMaps", 
             icon = icon("map"),
             startExpanded = FALSE,
             # Title
             h4("Select sentences")
             ),
    
    # SOCIAL VARIABLES MODE ---------------------------------------------------
    menuItem(text = "Social variables", expandedName = "socialVariables", 
             icon = icon("map"), startExpanded = FALSE,
             div(style = reduceSpacing,
                 selectInput("test", "Test", 
                             choices = c("a", "b", "c", "d", "e"),
                             selected = "a",
                             multiple = FALSE)))
  )
)