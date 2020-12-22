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
    menuItem(expandedName = "pointMaps", text = "Point maps", 
             tabName = "pointMaps",  icon = icon("map-marker"), 
             startExpanded = TRUE,
             class = "active",
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
                                selected = defaultSentence1, 
                                multiple = F)),
             prettyRatingSelector(sentenceNum = 1), # defined in dashboardFunctions.R
             hr(),
             br()
             ), # end of div "sentence1controls"
             actionButton("addSentence", "+ Add a sentence"), # button to add the second sentence
             div(style="display:inline-block", actionButton("sentencesReset", "Reset all", style = "background-color: #4AA8F7")),
             div(style="display:inline-block", actionButton("sentencesApply", "Apply", style = "background-color: #A8F74A"))
    ),
    hidden(menuItem("hiddenPointMaps", tabName = "hiddenPointMaps")),

    # INTERPOLATION MODE ------------------------------------------------------
    menuItem(text = "Interpolation maps", expandedName = "interpolationMaps", 
             tabName = "interpolationMaps",
             icon = icon("map"),
             startExpanded = FALSE,
             # Title
             h4("Select sentences"),
             # Inputs
             div(style = reduceSpacing,
                 selectInput("surveyI", "Survey", 
                             choices = str_replace(names(snl), "^S", ""),
                             selected = str_replace(names(snl), "^S", "")[1],
                             multiple = FALSE)),
             ### Sentence selector
             div(id = "sentence1controlsI", div(style = reduceSpacing,
                                               selectizeInput("sentence1I", "Sentence 1:",
                                                              choices = names(snl[[1]]),
                                                              selected = defaultSentence1, 
                                                              multiple = F)),
                 br(),
                 hr(),
                 br()
             ), # end of div "sentence1controlsI"
             actionButton("addSentenceI", "+ Add a sentence"), # button to add the second sentence
             div(style = "display:inline-block", actionButton("sentencesResetI", "Reset all", 
                                                            style = "background-color: #4AA8F7")),
             div(style = "display:inline-block", actionButton("sentencesApplyI", "Apply", 
                                                              style = "background-color: #A8F74A"))
             ),
    hidden(menuItem("hiddenInterpolationMaps", tabName = "hiddenInterpolationMaps")),
    
    # HOW TO MODE ---------------------------------------------------
    menuItem(text = "How to use", expandedName = "howTo", 
             tabName = "howTo",
             icon = icon("info"), startExpanded = FALSE,
             p("")),
    hidden(menuItem("hiddenHowTo", tabName = "hiddenHowTo")),
    

    # ABOUT -------------------------------------------------------------------
    menuItem(text = "About", expandedName = "about",
             tabName = "about",
             icon = icon("question"), startExpanded = FALSE,
             p("")),
    hidden(menuItem("hiddenAbout", tabName = "hiddenAbout"))
    
  )
)