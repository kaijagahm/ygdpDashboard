# Shinydashboard test

# Load the functions and libraries
library(shiny)
source("dashboardFunctions.R")
library(here)
library(rlist)

# Load data
load(here("data", "points", "sentencesNestedList.Rda"))

# Load the different parts of the UI, which I've separated out into separate scripts to make them cleaner
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

server <- function(input, output, session){
  # Initialize sentence counters
  nSentences <- reactiveVal(1)
  activeSentences <- reactiveVal(1)
  
  ## Data for sentence options (varies depending on which survey is selected)
  surveyData <- reactiveVal(sentencesNestedList[[1]]) # starts with the data from the first survey
  observeEvent(input$survey, { # when survey input changes, change data
    name <- paste0("S", input$survey) # paste on an S to create the name
    surveyData(sentencesNestedList[[name]]) # update to new survey data
  })
  
  observeEvent(surveyData(), { 
    # update choices for the sentence 1 selector when a new survey is selected
    updateSelectizeInput(session,
                         "sentence1",
                         label = "Sentence 1:",
                         choices = getSentenceChoices(surveyData()),
                         selected = getSentenceChoices(surveyData())[[1]][[1]])
  })
  
  # Update right sidebar color criteria choices when number of sentences changes
  colorCriteriaChoices <- reactiveVal(colorCriteriaChoicesPoints[[1]]) # initialize with 1st set of choices
  # [INSERT CODE TO UPDATE COLOR CRITERIA CHOICES]
  
  
  ## Add UI for additional sentences
  ### Define function to add UI components
  addSentenceUI <- function(id, dat){
    div(id = paste0("sentence", id, "Controls"),
        div(style = reduceSpacing,
            selectizeInput(inputId = paste0("sentence", id),
                           label = paste0("Sentence ", id, ":"),
            choices = getSentenceChoices(dat),
            selected = getSentenceChoices(dat)[[1]][[1]],
            multiple = F),
    prettyRatingSelector(sentenceNum = as.numeric(id))),
    div(style = "display:inline-block",
        prettyJoinSelector(sentenceNum = as.numeric(id))), # join selector and trash in same line
    #div(style = "display:inline-block", actionBttn(inputId = paste0("trash", id),
    #icon = icon("trash"),
    #style = "minimal")),
    hr()
    )
  }

### Observer to activate the function and add UI when requested
observeEvent(input$addSentence, { # when addSentence button is clicked
  insertUI(selector = ifelse(nSentences() == 1, "#sentence1controls", 
                             paste0("#sentence", max(activeSentences()), "Controls")),
           where = "afterEnd",
           ui = addSentenceUI(id = last(activeSentences()) + 1, 
                              dat = surveyData())) # make a sentence UI with the new number
  activeSentences(c(activeSentences(), last(activeSentences()) + 1)) # update activeSentences
  nSentences(nSentences() + 1) # update nSentences
})


## Actions on reset button click
observeEvent(input$sentencesReset, {
  activeSentences(1) # reset activeSentences
  nSentences(1) # reset nSentences
  removeUI( # remove additional controls
    selector = "div[id*='Controls']",
    multiple = T
  )
  
  ### update sentence 1 controls to restore them to defaults
  updateSelectizeInput(session, "sentence1", 
                       selected = getSentenceChoices(surveyData())[[1]][[1]]) # restore default sentence
  updateCheckboxGroupButtons(session, "ratingsSentence1", 
                             selected = ratingChoiceValues) #restore default values (all)
})

## Determine what shows up in the right menu bar, and when it opens/closes
observeEvent(input$sidebarItemExpanded, {
  if(req(input$sidebarItemExpanded) == "pointMaps"){
    message("Point maps view has been selected.")
    shinyjs::addClass(selector = "body", class = "control-sidebar-open") #open the "control sidebar" (righthand sidebar) when the menu tab is selected
    output$rightSidebar <- renderUI({
      rightSidebar(
        # Point mode filters ------------------------------------------------------
        rightSidebarTabContent(
          title = "Demographic filters",
          id = "pointDemoFilters",
          active = T,
          icon = "sliders",
          ageWidget, # defined in rightSidebar.R
          raceWidget,
          genderWidget, # defined in rightSidebar.R
          educationWidget,
          div(style="display:inline-block", 
              actionButton("pointFiltersReset", "Reset", 
                           style = "background-color: #4AA8F7")),
          div(style="display:inline-block", 
              actionButton("pointFiltersApply", "Apply", 
                           style = "background-color: #A8F74A")),
          style = 'margin-top: -2em'
        ),
        # Point mode display settings ---------------------------------------------
        rightSidebarTabContent(
          title = "Display settings",
          id = "pointDisplaySettings",
          icon = "gears",
          checkboxInput("showCriteria", 
                        label = "Show points that don't match criteria?", 
                        value = T),
          selectInput("colorCriteria",
                      label = "Color points by:",
                      choices = colorCriteriaChoices(),
                      multiple = F),
          actionButton("displaySettingsApply", "Apply",
                       style = "background-color: #A8F74A"),
          style = 'margin-top: -2em'
        )
      )
    })
  }else if(req(input$sidebarItemExpanded) == "interpolationMaps"){
    message("Interpolation map view has been selected.")
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    output$rightSidebar <- renderUI({
      # Interpolation mode display settings -------------------------------------
      rightSidebarTabContent(
        title = "Display settings",
        id = "interpolationDisplaySettings",
        icon = "gears",
        checkboxInput("showCriteria", 
                      label = "Show points that don't match criteria?", 
                      value = T),
        radioButtons("colorCriteria",
                     label = "Color points by:",
                     choices = colorCriteriaChoices()),
        actionButton("displaySettingsApply", "Apply",
                     style = "background-color: #A8F74A"),
        style = 'margin-top: -2em'
      )
    })
  }else if(req(input$sidebarItemExpanded) == "socialVariables"){
    message("Social variables view has been selected.")
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    output$rightSidebar <- renderUI({
      # Social variables display settings ---------------------------------------
      rightSidebarTabContent(
        id = "socialVariablesFilters",
        title = "Filters for social variables view",
        p("Some filters relevant to the social variables view"),
        style = 'margin-top: -2em'
      )
    })
  }
})
}

# Run the app
shinyApp(ui = ui, server = server)
