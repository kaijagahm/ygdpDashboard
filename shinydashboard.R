# Shinydashboard test
# Load data
library(here)
library(shiny)
library(dplyr)
library(stringr)
load("data/points/snl.Rda")
library(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

# Load the functions and libraries
source("dashboardFunctions.R")

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
  # Initialize sentence trackers
  nSentences <- reactiveVal(1)
  activeSentences <- reactiveVal(1)
  selectorIDs <- reactive({paste0("sentence", activeSentences())}) # inputId's of the active sentence selectors
  chosenSentences <- reactive({ # list of sentences the user has chosen.
    reactiveValuesToList(input)[selectorIDs()]
  })
  
  # RATINGS DATA ------------------------------------------------------------
  # Subset the sentences list to include only the chosen sentences.
  observeEvent(input$sentencesApply|input$pointFiltersApply|input$sentencesReset|input$pointFiltersReset, {
    if(!is.null(chosenSentences())){
      chosenData <- reactive({
        surveyData()[unique(unlist(chosenSentences()))] %>%
          lapply(., as.data.frame) %>%
          bind_rows(.id = NULL)
      })
    }

    filteredData <- reactive({
      req(chosenData())
      chosenData() %>%
        {if(input$ageTabs == "range") filter(., (is.na(age)) | (age > input$ageSlider[1] & age < input$ageSlider[2])) else .} %>%
        {if(input$ageTabs == "bins") filter(., (is.na(ageBin)) | (ageBin %in% input$ageButtons)) else .} %>%
        filter((is.na(gender)) | (gender %in% input$gender)) %>%
        filter((is.na(raceCats)) | (raceCats %in% input$race)) %>%
        filter((is.na(education)) | (education %in% input$education)) %>%
        {if(input$ageNAs == F) filter(., !is.na(age)) else .} %>% # since ageBin is derived from age, we don't need a separate test for which age filter tab is selected.
        {if(input$genderNAs == F) filter(., !is.na(gender)) else .} %>%
        {if(input$raceNAs == F) filter(., !is.na(raceCats)) else .} %>%
        {if(input$educationNAs == F) filter(., !is.na(education)) else .}
    })
    print(dim(filteredData()))
  }, ignoreInit = TRUE)
  
  ## Data for sentence options (varies depending on which survey is selected)
  surveyData <- reactiveVal(snl[[1]]) # starts with the data from the first survey
  observeEvent(input$survey, { # when survey input changes, change data
    name <- paste0("S", input$survey) # paste on an S to create the name
    surveyData(snl[[name]]) # update to new survey data
  }, ignoreInit = T)
  
  
  # UPDATE SENTENCE CHOICES -------------------------------------------------
  observeEvent(surveyData(), { 
    # Update choices for sentence 1
    updateSelectizeInput(session,
                         "sentence1",
                         label = "Sentence 1:",
                         choices = getSentenceChoices(surveyData()),
                         selected = getSentenceChoices(surveyData())[[1]][[1]])
    
    # Update choices for all other sentences
    lapply(activeSentences(), function(x){
      updateSelectizeInput(session,
                           paste0("sentence", x),
                           choices = getSentenceChoices(surveyData()),
                           selected = getSentenceChoices(surveyData())[[1]][[1]])
    })
  })

  
  # ADD SENTENCE ------------------------------------------------------------
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
    #print(paste("nsentences = ", nSentences()))
    print(activeSentences())
    
    # Update the color criteria choices
    updateSelectInput(session, 
                      inputId = "colorCriteriaPoints", 
                      choices = c("Selected criteria", 
                                  paste0("Sentence ", activeSentences(), " ratings"), 
                                  "Mean rating", 
                                  "Median rating", 
                                  "Min rating", 
                                  "Max rating"))
  })
  

  # RESET FILTERS -----------------------------------------------------------
  observeEvent(input$pointFiltersReset, {
    ## Reset age filters
    updateSliderInput(session, "ageSlider", min = 18, max = 100, value = c(18, 100))
    updateCheckboxGroupButtons(session, "ageButtons", choices = ageBinLevels, selected = ageBinLevels)
    updateCheckboxInput(session, "ageNAs", value = TRUE)
    
    ## Reset race filters
    updatePickerInput(session, "race", selected = raceLevels)
    updateCheckboxInput(session, "raceNAs", value = TRUE)
    
    ## Reset gender filters
    updatePickerInput(session, "gender", selected = genderLevels)
    updateCheckboxInput(session, "genderNAs", value = TRUE)
    
    ## Reset education filters
    updatePickerInput(session, "education", selected = educationLevels)
    updateCheckboxInput(session, "educationNAs", value = TRUE)
    
  }, ignoreInit = T)

  
  # RESET SENTENCES ------------------------------------------------------------
  observeEvent(input$sentencesReset, {
    # Reset sentence counters
    activeSentences(1)
    nSentences(1)
    
    # Reset survey selection
    updateSelectInput(session,
                      "survey",
                      selected = str_replace(names(snl), "^S", "")[1])
    
    # Reset sentence 1 controls to defaults
    updateSelectizeInput(session, "sentence1", 
                         selected = getSentenceChoices(surveyData())[[1]][[1]]) # restore default sentence
    updateCheckboxGroupButtons(session, "ratingsSentence1", 
                               selected = ratingChoiceValues) #restore default values (all)
    
    # Remove additional sentence controls
    removeUI(
      selector = "div[id*='Controls']", # "Controls", not "controls", to keep sentence1controls
      multiple = T # remove all, not just the first one.
    )
    
    print(activeSentences())
  }, ignoreInit = T)
  
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
            checkboxInput("showCriteriaPoints", 
                          label = "Show points that don't match criteria?", 
                          value = T),
            selectInput("colorCriteriaPoints",
                        label = "Color points by:",
                        choices = c("Selected criteria", "Sentence 1 ratings"),
                        multiple = F),
            actionButton("displaySettingsApplyPoints", "Apply",
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
          checkboxInput("showCriteriaInterp", 
                        label = "Show points that don't match criteria?", 
                        value = T),
          radioButtons("colorCriteriaInterp",
                       label = "Color points by:",
                       choices = c("Selected criteria", "Sentence 1 ratings")),
          actionButton("displaySettingsApplyInterp", "Apply",
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
shinyApp(ui = ui, server = server, options = list(display.mode = 'showcase'))
