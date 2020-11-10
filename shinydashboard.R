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
source("rightSidebar.R")
source("footer.R")

ui <- tagList(dashboardPagePlus(
  useShinyjs(),
  
  ## Dashboard header (defined in header.R)
  header = HEADER,
  
  ## Left sidebar and menu options (defined in leftSidebar.R)
  sidebar = LEFTSIDEBAR,
  
  ## Body (defined in body.R)
  body = dashboardBody(
    shinyDashboardThemes( # not sure why we define the theme in the body as opposed to at the beginning of the UI, but okay.
      theme = "grey_light"
    ),
    tabItems( # different outputs to be shown depending on which menu item is selected in the lefthand menu
      tabItem(
        tabName = "pointMaps",
        p("[Insert map here]")),
      tabItem(
        tabName = "socialCharts",
        p("[Insert charts here]"))
    )
  ),
  
  ## Right sidebar (defined in rightSidebar.R)
  rightsidebar = RIGHTSIDEBAR
),

## Footer (defined in footer.R)
FOOTER

)

server <- function(input, output, session){
  # Initialize sentence trackers
  nSentences <- reactiveVal(1) # start with 1 sentence
  activeSentences <- reactiveVal(1) # only sentence 1 active initially
  selectorIDs <- reactive({paste0("sentence", activeSentences())}) # inputId's of the active sentence selectors
  chosenSentences <- reactive({ # list of sentences the user has chosen.
    reactiveValuesToList(input)[selectorIDs()]
  })
  

  # Survey data -------------------------------------------------------------
  # Varies based on which survey is selected
  ## Real data, to be fed into reactive expression `dat`.
  surveyData <- reactiveVal(snl[[1]]) # initial survey data
  observeEvent(input$sentencesApply, { # When you click the "apply" button
    name <- paste0("S", input$survey)
    surveyData(snl[[name]]) # update to new survey data
  }, ignoreInit = T)
  
  ## Dummy data for sentence selector options
  surveySentencesDataList <- reactive({ # this is basically a replicate of surveyData(), except that `dat` doesn't depend on it. surveySentencesDataList is *only* used to generate choices to populate the sentence selectors. 
    snl[[paste0("S", input$survey)]]
  })
  
  
  # leftRV ------------------------------------------------------------------
  # reactiveValues object to store selected sentences and selected ratings (from the left panel)
  leftRV <- reactiveValues(chosenSentences = names(snl[[1]])[1], # initial values
                           chosenRatings = list(ratingsSentence1 = c("1", "2", "3", "4", "5")))
  
  observeEvent(input$sentencesApply, {
    leftRV$chosenSentences <- reactiveValuesToList(input)[names(input) %in% 
                                                            paste0("sentence", activeSentences())] %>% 
      unlist() # this is a VECTOR
    leftRV$chosenRatings <- reactiveValuesToList(input)[names(input) %in% 
                                                          paste0("ratingsSentence", activeSentences())] # this is a LIST
  }, ignoreInit = T)
  

  # rightRV -----------------------------------------------------------------
  # reactiveValues object to store selected demographic filters (from the right panel)
  rightRV <- reactiveValues(ageNAs = T,
                            ageButtons = ageBinLevels,
                            ageSlider = c(18, 100),
                            raceNAs = T,
                            race = raceLevels,
                            genderNAs = T,
                            gender = genderLevels,
                            educationNAs = T,
                            education = educationLevels)
  
  observeEvent(input$pointFiltersApply, {
    rightRV$ageNAs <- input$ageNAs
    if(input$ageTabs == "range"){ # when I run this if/else bit in a browser, it keeps giving me "debug" messages. But it seems to be working in the actual app. What the heck?
      rightRV$ageSlider <- as.numeric(input$ageSlider)
      rightRV$ageButtons <- NULL
    }else{
      rightRV$ageButtons <- as.character(input$ageButtons)
      rightRV$ageSlider <- NULL
    }
    rightRV$raceNAs <- input$raceNAs
    rightRV$race <- input$race
    rightRV$genderNAs <- input$genderNAs
    rightRV$gender <- input$gender
    rightRV$educationNAs <- input$educationNAs
    rightRV$education <- input$education
  }, ignoreInit = T)
  

  # DATA --------------------------------------------------------------------
  # The data to use for plotting is a reactive expression that depends on surveyData(), leftRV, and rightRV.
  dat <- reactive({
    surveyData()[leftRV$chosenSentences] %>%
      lapply(., as.data.frame) %>%
      # Filter by ratings for each sentence
      map2(., leftRV$chosenRatings, ~filter(..1, rating %in% as.numeric(..2))) %>%
      bind_rows(.id = NULL) %>% # now we have a single df, filtered by ratings.
      group_by(responseID) %>% # remove participants who don't meet criteria for all sentences (this is the `AND` stack)
      filter(n() == isolate(nSentences())) %>%
      ungroup() %>%
      #Filter by demography
      {if(is.null(rightRV$ageButtons)) filter(., is.na(age) | age >= rightRV$ageSlider[1] &
                                                age <= rightRV$ageSlider[2]) else .} %>%
      {if(is.null(rightRV$ageSlider)) filter(., is.na(ageBin) | ageBin %in% rightRV$ageButtons) else .} %>%
      filter(is.na(gender) | gender %in% rightRV$gender) %>%
      filter(is.na(raceCats) | raceCats %in% rightRV$race) %>%
      filter(is.na(education) | education %in% rightRV$education) %>%
      {if(rightRV$ageNAs == F) filter(., !is.na(age)) else .} %>% # since ageBin is derived from age, don't need a separate test here for which tab is selected.
      {if(rightRV$genderNAs == F) filter(., !is.na(gender)) else .} %>%
      {if(rightRV$raceNAs == F) filter(., !is.na(raceCats)) else .} %>%
      {if(rightRV$educationNAs == F) filter(., !is.na(education)) else .}
  })
  
  # Wide format data (for mapping and coloring)
  ## Calculated values: to be joined to wide format data
  calc <- reactive({ 
    dat() %>%
      select(responseID, sentenceID, rating) %>%
      group_by(responseID) %>%
      summarize(mn = mean(rating, na.rm = T),
                md = median(rating, na.rm = T),
                min = min(rating, na.rm = T),
                max = max(rating, na.rm = T))
  })
  
  ## Wide format data
  wideDat <- reactive({
    dat() %>%
      select(responseID, sentenceID, rating, lat, long) %>%
      pivot_wider(id_cols = c(responseID, lat, long), names_from = sentenceID, values_from = rating, names_prefix = "SENTENCE_") %>%
      left_join(calc(), by = "responseID") # join calc
  })
  
  observe({ # whenever dat() changes, print its dimensions.
    print(paste0("Data dimensions: ", paste(dim(dat()), collapse = ", ")))
  })

  # Reset buttons -----------------------------------------------------------
  ## 1. Left reset button: remove sentence controls besides sentence 1, reset sentence 1 selection, reset sentence 1 ratings, reset survey selection, reset sentence counters. (Note that this doesn't update `dat`--you still have to click the "Apply" button for the updates to go through.)
  observeEvent(input$sentencesReset, {
    # Reset sentence counters
    activeSentences(1)
    nSentences(1)
    
    # Reset survey selection
    updateSelectInput(session, "survey",
                      selected = str_replace(names(snl), "^S", "")[1])
    
    # Reset sentence 1 controls to defaults
    updateSelectizeInput(session, "sentence1",
                         selected = getSentenceChoices(surveySentencesDataList())[[1]][[1]]) # default sentence
    updateCheckboxGroupButtons(session, "ratingsSentence1",
                               selected = as.character(ratingChoiceValues)) # restore default values (all selected)
    
    # Remove additional sentence controls
    removeUI(
      selector = "div[id*='Controls']", # "Controls", not "controls", to keep sentence1controls
      multiple = T # remove all, not just the first one.
    )
    print(paste0("active sentences: ", activeSentences()))
  }, ignoreInit = T)
  
  ## 2. Right reset button: reset values in NA checkboxes and demographic filter selectors. (Note that this doesn't update `dat`--you still have to click the "Apply" button for the updates to go through.)
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
    
    ## Message for debugging
    print("Demographic filters have been reset. Click 'Apply' again to propagate changes.")
  })
  
  ## 3. Update the age selections when you toggle between the tabs
  observeEvent(input$ageTabs, {
      if(input$ageTabs == "range"){
        updateCheckboxGroupButtons(session, "ageButtons", choices = ageBinLevels, selected = ageBinLevels)
      }else{
        updateSliderInput(session, "ageSlider", min = 18, max = 100, value = c(18, 100))
      }
  })


  # Update sentence choices -------------------------------------------------
  # This observer listens to surveySentencesDataList(), not surveyData(), since the latter is only updated when you click "Apply".
  observeEvent(surveySentencesDataList(), { 
    # Update choices for sentence 1
    updateSelectizeInput(session,
                         "sentence1",
                         label = "Sentence 1:",
                         choices = getSentenceChoices(surveySentencesDataList()),
                         selected = getSentenceChoices(surveySentencesDataList())[[1]][[1]])
    
    # Update choices for all other sentences
    lapply(activeSentences(), function(x){
      updateSelectizeInput(session,
                           paste0("sentence", x),
                           choices = getSentenceChoices(surveySentencesDataList()),
                           selected = getSentenceChoices(surveySentencesDataList())[[1]][[1]])
    })
  })

  
  # Add a sentence ----------------------------------------------------------
  ### Function definition
  addSentenceUI <- function(id, dat){
    div(id = paste0("sentence", id, "Controls"),
        div(style = reduceSpacing,
            selectizeInput(inputId = paste0("sentence", id),
                           label = paste0("Sentence ", id, ":"),
                           choices = getSentenceChoices(dat),
                           selected = getSentenceChoices(dat)[[1]][[1]],
                           multiple = F),
            prettyRatingSelector(sentenceNum = as.numeric(id))),
        #div(style = "display:inline-block",
        #    prettyJoinSelector(sentenceNum = as.numeric(id))), # join selector and trash in same line
        #div(style = "display:inline-block", actionBttn(inputId = paste0("trash", id),
        #icon = icon("trash"),
        #style = "minimal")),
        br(),
        hr()
    )
  }
  
  ### Activate function to add UI when button is clicked
  observeEvent(input$addSentence, { # when addSentence button is clicked
    insertUI(selector = ifelse(nSentences() == 1, "#sentence1controls", 
                               paste0("#sentence", max(activeSentences()), "Controls")),
             where = "afterEnd",
             ui = addSentenceUI(id = last(activeSentences()) + 1, 
                                dat = surveySentencesDataList())) # make a sentence UI with the new number
    activeSentences(c(activeSentences(), last(activeSentences()) + 1)) # update activeSentences
    nSentences(nSentences() + 1) # update nSentences
    print(activeSentences())
  })
  

  # Update color criteria choices -------------------------------------------
  observeEvent(input$sentencesApply|input$sentencesReset, {
    if(nSentences() == 1){
      updateSelectInput(session, "colorCriteriaPoints",
                        choices = c("Selected criteria", "Sentence 1 ratings"))
    }else{
      updateSelectInput(session, "colorCriteriaPoints",
                        choices = c("Selected criteria",
                                    paste0("Sentence ", activeSentences(), " ratings"),
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating"))
    }
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
