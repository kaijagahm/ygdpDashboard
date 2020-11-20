# Shinydashboard test
# Load data
library(here)
library(shiny)
library(dplyr)
library(stringr)
library(leaflet)
library(leaflet.extras)
load("data/points/snl.Rda")
load("data/interpolations/interpListLarge.Rda")
# load("data/interpolations/interpListMedium.Rda")
# load("data/interpolations/interpListSmall.Rda")
# library(reactlog)

# tell shiny to log all reactivity
# reactlog_enable()

# Load the functions and libraries
source("dashboardFunctions.R")

# Load the different parts of the UI, which I've separated out into separate scripts to make them cleaner
source("header.R")
source("leftSidebar.R")
source("rightSidebar.R")
source("footer.R")


# To do
# Jitter lat/long points if they're nearby (can do this in pre-processing)
# Add additional label information to the points that don't meet the criteria.
# Fix bugs:
#   Sometimes, but not always, when you select survey 9 or survey 7 there's an error.
#   Sometimes there's an error when the filters are set a certain way, such as moving the age slider up to ~90-100.
#   I suspect this has something to do with the data having 0 rows.

ui <- tagList(dashboardPagePlus(
  useShinyjs(),
  
  ## Dashboard header (defined in header.R)
  header = HEADER,
  
  ## Left sidebar and menu options (defined in leftSidebar.R)
  sidebar = LEFTSIDEBAR,
  
  ## Body (defined in body.R)
  body = dashboardBody(
    shinyDashboardThemes( # why is theme defined in body, not at top?
      theme = "grey_dark"
    ),
    tabItems( # different outputs to be shown depending on which menu item is selected in the lefthand menu
      tabItem(tabName = "pointMaps",
              leafletOutput("pointMap", height = "525px"),
              br(),
              uiOutput("pointMapResetZoom")
      ),
      tabItem(tabName = "interpolationMaps",
              p("STUFF")
      ),
      tabItem(tabName = "socialVariables",
              p("[Insert charts here]")
      )
    )
  ),
  
  ## Right sidebar (defined in rightSidebar.R)
  rightsidebar = RIGHTSIDEBAR
),

## Footer (defined in footer.R)
FOOTER

)

server <- function(input, output, session){

# POINTS MODE (PTS) -------------------------------------------------------
  # (PTS) sentence counters -------------------------------------------------
  nSentences <- reactiveVal(1) # start with 1 sentence
  activeSentences <- reactiveVal(1) # only sentence 1 active initially
  chosenSentences <- reactive({ # list of sentences the user has chosen.
    reactiveValuesToList(input)[paste0("sentence", activeSentences())]
  })
  
  # (PTS) Survey data -------------------------------------------------------------
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
  
  
  # (PTS) leftRV ------------------------------------------------------------------
  # reactiveValues object to store selected sentences and selected ratings (from the left panel)
  ## initial values
  leftRV <- reactiveValues(chosenSentences = defaultSentence1,
                           chosenRatings = list(ratingsSentence1 = c("1", "2", "3", "4", "5")))
  
  observeEvent(input$sentencesApply, {
    leftRV$chosenSentences <- reactiveValuesToList(input)[paste0("sentence", activeSentences())] %>% 
      unlist() # this is a VECTOR
    leftRV$chosenRatings <- reactiveValuesToList(input)[paste0("ratingsSentence", activeSentences())] # this is a LIST
  }, ignoreInit = T)
  
  
  # (PTS) rightRV -----------------------------------------------------------------
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
  
  
  # (PTS) Data --------------------------------------------------------------------
  # The data to use for plotting is a reactive expression that depends on surveyData(), leftRV, and rightRV.
  dat <- reactive({
    surveyData()[leftRV$chosenSentences] %>%
      lapply(., as.data.frame) %>%
      map2(., 1:length(.), ~mutate(..1, whichSentence = paste0("sentence", ..2))) %>%
      # Filter by ratings for each sentence
      map2(., leftRV$chosenRatings, ~filter(..1, rating %in% as.numeric(..2))) %>%
      bind_rows(.id = NULL) %>% # now we have a single df, filtered by ratings.
      mutate(rating = as.numeric(as.character(rating))) %>%
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
  
  # The data that *doesn't* meet the criteria: all the responseID's except for the ones included in dat() (this is just a shortcut so I don't have to re-write all these filters backwards.)
  tad <- reactive({
    req(dat())
    surveyData()[leftRV$chosenSentences] %>%
      lapply(., as.data.frame) %>%
      bind_rows(.id = NULL) %>%
      filter(!(responseID %in% dat()$responseID)) %>% # people who were excluded from dat()
      select(responseID, lat, long, label) %>%
      distinct() %>%
      mutate(lat = as.numeric(lat),
             long = as.numeric(long))
  })
  
  # Wide format data (for mapping and coloring)
  ## Calculated values: to be joined to wide format data
  calc <- reactive({ 
    dat() %>%
      select(responseID, whichSentence, rating) %>%
      group_by(responseID) %>%
      summarize(mn = mean(rating, na.rm = T),
                md = median(rating, na.rm = T),
                min = min(rating, na.rm = T),
                max = max(rating, na.rm = T))
  })
  
  ## Wide format data
  wideDat <- reactive({
    dat() %>%
      select(responseID, whichSentence, rating, lat, long, label) %>%
      pivot_wider(id_cols = c(responseID, lat, long, label), 
                  names_from = whichSentence, values_from = rating) %>%
      left_join(calc(), by = "responseID") %>% # join calc
      mutate(lat = as.numeric(lat),
             long = as.numeric(long),
             meetsCriteria = 5) %>% # so the color choices will work
      {if(nSentences() > 1) mutate(., label = paste0(label, " <br> <b>Mean: </b> ", round(mn, 2), " <br> <b>Median: </b> ", md, " <br> <b>Min: </b> ", min, " <br> <b>Max: </b> ", max))else .} %>%
      upgradeLabels(.)
  })
  
  observe({ # whenever dat() changes, print its dimensions.
    print(paste0("Data dimensions: ", paste(dim(dat()), collapse = ", ")))
  })
  
  # (PTS) Map ---------------------------------------------------------------
  output$pointMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4)
  })
  
  
  # (PTS) map zoom ----------------------------------------------------------
  ## Define reset button
  output$pointMapResetZoom <- renderUI({
    div(style="display:inline-block", 
        actionButton("resetPointMapZoom", "Reset map view", style = "background-color: #4AA8F7"))
  })
  
  # (PTS) Reset buttons -----------------------------------------------------------
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
  
  
  # (PTS) Update sentence choices -------------------------------------------------
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
  
  
  # (PTS) Add a sentence ----------------------------------------------------------
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
        #    prettyJoinSelector(sentenceNum = as.numeric(id))), # opted not to include join
        #div(style = "display:inline-block", actionBttn(inputId = paste0("trash", id),
        #icon = icon("trash"), # individual sentence trash buttons: not implemented
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
  
  
  # (PTS) Update color criteria choices -------------------------------------------
  observeEvent(input$sentencesApply|input$sentencesReset, {
    if(nSentences() == 1){
      updateSelectInput(session, "colorCriteriaPoints",
                        choices = c("Sentence 1 ratings", "Selected criteria"))
    }else{
      updateSelectInput(session, "colorCriteriaPoints",
                        choices = c(paste0("Sentence ", activeSentences(), " ratings"),
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating",
                                    "Selected criteria"))
    }
  })


  # RIGHT MENU BAR CONTROLS -------------------------------------------------
  observeEvent(input$sidebarItemExpanded, {
    if(req(input$sidebarItemExpanded) == "pointMaps"){
      message("Point maps view has been selected.")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open") #open the "control sidebar" (righthand sidebar) when the menu tab is selected
      output$rightSidebar <- renderUI({
        rightSidebar(
          ### (PTS) Demographic filters
          rightSidebarTabContent(
            title = "Filter data",
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
          ### (PTS) Display settings
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
            div(style="display:inline-block", 
                actionButton("pointDisplaySettingsApply", "Apply", 
                             style = "background-color: #A8F74A")),
            style = 'margin-top: -2em'
          )
        )
      })
    }else if(req(input$sidebarItemExpanded) == "interpolationMaps"){
      message("Interpolation map view has been selected.")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
      output$rightSidebar <- renderUI({
        # Interpolation mode display settings
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
        # Social variables display settings
        rightSidebarTabContent(
          id = "socialVariablesFilters",
          title = "Filters for social variables view",
          p("Some filters relevant to the social variables view"),
          style = 'margin-top: -2em'
        )
      })
    }
  })
  

  
  ## When reset button is clicked, reset the map zoom
  observe({
    input$resetPointMapZoom
    leafletProxy("pointMap") %>% 
      setView(-96, 37.8, 4)
  })
  
  # Translate input$colorCriteriaPoints into the names of the columns in wideDat()
  colorCol <- reactiveVal("sentence1") # initial value is sentence1
  observeEvent(input$colorCriteriaPoints, { # reassign the value based on the input
    if(grepl("ratings", input$colorCriteriaPoints)){
      colorCol(input$colorCriteriaPoints %>% 
                 str_replace(., "ratings", "") %>% 
                 tolower() %>% 
                 str_replace_all(., " ", ""))
    }else if(input$colorCriteriaPoints == "Selected criteria"){
      colorCol("meetsCriteria")
    }else if(input$colorCriteriaPoints == "Mean rating"){
      colorCol("mn")
    }else if(input$colorCriteriaPoints == "Median rating"){
      colorCol("md")
    }else if(input$colorCriteriaPoints == "Min rating"){
      colorCol("min")
    }else if(input$colorCriteriaPoints == "Max rating"){
      colorCol("max")
    }
  })
  
  # Plot points
  observeEvent(wideDat(), {
    # if(!is.null(input$pointFiltersApply)){
    #   browser()
    # }
    if(is.null(input$showCriteriaPoints)){
      leafletProxy("pointMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5,
                         radius = 7, opacity = 1,
                         fillOpacity = 0.8) %>%
        addCircleMarkers(data = tad(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = "black",
                         color = "black",
                         weight = 0.5,
                         radius = 2, opacity = 1,
                         fillOpacity = 1)
    }else if(!is.null(input$showCriteriaPoints) & input$showCriteriaPoints == T){
      leafletProxy("pointMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5,
                         radius = 7, opacity = 1,
                         fillOpacity = 0.8) %>%
        addCircleMarkers(data = tad(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = "black",
                         color = "black",
                         weight = 0.5,
                         radius = 2, opacity = 1,
                         fillOpacity = 1)
    }else if(!is.null(input$showCriteriaPoints) & input$showCriteriaPoints == F){
      leafletProxy("pointMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5,
                         radius = 7, opacity = 1,
                         fillOpacity = 0.8)
    }
  })
  
  # observe({
  #   if(input$survey == "7"){
  #     browser()
  #   }
  # })
  
  # Change point colors
  observeEvent(input$pointDisplaySettingsApply, {
    req(wideDat()) # wideDat() must already exist
    req(tad()) # tad() must already exist
    if(input$showCriteriaPoints == T){
      leafletProxy("pointMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5,
                         radius = 7, opacity = 1,
                         fillOpacity = 0.8) %>%
        addCircleMarkers(data = tad(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = "black",
                         color = "black",
                         weight = 0.5,
                         radius = 2, opacity = 1,
                         fillOpacity = 1)
    }else{
      leafletProxy("pointMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5,
                         radius = 7, opacity = 1,
                         fillOpacity = 0.8)
    }
  }, ignoreInit = T)

  # observe({
  #   if(length(unique(dat()$sentenceID)) > 1){
  #     browser()
  #   }
  # })
  # 
  
}


# Run the app
shinyApp(ui = ui, server = server, options = list(display.mode = 'showcase'))
