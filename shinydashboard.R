# Shinydashboard test
# Load data
library(here)
library(shiny)
library(dplyr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(sf)
load("data/points/snl.Rda")
load("data/interpolations/interpListMedium.Rda")
mediumGrid <- interpListMedium[["I'm after forgettin' the name of my favorite bakery."]]
interpListMedium <- lapply(interpListMedium, st_drop_geometry)
load("data/interpolations/surveySentencesTable.Rda")
#library(reactlog)

# tell shiny to log all reactivity
#reactlog_enable()

# Load the functions and libraries
source("dashboardFunctions.R")

# Load the different parts of the UI, which I've separated out into separate scripts to make them cleaner
source("header.R")
source("leftSidebar.R")
source("rightSidebar.R")

# To do
# Add additional label information to the points that don't meet the criteria.
# Fix bugs:
#   Sometimes, but not always, when you select survey 9 or survey 7 there's an error.
#   Sometimes there's an error when the filters are set a certain way, such as moving the age slider up to ~90-100.
#   I suspect this has something to do with the data having 0 rows.
# Confirmed--definitely has to do with the data having 0 rows. Maybe some if/else logic in the dat() and datI() definitions would help.

ui <- tagList(dashboardPagePlus(
  tags$head(
    tags$style(
      HTML(
        ".control-sidebar-tabs {display:none;}"
      )
    )
  ),
  
  useShinyjs(),
  
  
  # HEADER ------------------------------------------------------------------
  header = HEADER,
  
  
  # LEFT SIDEBAR ------------------------------------------------------------
  sidebar = LEFTSIDEBAR,
  
  
  # BODY --------------------------------------------------------------------
  body = dashboardBody(
    # remove icon to close right sidebar: keep r sidebar permanently open
    # Code from: https://stackoverflow.com/questions/63837262/is-it-possible-to-fix-the-left-and-right-sidebars-in-shinydashboardplus-to-perma/
    tags$script(HTML( 
      '$("body > div > header > nav > div:nth-child(4) > ul > li > a").hide();
         document.getElementsByClassName("sidebar-toggle")[0].style.visibility = "hidden";'
    )),
    shinyDashboardThemes( # why is theme defined in body, not at top?
      theme = "grey_dark"
    ),
    tabItems( # different outputs to be shown depending on which menu item is selected in the lefthand menu
      tabItem(tabName = "hiddenPointMaps",
              leafletOutput("pointMap", height = "525px"),
              br(),
              uiOutput("pointMapResetZoom")
      ),
      tabItem(tabName = "hiddenInterpolationMaps",
              leafletOutput("interpolationMap", height = "525px"),
              br(),
              uiOutput("interpolationMapResetZoom")
      ),
      tabItem(tabName = "hiddenHowTo",
              br(),
              tabBox(width = 12,
                     height = 12,
                     # The id lets us use input$howToBox on the server to find the current tab
                     id = "howToBox",
                     ## How to use points mode
                     tabPanel("Point maps",
                              div(style = 'overflow-y:scroll;height:500px',
                              boxPlus(title = "Left sidebar",
                                      width = 12,
                                      column(width = 4,
                                             imageOutput("PTSLeftSidebar", 
                                                         height = "50%")
                                             ),
                                      column(width = 8,
                                             div(style = "font-size:16px;",
                                                 tags$ol(
                                                   tags$li("Select a survey. The sentence choices will update dynamically."), 
                                                   br(),
                                                   tags$li("Select a sentence. Sentences are organized by grammatical phenomenon."), 
                                                   br(),
                                                   tags$li("You can use the rating buttons to restrict the data by participants' chosen ratings for the selected sentence. The default is to show all ratings."),
                                                   br(),
                                                   tags$li("To add more sentences, click 'Add a sentence.' Use the corresponding sentence and rating selectors that appear, as described in (2) and (3)."),
                                                   br(),
                                                   tags$li("To see your choices on the map, click 'Apply.'"),
                                                   br(),
                                                   tags$li("To reset the sentence and rating selections, use 'Reset all.' Note that after you reset the sentences, you will have to click 'Apply' again for your changes to be shown.")
                                                 )
                                             )
                                             ),
                                      collapsible = T,
                                      collapsed = F,
                                      closable = F),
                                  boxPlus(title = "Map",
                                      width = 12,
                                      imageOutput("PTSMap", 
                                                  height = "50%"),
                                      br(),
                                      div(
                                        style = "font-size:16px;",
                                        tags$ol(
                                          tags$li("Each point on the map represents a single survey participant."), 
                                          br(),
                                          tags$li("When several points are located in the same city, they have been jittered very slightly for visibility. You may have to zoom in very far to distinguish the points from each other. Try zooming in on New York City, for example!"), 
                                          br(),
                                          tags$li("Large, colored points match the criteria that you have chosen: selected ratings (left sidebar) and demographic criteria (right sidebar). Depending on your selection in the 'Display settings' tab (see the next section of this how-to), the points may be colored by whether or not they meet the criteria, or by participants' ratings of the sentences. If the latter, then a legend at the bottom corner of the map will key the map colors."),
                                          br(),
                                          tags$li("Small, black points do not meet the criteria that you have selected in the left and right sidebars."),
                                          br(),
                                          tags$li("You can toggle whether to display these small, black points using the checkbox at the top of the map."),
                                          br(),
                                          tags$li("Zoom in and out using the map controls or your mouse. Pan around the map by clicking and dragging."),
                                          br(),
                                          tags$li("Reset the map zoom and center focus using the reset button below the map."),
                                          br(),
                                          tags$li("Click on a point to see demographic information about that participant, as well as their ratings of each selected sentence.")
                                        )
                                      ),
                                      collapsible = T,
                                      collapsed = T,
                                      closable = F),
                              boxPlus(title = "Right sidebar",
                                      width = 12,
                                      fluidRow(
                                        column(
                                          width = 8,
                                          div(
                                            style = "font-size:16px;",
                                            tags$ol(
                                              tags$li("In the 'Filter' tab of the right sidebar, you can filter the points you'd like to display by various demographic categories."), 
                                              br(),
                                              tags$li("There are two ways to filter by age: with a continuous slider, or by categorical age bins. You can toggle between these with the 'range' and 'bins' tabs."), 
                                              br(),
                                              tags$li("For each demographic category, you can choose to exclude missing values (NA's). Some participants declined to provide demographic information, resulting in these missing values."),
                                              br(),
                                              tags$li("Within each demographic category dropdown menu, you can use the 'Select All' and 'Deselect All' options to make it easier to select multiple categories."),
                                              br(),
                                              tags$li("To update the map to reflect your selections, use the 'Apply' button."),
                                              br(),
                                              tags$li("Use the 'Reset' button to reset the demographic filters to their original values. Note that you will have to click 'Apply' again after resetting the filters to apply your changes.")
                                            )
                                          )
                                        ),
                                        column(
                                          width = 4,
                                          
                                          imageOutput("PTSRightSidebar",
                                                      height = "50%")
                                               )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(
                                          width = 8,
                                          p("here is how you use the right sidebar display settings")
                                        ),
                                        column(
                                          width = 4,
                                          imageOutput("PTSDisplaySettings",
                                                      height = "50%")
                                        )
                                      ),
                                      collapsible = T,
                                      collapsed = T,
                                      closable = F)
                              )
                     ),
                     ## How to use interpolation mode
                     tabPanel("Interpolation maps",
                              div(style = 'overflow-y:scroll;height:500px',
                                  boxPlus(title = "Left sidebar",
                                          width = 12,
                                          column(width = 4,
                                                 imageOutput("INTLeftSidebar", 
                                                             height = "50%")
                                          ),
                                          column(width = 8,
                                                 div(style = "font-size:16px;",
                                                     tags$ol(
                                                       tags$li("Select a survey. The sentence choices will update dynamically."), 
                                                       br(),
                                                       tags$li("Select a sentence. Sentences are organized by grammatical phenomenon."), 
                                                       br(),
                                                       tags$li("To add more sentences, click 'Add a sentence.' Use the corresponding sentence selector that appears, as described in (2)."),
                                                       br(),
                                                       tags$li("To see your choices on the map, click 'Apply.'"),
                                                       br(),
                                                       tags$li("To reset the sentence and rating selections, use 'Reset all.' Note that after you reset the sentences, you will have to click 'Apply' again for your changes to be shown.")
                                                     )
                                                 )
                                          ),
                                          collapsible = T,
                                          collapsed = F,
                                          closable = F),
                                  boxPlus(title = "Map",
                                          width = 12,
                                          imageOutput("INTMap", 
                                                      height = "50%"),
                                          br(),
                                          div(
                                            style = "font-size:16px;",
                                            tags$ol(
                                              tags$li("The map is divided into a grid of hexagons. Point ratings have been interpolated to create a continuous surface, and then each hexagon is colored according to the predicted interpolation value at its centroid."), ## Check whether this is averaged or just sampled
                                              br(),
                                              tags$li("The legend provides a key to the hexagon colors. The scale is from 1 to 5 if the map is set to display ratings for a single sentence or aggregate statistics for more than one sentence, or from -4 to 4 if the map is set to display the difference between the ratings for two different sentences."), 
                                              br(),
                                              tags$li("Zoom in and out using the map controls or your mouse. Pan around the map by clicking and dragging."),
                                              br(),
                                              tags$li("Reset the map zoom and center focus using the reset button below the map.")
                                            )
                                          ),
                                          collapsible = T,
                                          collapsed = T,
                                          closable = F),
                                  boxPlus(title = "Right sidebar",
                                          width = 12,
                                          fluidRow(
                                            column(
                                              width = 8,
                                              div(
                                                style = "font-size:16px;",
                                                tags$ol(
                                                  tags$li("Select a method for coloring the hexagons. If one sentence is selected, you can only color by the ratings for that sentence. If two sentences are selected, you can color by the ratings for either sentence, or by the difference between the sentences for each hexagon, or by aggregate statistics like that minimum, maximum, mean, or median. If more than two sentences are selected, the difference options go away."), 
                                                  br(),
                                                  tags$li("Use the 'Apply' button to update the map to reflect your changes.")
                                                )
                                              )
                                            ),
                                            column(
                                              width = 4,
                                              
                                              imageOutput("INTDisplaySettings",
                                                          height = "50%")
                                            )
                                          ),
                                          collapsible = T,
                                          collapsed = T,
                                          closable = F)
                              )
                              
                              )
              )
      ),
      tabItem(tabName = "hiddenAbout",
              p("Here is some information about the YGDP"))
    )
  ),
  
  
  # RIGHT SIDEBAR -----------------------------------------------------------
  rightsidebar = rightSidebar(
    background = "dark",
    title = "Right Sidebar",
    # Tabset for the right sidebar: switch between PTS/INT
    tabsetPanel(id = "rightSidebarTabset",
                type = "hidden", # we don't want to see these tabs
                # (PTS)
                tabPanel(title = "Point map controls",
                         tabsetPanel(id = "pointMapTabset",
                                     type = "tabs",
                                     tabPanel(title = "Filter",
                                              ageWidget,
                                              raceWidget,
                                              genderWidget,
                                              educationWidget,
                                              div(style = "display:inline-block", 
                                                  actionButton("pointFiltersReset", "Reset",
                                                               style = "background-color: #4AA8F7")
                                              ),
                                              div(style = "display:inline-block",
                                                  actionButton("pointFiltersApply", "Apply",
                                                               style = "background-color: #A8F74A")
                                              )
                                     ),
                                     tabPanel(title = "Display settings",
                                              br(),
                                              selectInput("colorCriteriaPoints",
                                                          label = "Color points by:",
                                                          choices = c("Sentence 1 ratings", "Selected criteria"),
                                                          multiple = F),
                                              div(style = "display:inline-block",
                                                  actionButton("pointDisplaySettingsApply", "Apply",
                                                               style = "background-color: #A8F74A")))
                         )
                ),
                # (INT)
                tabPanel(title = "Interpolation map controls",
                         tabsetPanel(id = "interpolationMapTabset",
                                     type = "tabs",
                                     tabPanel(
                                       title = "Display settings",
                                       br(),
                                       selectInput("colorCriteriaInterpolation",
                                                   label = "Show:",
                                                   choices = c("Sentence 1 ratings"),
                                                   multiple = F),
                                       div(style = "display:inline-block",
                                           actionButton("interpolationDisplaySettingsApply", "Apply",
                                                        style = "background-color: #A8F74A"))
                                     )
                         )
                ),
                # (HT)
                tabPanel(title = "How to controls",
                         tabsetPanel(id = "howToTabset",
                                     type = "tabs",
                                     tabPanel(
                                       title = "Credits",
                                       br(),
                                       p("This app was created by Kaija Gahm for the YGDP in Fall 2020, with help from Ian Niedel."),
                                       hr(),
                                       p("Code at https://github.com/kaijagahm/
                                         ygdpDashboard."),
                                       hr(),
                                       p("Survey data collected by Jim Wood, Raffaella Zanuttini, and the rest of the Yale Grammatical Diversity Project (ygdp.yale.edu)")
                                     ))),
                tabPanel(title = "About controls",
                         tabsetPanel(id = "aboutTabset",
                                     type = "tabs",
                                     tabPanel(
                                       title = "Resources",
                                       br(),
                                       p("LINK TO MAPBOOK"),
                                       p("LINK TO YGDP WEBSITE"),
                                       p("LINK TO TWITTER")
                                     )))
    )
  )
)

)

server <- function(input, output, session){
  # Update selected menu item -----------------------------------------------
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "pointMaps"){
      updateTabItems(session, "leftSidebar", selected = "hiddenPointMaps")
    }else if(input$sidebarItemExpanded == "interpolationMaps"){
      updateTabItems(session, "leftSidebar", selected = "hiddenInterpolationMaps")
    }else if(input$sidebarItemExpanded == "howTo"){
      updateTabItems(session, "leftSidebar", selected = "hiddenHowTo")
    }else if(input$sidebarItemExpanded == "about"){
      updateTabItems(session, "leftSidebar", selected = "hiddenAbout")
    }
  })
  
  # Update right sidebar tabs -----------------------------------------------
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "pointMaps"){
      updateTabsetPanel(session, "rightSidebarTabset", 
                        selected = "Point map controls")
    }else if(input$sidebarItemExpanded == "interpolationMaps"){
      updateTabsetPanel(session, "rightSidebarTabset",
                        selected = "Interpolation map controls")
    }else if(input$sidebarItemExpanded == "howTo"){
      updateTabsetPanel(session, "rightSidebarTabset",
                        selected = "How to controls")
    }else if(input$sidebarItemExpanded == "about"){
      updateTabsetPanel(session, "rightSidebarTabset",
                        selected = "About controls")
    }
  })
  
  # (HT) Banner images ------------------------------------------------------
  output$PTSBanner <- renderImage({
    return(list(src = "data/howTo/PTSBanner.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  output$INTBanner <- renderImage({
    return(list(src = "data/howTo/INTBanner.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  
  # (HT) Other images -------------------------------------------------------
  ## (PTS)
  ### Left sidebar
  output$PTSLeftSidebar <- renderImage({
    return(list(src = "data/howTo/PTSLeftSidebar.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  ### Map
  output$PTSMap <- renderImage({
    return(list(src = "data/howTo/PTSMap.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  ### Right sidebar (filters)
  output$PTSRightSidebar <- renderImage({
    return(list(src = "data/howTo/PTSRightSidebar.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  ### Right sidebar (display settings)
  output$PTSDisplaySettings <- renderImage({
    return(list(src = "data/howTo/PTSDisplaySettings.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  ## (INT)
  ### Left sidebar
  output$INTLeftSidebar <- renderImage({
    return(list(src = "data/howTo/INTLeftSidebar.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  ### Map
  output$INTMap <- renderImage({
    return(list(src = "data/howTo/INTMap.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  ### Right sidebar (display settings)
  output$INTDisplaySettings <- renderImage({
    return(list(src = "data/howTo/INTDisplaySettings.png",
                contentType = "image/png",
                width = "100%"))
  }, deleteFile = FALSE)
  
  
  
  
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
  # observe({
  #   browser()
  #   })
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
  
  
  # (PTS) Points on map -----------------------------------------------------
  # Plot points
  ## Re-plot when wideDat() changes
  ### Data points
  observeEvent(wideDat(), {
    leafletProxy("pointMap") %>%
      clearMarkers() %>%
      addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                       popup = ~label,
                       fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                       color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                       weight = 0.5,
                       radius = 8,
                       opacity = 1,
                       fillOpacity = 0.8) %>%
      addCircleMarkers(data = tad(), 
                       lat = ~lat, lng = ~long,
                       popup = ~label,
                       fillColor = "black",
                       color = "black",
                       weight = 0.5,
                       radius = 2, opacity = 1,
                       fillOpacity = 1,
                       group = "Show points that don't meet selected criteria")
  })
  ### Controls
  observeEvent(wideDat(), {
    if(colorCol() == "meetsCriteria"){
      leafletProxy("pointMap") %>%
        clearControls() %>%
        addLayersControl(
          overlayGroups = "Does not meet criteria",
          options = layersControlOptions(collapsed = F)) 
    }else{
      leafletProxy("pointMap") %>%
        clearControls() %>%
        addLayersControl(
          overlayGroups = "Does not meet criteria",
          options = layersControlOptions(collapsed = F)) %>%
        addLegend("bottomright", pal = continuousBlueYellowLegend, values = 1:5,
                  title = "Rating",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
  })
  
  ## Change point colors
  ## Re-plot when input$pointDisplaySettingsApply is clicked
  observeEvent(input$pointDisplaySettingsApply, {
    req(wideDat())
    req(tad())
    ### Data points
    observeEvent(wideDat(), {
      leafletProxy("pointMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = wideDat(), lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5,
                         radius = 8,
                         opacity = 1,
                         fillOpacity = 0.8) %>%
        addCircleMarkers(data = tad(), 
                         lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = "black",
                         color = "black",
                         weight = 0.5,
                         radius = 2, opacity = 1,
                         fillOpacity = 1,
                         group = "Does not meet criteria")
    })
    ### Controls
    observeEvent(wideDat(), {
      if(colorCol() == "meetsCriteria"){
        leafletProxy("pointMap") %>%
          clearControls() %>%
          addLayersControl(
            overlayGroups = "Does not meet criteria",
            options = layersControlOptions(collapsed = F)) 
      }else{
        leafletProxy("pointMap") %>%
          clearControls() %>%
          addLayersControl(
            overlayGroups = "Does not meet criteria",
            options = layersControlOptions(collapsed = F)) %>%
          addLegend("bottomright", pal = continuousBlueYellowLegend, values = 1:5,
                    title = "Rating",
                    opacity = 1,
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      }
    })
  })
  
  
  # (PTS) map zoom ----------------------------------------------------------
  ## Define reset button
  output$pointMapResetZoom <- renderUI({
    div(style="display:inline-block", 
        actionButton("resetPointMapZoom", "Reset map zoom", style = "background-color: #4AA8F7"))
  })
  
  ## When reset button is clicked, reset the map zoom
  observe({
    input$resetPointMapZoom
    leafletProxy("pointMap") %>% 
      setView(-96, 37.8, 4)
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
  
  # (PTS) Update color criteria choices -------------------------------------------
  observeEvent(input$sentencesApply|input$sentencesReset, {
    val <- input$colorCriteriaPoints
    choices1 <- c("Sentence 1 ratings", "Selected criteria")
    
    if(nSentences() == 1){
      updateSelectInput(session, "colorCriteriaPoints",
                        choices = c("Sentence 1 ratings", "Selected criteria"),
                        selected = ifelse(val %in% choices1, val, "Sentence 1 ratings"))
    }else{
      updateSelectInput(session, "colorCriteriaPoints",
                        choices = c(paste0("Sentence ", activeSentences(), " ratings"),
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating",
                                    "Selected criteria"),
                        selected = val)
    }
  }, ignoreInit = T)
  
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
  
  # INTERPOLATION MODE ------------------------------------------------------
  # (INT) sentence counters -------------------------------------------------
  nSentencesI <- reactiveVal(1) # start with 1 sentence
  activeSentencesI <- reactiveVal(1) # only sentence 1 active initially
  chosenSentencesI <- reactive({ # list of sentences the user has chosen.
    reactiveValuesToList(input)[paste0("sentence", activeSentencesI(), "I")]
  }) # selectors take form of "sentence1I"
  
  # (INT) Survey data -------------------------------------------------------------
  # Varies based on which survey is selected
  ## Real data, to be fed into reactive expression `datI`.
  surveyDataI <- reactiveVal(interpListMedium[names(interpListMedium) %in% surveySentencesTable$sentenceText[surveySentencesTable$surveyID == paste0("S", str_replace(names(snl), "^S", "")[1])]]) # initial interp list for the chosen survey
  observeEvent(input$sentencesApplyI, { # When you click the "apply" button
    name <- paste0("S", input$surveyI)
    surveyDataI(interpListMedium[names(interpListMedium) %in% surveySentencesTable$sentenceText[surveySentencesTable$surveyID == name]]) # update to new survey data
  }, 
  ignoreInit = T)
  
  ## Dummy data for sentence selector options
  surveySentencesDataListI <- reactive({ # this is basically a replicate of surveyDataI(), except that `datI` doesn't depend on it. surveySentencesDataListI is *only* used to generate choices to populate the sentence selectors. 
    interpListMedium[names(interpListMedium) %in%
                       surveySentencesTable$sentenceText[surveySentencesTable$surveyID == 
                                                           paste0("S", input$surveyI)]]
  })
  
  # (INT) leftRVI ------------------------------------------------------------------
  # reactiveValues object to store selected sentences (from left panel)
  ## initial values
  leftRVI <- reactiveValues(chosenSentences = defaultSentence1)
  
  observeEvent(input$sentencesApplyI, {
    leftRVI$chosenSentences <- reactiveValuesToList(input)[paste0("sentence", activeSentencesI(), "I")] %>% # e.g. "sentence1I"
      unlist() # this is a VECTOR
  }, ignoreInit = T)
  
  # Could maybe take a different approach to the data aggregation.
  # (INT) Data --------------------------------------------------------------
  datI <- reactive({
    surveyDataI()[leftRVI$chosenSentences] %>%
      bind_cols() %>%
      setNames(paste0("sentence", 1:ncol(.), ".pred")) %>%
      rowwise() %>%
      {if(ncol(.) == 2) mutate(.,
                               diff12 = sentence1.pred - sentence2.pred,
                               diff21 = sentence2.pred - sentence1.pred,
                               mn = mean(c_across(contains("sentence")), na.rm = T),
                               max = max(c_across(contains("sentence")), na.rm = T),
                               min = min(c_across(contains("sentence")), na.rm = T))
        else .} %>%
      {if(ncol(.) >= 3) mutate(.,
                               min = min(c_across(contains("sentence")), na.rm = T),
                               max = max(c_across(contains("sentence")), na.rm = T),
                               med = median(c_across(contains("sentence")), na.rm = T),
                               mn = mean(c_across(contains("sentence")), na.rm = T))
        else .} %>%
      as.data.frame() %>%
      {if(nrow(.) == nrow(mediumGrid)) bind_cols(., mediumGrid) %>% st_as_sf() else .}
  })
  # observeEvent(surveyDataI(), {
  #   browser()
  # }, ignoreInit = T)
  
  # (INT) Map ---------------------------------------------------------------
  output$interpolationMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addPolygons(data = mediumGrid %>%
                    st_transform(4326),
                  weight = 1,
                  color = ~continuousBlueYellow(pred),
                  fillColor = ~continuousBlueYellow(pred),
                  fillOpacity = 1,
                  opacity = 1) %>%
      addLegend("bottomright", pal = continuousBlueYellowLegend, values = 1:5,
                title = "Rating",
                opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
  })
  
  # (INT) Polygons on map ---------------------------------------------------
  observeEvent(datI(), {
    if(nrow(datI()) == nrow(mediumGrid) & "sentence1.pred" %in% names(datI())){
      leafletProxy("interpolationMap") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = datI() %>%
                      st_transform(4326),
                    weight = 1,
                    color = ~continuousBlueYellow(sentence1.pred),
                    fillColor =~continuousBlueYellow(sentence1.pred),
                    fillOpacity = 1,
                    opacity = 1) %>%
        addLegend("bottomright", pal = continuousBlueYellowLegend, values = 1:5,
                  title = "Rating",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
  })
  
  # Change polygon colors
  observeEvent(input$interpolationDisplaySettingsApply, {
    req(datI()) # wideDat() must already exist
    req(colorColI())
    if(colorColI() %in% c("diff21", "diff12")){
      leafletProxy("interpolationMap") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = datI() %>%
                      st_transform(4326),
                    weight = 1,
                    color = ~continuous44(eval(as.symbol(colorColI()))),
                    fillColor = ~continuous44(eval(as.symbol(colorColI()))),
                    fillOpacity = 1,
                    opacity = 1) %>%
        addLegend("bottomright", pal = continuous44Legend, values = -4:4,
                  title = "Difference",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }else{
      leafletProxy("interpolationMap") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = datI() %>%
                      st_transform(4326),
                    weight = 1,
                    color = ~continuousBlueYellow(eval(as.symbol(colorColI()))),
                    fillColor =~continuousBlueYellow(eval(as.symbol(colorColI()))),
                    fillOpacity = 1,
                    opacity = 1) %>%
        addLegend("bottomright", pal = continuousBlueYellowLegend, values = 1:5,
                  title = "Rating",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
  }, ignoreInit = T)
  
  # (INT) map zoom ----------------------------------------------------------
  ## Define reset button
  output$interpolationMapResetZoom <- renderUI({
    div(style="display:inline-block", 
        actionButton("resetInterpolationMapZoom", 
                     "Reset map view", style = "background-color: #4AA8F7"))
  })
  
  ## When reset button is clicked, reset the map zoom
  observe({
    input$resetInterpolationMapZoom
    leafletProxy("interpolationMap") %>% 
      setView(-96, 37.8, 4)
  })
  
  # (INT) Add a sentence ----------------------------------------------------------
  ### Function definition
  addSentenceUII <- function(id, inputList, surveyIDString, surveySentencesTable){
    div(id = paste0("sentence", id, "Controls", "I"),
        div(style = reduceSpacing,
            selectizeInput(inputId = paste0("sentence", id, "I"),
                           label = paste0("Sentence ", id, ":"),
                           choices = getSentenceChoicesI(inputList,
                                                         surveyIDString,
                                                         surveySentencesTable),
                           selected = getSentenceChoicesI(inputList, 
                                                          surveyIDString,
                                                          surveySentencesTable)[[1]][[1]],
                           multiple = F)),
        br(),
        hr()
    )
  }
  
  ### Activate function to add UI when button is clicked
  observeEvent(input$addSentenceI, { # when addSentenceI button is clicked
    insertUI(selector = ifelse(nSentencesI() == 1, "#sentence1controlsI", 
                               paste0("#sentence", max(activeSentencesI()), "Controls", "I")),
             where = "afterEnd",
             ui = addSentenceUII(id = last(activeSentencesI()) + 1, 
                                 inputList = surveySentencesDataListI(), 
                                 surveyIDString = paste0("S", input$surveyI),
                                 surveySentencesTable = surveySentencesTable)) # make a sentence UI with the new number
    activeSentencesI(c(activeSentencesI(), last(activeSentencesI()) + 1)) # update activeSentencesI
    nSentencesI(nSentencesI() + 1) # update nSentencesI
    print(paste("Number of active sentences:", nSentencesI()))
  })
  
  # (INT) Reset button -----------------------------------------------------------
  ## Left reset button: remove sentence controls besides sentence 1, reset sentence 1 selection, reset survey selection, reset sentence counters.
  ## No right reset button for interpolation mode.
  observeEvent(input$sentencesResetI, {
    # Reset sentence counters
    activeSentencesI(1)
    nSentencesI(1)
    
    # Reset survey selection
    updateSelectInput(session, "surveyI",
                      selected = str_replace(names(snl), "^S", "")[1]) # reset to default
    
    # Reset sentence 1 controls to defaults
    updateSelectizeInput(session, "sentence1I",
                         selected = defaultSentence1) # default sentence
    
    # Remove additional sentence controls
    removeUI(
      selector = "div[id*='ControlsI']", # "ControlsI", not "controlsI", to keep sentence1controlsI
      multiple = T # remove all, not just the first one.
    )
    print(paste0("active sentences: ", activeSentencesI()))
  }, ignoreInit = T)
  
  # (INT) Update color criteria choices -------------------------------------------
  observeEvent(input$sentencesApplyI|input$sentencesResetI, {
    if(nSentencesI() == 1){
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c("Sentence 1 ratings"))
    }else if(nSentencesI() == 2){
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c("Sentence 1 ratings",
                                    "Sentence 2 ratings",
                                    "Difference (1-2)",
                                    "Difference (2-1)",
                                    "Mean rating",
                                    "Min rating",
                                    "Max rating"))
    }else if(nSentencesI() == 3){
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c("Sentence 1 ratings",
                                    "Sentence 2 ratings",
                                    "Sentence 3 ratings",
                                    #"RGB scale",
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating"))
    }else{
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c(paste0("Sentence ", 
                                           activeSentencesI(), " ratings"),
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating"))
    }
  })
  
  # Translate input$colorCriteriaInterpolation into the names of the columns in datI()
  colorColI <- reactiveVal("sentence1.pred") # initial value is sentence1.pred
  observeEvent(input$colorCriteriaInterpolation, { # reassign the value based on the input
    if(grepl("ratings", input$colorCriteriaInterpolation)){
      colorColI(input$colorCriteriaInterpolation %>% 
                  str_replace(., "ratings", "") %>% 
                  tolower() %>% 
                  str_replace_all(., " ", "") %>%
                  paste0(., ".pred"))
    }else if(input$colorCriteriaInterpolation == "Difference (1-2)"){
      colorColI("diff12")
    }else if(input$colorCriteriaInterpolation == "Difference (2-1)"){
      colorColI("diff21")
    }else if(input$colorCriteriaInterpolation == "Mean rating"){
      colorColI("mn")
    }else if(input$colorCriteriaInterpolation == "Median rating"){
      colorColI("med")
    }else if(input$colorCriteriaInterpolation == "Min rating"){
      colorColI("min")
    }else if(input$colorCriteriaInterpolation == "Max rating"){
      colorColI("max")
    }
  })
  
  
  
  
  # (INT) Update sentence choices -------------------------------------------------
  # This observer listens to surveySentencesDataListI(), not surveyDataI(), since the latter is only updated when you click "Apply".
  observeEvent(surveySentencesDataListI(), { 
    # Update choices for sentence 1
    updateSelectizeInput(session,
                         "sentence1I",
                         label = "Sentence 1:",
                         choices = getSentenceChoicesI(surveySentencesDataListI(), 
                                                       paste0("S", input$surveyI), 
                                                       surveySentencesTable),
                         selected = getSentenceChoicesI(surveySentencesDataListI(), 
                                                        paste0("S", input$surveyI), 
                                                        surveySentencesTable)[[1]][[1]])
    
    # Update choices for all other sentences
    lapply(activeSentencesI(), function(x){
      updateSelectizeInput(session,
                           paste0("sentence", x, "I"),
                           choices = getSentenceChoicesI(surveySentencesDataListI(), 
                                                         paste0("S", input$surveyI), 
                                                         surveySentencesTable),
                           selected = getSentenceChoicesI(surveySentencesDataListI(), 
                                                          paste0("S", input$surveyI), 
                                                          surveySentencesTable)[[1]][[1]])
    })
  })
  
  # Open the right sidebar
  shinyjs::addClass(selector = "body", class = "control-sidebar-open")
}


# Run the app
shinyApp(ui = ui, server = server)