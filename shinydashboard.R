# Intro ------------------------------------------------------------------
# Welcome! This is the main script to generate the ygdpDashboard Shiny app. It calls a couple of other scripts that define static parts of the app (left sidebar, etc.), but this is where the bulk of the code is located.
# Author: Kaija Gahm
# Contributor: Ian Neidel
# Created: June - December 2020
# Github repository: https://github.com/kaijagahm/ygdpDashboard
# Issue tracker: https://github.com/kaijagahm/ygdpDashboard/issues

# Some notes on the code --------------------------------------------------
## - I use consistent acronyms in this script to help organize the code. (PTS) refers to "points mode" (i.e. the tab of the app that shows a map with points on it). (INT) refers to "interpolation mode". (HT) refers to the "How to use" tab. (AB) refers to the "About" tab. #XXX add (AB) to the "about" sections!

## - "XXX" is a marker I use when there is something in the code I want to come back to and fix. It's easy to find with Command + F in RStudio. 

## - Divider bars separate the sections of this code. RStudio automatically recognizes those as section breaks and uses them to create a very useful **code outline**. You can access it by clicking on the "Show document outline" button (a bunch of stacked horizontal lines) at the top righthand corner of the script panel in RStudio, or with the keyboard shortcut Shift + Command + O (on a Mac). This outline is where the (PTS), (INT), etc. abbreviations really come in handy.

## - Common formatting abbreviations: `br()` inserts a line break. `div()` creates an html div, which can encompass other Shiny or html elements. `hr()` creates a horizontal rule/line to separate sections. `a()` creates a hyperlink. `p()` creates a paragraph of text.

## - the 'ignoreInit = T' argument is used a lot in `observeEvent()` calls. This argument means that when the observer is first created, its handler expression (i.e. the code that says what to DO after observing the condition) does not run, regardless of whether the triggering condition is true. Prevents weird side effects and unnecessary slowdown on app load.

## - There are some places in the app where I took hacky shortcuts that made things work but that might trip you up if you try to modify seemingly-basic things about the app. I apologize in advance! I was learning this as I went. I've marked places that I think fall into this category with # AAA.

# Load required packages (libraries) --------------------------------------
library(here) # for writing filepaths relative to the root app directory
library(shiny) # for... building a Shiny app :)
library(dplyr) # for data manipulation with %>%
library(stringr) # for string manipulation, find and replace, etc.
library(leaflet) # for making dynamic and interactive maps
library(sf) # for shapefile manipulation, e.g. st_drop_geometry()
library(reactlog) # for creating a reactive graph. More information here: https://rstudio.github.io/reactlog/
source("dashboardFunctions.R") # custom-written functions and object definitions for use later in the app.

# Load data for the app ---------------------------------------------------
## (PTS)
load("data/points/snl.Rda") # snl stands for "sentences nested list". This is the data for the point map mode, organized in a nested list (split first by surveys, then by sentences)

## (INT)
# This is the data for interpolation mode. It's a list, with one interpolation (an sf object) per sentence.
# # AAA I deal with it in three steps:
## 1. Load the list, where each list element is an sf object (and therefore they all have the same hexagon geometries, which is redundant)
load("data/interpolations/interpListMedium.Rda")

## 2. Grab an initial grid that's just one element of this list.
mediumGrid <- interpListMedium[[defaultSentence1]] # This is the "initial" interpolation grid, shown when the app loads.

## 3. Drop the geometry column from all of the list elements, leaving only the predicted values for each sentence.
interpListMedium <- lapply(interpListMedium, st_drop_geometry)

# This is a reference table to be used in defining the menu choices for interpolation mode.
## For points mode, we had a nested list, split first by surveys and then by sentences. For interpolation mode, for efficiency, I have a flat list, so this table cross-references surveys and sentences to allow sentence choices to display according to which survey is selected.
load("data/interpolations/surveySentencesTable.Rda")

# Use the reactlog --------------------------------------------------------
# Enable the reactlog if you want to visualize reactivity. If you do this, run the app and then in the console you can run shiny::reactlogShow() to pull up the reactlog. You have to start a new R session each time unless you also want to see past uses of the app. More on how to use the reactlog here: https://rstudio.github.io/reactlog/articles/reactlog.html
#reactlog_enable() # un-comment this line to actually use the reactlog.

# Load separate UI scripts ------------------------------------------------
# I've separated out a few parts of the UI into separate scripts.
# Note that this only works for code that doesn't depend on reactive values. I *should* have used Shiny Modules (https://shiny.rstudio.com/articles/modules.html), but I was intimidated, so I didn't.
source("header.R") # the dashboard header (title etc)
source("leftSidebar.R") # the initial left sidebar def (before updating selectInputs etc)
source("rightSidebar.R") # the initial right sidebar def (before updating selectInputs etc)
source("howToAboutContent.R") # text and images for the 'how to use' tab (HT)

# UI function -------------------------------------------------------------
ui <- function(request){ # Defined this as a function so that URL bookmarking would work. It still doesn't. Alas. See issue #33.
  tagList(dashboardPagePlus(
    tags$head(
      tags$style(
        # Removes dark space at the top of the right sidebar
        # code from https://stackoverflow.com/questions/59289622/remove-the-dark-space-at-the-top-of-the-right-sidebar-in-a-shinydashboardplus
        HTML( 
          ".control-sidebar-tabs {display:none;}"
        )
      )
    ),
    
    useShinyjs(), # enables use of javascript in the code.
    
    
    # HEADER ------------------------------------------------------------------
    header = HEADER, # HEADER is defined in header.R.
    
    
    # LEFT SIDEBAR ------------------------------------------------------------
    sidebar = LEFTSIDEBAR, # LEFTSIDEBAR is defined in leftSidebar.R
    
    
    # BODY --------------------------------------------------------------------
    body = dashboardBody(
      # Keep right sidebar open by removing the icon to close it
      # Code from: https://stackoverflow.com/questions/63837262/is-it-possible-to-fix-the-left-and-right-sidebars-in-shinydashboardplus-to-perma/
      tags$script(HTML( 
        '$("body > div > header > nav > div:nth-child(4) > ul > li > a").hide();
         document.getElementsByClassName("sidebar-toggle")[0].style.visibility = "hidden";'
      )),
      
      # Define the theme for the dashboard
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      
      # Body tabs ---------------------------------------------------------------
      # A different body tab (i.e. different content) is shown depending on which menu item is selected in the left sidebar
      # The hidden menu items are a hacky solution to fix a problem where tabs were not hiding/showing correctly. I documented the problem and solution in more detail here: https://community.rstudio.com/t/sidebaritemexpanded-only-works-when-tab-is-closed-not-when-switching-tabs/89246
      tabItems(
        # (PTS)
        tabItem(tabName = "hiddenPointMaps",
                leafletOutput("pointMap", height = "525px"), # the actual map! Generated in server as `output$pointMap` with `renderLeaflet()`
                br(),
                uiOutput("pointMapResetZoom") # button to reset map zoom
        ),
        # (INT)
        tabItem(tabName = "hiddenInterpolationMaps",
                leafletOutput("interpolationMap", height = "525px"), # the interp map! Generated in server as `output$interpolationMap` with `renderLeaflet()`
                br(),
                uiOutput("interpolationMapResetZoom") # button to reset map zoom
        ),
        # (HT)
        tabItem(tabName = "hiddenHowTo",
                br(),
                tabBox(width = 12, # a content box with tabs
                       height = 12,
                       # id lets us use input$howToBox in server to get current tab
                       id = "howToBox",
                       ## (HT PTS)
                       howToPoints, # defined in howToAboutContent.R
                       ## (HT INT)
                       howToInterpolation # defined in howToAboutContent.R
                )
        ),
        # (AB)
        tabItem(tabName = "hiddenAbout",
                tabBox( # a content box with tabs
                  width = 12,
                  height = 12,
                  id = "aboutBox",
                  ## About the YGDP
                  aboutYGDP, # defined in howToAboutContent.R
                  aboutSurveys # defined in howToAboutContent.R
                )
        )
      )
    ),
    
    
    # RIGHT SIDEBAR -----------------------------------------------------------
    rightsidebar = rightSidebar(
      background = "dark", # to match body style
      title = "Right Sidebar",
      # Tabset for the right sidebar: switch between PTS/INT
      tabsetPanel(id = "rightSidebarTabset",
                  type = "hidden", # Switch programmatically; hide tabs
                  # (PTS)
                  tabPanel(title = "Point map controls",
                           # Panel containing tabs for filters and display settings
                           tabsetPanel(id = "pointMapTabset",
                                       type = "tabs",
                                       tabPanel(title = "Filter",
                                                # Widgets defined in rightSidebar.R
                                                ageWidget,
                                                raceWidget,
                                                genderWidget,
                                                educationWidget,
                                                # The divs here put the buttons next to each other
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
                                                            choices = c("Sentence 1 ratings", "Selected criteria"), # this will be updated for more sentences in the server
                                                            multiple = F),
                                                div(style = "display:inline-block",
                                                    actionButton("pointDisplaySettingsApply", "Apply",
                                                                 style = "background-color: #A8F74A")))
                           )
                  ),
                  # (INT)
                  tabPanel(title = "Interpolation map controls",
                           # Panel containing just a tab for display settings
                           ## Kept as a tabsetPanel for consistency of code and because I was afaid to break something. # AAA
                           tabsetPanel(id = "interpolationMapTabset",
                                       type = "tabs",
                                       tabPanel(
                                         title = "Display settings",
                                         br(),
                                         selectInput("colorCriteriaInterpolation",
                                                     label = "Show:",
                                                     choices = c("Sentence 1 ratings"), # updated for more sentences in server
                                                     multiple = F),
                                         div(style = "display:inline-block",
                                             actionButton("interpolationDisplaySettingsApply", "Apply",
                                                          style = "background-color: #A8F74A"))
                                       )
                           )
                  ),
                  # (HT)
                  tabPanel(title = "How to controls",
                           # Again, this maybe doesn't need to be a tabsetPanel since there's only one tab, but I was afraid of breaking something.
                           tabsetPanel(id = "howToTabset",
                                       type = "tabs",
                                       tabPanel(
                                         title = "Credits",
                                         br(),
                                         p("This app was created by Kaija Gahm for the YGDP in Fall 2020, with help from Ian Niedel."),
                                         hr(), # lines to separate sections
                                         a(href = "https://github.com/kaijagahm/ygdpDashboard", 
                                           "App source code"), # hyperlink
                                         hr(),
                                         # hyperlink within paragraph
                                         p("Survey data collected by Jim Wood, Raffaella Zanuttini, and the rest of the Yale Grammatical Diversity Project", a(href = "(ygdp.yale.edu)", "ygdp.yale.edu"))
                                       )
                           )
                  ),
                  tabPanel(title = "About controls",
                           # See above notes about maybe not needing tabsetPanel.
                           tabsetPanel(id = "aboutTabset",
                                       type = "tabs",
                                       tabPanel(
                                         title = "Resources",
                                         br(),
                                         a(href = "https://ling.auf.net/lingbuzz/005277", "Mapbook (2020)"),
                                         br(),
                                         a(href = "https://ygdp.yale.edu", "YGDP Website"),
                                         br(),
                                         a(href = "https://twitter.com/YaleGramDiv", "Twitter"),
                                         br(),
                                         a(href = "https://www.facebook.com/YaleGramDiv", "Facebook")
                                       )
                           )
                  )
      )
    )
  )
  
  )
} # end of UI function (again, only enclosed this in a function to make URL bookmarking work)


# Server function ---------------------------------------------------------
server <- function(input, output, session){
  # Enable URL bookmarking
  enableBookmarking(store = "url")

  # Update body tabs --------------------------------------------------------
  ## Here's where the hidden vs. non-hidden tabs thing explicitly comes into play.
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
  },
  label = "oeUpdateBodyTabs") # label for this observer, useful for debugging
  
  # Update right sidebar tabs -----------------------------------------------
  ## Same logic as I used above for the body tabs.
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
  },
  label = "oeUpdateRSTabs")

  # (HT) Images -------------------------------------------------------------
  # Render images to display in the How To Use section. Images are stored in the data/ folder of the app directory.
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
  # These reactive counters keep track of how many sentences are selected and which sentences are active.
  
  # How many sentences are active, and which numbers?
  nSentences <- reactiveVal(1) # initialize with 1 sentence
  activeSentences <- reactiveVal(1) # only sentence 1 active initially
  
  # list of sentences the user has chosen.
  chosenSentences <- reactive({
    reactiveValuesToList(input)[paste0("sentence", activeSentences())]
  },
  label = "rChosenSentences")
  
  # (PTS) Survey data -------------------------------------------------------------
  # Varies based on which survey is selected
  ## Real data, to be fed into reactive expression `dat`.
  surveyData <- reactiveVal(snl[[1]], label = "rvSurveyData") # initial survey data--start with first survey contained in snl.
  observeEvent(input$sentencesApply, { # When you click the "apply" button
    name <- paste0("S", input$survey)
    surveyData(snl[[name]]) # update to new survey data
  }, 
  ignoreInit = T,
  label = "oeUpdateSurveyData")
  
  ## Dummy mirror of real data: for creating sentence selector options
  surveySentencesDataList <- reactive({ # this is basically a replicate of surveyData(), except that `dat` doesn't depend on it. surveySentencesDataList is *only* used to generate choices to populate the sentence selectors. 
    snl[[paste0("S", input$survey)]]
  },
  label = "rSurveySentencesDataList")
  
  # (PTS) leftRV ------------------------------------------------------------------
  # Selected sentences and ratings from left sidebar get stored in a reactiveValues obj.
  ## initial values
  leftRV <- reactiveValues(chosenSentences = defaultSentence1, # defined in dashboardFunctions.R # AAA XXX
                           # initialize with all ratings selected
                           chosenRatings = list(ratingsSentence1 = c("1", "2", "3", "4", "5")))
  
  # When the apply button is clicked, update the chosen sentences and ratings
  observeEvent(input$sentencesApply, {
    leftRV$chosenSentences <- reactiveValuesToList(input)[paste0("sentence", 
                                                                 activeSentences())] %>% 
      unlist() # note: this is a VECTOR
    leftRV$chosenRatings <- reactiveValuesToList(input)[paste0("ratingsSentence", 
                                                               activeSentences())] # note: this is a LIST
  }, 
  ignoreInit = T, # ingnoreInit tells this observer not to fire on app load
  label = "oeUpdateSentencesRatings") 
  
  
  # (PTS) rightRV -----------------------------------------------------------------
  # Filter values from the right sidebar get stored in a reactiveValues obj.
  ## Initialize rightRV with default values
  rightRV <- reactiveValues(ageNAs = T,
                            ageButtons = ageBinLevels, # levels defined in dashboardFunctions.R
                            ageSlider = c(18, 100), # age slider goes from 18 to 100
                            raceNAs = T,
                            race = raceLevels,
                            genderNAs = T,
                            gender = genderLevels,
                            educationNAs = T,
                            education = educationLevels)
  
  ## values in rightRV are updated when the "apply" button is clicked in the right sidebar.
  observeEvent(input$pointFiltersApply, {
    rightRV$ageNAs <- input$ageNAs
    # We have a two-tab interface for selecting ages: either buttons or a range. 
    ## If the age slider widget ("range") is currently selected, then set the ageButtons reactive val to NULL, and set the ageSlider reactive val to the chosen values.
    if(input$ageTabs == "range"){
      rightRV$ageSlider <- as.numeric(input$ageSlider)
      rightRV$ageButtons <- NULL
    ## If the age buttons widget is currently selected, then do the opposite: set "range" to NULL etc.
    }else{
      rightRV$ageButtons <- as.character(input$ageButtons)
      rightRV$ageSlider <- NULL
    }
    rightRV$raceNAs <- input$raceNAs # this is the "include NA's?" checkbox
    rightRV$race <- input$race
    rightRV$genderNAs <- input$genderNAs
    rightRV$gender <- input$gender
    rightRV$educationNAs <- input$educationNAs
    rightRV$education <- input$education
  }, 
  ignoreInit = T, # we don't need to run this observer on load because we already define default values of rightRV above.
  label = "oeUpdateFilters") 
  
  
  # (PTS) Data --------------------------------------------------------------
  ## Unlike the reactiveValues objects above, which only update when the observer runs, dat() is a reactive. It listens to: leftRV, rightRV, and surveyData().
  ## Note the use of the `map2()` function for data filtering here. Applies elements of one vector/list to corresponding elements of another vector/list. Supremely useful.
  dat <- reactive({
    surveyData()[leftRV$chosenSentences] %>% # select the chosen sentences from the survey data
      lapply(., as.data.frame) %>% # convert each sentence to a df
      map2(., # first input: the list of df's fed in from the pipe above
           1:length(.), # second input: a vector of numbers from 1 to length(.)
           ~mutate(..1, # add a column called "whichSentence" to each df, with the corresponding number. This works because `surveyData()[leftRV$chosenSentences]` (above) subsets those sentences from the list *in order*
                   whichSentence = paste0("sentence", ..2))
           ) %>%
      # Filter by ratings for each sentence
      map2(., # first input: the list of df's fed in from the pipe above
           leftRV$chosenRatings, # second input: the chosen ratings for each sentence
           ~filter(..1, rating %in% as.numeric(..2))) %>% # filter each data frame by the ratings the user has selected *for that sentence*
      bind_rows(.id = NULL) %>% # bind list into a single df, filtered by ratings.
      mutate(rating = as.numeric(as.character(rating))) %>% # convert ratings to numeric
      
      # remove participants who don't meet criteria for all sentences (this is the `AND` stack)
      group_by(responseID) %>% 
      filter(n() == isolate(nSentences())) %>% # only keep participants who have as many rows as there are sentences, i.e. not fewer.
      ungroup() %>%
      
      #Filter by demography
      ## each {} is a conditional pipe statement.
      ## In the filtering statements, we explicitly keep the NA's. NA's are removed in the later rightRV$*NAs statements.
      {if(is.null(rightRV$ageButtons)) # if the user filtered age using the slider...
        filter(., is.na(age) | age >= rightRV$ageSlider[1] & 
                 age <= rightRV$ageSlider[2]) else .} %>%
      {if(is.null(rightRV$ageSlider))  # if the user filtered age using buttons...
        filter(., is.na(ageBin) | ageBin %in% rightRV$ageButtons) else .} %>%
      filter(is.na(gender) | gender %in% rightRV$gender) %>%
      filter(is.na(raceCats) | raceCats %in% rightRV$race) %>%
      filter(is.na(education) | education %in% rightRV$education) %>%
      
      # Remove NA's if the checkboxes are unchecked
      {if(rightRV$ageNAs == F) filter(., !is.na(age)) else .} %>% # since ageBin is derived from age, don't need a separate test here for which age filter tab is selected--all rows that are NA for age will also be NA for ageBin.
      {if(rightRV$genderNAs == F) filter(., !is.na(gender)) else .} %>%
      {if(rightRV$raceNAs == F) filter(., !is.na(raceCats)) else .} %>%
      {if(rightRV$educationNAs == F) filter(., !is.na(education)) else .}
  },
  label = "rDat")
  
  # The data that *doesn't* meet the criteria: all the responseID's except for the ones included in dat() (this is just a shortcut so I don't have to re-write all these filters backwards.)
  tad <- reactive({
    req(dat()) # dat() must already exist for this to work
    surveyData()[leftRV$chosenSentences] %>%
      lapply(., as.data.frame) %>%
      bind_rows(.id = NULL) %>%
      filter(!(responseID %in% dat()$responseID)) %>% # people who were excluded from dat()
      ## Note: this is where you could add additional filters to address GH issue #37
      select(responseID, lat, long, label) %>% # we don't need their ratings of the various sentences, just their location.
      distinct() %>% # only need one row per participant
      mutate(lat = as.numeric(lat), # make sure lat and long are numeric before plotting
             long = as.numeric(long))
  },
  label = "rTad")
  
  # Wide format data (for mapping and coloring)
  ## First we calculate stats on the selected sentence ratings for each participant
  calc <- reactive({ 
    dat() %>%
      select(responseID, whichSentence, rating) %>%
      group_by(responseID) %>%
      summarize(mn = mean(rating, na.rm = T),
                md = median(rating, na.rm = T),
                min = min(rating, na.rm = T),
                max = max(rating, na.rm = T))
  },
  label = "rCalc")
  
  ## Then we pivot dat() to wide format and combine it with calc()
  wideDat <- reactive({
    dat() %>%
      select(responseID, whichSentence, rating, lat, long, label) %>%
      pivot_wider(id_cols = c(responseID, lat, long, label), 
                  names_from = whichSentence, values_from = rating) %>%
      left_join(calc(), by = "responseID") %>% # join calc
      mutate(lat = as.numeric(lat),
             long = as.numeric(long),
             # NOTE: this is something of a shortcut. AAA
             # meetsCriteria is a TRUE/FALSE column. But because of the way I've set up the color palettes, I'm just assigning the meetsCriteria column to 5 so that the same color palette can be used, with one color standing for both 5 (on a scale of 1-5, when points are colored by the ratings for a single sentence), or "meets criteria" (when points are colored by whether they meet the criteria). 
             # A more robust way of doing this would be to follow the method I used to recolor the hexagons in interpolation mode depending on the selected value of input$colorCriteriaInterpolation.
             meetsCriteria = 5) %>%
      # Add the calculated statistics to the popup labels for each point.
      {if(nSentences() > 1) 
        mutate(., label = paste0(label, " <br> <b>Mean: </b> ", 
                                 round(mn, 2), " <br> <b>Median: </b> ", 
                                 md, " <br> <b>Min: </b> ", min, 
                                 " <br> <b>Max: </b> ", max)) else .} %>%
      upgradeLabels(.) # format the labels nicely. Function defined in dashboardFunctions.R
  },
  label = "rWideDat")
  
  # This was just a useful debugging tool; not at all necessary to the actual app.
  # observe({ # whenever dat() changes, print its dimensions.
  #   print(paste0("Data dimensions: ", paste(dim(dat()), collapse = ", ")))
  # },
  # label = "oDimDat")
  
  # (PTS) Map ---------------------------------------------------------------
  # This initializes a basic map of the US, without any points on it.
  output$pointMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # Don't allow people to zoom out beyond 4
      setView(-96, 37.8, 4) # Set initial view to the center of the continental US
  })
  
  # (PTS) Points on map -----------------------------------------------------
  # Plot points onto the map. Points get re-plotted each time wideDat() changes.
  ### Data points
  observeEvent(wideDat(), {
    if(nrow(wideDat()) > 0){ # if there are at least some points that meet the criteria...
      leafletProxy("pointMap") %>% # we use leafletProxy() to modify the existing "pointMap" object, instead of re-drawing the whole map.
        clearMarkers() %>% # remove any existing point markers
        # Note that in leaflet, markers are circles, not points; they have a border and fill by default
        addCircleMarkers(data = wideDat(), # take data from wideDat()
                         lat = ~lat,
                         lng = ~long,
                         popup = ~label, # use the 'label' column for the point popups
                         # The continuousBlueYellow palette is defined in dashboardFunctions.R
                         # eval(as.symbol()) allows us to use the character vector in the colorCol() reactiveVal as a column name within the ~ formula.
                         fillColor = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         color = ~continuousBlueYellow(eval(as.symbol(colorCol()))),
                         weight = 0.5, # width of the point border
                         radius = 8, # size of the points
                         opacity = 1, # opacity of the point border
                         fillOpacity = 0.8) %>% # opacity of the point fill
        # Add circle markers for the points that *don't* meet the criteria.
        addCircleMarkers(data = tad(), 
                         lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = "black",
                         color = "black",
                         weight = 0.5,
                         radius = 2, # make the black points really small
                         opacity = 1,
                         fillOpacity = 1,
                         # We define a "group" for these points in order to take advantage of the built-in leaflet legend functionality.
                         group = "Show points that don't meet selected criteria")
    }else{ # if all points fail to meet the criteria, then we plot only the black points.
      leafletProxy("pointMap") %>%
        clearMarkers() %>% # remove any markers
        addCircleMarkers(data = tad(), 
                         lat = ~lat, lng = ~long,
                         popup = ~label,
                         fillColor = "black",
                         color = "black",
                         weight = 0.5, # width of the border
                         radius = 2, opacity = 1,
                         fillOpacity = 1,
                         group = "Show points that don't meet selected criteria")
    }
  },
  label = "oePointMapPoints")
  
  ### Controls
  observeEvent(wideDat(), {
    # If we're coloring by "meets criteria", then we add a checkbox control only, and no color legend.
    if(colorCol() == "meetsCriteria"){
      leafletProxy("pointMap") %>%
        clearControls() %>%
        addLayersControl(
          overlayGroups = "Show points that don't meet selected criteria",
          options = layersControlOptions(collapsed = F)
          ) 
      # If we're coloring by individual sentence ratings, then we add a checkbox control and also a color legend.
    }else{
      leafletProxy("pointMap") %>%
        clearControls() %>%
        addLayersControl(
          overlayGroups = "Show points that don't meet selected criteria",
          options = layersControlOptions(collapsed = F)) %>%
        # To get the colors in the right order, we have to use a reversed color palette, continuousBlueYellowLegend, defined in dashboardFunctions.R, AND we have to transform the labels to be in decreasing order
        addLegend("bottomright", pal = continuousBlueYellowLegend, 
                  values = 1:5,
                  title = "Rating",
                  opacity = 1,
                  # transform labels to be in decreasing order
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)
                                          )
                  )
    }
  },
  label = "oePointMapControls")
  
  # XXX START HERE WITH COMMENTING/DOCUMENTING
  ## Change point colors
  ## Re-plot when input$pointDisplaySettingsApply is clicked
  observeEvent(input$pointDisplaySettingsApply, {
    req(wideDat())
    req(tad())
    ### Data points
    observeEvent(wideDat(), {
      if(nrow(wideDat()) > 0){
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
      }else{
        leafletProxy("pointMap") %>%
          clearMarkers() %>%
          addCircleMarkers(data = tad(), 
                           lat = ~lat, lng = ~long,
                           popup = ~label,
                           fillColor = "black",
                           color = "black",
                           weight = 0.5,
                           radius = 2, opacity = 1,
                           fillOpacity = 1,
                           group = "Show points that don't meet selected criteria")
      }
    },
    label = "oePointMapChangeColors")
    ### Controls
    observeEvent(wideDat(), {
      if(colorCol() == "meetsCriteria"){
        leafletProxy("pointMap") %>%
          clearControls() %>%
          addLayersControl(
            overlayGroups = "Show points that don't meet selected criteria",
            options = layersControlOptions(collapsed = F)) 
      }else{
        leafletProxy("pointMap") %>%
          clearControls() %>%
          addLayersControl(
            overlayGroups = "Show points that don't meet selected criteria",
            options = layersControlOptions(collapsed = F)) %>%
          addLegend("bottomright", pal = continuousBlueYellowLegend, values = 1:5,
                    title = "Rating",
                    opacity = 1,
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      }
    },
    label = "oePointMapUpdateControls")
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
  },
  label = "oResetPointMapZoom")
  
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
  },
  label = "oeAddSentence")
  
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
  }, 
  ignoreInit = T,
  label = "oeSentencesReset")
  
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
  },
  label = "oePointFiltersReset")
  
  ## 3. Update the age selections when you toggle between the tabs
  observeEvent(input$ageTabs, {
    if(input$ageTabs == "range"){
      updateCheckboxGroupButtons(session, "ageButtons", choices = ageBinLevels, selected = ageBinLevels)
    }else{
      updateSliderInput(session, "ageSlider", min = 18, max = 100, value = c(18, 100))
    }
  },
  label = "oeUpdateAgeFilters")
  
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
  },
  label = "oeUpdateSentenceChoices")
  
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
  }, 
  ignoreInit = T,
  label = "oeUpdateColorCriteriaPoints")
  
  # Translate input$colorCriteriaPoints into the names of the columns in wideDat()
  colorCol <- reactiveVal({"sentence1"}, # initial value is sentence1
                          label = "rvColorCol") 
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
  },
  label = "oeUpdateColorCol")
  
   ### XXX START HERE WITH THE LABELING
  
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
                               min = min(c_across(contains("sentence")), na.rm = T)
      )
        else .} %>%
      {if(ncol(.) == 3) mutate(.,
                               min = min(c_across(contains("sentence")), na.rm = T),
                               max = max(c_across(contains("sentence")), na.rm = T),
                               med = median(c_across(contains("sentence")), na.rm = T),
                               mn = mean(c_across(contains("sentence")), na.rm = T),
                               r = (sentence1.pred/5)*255, 
                               g = (sentence2.pred/5)*255, 
                               b = (sentence3.pred/5)*255,
                               rgbColor = rgb(r, g, b, maxColorValue = 255)
      )
        else .} %>%
      {if(ncol(.) >= 3) mutate(.,
                               min = min(c_across(contains("sentence")), na.rm = T),
                               max = max(c_across(contains("sentence")), na.rm = T),
                               med = median(c_across(contains("sentence")), na.rm = T),
                               mn = mean(c_across(contains("sentence")), na.rm = T)
      )
        else .} %>%
      as.data.frame() %>%
      {if(nrow(.) == nrow(mediumGrid)) bind_cols(., mediumGrid) %>% st_as_sf() else .}
  })
  
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
  # We want to update the polygon colors in each of two cases: either (1) the data updates (new sentences are chosen), or (2) the color criteria choice updates. In each case, we need to check what the colorColI() value is so that we get the color palette correct.
  ## (1) DATA CHANGES
  observeEvent(datI(), { 
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
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE)))
    }else if(colorColI() == "rgbColor"){
      leafletProxy("interpolationMap") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = datI() %>%
                      st_transform(4326),
                    weight = 1,
                    color = ~rgbColor, # color by the literal values of the rgb color column
                    fillColor = ~rgbColor,
                    fillOpacity = 1) %>%
        addLegend("bottomleft",
                  pal = redLegend, values = 1:5,
                  title = "Sentence 1",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE))) %>%
        addLegend("topright",
                  pal = greenLegend, values = 1:5,
                  title = "Sentence 2",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE))) %>%
        addLegend("bottomright",
                  pal = blueLegend, values = 1:5,
                  title = "Sentence 3",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE)))
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
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE)))
    }
  })
  
  ## (2) COLOR CRITERIA CHOICE CHANGES
  observeEvent(input$interpolationDisplaySettingsApply, {
    req(datI()) # datI() must already exist
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
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE)))
    }else if(colorColI() == "rgbColor"){
      leafletProxy("interpolationMap") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = datI() %>%
                      st_transform(4326),
                    weight = 1,
                    color = ~rgbColor, # color by the literal values of the rgb color column
                    fillColor = ~rgbColor,
                    fillOpacity = 1) %>%
        addLegend("bottomleft",
                  pal = redLegend, values = 1:5,
                  title = "Sentence 1",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE))) %>%
        addLegend("topright",
                  pal = greenLegend, values = 1:5,
                  title = "Sentence 2",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE))) %>%
        addLegend("bottomright",
                  pal = blueLegend, values = 1:5,
                  title = "Sentence 3",
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) 
                    sort(x, decreasing = TRUE)))
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
                         selected = defaultSentence1)
    
    # Remove additional sentence controls
    removeUI(
      selector = "div[id*='ControlsI']", # "ControlsI", not "controlsI", to keep sentence1controlsI
      multiple = T # remove all, not just the first one.
    )
    print(paste0("active sentences: ", activeSentencesI()))
  }, ignoreInit = T)
  
  # (INT) Update color criteria choices -------------------------------------------
  observeEvent(input$sentencesApplyI|input$sentencesResetI, { ## AAA
    val <- input$colorCriteriaInterpolation
    choices1 <- "Sentence 1 ratings"
    choices2 <- c("Sentence 1 ratings",
                  "Sentence 2 ratings",
                  "Difference (1-2)",
                  "Difference (2-1)",
                  "Mean rating",
                  "Min rating",
                  "Max rating")
    choices3 <- c("Sentence 1 ratings",
                  "Sentence 2 ratings",
                  "Sentence 3 ratings",
                  "RGB scale",
                  "Mean rating",
                  "Median rating",
                  "Min rating",
                  "Max rating")
    choicesMore <- c(paste0("Sentence ", 
                            activeSentencesI(), " ratings"),
                     "Mean rating",
                     "Median rating",
                     "Min rating",
                     "Max rating")
    
    if(nSentencesI() == 1){
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c("Sentence 1 ratings"),
                        selected = "Sentence 1 ratings"
      )
    }else if(nSentencesI() == 2){
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c("Sentence 1 ratings",
                                    "Sentence 2 ratings",
                                    "Difference (1-2)",
                                    "Difference (2-1)",
                                    "Mean rating",
                                    "Min rating",
                                    "Max rating"),
                        selected = ifelse(val %in% choices2, val, "Sentence 1 ratings"))
    }else if(nSentencesI() == 3){
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c("Sentence 1 ratings",
                                    "Sentence 2 ratings",
                                    "Sentence 3 ratings",
                                    "RGB scale",
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating"),
                        selected = ifelse(val %in% choices3, val, "Sentence 1 ratings"))
    }else{
      updateSelectInput(session, "colorCriteriaInterpolation",
                        choices = c(paste0("Sentence ", 
                                           activeSentencesI(), " ratings"),
                                    "Mean rating",
                                    "Median rating",
                                    "Min rating",
                                    "Max rating"),
                        selected = ifelse(val %in% choicesMore, val, "Sentence 1 ratings"))
    }
  })
  
  # Translate input$colorCriteriaInterpolation into the names of the columns in datI()
  colorColI <- reactiveVal("sentence1.pred") # initial value is sentence1.pred
  observeEvent(input$colorCriteriaInterpolation, { # reassign the value based on the input
    print(input$colorCriteriaInterpolation)
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
    }else if(input$colorCriteriaInterpolation == "RGB scale"){
      colorColI("rgbColor")
    }
    print(colorColI())
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
shinyApp(ui, server, enableBookmarking = "url")