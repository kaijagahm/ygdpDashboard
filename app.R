# Libraries and data
library(shiny)
library(here)
load(here("data", "s11.Rda"))
library(htmltools)
source(here("scripts", "app_functions.R"))
source(here("scripts", "input_output_tabs.R"))
source("libraries.R")
source(here("scripts", "input_output_tabs.R"))


# Define color palette as a leaflet colorFactor palette
pal <- colorFactor(
  palette = color_palette, # defined in app_functions.R
  domain = factor(s11$Judgment)
)

palBoolean <- colorFactor(
  palette = boolean_palette,
  domain = factor(c("color1", "color2"))
)

ui <- fluidPage(
  # Title
  titlePanel("Exploring dialect variation in the United States"),
  sidebarLayout(position = "right",
                sidebarPanel(
                  h3("Test Sentences"),
                  
                  # Select a construction
                  selectInput("construction", "Choose a grammatical construction:", 
                              choices = unique(s11$Construction), 
                              selected = "Come with"),
                  
                  # Select a sentence
                  htmlOutput("sentenceSelector"), # this will be generated from the server
                  
                  h3("Ways to Explore"),
                  # input tab selector
                  selectInput("whatdo", "What would you like to do?", 
                              choices = c("Map this sentence" = "map", "See ratings by social categories" = "plot", "Use this sentence in a Boolean map view" = "mapBoolean")),
                  input_control_tabs
                ),
                mainPanel(
                  # Introductory text
                  p("This dashboard allows you to explore data collected by the YGDP from 2015-2019. Through online surveys, we ask people from across the U.S. to judge sentences as acceptable/unacceptable."),
                  p("We are NOT interested in what is 'proper' English. We want to study how people speak naturally and casually, and how their dialects vary according to geographic and social factors."),
                  output_view_tabs
                )
  )
)

server <- function(input, output, session){
  initialDat <- s11 %>% filter(Construction == initialConstruction, SentenceText == initialSentence)
  initialBoolDat <- booleanPivot(s11, initialSentence, initialSentence2, initialAllowratings1, initialAllowratings2)

  # Initial sentence data
  currentDat <- reactiveVal(s11 %>% filter(Construction == initialConstruction, SentenceText == initialSentence)) # give it an initial data frame based on initialConstruction and initialSentence (defined at the top of the script)
  
  # Initial sentence data for the boolean map view
  boolDat <- reactiveVal({
    booleanPivot(s11, initialSentence, initialSentence2, initialAllowratings1, initialAllowratings2) # see app_functions.R
  })
  
  # CONSTRUCTION/SENTENCE DATA ----------------------------------------------
  # Get the data for the current construction
  constructionDat <- reactive({s11 %>% filter(Construction == input$construction)}) # for use in the sentenceSelector
  
  # Define the sentenceSelector
  output$sentenceSelector <- renderUI({
    # Define the selector for the UI
    selectInput(
      inputId = "sentence", 
      label = "Choose a sentence to explore:",
      choices = unique(constructionDat()$SentenceText),
      selected = unique(constructionDat()$SentenceText)[1])
  })
  
  # Update currentDat based on the sentence selector, whenever a new sentence is chosen and the update button is clicked
  observeEvent(input$updatePlot, {
    currentDat(s11 %>% filter(SentenceText == input$sentence))
  })
  # Same thing for the plot, but additionally filter by rating
  observeEvent(input$updateMap, {
    currentDat(s11 %>% filter(SentenceText == input$sentence,
                              as.character(Judgment) %in% input$ratings))
  })
  #Same thing for the boolean plot, but allow both chosen sentences and filter by ratings
  observeEvent(input$updateMapBoolean, {
    boolDat(booleanPivot(s11, input$sentence, input$sentence2, input$allowratings1, input$allowratings2))
  })
  
  # Have dataset respond to checkbox input to hide/show markers
  boolMapDat <- reactive({
    if(input$showneg){
      boolDat()
    }else{
      boolDat() %>%
        filter(criteria == "color1")
    }
  })
  
  
  # INPUT TABS -------------------------------------------------------------
  observeEvent(input$whatdo, {
    updateTabsetPanel(session, "inputControls", selected = input$whatdo) # Note: we can only do the thing with "selected" here because the names of the choices in input$whatdo match the names of the panels in inputControls.
  }) 
  
  # OUTPUT TABS -------------------------------------------------------------
  ## update which tab is shown based on the selection
  observeEvent(input$whatdo,{
    updateTabsetPanel(session, "outputViews", selected = input$whatdo)
  })
  
  # SIMPLE MAP OUTPUT -------------------------------------------------------
  output$map <- renderLeaflet({ # initialize the map with initialDat as the data: static map
    leaflet(data = initialDat) %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = initialDat, 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       label = lapply(initialDat$label, htmltools::HTML),
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 6, opacity = 1,
                       fillOpacity = 0.7)
  })
  
  observe({ # observe when currentDat() changes/is invalidated (update map button is clicked). Clear the existing map markers, then add the new markers.
      # I don't love the way this makes the markers flash--would be nice if we could animate this differently.
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = currentDat(), 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       label = lapply(currentDat()$label, htmltools::HTML),
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 6, opacity = 1,
                       fillOpacity = 0.7)
  })

  # PLOT OUTPUT -------------------------------------------------------
  ## Set up the covariate as a reactiveVal
  covariate <- reactiveVal(initialCovariate)
  
  ## Update the covariate on button click
  observeEvent(input$updatePlot, {
    covariate(input$covariate)
  })
  
  ## Get name of covariate (to use in the plot title)
  covariateName <- reactive({names(demographic_vars[which(demographic_vars == covariate())])})
  
  ## Sort by mean?
  meansort <- reactive({ifelse(covariate() %in% c("Gender", "Race", "RegANAE", "CarverReg"), T, F)})
  
  ## Plot
  output$plot <- renderPlot({
    sentence_barplot(currentDat()[!is.na(currentDat()[,covariate()]),], xvar = covariate(), #exclude rows that are NA for the covariate
                     yvar = "Judgment", xlab = F, order_by_mean = meansort()) +
      theme(text = element_text(size = 17))+
      scale_color_manual(color_palette)+
      ggtitle(paste0("Sentence judgments by ", covariateName())
      )
  })
  
  # BOOLEAN MAP OUTPUT -------------------------------------------------------
  construction2Dat <- reactive({s11 %>% filter(Construction == input$construction2)}) # for use in the sentenceSelector 
  
  # Define the sentenceSelector
  output$sentence2Selector <- renderUI({
    # Define the selector for the UI
    selectInput(
      inputId = "sentence2", 
      label = "Choose a second sentence:",
      choices = unique(construction2Dat()$SentenceText),
      selected = unique(construction2Dat()$SentenceText)[2])
  })
  
  ## Make the initial map
  output$mapBoolean <- renderLeaflet({
    leaflet(data = initialBoolDat) %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = initialBoolDat,
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       label = lapply(initialBoolDat$label, htmltools::HTML),
                       fill = ~criteria,
                       fillColor = ~palBoolean(criteria),
                       color = "black",
                       weight = 0.5,
                       radius = 6, opacity = 1,
                       fillOpacity = 0.7)
  })
  
  observe({ # observe when boolDat() changes/is invalidated (update map button is clicked). Clear the existing map markers, then add the new markers.
    leafletProxy("mapBoolean") %>%
      clearMarkers() %>%
      addCircleMarkers(data = boolMapDat(),
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       label = lapply(boolMapDat()$label, htmltools::HTML),
                       fill = ~criteria,
                       fillColor = ~palBoolean(criteria),
                       color = "black",
                       weight = 0.5,
                       radius = 6, opacity = 1,
                       fillOpacity = 0.7)
  })
  
  
  # Initial legend (before sentences and/or ratings are updated)
  mapBooleanLegend <- reactiveVal({
    tags$div(
      HTML(paste(strong(tags$span(style = "color:#e6b23b", "Gold-colored points")), "represent participants who rated '", em(initialSentence), "' as 5, 4, 3, 2, or 1 ", strong("AND '"), em(initialSentence2), "' as 5 or 4.", sep = ""))
    )
  })
  
  # Update legend with sentences and ratings when button is clicked
  observeEvent(input$updateMapBoolean, {
    mapBooleanLegend(
      tags$div( # dynamically-generated text
      HTML(paste(strong(tags$span(style = "color:#e6b23b", "Gold-colored points")), 
                 "represent participants who rated '", em(input$sentence), "' as ", 
                 vecToHumanList(input$allowratings1, conjunction = "or"),
                 strong(" AND "),
                 "'", em(input$sentence2), "' as ",
                 vecToHumanList(input$allowratings2, conjunction = "or"),
                 sep = ""))
    ))
  })
  
  output$mapBooleanLegend <- renderUI({
    validate(
      need(!is.null(input$allowratings1) & !is.null(input$allowratings2), "Please select at least one rating per sentence")
    )
      mapBooleanLegend()
    })
}

shinyApp(ui = ui, server = server)

# Note: it can be useful to use isolate() to wrap calls to reactive expressions when debugging in the console.

# I think there's a better way to do the conditional points view so that the whole map doesn't flash every time you show/hide negative points...
