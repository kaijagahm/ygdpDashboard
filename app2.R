# Libraries and data
library(shiny)
library(here)
source("libraries.R")
load(here("data", "s11.Rda"))
source(here("scripts", "app_functions.R"))
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
  # Initial sentence data
  currentDat <- reactiveVal(s11 %>% filter(Construction == initialConstruction, SentenceText == initialSentence)) # give it an initial data frame based on initialConstruction and initialSentence (defined at the top of the script)
  
  # Initial sentence data for the boolean map view
  boolDat <- reactiveVal({
    booleanPivot(s11, initialSentence, initialSentence2, c(1:5), c(1:5)) # see app_functions.R
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
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = currentDat(), 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                      # label = HTML(paste(df()$Gender, df()$Age, breaks = "<br>")), # super stuck on the multiline labels
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
  
  ## Make the map
  output$mapBoolean <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = boolDat(), 
                       lat = ~Latitude, lng = ~Longitude,
                       #popup = ~label,
                       # label = HTML(paste(df()$Gender, df()$Age, breaks = "<br>")), # super stuck on the multiline labels
                       fill = ~criteria,
                       fillColor = ~palBoolean(criteria), 
                       color = "black",
                       weight = 0.5,
                       radius = 6, opacity = 1,
                       fillOpacity = 0.7)
  })
  
  
  
}

shinyApp(ui = ui, server = server)

# Note: it can be useful to use isolate() to wrap calls to reactive expressions when debugging in the console.