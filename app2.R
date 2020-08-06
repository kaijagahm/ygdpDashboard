# New test app, after rethinking the layout/logic
source("libraries.R")

#Import data
load(here("data", "s11.Rda"))
s11 <- s11 %>%
  mutate(SentenceText = as.character(SentenceText),
         Construction = as.character(Construction))

source("scripts/app_functions.R")
ui <- shinyUI(fluidPage(
  # Title
  titlePanel("Exploring dialect diversity in the United States"),
  
  # Sidebar layout
  sidebarLayout(position = "right",
    
    # Sidebar panel: inputs, logic control (determines the dynamic UI of the rest of the app)
    sidebarPanel(
      h3("Test Sentences"),
      # Select a construction
      selectInput("construction", "Choose a grammatical construction:", choices = unique(s11$Construction), selected = "Come with"),
      
      # Select a sentence
      htmlOutput("sentenceSelector"), # this will be generated from the server
      
      # What would you like to do?
      h3("Ways to Explore"),
      selectInput("whatdo", "What would you like to do?", choices = c("Map this sentence", "See ratings by social categories", "Use this sentence in a boolean map view"), selected = "Map this sentence"),
      
      conditionalPanel("input.whatdo == 'Map this sentence'",
                       actionButton("updatemap", "Update map"))
    ),
    
    # Main panel: outputs, graphs, etc.
    mainPanel(
      # Introductory text
      p("This dashboard allows you to explore data collected by the YGDP from 2015-2019. Through online surveys, we ask people from across the U.S. to judge sentences as acceptable/unacceptable."),
      p("We are NOT interested in what is 'proper' English. We want to study how people speak naturally and casually, and how their dialects vary according to geographic and social factors."),
      leafletOutput("map")
    )
  )
))

server <- shinyServer(function(input, output, session){
  # Define the color palette for the maps and graphs
  pal <- colorFactor(
    palette = color_palette, # defined in app_functions.R
    domain = factor(s11$Judgment)
  )
  
  # Get the data for the current construction
  constructionDat <- reactive({s11 %>% filter(Construction == input$construction)})
  
  # Define the sentenceSelector
  output$sentenceSelector <- renderUI({
    # Define the selector for the UI
    selectInput(
      inputId = "sentence", 
      label = "Choose a sentence to explore:",
      choices = unique(constructionDat()$SentenceText),
      selected = unique(constructionDat()$SentenceText)[1])
  })
  
  # Define the initial data
  initialDat <- isolate(constructionDat() %>% filter(SentenceText == unique(constructionDat()$SentenceText)[1]))
  
  # Make the initial map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = initialDat, 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5)
    
  })
  
  
  # When user clicks "Update map"...
  observeEvent(input$updatemap, {
    # Define new data subset
    sentenceDat <- constructionDat() %>% filter(SentenceText == input$sentence)
    
    # Update the map
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = sentenceDat, 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5)
  })
  
})

shinyApp(ui = ui, server = server)
                      
                      