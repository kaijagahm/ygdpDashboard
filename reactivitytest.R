# Prototyping a Shiny app for ygdp data
source("libraries.R")

#Import data
load(here("data", "s11.Rda"))
s11 <- s11 %>%
  mutate(SentenceText = as.character(SentenceText),
         Construction = as.character(Construction))

source("scripts/app_functions.R")

#UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  sidebarLayout(
    # sidebar panel for inputs
    sidebarPanel(
      h1("YGDP Survey 11"),
      # Select a sentence
      p("Select a grammatical construction to explore:"),
      selectInput("construction", label = NULL, choices = unique(s11$Construction)),
      p("Select an example sentence:"),
      selectInput("sentence", label = NULL, choices = NULL),
      actionButton("updatemap", "Update map")
    ),
    
    # main panel for plots etc.
    mainPanel(
      # Map
      h4("Map of sentence judgments"),
      leafletOutput(outputId = "mymap"), 
      absolutePanel(top = "30%", right = "2%",
                    id="controls",
                    class = "panel panel-default",
                    width = 110,
                    draggable = TRUE, 
                    div(htmlDependency("font-awesome", "5.13.0", "www/shared/fontawesome", package = "shiny", 
                                       stylesheet = c("css/all.min.css", "css/v4-shims.min.css")), # help idk what this does
                        checkboxGroupInput(inputId = "ratings",
                                           label = "Rating", # name of the checkbox panel
                                           choiceNames = htmlchoicenames, # created in app_functions.R
                                           choiceValues = 5:1, selected = 5:1) # underlying values, and start with all selected
                    ))
    )
  )
))

server <- shinyServer(function(input, output, session) {
  # Data frame for the map: subset by sentence and rating limits
  
  eventReactive(input$updatemap, {
    
  })
  
  
  
  
  constructiondf <- reactive({s11 %>%
      filter(Construction == input$construction)
  })
  
  observeEvent(constructiondf(), {
    choices <- unique(constructiondf()$SentenceText)
    updateSelectInput(session, "sentence", choices = choices)
  })
  
  sentencedf <- reactive({
    req(input$construction) # only proceed with this operation if there is a construction selected
    constructiondf() %>% filter(SentenceText == input$sentence)
  })
  
  sentencedf <- reactive({
    s11 %>% filter(SentenceText == input$sentence)
  })
  
  # Filter min/max ratings
  mapdf <- reactive({sentencedf() %>% 
      filter(SentenceText == input$sentence,
             as.character(Judgment) %in% input$ratings) %>%
      mutate(label = paste(Gender, Age, Education, Income, Race, sep = "<br/>"))
  })
  
  # Define a color palette
  pal <- colorFactor(
    palette = color_palette, # defined in app_functions.R
    domain = factor(s11$Judgment)
  )
  
  output$mymap <- renderLeaflet({
    leaflet(mapdf()) %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = mapdf(), lat = ~Latitude, lng = ~ Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5)
    
  })
})

#generic line that launches the app
shinyApp(ui = ui, server = server)
