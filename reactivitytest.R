# Prototyping a Shiny app for ygdp data
source("libraries.R")

#Import data
load(here("data", "s11.Rda"))
s11 <- s11 %>%
  mutate(SentenceText = as.character(SentenceText),
         Construction = as.character(Construction)) %>%
  mutate(label = paste(Gender, Age, Education, Income, Race, sep = "<br/>"))

source("scripts/app_functions.R")

#UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  sidebarLayout(
    # sidebar panel for inputs
    sidebarPanel(
      h1("YGDP Survey 11"),
      # Select a sentence
      p("Select an example sentence:"),
      selectInput("sentence", label = NULL, choices = unique(s11$SentenceText)),
      actionButton("updatemap", "Update map")
    ),
    
    # main panel for plots etc.
    mainPanel(
      # Map
      h4("Map of sentence judgments"),
      leafletOutput(outputId = "mymap"), 
      absolutePanel(top = "255", right = "20",
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
  # Define a color palette
  pal <- colorFactor(
    palette = color_palette, # defined in app_functions.R
    domain = factor(s11$Judgment)
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4)
    
  })
  
  observeEvent(input$updatemap, {
  leafletProxy("mymap") %>%
    clearMarkers() %>%
    addCircleMarkers(data = s11 %>% filter(SentenceText == input$sentence), 
                     lat = ~Latitude, lng = ~Longitude,
                     popup = ~label,
                     fillColor = ~rev(pal(Judgment)), 
                     color = "black",
                     weight = 0.5,
                     radius = 7, opacity = 1,
                     fillOpacity = 0.5)
  })
  
  # mapdf <- reactive({
  #   s11 %>% filter(SentenceText == input$sentence) %>%
  #     mutate(label = paste(Gender, Age, Education, Income, Race, sep = "<br/>"))
  # })
  
})

#generic line that launches the app
shinyApp(ui = ui, server = server)
