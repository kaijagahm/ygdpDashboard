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
      
      # Map control panel
      conditionalPanel("input.whatdo == 'Map this sentence'",
                       div(htmlDependency("font-awesome", "5.13.0", "www/shared/fontawesome", package = "shiny", 
                                          stylesheet = c("css/all.min.css", "css/v4-shims.min.css")), # help idk what this does
                           checkboxGroupInput(inputId = "ratings",
                                              label = "Filter by rating", # name of the checkbox panel
                                              choiceNames = htmlchoicenames, # from app_functions.R
                                              choiceValues = 5:1, selected = 5:1)
                       )),
      
      # Plots control panel
      conditionalPanel("input.whatdo == 'See ratings by social categories'",
                       # Select covariate
                       selectInput("covariate", "Choose a demographic variable:", 
                                   choices = demographic_vars, selected = demographic_vars[1])
                       ),
      
      # Button to update the view
      actionButton("viewbutton", "Submit")
    ),
    
    # Main panel: outputs, graphs, etc.
    mainPanel(
      # Introductory text
      p("This dashboard allows you to explore data collected by the YGDP from 2015-2019. Through online surveys, we ask people from across the U.S. to judge sentences as acceptable/unacceptable."),
      p("We are NOT interested in what is 'proper' English. We want to study how people speak naturally and casually, and how their dialects vary according to geographic and social factors."),
      conditionalPanel("input.whatdo == 'Map this sentence'",
                       leafletOutput("map")),
      conditionalPanel("input.whatdo == 'See ratings by social categories'",
                       plotOutput("plot")),
      conditionalPanel("input.whatdo == 'Use this sentence in a boolean map view'",
                       leafletOutput("mapBoolean"))
    )
  )
))

server <- shinyServer(function(input, output, session){
# HOUSEKEEPING ------------------------------------------------------------
  # Define the color palette for the maps and graphs
  pal <- colorFactor(
    palette = color_palette, # defined in app_functions.R
    domain = factor(s11$Judgment)
  )

# DEFINE DATASET ----------------------------------------------------------
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
  initialCovariate <- isolate(input$covariate)
  
# MAP ---------------------------------------------------------------------
  # Make the initial static map
  
  output$map <- renderLeaflet({
    if(input$whatdo == "Map this sentence"){
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
    }
  })
  
  
  # On button click...
  observeEvent(input$viewbutton, {
    req(input$whatdo == "Map this sentence")
    # Define new data subset
    sentenceDat <- constructionDat() %>% 
      filter(SentenceText == input$sentence,
             as.character(Judgment) %in% input$ratings)
    
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

# DEMOGRAPHIC PLOTS -------------------------------------------------------
  # Get the covariate name
  #covariateName <- reactive({names(demographic_vars[which(demographic_vars == initialCovariate())])})
  
  observeEvent(input$viewbutton, {
    req(input$whatdo == "See ratings by social categories")

    # Sort by mean for only some variables
    meansort <- ifelse(input$covariate %in% c("Gender", "Race", "RegANAE", "CarverReg"), T, F)
    
    # Define new data subset
    sentenceDat <- constructionDat() %>% 
      filter(SentenceText == input$sentence,
             as.character(Judgment) %in% input$ratings)
    
    # Make a new plot
    output$plot <- renderPlot({
      sentence_barplot(sentenceDat, xvar = input$covariate, 
                       yvar = "Judgment", show_legend = F, xlab = T, order_by_mean = meansort) +
        theme(text = element_text(size = 17))+
        scale_color_manual(color_palette)+
        ggtitle(paste0("Sentence judgments by ", input$covariate)
        )
    })
  })
})

shinyApp(ui = ui, server = server)
                      
                      