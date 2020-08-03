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
      p("Select a grammatical construction to explore:"),
      selectInput("construction", label = NULL, choices = unique(s11$Construction)),
      p("Select an example sentence:"),
      selectInput("sentence", label = NULL, choices = unique(s11$SentenceText)),
      actionButton("updatemap", "Update map"),
      
      # Select a demographic variable
      br(),
      h4("Explore covariates (target sentence)"),
      selectInput("covariate", "Choose a demographic variable:", choices = demographic_vars, selected = demographic_vars[1]),
      actionButton("updateplot", "Update plot")
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
                    )),
      br(),
      
      # Demographic plot
      plotOutput("barplot")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  # Define a color palette
  pal <- colorFactor(
    palette = color_palette, # defined in app_functions.R
    domain = factor(s11$Judgment)
  )
  
  # make a construction df
  constructiondf <- reactive({s11 %>%
      filter(Construction == input$construction)
  })
  
  # when the construction df exists, generate new choices for the sentence menu
  observeEvent(constructiondf(), {
    choices <- unique(constructiondf()$SentenceText)
    updateSelectInput(session, "sentence", choices = choices)
  })
  
  # Define initial data for the map to load
  initial_data <- isolate(s11 %>% filter(SentenceText == input$sentence))
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = initial_data, 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5)
    
  })
  
  observeEvent(input$updatemap, { # when you click the "update map" button
    # create an updated data frame
    mapdf <- reactive({s11 %>% filter(SentenceText == input$sentence,
                                      as.character(Judgment) %in% input$ratings)})
    
    # update the map with new points
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addCircleMarkers(data = mapdf(), 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5)
  })
  
  # Sort by mean for only some variables
  meansort <- reactive(ifelse(input$covariate %in% c("Gender", "Race", "RegANAE", "CarverReg"), T, F))
  
  # Make a reactive expression for the selected demographic covariate name
  covariateTitleName <- reactive(names(demographic_vars[which(demographic_vars == input$covariate)]))
  covariateTitle <- reactive({ifelse(covariateTitleName() %in% c("Carver dialect region", "ANAE dialect region"), 
                                     paste0("Sentence judgmentes by ", covariateTitleName), 
                                     paste0("Sentence judgments by ", tolower(as.character(covariateTitleName))))})
  
  observeEvent(input$updateplot, { # when you click the "update plot" button
    # create an updated data frame
    plotdf <- reactive({s11 %>% filter(SentenceText == input$sentence)})
  
  # Barplot of the demographic variable chosen
  output$barplot <- renderPlot({
    sentence_barplot(plotdf(), xvar = input$covariate, yvar = "Judgment", 
                     show_legend = F, xlab = T, order_by_mean = meansort()) +
      theme(text = element_text(size = 17))+
      scale_color_manual(color_palette)+
      ggtitle(covariateTitle())
  })
  })
  
})

#generic line that launches the app
shinyApp(ui = ui, server = server)
