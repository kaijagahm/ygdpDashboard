# Libraries and data
library(shiny)
source("libraries.R")
source(here("scripts", "input_output_tabs.R"))
load(here("data", "s11.Rda"))

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
  
  # CONSTRUCTION/SENTENCE DATA ----------------------------------------------
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
  
  # INPUT TABS -------------------------------------------------------------
  observeEvent(input$whatdo, {
    updateTabsetPanel(session, "inputControls", selected = input$whatdo) # Note: we can only do the thing with "selected" here because the names of the choices in input$whatdo match the names of the panels in inputControls.
  }) 
  
  df <- reactive({ # subset the data differently depending on the chosen action
    switch(input$whatdo,
           map = constructionDat() %>% filter(SentenceText == input$sentence,
                                              as.character(Judgment) %in% input$ratings),
           plot = constructionDat() %>% filter(SentenceText == input$sentence)
           #mapBoolean = s11 %>% filter(SentenceText == input$sentence) 
    )
  })
  
  # OUTPUT TABS -------------------------------------------------------------
  ## update which tab is shown based on the selection
  observeEvent(input$whatdo,{
    updateTabsetPanel(session, "outputViews", selected = input$whatdo)
  })
  
  # Simple map output
  output$map <- renderLeaflet({
    req(input$sentence)
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 4)) %>% # no zooming out
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = df(), 
                       lat = ~Latitude, lng = ~Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5)
  })
  
  # Plot output
  ## Sort by mean?
  meansort <- reactive({
    req(input$covariate)
    ifelse(input$covariate %in% c("Gender", "Race", "RegANAE", "CarverReg"), T, F)
  })
  
  ## Get name of covariate
  covariateName <- reactive({names(demographic_vars[which(demographic_vars == input$covariate)])})
  
  ## Plot
  output$plot <- renderPlot({
    sentence_barplot(df()[!is.na(df()[,input$covariate]),], xvar = input$covariate, #exclude rows that are NA for the covariate
                     yvar = "Judgment", xlab = F, order_by_mean = meansort()) +
      theme(text = element_text(size = 17))+
      scale_color_manual(color_palette)+
      ggtitle(paste0("Sentence judgments by ", covariateName())
      )
  })
  
  # mapBoolean output
}

shinyApp(ui = ui, server = server)
