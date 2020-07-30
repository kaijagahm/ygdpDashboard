# Prototyping a Shiny app for ygdp data
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(boot)
library(htmltools)
source("addLegend_decreasing.R")
source("app_functions.R")

#Import data
load("data/s11.Rda")
s11 <- s11 %>%
  mutate(SentenceText = as.character(SentenceText),
         Construction = as.character(Construction))
sentences <- s11 %>% pull(SentenceText) %>% unique()
demographic_vars <- c("Age group" = "AgeBin", "Gender", "Education", "Income bracket" = "Income", "Race", "ANAE dialect region" = "RegANAE", "Carver dialect region" = "CarverReg")

ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("YGDP Data: Survey 11"),
  
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    # sidebar panel for inputs
    sidebarPanel(
      
      # Select a sentence
      selectInput("construction", "Grammatical construction", choices = unique(s11$Construction)),
      selectInput("sentence", "Sentence", choices = sentences),
      
      # Specify map settings
      h4("Map settings"),
      
      ## Range of ratings to show on the map
      sliderInput("minmaxratings", "Choose min/max ratings to map:", min = 1, max = 5, step = 1, value = c(1, 5)),
      
      # Select a demographic variable
      h4("Explore covariates (target sentence)"),
      selectInput("covariate", "Choose a demographic variable:", choices = demographic_vars, selected = demographic_vars[1])
    ),
    
    # main panel for plots etc.
    mainPanel(
      # Map
      h4("Map of sentence judgments"),
      leafletOutput(outputId = "mymap"), 
      br(),
      
      # Demographic plot
      plotOutput("barplot")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  # Data frame for the map: subset by sentence and rating limits

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
  
  ## Min/max ratings
  mintarget <- reactive(input$minmaxratings[1])
  maxtarget <- reactive(input$minmaxratings[2])
  
  # Filter min/max ratings
  mapdf <- reactive({sentencedf() %>% 
      filter(SentenceText == input$sentence,
             Judgment >= mintarget(),
             Judgment <= maxtarget()) %>%
      mutate(label = paste(Gender, Age, Education, Income, Race, sep = "<br/>"))
    })
  
  # Define a color palette
  pal <- colorFactor(
    palette = color_palette, # defined in app_functions.R
    domain = factor(s11$Judgment)
  )
  
  output$mymap <- renderLeaflet({
    leaflet(mapdf()) %>% 
      addTiles(options = providerTileOptions(minZoom = 4)) %>% # can't zoom out, only in.
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(data = mapdf(), lat = ~Latitude, lng = ~ Longitude,
                       popup = ~label,
                       fillColor = ~rev(pal(Judgment)), 
                       color = "black",
                       weight = 0.5,
                       radius = 7, opacity = 1,
                       fillOpacity = 0.5) %>%
      addLegend_decreasing("bottomright", pal = pal, values = c("5", "4", "3", "2", "1"),
                title = "Sentence rating",
                opacity = 1,
                decreasing = T)

  })
  
  # Sort by mean for only some variables
  meansort <- reactive(ifelse(input$covariate %in% c("Gender", "Race", "RegANAE", "CarverReg"), T, F))
  
  # Make a reactive expression for the selected demographic covariate name
  covariateTitleName <- reactive(names(demographic_vars[which(demographic_vars == input$covariate)]))

  # Barplot of the demographic variable chosen
  output$barplot <- renderPlot({
      sentence_barplot(df = sentencedf(), xvar = input$covariate, yvar = "Judgment", show_legend = F, xlab = T, order_by_mean = meansort()) +
      theme(text = element_text(size = 17))+
      scale_color_manual(color_palette)+
      ggtitle(paste0("Sentence judgments by ", # convert to lower if not Carver
                     if(covariateTitleName() %in% c("Carver dialect region", "ANAE dialect region")){covariateTitleName()}else{tolower(as.character(covariateTitleName()))})
                     )
  })
})

#generic line that launches the app
shinyApp(ui = ui, server = server)

# One problem I've noticed is that if you zoom in on the map and then switch to a different sentence, it resets the zoom.
# This link (https://stackoverflow.com/questions/34985889/how-to-get-the-zoom-level-from-the-leaflet-map-in-r-shiny) has a solution to that, but it uses slightly more advanced reactivity than I want to get into right this second. Come back to this.

# Show demographic relationships only for the part of the map you're zoomed into
# Hovertext with demographics for each point