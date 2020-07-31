# Prototyping a Shiny app for ygdp data
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(boot)
library(htmltools)
library(here)
source(here("scripts", "addLegend_decreasing.R"))
source(here("scripts", "app_functions.R"))

#Import data
load(here("data", "s11.Rda"))
s11 <- s11 %>%
  mutate(SentenceText = as.character(SentenceText),
         Construction = as.character(Construction))
sentences <- s11 %>% pull(SentenceText) %>% unique()
demographic_vars <- c("Age group" = "AgeBin", "Gender", "Education", "Income bracket" = "Income", "Race", "ANAE dialect region" = "RegANAE", "Carver dialect region" = "CarverReg")
list_content <- function(col,content){
  paste0('<div style="display:flex;"><i class="fa fa-circle"
                                         style="color:',
         col,';margin-top:3px;opacity:0.8;"></i><div style="color:black;padding-left:5px;">',
         content,'</div></div>')
}

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
      
      # Select a demographic variable
      h4("Explore covariates (target sentence)"),
      selectInput("covariate", "Choose a demographic variable:", choices = demographic_vars, selected = demographic_vars[1])
    ),
    
    # main panel for plots etc.
    mainPanel(
      # Map
      h4("Map of sentence judgments"),
      leafletOutput(outputId = "mymap"), 
      absolutePanel(top = "30%", right = "2%",
                    id="controls",
                    style="z-index:500;",
                    class = "panel panel-default",
                    width = 110,
                    draggable = TRUE, 
                    div(htmlDependency("font-awesome", 
                                       "5.13.0", "www/shared/fontawesome", package = "shiny", 
                                       stylesheet = c("css/all.min.css", "css/v4-shims.min.css")),
                        checkboxGroupInput(inputId = "ratings",
                                           label = "Rating",
                                           choiceNames = list(HTML(list_content(color_palette[5],"5 (accept)")),
                                                              HTML(list_content(color_palette[4],"4")),
                                                              HTML(list_content(color_palette[3],"3")),
                                                              HTML(list_content(color_palette[2], "2")),
                                                              HTML(list_content(color_palette[1], "1 (reject)"))),
                                           choiceValues = c(5, 4, 3, 2, 1),
                                           selected = c(5, 4, 3, 2, 1))
                    )),
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
                       fillOpacity = 0.5) #%>%
    # addLegend_decreasing("bottomright", pal = pal, values = c("5", "4", "3", "2", "1"),
    #           title = "Sentence rating",
    #           opacity = 1,
    #           decreasing = T) #%>%
    #addControl(checkboxGroupInput("rSelect", label = "", choices = c("5", "4", "3", "2", "1"), selected = c("5", "4", "3", "2", "1"), width = '50%'), position = "topright") # commenting this out for now since I can't figure out how to merge it with the legend. Ultimately, this should replace the sliderInput "minmaxratings".
    
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
