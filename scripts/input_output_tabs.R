# input tabset
initialConstruction <- "Come with"
initialSentence <- "If I go to the store, do you want to come with?"
initialSentence2 <- "If you go there, bring a friend with."
initialCovariate <- demographic_vars[1]
  
input_control_tabs <- tabsetPanel(
  id = "inputControls",
  type = "hidden",
  tabPanel("map",
           div(htmlDependency("font-awesome", "5.13.0", "www/shared/fontawesome", package = "shiny", 
                              stylesheet = c("css/all.min.css", "css/v4-shims.min.css")), # help idk what this does
               checkboxGroupInput(inputId = "ratings", label = "Filter by rating", 
                                  choiceNames = htmlchoicenames, # from app_functions.R
                                  choiceValues = 5:1, selected = 5:1)
           ),
           actionButton("updateMap", "Update map")
  ),
  tabPanel("plot", 
           # Select covariate
           selectInput("covariate", "Choose a demographic variable:", 
                       choices = demographic_vars, selected = demographic_vars[1]),
           actionButton("updatePlot", "Update plot")
  ),
  tabPanel("mapBoolean",
           h3("Map parameters"),
           checkboxGroupInput("allowratings1", "Allow ratings (sentence 1)",
                              choiceNames = c("5", "4", "3", "2", "1"),
                              choiceValues = 5:1, selected = 5:1,
                              inline = T),
           selectInput("construction2", "Choose a second construction:", choices = unique(s11$Construction), 
                       selected = "Come with"),
           # Select a sentence
           htmlOutput("sentence2Selector"), # this will be generated from the server
           checkboxGroupInput("allowratings2", "Allow ratings (sentence 2)",
                              choiceNames = c("5", "4", "3", "2", "1"),
                              choiceValues = 5:1, selected = 5:1,
                              inline = T),
           actionButton("updateMapBoolean", "Update map")
  )
)


# output tabset
output_view_tabs <- tabsetPanel(
  id = "outputViews",
  type = "hidden",
  tabPanel("map",
           leafletOutput("map")
           ),
  tabPanel("plot",
           plotOutput("plot")
           ),
  tabPanel("mapBoolean",
           leafletOutput("mapBoolean")
           )
)