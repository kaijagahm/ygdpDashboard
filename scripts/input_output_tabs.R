# input tabset
input_control_tabs <- tabsetPanel(
  id = "inputControls",
  type = "hidden",
  tabPanel("map",
           div(htmlDependency("font-awesome", "5.13.0", "www/shared/fontawesome", package = "shiny", 
                              stylesheet = c("css/all.min.css", "css/v4-shims.min.css")), # help idk what this does
               checkboxGroupInput(inputId = "ratings",
                                  label = "Filter by rating", # name of the checkbox panel
                                  choiceNames = htmlchoicenames, # from app_functions.R
                                  choiceValues = 5:1, selected = 5:1)
           ),
           actionButton("updatemap", "Update map")
  ),
  tabPanel("plot", 
           # Select covariate
           selectInput("covariate", "Choose a demographic variable:", 
                       choices = demographic_vars, selected = demographic_vars[1]),
           actionButton("updateplot", "Update plot")
  ),
  tabPanel("mapBoolean",
           
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
  tabPanel("mapBoolean"#,
           #output$mapBoolean
           )
)