# Shinydashboard test

library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

ui <- dashboardPagePlus(
  useShinyjs(),
  header = dashboardHeaderPlus(
    title = "YGDP Data Explorer",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "sliders"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "leftSidebar",
      menuItem("Map view", tabName = "mapView", icon = icon("map")),
      menuItem("Social variables", tabName = "socialVariables", icon = icon("people-fill"))#,
      #menuItem("Section C", tabName = "Section_C", icon = icon( "gears"))
    )
  ),
  body = dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabItems(
      tabItem(
        tabName = "map",
        p("[Insert map here]")),
      tabItem(
        tabName = "socialCharts",
        p("[Insert charts here]"))#,
      # tabItem(
      #   tabName = "Section_C",
      #   p("Some content for section C"))
    )
  ),
  rightsidebar = rightSidebar(
    background = "dark",
    uiOutput("rightSidebar"),
    title = "Right Sidebar"
  )
)

server <- function(input, output) {
  observe({
    if (req(input$leftSidebar) == "mapView"){
      message("Map view has been selected")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open") # the "control sidebar" is the righthand sidebar.
      output$rightSidebar <- renderUI({
        rightSidebarTabContent(
          id = "mapViewFilters",
          title = "Filters for map view",
          p("Some filters relevant to the map view"),
          # sliderInput(
          #   "obs",
          #   "Number of observations:",
          #   min = 0, max = 1000, value = 500
          # )
        )
      })
    }
    if (req(input$leftSidebar) == "socialVariables"){
      message("Social variables view has been selected")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
      output$rightSidebar <- renderUI({
        rightSidebarTabContent(
          id = "socialVariablesFilters",
          title = "Filters for social variables view",
          p("Some filters relevant to the social variables view"),
          # textInput("caption", "Caption", "Data Summary")
        )
      })
    }
    
    # if (req(input$leftSidebar) == "Section_C"){
    #   message("Section C has been selected")
    #   # added in edit
    #   shinyjs::removeClass(selector = "body", class = "control-sidebar-open") # remove the class of being open from the right (control) sidebar.
    #   
    #   output$rightSidebar <- renderUI({ div() })
    # }
  })
}


shinyApp(ui = ui, server = server)