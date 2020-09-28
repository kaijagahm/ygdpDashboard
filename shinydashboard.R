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
      id = "leftMenu",
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
    if (req(input$leftMenu) == "mapView"){
      message("Map view has been selected")
      shinyjs::addClass(selector = "body", class = "control-sidebar-open") # the "control sidebar" is the righthand sidebar.
      output$side_bar <- renderUI({
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
    if (req(input$nav) == "Section_B"){
      message("Section B has been selected")
      # added in edit
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
      output$side_bar <- renderUI({
        rightSidebarTabContent(
          id = "T_B",
          title = "Tab for section B",
          p("Some content relevant for section B"),
          textInput("caption", "Caption", "Data Summary")
        )
      })
    }
    
    if (req(input$nav) == "Section_C"){
      message("Section C has been selected")
      # added in edit
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open") # remove the class of being open from the right (control) sidebar.
      
      output$rightSidebar <- renderUI({ div() })
    }
  })
}


shinyApp(ui = ui, server = server)