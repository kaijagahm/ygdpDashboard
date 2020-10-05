# Shinydashboard test
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
source("dashboardFunctions.R")

ui <- dashboardPagePlus(
  useShinyjs(),
  
  ## Define dashboard header: title, icon for right sidebar, etc.
  header = dashboardHeaderPlus(
    title = "YGDP Data Explorer",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "sliders"
  ),
  
  ## Define left sidebar and menu options
  sidebar = dashboardSidebar(
    leftSidebarScroll, # CSS to make the sidebar scroll. See dashboardFunctions.R
    sidebarMenu(
      id = "leftSidebar", # name of the left sidebar
      menuItem(expandedName = "mapView", text = "Map view",  icon = icon("map"), startExpanded = TRUE,
               # Inputs
               div(style = reduceSpacing,
                   selectInput("survey", "Survey", 
                               choices = c("5", "5b", "6", "6b", "7", "8", "9", "11", "12"),
                               selected = "11",
                               multiple = FALSE)),
               ### Sentence selector
               div(style = reduceSpacing,
                   selectizeInput("sentence", "Sentence 1:",
                                  choices = list(
                                    `After perfect` = c("I'm after eating ice cream.",
                                                        "She's just after telling me that she got the promotion."),
                                    `Needs washed` = c("Most babies like cuddled.",
                                                       "My car needs washed.")
                                  ), multiple = F)),
               ### Ratings selector
               checkboxGroupButtons("ratingsSentence1", "Rated:",
                                    choiceNames = c("1", "2", "3", "4", "5"),
                                    choiceValues = 1:5),
               ## CSS formatting for coloring the buttons. Wish this was more compact. See dashboardFunctions.R
               tags$script(formatButtons(1)[1]),
               tags$script(formatButtons(1)[2]),
               tags$script(formatButtons(1)[3]),
               tags$script(formatButtons(1)[4]),
               tags$script(formatButtons(1)[5]),
               hr(),
               uiOutput("sentence2Controls"),
               uiOutput("sentence3Controls"),
               actionButton("addSentence", "+ Add a sentence")
      ),
      menuItem(text = "Social variables", expandedName = "socialVariables", icon = icon("map"), startExpanded = FALSE,
               div(style = reduceSpacing,
                   selectInput("test", "Test", 
                               choices = c("a", "b", "c", "d", "e"),
                               selected = "a",
                               multiple = FALSE)))
    )
  ),
  
  ## Define dashboard body (maps/graphs)
  body = dashboardBody(
    shinyDashboardThemes( # not sure why we define the theme in the body as opposed to at the beginning of the UI, but okay.
      theme = "grey_dark"
    ),
    tabItems( # different outputs to be shown depending on which menu item is selected in the lefthand menu
      tabItem(
        tabName = "map",
        p("[Insert map here]")),
      tabItem(
        tabName = "socialCharts",
        p("[Insert charts here]"))
    )
  ),
  rightsidebar = rightSidebar(
    background = "dark",
    uiOutput("rightSidebar"), # We'll define this dynamically using renderUI in the server
    title = "Right Sidebar"
  )
)

server <- function(input, output) {
  observeEvent(input$addSentence, {
    if(is.null(input$joinType2)){
      output$sentence2Controls <- renderUI({
        tagList(
          div(style = reduceSpacing,
              selectizeInput("sentence", "Sentence 2:",
                             choices = list(
                               `After perfect` = c("I'm after eating ice cream.", "She's just after telling me that she got the promotion."),
                               `Needs washed` = c("Most babies like cuddled.",
                                                  "My car needs washed.")
                             ),
                             multiple = F)),
          checkboxGroupButtons("ratingsSentence2", "Rated:",
                               choiceNames = c("1", "2", "3", "4", "5"),
                               choiceValues = 1:5,
                               checkIcon = list(yes = icon("check"))),
          tags$script(formatButtons(2)[1]),
          tags$script(formatButtons(2)[2]),
          tags$script(formatButtons(2)[3]),
          tags$script(formatButtons(2)[4]),
          tags$script(formatButtons(2)[5]),
          radioGroupButtons("joinType2", label = "How joined:",
                            choices = c("AND", "OR", "NOT"),
                            selected = character(0),
                            status = "info"),
          hr() # horizontal bar
        )
      })
    }else if(length(input$joinType2) == 1){
      output$sentence3Controls <- renderUI({
        tagList(
          div(style = reduceSpacing,
              selectizeInput("sentence", "Sentence 3:",
                             choices = list(
                               `After perfect` = c("I'm after eating ice cream.",
                                                   "She's just after telling me that she got the promotion."),
                               `Needs washed` = c("Most babies like cuddled.",
                                                  "My car needs washed.")
                             ),
                             multiple = F),
              checkboxGroupButtons("ratingsSentence3", "Rated:",
                                   choiceNames = c("1", "2", "3", "4", "5"),
                                   choiceValues = 1:5),
              tags$script(formatButtons(3)[1]),
              tags$script(formatButtons(3)[2]),
              tags$script(formatButtons(3)[3]),
              tags$script(formatButtons(3)[4]),
              tags$script(formatButtons(3)[5]),
              radioGroupButtons("joinType3", label = "How Joined:",
                                choices = c("AND", "OR", "NOT"),
                                selected = character(0),
                                status = "info"),
              hr() # horizontal bar
              )
        )
      })
    }
    removeUI(
      selector = "#addSentence"
    )
  })

## Determine what shows up in the right menu bar, and when it opens/closes
observeEvent(input$sidebarItemExpanded, {
  if(req(input$sidebarItemExpanded) == "mapView"){
    message("Map view has been selected.")
    shinyjs::addClass(selector = "body", class = "control-sidebar-open") #open the "control sidebar" (righthand sidebar) when the menu tab is selected
    output$rightSidebar <- renderUI({
      rightSidebarTabContent(
        id = "mapViewFilters",
        sliderInput("age", "Age:", min = 18, max = 100, value = c(18, 100))
      )
    })
  }else if(req(input$sidebarItemExpanded) == "socialVariables"){
    message("Social variables view has been selected.")
    shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    output$rightSidebar <- renderUI({
      rightSidebarTabContent(
        id = "socialVariablesFilters",
        title = "Filters for social variables view",
        p("Some filters relevant to the social variables view")
      )
    })
  }
})
}


shinyApp(ui = ui, server = server)