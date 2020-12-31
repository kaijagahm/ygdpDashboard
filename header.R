# Header portion of the Shiny dashboard
source("dashboardFunctions.R")

HEADER <- dashboardHeaderPlus(
  title = "YGDP Data Explorer",
  tags$li(class = "dropdown", bookmarkButton(label = "Bookmark app state")),
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "sliders"
)