# Header portion of the Shiny dashboard
source("dashboardFunctions.R")

HEADER <- dashboardHeaderPlus(
  title = "YGDP Data Explorer",
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "sliders"
)