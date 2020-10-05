# Dashboard right sidebar
source("dashboardFunctions.R")

RIGHTSIDEBAR <- rightSidebar(
  background = "dark",
  uiOutput("rightSidebar"), # We'll define this dynamically using renderUI in the server
  title = "Right Sidebar"
)