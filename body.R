# Dashboard body
source("dashboardFunctions.R")

BODY <- dashboardBody(
  shinyDashboardThemes( # not sure why we define the theme in the body as opposed to at the beginning of the UI, but okay.
    theme = "grey_dark"
  ),
  tabItems( # different outputs to be shown depending on which menu item is selected in the lefthand menu
    tabItem(
      tabName = "pointMaps",
      p("[Insert map here]")),
    tabItem(
      tabName = "socialCharts",
      p("[Insert charts here]"))
  )
)