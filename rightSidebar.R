# Dashboard right sidebar
source("dashboardFunctions.R")
includeNAsText <- "include NA's?"

RIGHTSIDEBAR <- rightSidebar(
  background = "dark",
  uiOutput("rightSidebar"), # We'll define this dynamically using renderUI in the server
  title = "Right Sidebar"
)

# Age input widget --------------------------------------------------------
ageWidget <- div(
  checkboxGroupInput("ageNAs", "Age:", choices = includeNAsText, selected = includeNAsText),
  tabsetPanel(
    tabPanel("range",
             br(),
             sliderInput("ageSlider", label = NULL, 
                         min = 18, 
                         max = 100, 
                         value = c(18, 100))),
    tabPanel("bins",
             br(),
             checkboxGroupButtons("ageButtons", label = NULL,
                                  choices = c("18-30", "31-40", 
                                              "41-50", "51-60", "61+")
             ))
  )
)


# Gender input widget -----------------------------------------------------
genderWidget <- div(
  checkboxGroupInput("genderNAs", "Gender:", choices = includeNAsText, selected = includeNAsText),
  pickerInput("gender", label = NULL,
              choices = tolower(c("Male", "Female", "Nonbinary/Other")),
              selected = tolower(c("Male", "Female", "Nonbinary/Other")), 
              options = list(`actions-box` = TRUE), 
              multiple = T)
)


# Race input widget -------------------------------------------------------
raceWidget <- div(
  checkboxGroupInput("raceNAs", "Race:", choices = includeNAsText, selected = includeNAsText),
  pickerInput("race", label = NULL,
              choices = c("asian", "black", "hispanic/latinx", 
                          "native american", "pacific islander", "white", "other"),
              selected = c("asian", "black", "hispanic/latinx", 
                           "native american", "pacific islander", "white", "other"),
              options = list(`actions-box` = TRUE),
              multiple = T)
)


# Education input widget --------------------------------------------------
educationWidget <- div(
  checkboxGroupInput("educationNAs", "Education level:", choices = includeNAsText, selected = includeNAsText),
  pickerInput("education", label = NULL, 
              choices = tolower(c("Some HS", "High school diploma", 
                                  "Some college", "Bachelor's degree", 
                                  "Associate degree", "Graduate degree")), 
              selected = tolower(c("Some HS", "High school diploma", 
                                   "Some college", "Bachelor's degree", 
                                   "Associate degree", "Graduate degree")), 
              options = list(`actions-box` = TRUE), 
              multiple = T)
)




