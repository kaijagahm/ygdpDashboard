# Dashboard right sidebar
source("dashboardFunctions.R")
includeNAsText <- "include NA's?"

# Age input widget --------------------------------------------------------
ageWidget <- div(
  h5("Age:"),
  checkboxInput("ageNAs", includeNAsText, value = TRUE),
  tabsetPanel(id = "ageTabs",
              type = "tabs",
    tabPanel("range",
             br(),
             sliderInput("ageSlider", label = NULL, 
                         min = 18, 
                         max = 100, 
                         value = c(18, 100))),
    tabPanel("bins",
             br(),
             checkboxGroupButtons("ageButtons", label = NULL,
                                  choices = ageBinLevels,
                                  selected = ageBinLevels
             ))
  )
)


# Gender input widget -----------------------------------------------------
genderWidget <- div(
  h5("Gender:"),
  checkboxInput("genderNAs", includeNAsText, value = T),
  pickerInput("gender", label = NULL,
              choices = genderLevels,
              selected = genderLevels, 
              options = list(`actions-box` = TRUE), 
              multiple = T)
)


# Race input widget -------------------------------------------------------
raceWidget <- div(
  h5("Race:"),
  checkboxInput("raceNAs", includeNAsText, value = T),
  pickerInput("race", label = NULL,
              choices = raceLevels,
              selected = raceLevels,
              options = list(`actions-box` = TRUE),
              multiple = T)
)


# Education input widget --------------------------------------------------
educationWidget <- div(
  h5("Education level:"),
  checkboxInput("educationNAs", includeNAsText, value = T),
  pickerInput("education", label = NULL, 
              choices = educationLevels, 
              selected = educationLevels, 
              options = list(`actions-box` = TRUE), 
              multiple = T)
)




