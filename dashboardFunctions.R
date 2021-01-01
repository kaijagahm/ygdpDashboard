# Support functions etc. for shinydashboard.R

# Load packages -----------------------------------------------------------
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(devtools)
#install_version("shinydashboardPlus", version = "0.7.5", repos = "http://cran.us.r-project.org")
library(shinydashboardPlus) # AAA This is a MAJOR problem: shinydashboardPlus version 2.0.0.9000 is a breaking version. A lot of changes will have to be made to the code. It's documented in detail here: https://rinterface.github.io/shinydashboardPlus/news/index.html. See also GH issue #38. I included a line of code above to install the older version of shinydashboardPlus (the version that comes before the breaking changes.)
library(dashboardthemes)

# Define a default sentence -----------------------------------------------
defaultSentence1 <- "I'm after bein' up there for five hours." # AAA: if you put this line into shinydashboard.R, instead of in this script, even if you put it at the top, the app fails to load. I think it has something to do with the different lines running in a weird random order, because Shiny does that? My guess is that Shiny runs all library() and source() calls *first*, and then runs the rest of the code in an unpredictable order. So if you don't have defaultSentence1 defined in this script (which gets called with source()), then you get burned. But I could be wrong.
# AAA Another problem with doing things this way is that if another survey is added to the data and doesn't include this sentence, all the places where we have snl[[1]] for the survey data and then use defaultSentence1 as the initial sentence value won't work. Need to go in and make this more robust. See issue #39.

# Tab children function ---------------------------------------------------
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

# Named group split function from Romain Francois
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

# COLOR PALETTE ----------------------------------------------------------
color_palette <- c("1" = "#a6611a", "2" = "#dfc27d", "3" = "white", "4" = "#80cdc1", "5" = "#018571")
boolean_palette <- c("color1" = "goldenrod2", "color2" = "gray20")


# Spacing -----------------------------------------------------------------
reduceSpacing <- "margin-bottom: -2em; margin-top: -1em; padding: 0px"


# CSS/HTML ---------------------------------------------------------------------
base <- "$(\"input:checkbox[name='INPUTNAME'][value='VAL']\").parent().css('background-color', 'COLOR');"

formatButtons <- function(name, baseString = base, colors = color_palette){
  formats <- lapply(color_palette, function(x) str_replace(base, "COLOR", x)) %>% 
    unlist() %>% 
    str_replace(., "INPUTNAME", name) %>% 
    map2(.x = ., .y = as.character(1:5), function(.x, .y) str_replace(.x, "VAL", .y)) %>% 
    unlist()
  return(formats)
}

leftSidebarScroll <- tags$style( # css code to make the left sidebar scroll.
  "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh;
        }"
)

# Rating choices ----------------------------------------------------------
ratingChoiceNames <- c("1", "2", "3", "4", "5")
ratingChoiceValues <- 1:5

## full function to define the pretty colored rating buttons
prettyRatingSelector <- function(sentenceNum){
  name <- paste0("ratingsSentence", sentenceNum)
  prs <- tagList(checkboxGroupButtons(inputId = name, "Rated:",
                                      choiceNames = ratingChoiceNames,
                                      choiceValues = ratingChoiceValues,
                                      selected = ratingChoiceValues,
                                      status = "primary")#,
                 # tags$script(formatButtons(name)[1]), # nicely formatted rating selector buttons for sentence 2
                 # tags$script(formatButtons(name)[2]),
                 # tags$script(formatButtons(name)[3]),
                 # tags$script(formatButtons(name)[4]),
                 # tags$script(formatButtons(name)[5])
  )
  return(prs)
}


# Create join selector ----------------------------------------------------
prettyJoinSelector <- function(sentenceNum){
  name <- paste0("joinType", sentenceNum)
  pjs <- tagList(
    radioGroupButtons(name, label = "How joined:",
                      choices = c("AND", "OR"),
                      selected = "AND",
                      status = "info")
  )
  return(pjs)
}

# Sentence choices --------------------------------------------------------
# Sentence choices, in list
getSentenceChoices <- function(inputList){ # function to get a list of sentence choices from a surveyData() list of data frames
  a <- bind_rows(lapply(inputList, as.data.frame)) %>%
    select(constructionName, sentenceText) %>%
    distinct() %>%
    group_by(constructionName) %>%
    named_group_split(constructionName) %>%
    lapply(., function(x) x %>% pull(sentenceText)) %>%
    lapply(., as.list)
  return(a)
}

getSentenceChoicesI <- function(inputList, surveyIDString, surveySentencesTable){
  sentences <- names(inputList)
  a <- surveySentencesTable %>%
    filter(sentenceText %in% sentences, 
           surveyID == surveyIDString) %>%
    select(constructionName, sentenceText) %>%
    distinct() %>%
    group_by(constructionName) %>%
    named_group_split(constructionName) %>%
    lapply(., function(x) x %>% pull(sentenceText)) %>%
    lapply(., as.list)
  return(a)
}

# Factor levels -----------------------------------------------------------
genderLevels <- c("female", "male", "nonbinary/other" = "other")
raceLevels <- c("asian", "black", "hispanic/latinx", "native american", "pacific islander", "white", "other")
educationLevels <- c("some high school", "high school diploma", "some college", "associate degree" = "associate", "bachelor's degree" = "bachelor's", "graduate degree" = "graduate")
ageBinLevels <- c("18-30", "31-40", "41-50", "51-60", ">61")


# COLOR PALETTES --------------------------------------------------------
# discreteBlueYellow <- colorFactor(
#   palette = c("#3169d8", "#31a0d8", "#1eeba7", "#85d831", "#d8bc31"),
#   levels = 1:5,
#   na.color = "#ffffff00"
# )
# 
# discreteBlueYellowLegend <- colorFactor(
#   palette = rev(c("#3169d8", "#31a0d8", "#1eeba7", "#85d831", "#d8bc31")),
#   levels = 1:5,
#   na.color = "#ffffff00"
# )

continuousBlueYellow <- colorNumeric(
  palette = c("#3169d8", "#31a0d8", "#1eeba7", "#85d831", "#d8bc31"),
  domain = 1:5,
  na.color = "#ffffff00"
)

continuousBlueYellowLegend <- colorNumeric(
  palette = rev(c("#3169d8", "#31a0d8", "#1eeba7", "#85d831", "#d8bc31")),
  domain = 1:5,
  na.color = "#ffffff00"
)

continuous44 <- colorNumeric(
  palette = c("#562c90", "#913aa6", "#b845b4", "#d267cc", "#eebfe9", "#fff9c2", "#fff06b", "#ffec33", "#f7bf33", "#e88126"),
  domain = -4:4,
  na.color = "#ffffff00"
)

continuous44Legend <- colorNumeric(
  palette = rev(c("#562c90", "#913aa6", "#b845b4", "#d267cc", "#eebfe9", "#fff9c2", "#fff06b", "#ffec33", "#f7bf33", "#e88126")),
  domain = -4:4,
  na.color = "#ffffff00"
)

redLegend <- colorNumeric(
  palette = rev(c(rgb(0,0,0, maxColorValue = 255), rgb(255, 0, 0, maxColorValue = 255), rgb(255,255,255, maxColorValue = 255))),
  domain = 1:5,
  na.color = "#ffffff00"
)

greenLegend <- colorNumeric(
  palette = rev(c(rgb(0,0,0, maxColorValue = 255), rgb(0, 255, 0, maxColorValue = 255), rgb(255,255,255, maxColorValue = 255))),
  domain = 1:5,
  na.color = "#ffffff00"
)

blueLegend <- colorNumeric(
  palette = rev(c(rgb(0,0,0, maxColorValue = 255), rgb(0, 0, 255, maxColorValue = 255), rgb(255,255,255, maxColorValue = 255))),
  domain = 1:5,
  na.color = "#ffffff00"
)

# discretePurpleGreen <- colorNumeric(
#   palette = c("#8a31a0", "#6c37d7", "#2569bb", "#04be96", "#13ec4d"),
#   domain = 1:5,
#   na.color = "#ffffff00"
# )


# Bo's labeling function --------------------------------------------------
# Thanks to my suitemate Bo You for writing this function! 
upgradeLabels <- function(x) {
  y <- x[, grepl("sentence", names(x))] # get just the `sentence` cols
  y_names <- paste0("<b>", names(y), ": </b>") # extract names and add bold html formatting
  y_names <- str_replace(y_names, "sentence", "Sentence ") # "sentence1" --> "Sentence 1"
  x$temp <- apply(y, 1, function(x) paste0(y_names, " ", x, " <br>", collapse = " ")) # paste together the sentence names and their ratings
  x$temp <- paste0("<br> ", x$temp) # add initial line break to separate the sentence ratings from the rest of the label text
  x$label <- paste(x$label, x$temp)
  x <- x %>% select(-temp)
  return(x)
}

# addSentenceUI -----------------------------------------------------------
# Function to add the dynamic UI components for a new sentence in points mode
## Function definition
addSentenceUI <- function(id, dat){
  div(id = paste0("sentence", id, "Controls"),
      div(style = reduceSpacing,
          selectizeInput(inputId = paste0("sentence", id),
                         label = paste0("Sentence ", id, ":"),
                         choices = getSentenceChoices(dat),
                         selected = getSentenceChoices(dat)[[1]][[1]],
                         multiple = F),
          prettyRatingSelector(sentenceNum = as.numeric(id))),
      #div(style = "display:inline-block",
      #    prettyJoinSelector(sentenceNum = as.numeric(id))), # opted not to include join
      #div(style = "display:inline-block", actionBttn(inputId = paste0("trash", id),
      #icon = icon("trash"), # individual sentence trash buttons: not implemented
      #style = "minimal")),
      br(),
      hr()
  )
}


# addSentenceUII ----------------------------------------------------------
# Function to add the dynamic UI components for a new sentence in interpolation mode
addSentenceUII <- function(id, inputList, surveyIDString, surveySentencesTable){
  div(id = paste0("sentence", id, "Controls", "I"),
      div(style = reduceSpacing,
          selectizeInput(inputId = paste0("sentence", id, "I"),
                         label = paste0("Sentence ", id, ":"),
                         choices = getSentenceChoicesI(inputList,
                                                       surveyIDString,
                                                       surveySentencesTable),
                         selected = getSentenceChoicesI(inputList, 
                                                        surveyIDString,
                                                        surveySentencesTable)[[1]][[1]],
                         multiple = F)),
      br(),
      hr()
  )
}


