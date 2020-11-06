# Support functions etc. for shinydashboard.R


# Load packages -----------------------------------------------------------
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

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
## default sentence
defaultSentence1 <- "I'm after eating ice cream."


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


# Factor levels -----------------------------------------------------------
genderLevels <- c("female", "male", "nonbinary/other" = "other")
raceLevels <- c("asian", "black", "hispanic/latinx", "native american", "pacific islander", "white", "other")
educationLevels <- c("some high school", "high school diploma", "some college", "associate degree" = "associate", "bachelor's degree" = "bachelor's", "graduate degree" = "graduate")
ageBinLevels <- c("18-30", "31-40", "41-50", "51-60", ">61")




