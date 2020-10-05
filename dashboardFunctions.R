# Support functions etc. for shinydashboard.R


# Load packages -----------------------------------------------------------
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

# COLOR PALETTE ----------------------------------------------------------
color_palette <- c("1" = "#a6611a", "2" = "#dfc27d", "3" = "white", "4" = "#80cdc1", "5" = "#018571")
boolean_palette <- c("color1" = "goldenrod2", "color2" = "gray20")


# Spacing -----------------------------------------------------------------
reduceSpacing <- "margin-bottom: -2em; padding: 0px"


# CSS/HTML ---------------------------------------------------------------------
base <- "$(\"input:checkbox[name='INPUTNAME'][value='VAL']\").parent().css('background-color', 'COLOR');"

formatButtons <- function(sentenceNumber, baseString = base, colors = color_palette){
  formats <- lapply(color_palette, function(x) str_replace(base, "COLOR", x)) %>% 
    unlist() %>% 
    str_replace(., "INPUTNAME", paste0("ratingsSentence", as.character(sentenceNumber))) %>% 
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
  prs <- tagList(checkboxGroupButtons(name, "Rated:",
                                      choiceNames = ratingChoiceNames,
                                      choiceValues = ratingChoiceValues,
                                      selected = ratingChoiceValues),
                 tags$script(formatButtons(sentenceNum)[1]), # nicely formatted rating selector buttons for sentence 2
                 tags$script(formatButtons(sentenceNum)[2]),
                 tags$script(formatButtons(sentenceNum)[3]),
                 tags$script(formatButtons(sentenceNum)[4]),
                 tags$script(formatButtons(sentenceNum)[5]))
  return(prs)
}


# Create join selector ----------------------------------------------------
prettyJoinSelector <- function(sentenceNum){
  name <- paste0("joinType", sentenceNum)
  pjs <- tagList(
    radioGroupButtons(name, label = "How joined:",
                      choices = c("AND", "OR", "NOT"),
                      selected = "AND",
                      status = "info")
  )
  return(pjs)
}

