# Support functions etc. for shinydashboard.R

# Load packages -----------------------------------------------------------
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes) # for custom theming
library(devtools)
library(sysfonts) # for fun new fonts
library(shinydashboardPlus) # AAA This is a MAJOR problem: shinydashboardPlus version 2.0.0.9000 is a breaking version. A lot of changes will have to be made to the code. It's documented in detail here: https://rinterface.github.io/shinydashboardPlus/news/index.html. See also GH issue #38. I included a line of code above to install the older version of shinydashboardPlus (the version that comes before the breaking changes.)
load(here("data", "points", "snl.Rda"))

# Tab children function ---------------------------------------------------
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}


# Text for NA checkboxes in right sidebar ---------------------------------
includeNAsText <- "include NA's?"

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

getSentenceChoicesI <- function(inputList, surveySentencesTable){
  sentences <- names(inputList)
  a <- surveySentencesTable %>%
    filter(sentenceText %in% sentences) %>%
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
urbanRuralLevels <- c("rural", "urban cluster", "urban")


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
  palette = rev(c(rgb(0,0,0, maxColorValue = 255), rgb(130, 0, 0, maxColorValue = 255), rgb(130,130,130, maxColorValue = 255))),
  domain = 1:5,
  na.color = "#ffffff00"
)

greenLegend <- colorNumeric(
  palette = rev(c(rgb(0,0,0, maxColorValue = 255), rgb(0, 130, 0, maxColorValue = 255), rgb(130,130,130, maxColorValue = 255))),
  domain = 1:5,
  na.color = "#ffffff00"
)

blueLegend <- colorNumeric(
  palette = rev(c(rgb(0,0,0, maxColorValue = 255), rgb(0, 0, 130, maxColorValue = 255), rgb(130,130,130, maxColorValue = 255))),
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
                         multiple = F,
                         options = list(
                           onDropdownOpen = I(onDropdownOpen)
                         )
          ),
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
addSentenceUII <- function(id, inputList, surveySentencesTable){
  div(id = paste0("sentence", id, "Controls", "I"),
      div(style = reduceSpacing,
          selectizeInput(inputId = paste0("sentence", id, "I"),
                         label = paste0("Sentence ", id, ":"),
                         choices = getSentenceChoicesI(inputList,
                                                       #surveyIDString,
                                                       surveySentencesTable),
                         selected = getSentenceChoicesI(inputList, 
                                                        #surveyIDString,
                                                        surveySentencesTable)[[1]][[1]],
                         multiple = F,
                         options = list(
                           onDropdownOpen = I(onDropdownOpen)
                         )
          )),
      br(),
      hr()
  )
}

# Checkbox Text -----------------------------------------------------------
# Text to display alongside the checkbox in the corner of the PTS leaflet map that indicates whether or not to show points that don't meet the selected critiera.
checkboxText <- "Show points that don't meet selected criteria"


# Point sizes -------------------------------------------------------------
coloredPointSize <- 6
blackPointSize <- 2

# selectInput dropdown collapse -------------------------------------------
onInitialize <- '
$(function() {
  $("body").on("mousedown", ".selectize-dropdown-content", function(e){
    e.preventDefault(); 
    return false;
  }); 
  $("body").on("click", ".optgroup-header", function(){
    $(this).siblings().toggle();
  });
});'

onDropdownOpen <- '
function(el){
  setTimeout(function(){
    $(el).find(".optgroup .option").hide();
  }, 0);
}'


# Custom theme ------------------------------------------------------------
## This theme was created with the beta dashboardthemes theme designer, here: https://nik01010.shinyapps.io/dashboardThemeDesigner/
## Note that the theme designer currently does not include themes for the right sidebar, so it's a bit off, style-wise. That could probably be corrected manually with a bunch of css, but I don't know how.
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#FFFFFF"
  
  ### header
  ,logoBackColor = "#00356B"
  
  ,headerButtonBackColor = "#00356B"
  ,headerButtonIconColor = "#DCDCDC"
  ,headerButtonBackColorHover = "#00356B"
  ,headerButtonIconColorHover = "#949494"
  
  ,headerBackColor = "#00356B"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#F9FAFC"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 10
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#0F0F0F"
  
  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"
  
  ,sidebarTabTextColor = "#0F0F0F"
  ,sidebarTabTextSize = "15"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "1"
  
  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#0F0F0F"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#0F0F0F"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#E1E1E1"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"
  
  ### inputs
  ,buttonBackColor = "#DCDCDC"
  ,buttonTextColor = "#0F0F0F"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "10"
  
  ,buttonBackColorHover = "#B3B3B3"
  ,buttonTextColorHover = "#0F0F0F"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#DCDCDC"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#EEEEEE"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)

# Define a default sentence -----------------------------------------------
# The default sentence needs to be the sentence that ends up being first *after* the list groupings are applied.
defaultSentence1 <- getSentenceChoices(snl[[1]])[[1]] %>% 
  unlist()