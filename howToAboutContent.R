# Content for the "How to use" (HT) and "About" (AB) tabs of the YGDP dashboard. 
# Since this is text and images and it doesn't use any reactive values, I've separated it out into its own script to avoid making shinydashboard.R really long.
# Author: Kaija Gahm
# 31 December 2020

# Load packages -----------------------------------------------------------
library(devtools)
#install_version("shinydashboardPlus", version = "0.7.5", repos = "http://cran.us.r-project.org")
library(shinydashboardPlus) # AAA This is a MAJOR problem: shinydashboardPlus version 2.0.0.9000 is a breaking version. A lot of changes will have to be made to the code. It's documented in detail here: https://rinterface.github.io/shinydashboardPlus/news/index.html. See also GH issue #38. I included a line of code above to install the older version of shinydashboardPlus (the version that comes before the breaking changes.)
library(shiny)

# HT PTS ------------------------------------------------------------------
howToPoints <- tabPanel("Point maps",
                        div(style = 'overflow-y:scroll;height:500px',
                            boxPlus(title = "Left sidebar",
                                    width = 12,
                                    column(width = 4,
                                           imageOutput("PTSLeftSidebar", 
                                                       height = "50%")
                                    ),
                                    column(width = 8,
                                           div(style = "font-size:16px;",
                                               tags$ol(
                                                 tags$li("Select a survey. The sentence choices will update dynamically."), 
                                                 br(),
                                                 tags$li("Select a sentence. Sentences are organized by grammatical phenomenon."), 
                                                 br(),
                                                 tags$li("You can use the rating buttons to restrict the data by participants' chosen ratings for the selected sentence. The default is to show all ratings."),
                                                 br(),
                                                 tags$li("To add more sentences, click 'Add a sentence.' Use the corresponding sentence and rating selectors that appear, as described in (2) and (3)."),
                                                 br(),
                                                 tags$li("To see your choices on the map, click 'Apply.'"),
                                                 br(),
                                                 tags$li("To reset the sentence and rating selections, use 'Reset all.' Note that after you reset the sentences, you will have to click 'Apply' again for your changes to be shown.")
                                               )
                                           )
                                    ),
                                    collapsible = T,
                                    collapsed = F,
                                    closable = F),
                            boxPlus(title = "Map",
                                    width = 12,
                                    imageOutput("PTSMap", 
                                                height = "50%"),
                                    br(),
                                    div(
                                      style = "font-size:16px;",
                                      tags$ol(
                                        tags$li("Each point on the map represents a single survey participant."), 
                                        br(),
                                        tags$li("When several points are located in the same city, they have been jittered very slightly for visibility. You may have to zoom in very far to distinguish the points from each other. Try zooming in on New York City, for example!"), 
                                        br(),
                                        tags$li("Large, colored points match the criteria that you have chosen: selected ratings (left sidebar) and demographic criteria (right sidebar). Depending on your selection in the 'Display settings' tab (see the next section of this how-to), the points may be colored by whether or not they meet the criteria, or by participants' ratings of the sentences. If the latter, then a legend at the bottom corner of the map will key the map colors."),
                                        br(),
                                        tags$li("Small, black points do not meet the criteria that you have selected in the left and right sidebars."),
                                        br(),
                                        tags$li("You can toggle whether to display these small, black points using the checkbox at the top of the map."),
                                        br(),
                                        tags$li("Zoom in and out using the map controls or your mouse. Pan around the map by clicking and dragging."),
                                        br(),
                                        tags$li("Reset the map zoom and center focus using the reset button below the map."),
                                        br(),
                                        tags$li("Click on a point to see demographic information about that participant, as well as their ratings of each selected sentence.")
                                      )
                                    ),
                                    collapsible = T,
                                    collapsed = T,
                                    closable = F),
                            boxPlus(title = "Right sidebar",
                                    width = 12,
                                    fluidRow(
                                      column(
                                        width = 8,
                                        div(
                                          style = "font-size:16px;",
                                          tags$ol(
                                            tags$li("In the 'Filter' tab of the right sidebar, you can filter the points you'd like to display by various demographic categories."), 
                                            br(),
                                            tags$li("There are two ways to filter by age: with a continuous slider, or by categorical age bins. You can toggle between these with the 'range' and 'bins' tabs."), 
                                            br(),
                                            tags$li("For each demographic category, you can choose to exclude missing values (NA's). Some participants declined to provide demographic information, resulting in these missing values."),
                                            br(),
                                            tags$li("Within each demographic category dropdown menu, you can use the 'Select All' and 'Deselect All' options to make it easier to select multiple categories."),
                                            br(),
                                            tags$li("To update the map to reflect your selections, use the 'Apply' button."),
                                            br(),
                                            tags$li("Use the 'Reset' button to reset the demographic filters to their original values. Note that you will have to click 'Apply' again after resetting the filters to apply your changes.")
                                          )
                                        )
                                      ),
                                      column(
                                        width = 4,
                                        
                                        imageOutput("PTSRightSidebar",
                                                    height = "50%")
                                      )
                                    ),
                                    br(),
                                    fluidRow(
                                      column(
                                        width = 8,
                                        p("here is how you use the right sidebar display settings")
                                      ),
                                      column(
                                        width = 4,
                                        imageOutput("PTSDisplaySettings",
                                                    height = "50%")
                                      )
                                    ),
                                    collapsible = T,
                                    collapsed = T,
                                    closable = F)
                        )
)


# HT INT ------------------------------------------------------------------
howToInterpolation <- tabPanel("Interpolation maps",
                               div(style = 'overflow-y:scroll;height:500px',
                                   boxPlus(title = "Left sidebar",
                                           width = 12,
                                           column(width = 4,
                                                  imageOutput("INTLeftSidebar", 
                                                              height = "50%")
                                           ),
                                           column(width = 8,
                                                  div(style = "font-size:16px;",
                                                      tags$ol(
                                                        tags$li("Select a survey. The sentence choices will update dynamically."), 
                                                        br(),
                                                        tags$li("Select a sentence. Sentences are organized by grammatical phenomenon."), 
                                                        br(),
                                                        tags$li("To add more sentences, click 'Add a sentence.' Use the corresponding sentence selector that appears, as described in (2)."),
                                                        br(),
                                                        tags$li("To see your choices on the map, click 'Apply.'"),
                                                        br(),
                                                        tags$li("To reset the sentence and rating selections, use 'Reset all.' Note that after you reset the sentences, you will have to click 'Apply' again for your changes to be shown.")
                                                      )
                                                  )
                                           ),
                                           collapsible = T,
                                           collapsed = F,
                                           closable = F),
                                   boxPlus(title = "Map",
                                           width = 12,
                                           imageOutput("INTMap", 
                                                       height = "50%"),
                                           br(),
                                           div(
                                             style = "font-size:16px;",
                                             tags$ol(
                                               tags$li("The map is divided into a grid of hexagons. Point ratings have been interpolated to create a continuous surface, and then each hexagon is colored according to the predicted interpolation value at its centroid."), ## Check whether this is averaged or just sampled
                                               br(),
                                               tags$li("The legend provides a key to the hexagon colors. The scale is from 1 to 5 if the map is set to display ratings for a single sentence or aggregate statistics for more than one sentence, or from -4 to 4 if the map is set to display the difference between the ratings for two different sentences."), 
                                               br(),
                                               tags$li("Zoom in and out using the map controls or your mouse. Pan around the map by clicking and dragging."),
                                               br(),
                                               tags$li("Reset the map zoom and center focus using the reset button below the map.")
                                             )
                                           ),
                                           collapsible = T,
                                           collapsed = T,
                                           closable = F),
                                   boxPlus(title = "Right sidebar",
                                           width = 12,
                                           fluidRow(
                                             column(
                                               width = 8,
                                               div(
                                                 style = "font-size:16px;",
                                                 tags$ol(
                                                   tags$li("Select a method for coloring the hexagons. If one sentence is selected, you can only color by the ratings for that sentence. If two sentences are selected, you can color by the ratings for either sentence, or by the difference between the sentences for each hexagon, or by aggregate statistics like that minimum, maximum, mean, or median. If more than two sentences are selected, the difference options go away."), 
                                                   br(),
                                                   tags$li("Use the 'Apply' button to update the map to reflect your changes.")
                                                 )
                                               )
                                             ),
                                             column(
                                               width = 4,
                                               
                                               imageOutput("INTDisplaySettings",
                                                           height = "50%")
                                             )
                                           ),
                                           collapsible = T,
                                           collapsed = T,
                                           closable = F)
                               )
                               
)

# AB YGDP -----------------------------------------------------------------
aboutYGDP <- tabPanel("About the YGDP",
                      div(style = 'overflow-y:scroll;height:500px',
                          p("The Yale Grammatical Diversity Project (YGDP) is a group of researchers (including professors, grad students, undergrads, and even high school sudents), based in the Department of Linguistics at Yale University. We study syntax diversity in varieties of U.S. English. We use online surveys to collect acceptability judgments about sentences representing a variety of grammatical constructions. We analyze the results in scholarly publications, articles on our website, and other public-facing outputs like this dashboard.")
                      )
)

# AB Surveys --------------------------------------------------------------
aboutSurveys <- tabPanel("About the surveys",
                        div(style = 'overflow-y:scroll;height:500px',
                            p("We administer surveys through Amazon Mechanical Turk (AMT). On each survey, we make it clear to participants that we are looking for their judgments on informal, casual, speech, not their opinions about how 'proper' English should be spoken. Then we present them with approximately 45 sentences, one at a time, and ask them to rate each sentence on a scale of 1 (totally unacceptable, even in informal settings) to 5 (totally acceptable sentence)."),
                            p("On each survey, we mix up the sentences, testing many different constructions at once so that participants aren't seeing similar sentences over and over. But we still try to include enough similar sentences of the same type so that we can make statistical inferences about the results."),
                            p("We also collect some demographic information about our survey participants, which is what goes into creating the demographic filters that you see in the righthand sidebar for the point map mode in this app.")
                        )
)

