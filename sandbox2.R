library(shiny)
library(htmltools)
list_content <- function(col,content){
  paste0('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:',
         col,';margin-top:3px;"></i><div style="color:black;padding-left:5px;">',
         content,'</div></div>')
}
ui <- fluidPage(
  div(htmlDependency("font-awesome", 
                     "5.13.0", "www/shared/fontawesome", package = "shiny", 
                     stylesheet = c("css/all.min.css", "css/v4-shims.min.css")),
      checkboxGroupInput(inputId = "mybuttons",
                         label="styled choices",
                         choiceNames = list(HTML(list_content("lightblue","1")),
                                            HTML(list_content("blue","2")),
                                            HTML(list_content("darkblue","3"))),
                         choiceValues= letters[1:3],
                         selected = letters[1:3])
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)