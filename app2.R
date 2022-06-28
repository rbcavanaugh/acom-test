#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
# Define UI for application that draws a histogram
ui <- navbarPage(title = "ACOM test app",
  header = 
    div(
    inlineCSS('.container {position: relative;}.container input[type="radio"] {position: absolute;}'),
    tags$script(src = "www/increment.js")
  ),
  tabPanel("Fixed position",
    
    # Application title
    shiny::fluidRow(
      column(width = 10, offset = 1, align = "center",
      uiOutput("questionText")
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 9, offset = 3, 
        uiOutput("scale")
        )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$radiooption1,{
    print("radio1 change")
  })
  
  output$questionText <- renderUI({
    tags$p("How effectively do you ask for information from store employees?", style="font-size:3rem;margin-top:15px;")
  })

    output$scale <- renderUI({
      HTML('
      <div class="container">
        <img src="acom_scale.png" style="height:550px;position:absolute;">
          <input type="radio" class = "customradio" name="option1" style="top:21px;left:280px;">
            <input type="radio" class = "customradio" name="option1" style="top:115px;left:280px;">
              <input type="radio" class = "customradio" name="option1" style="top:250px;left:280px;">
                <input type="radio" class = "customradio" name="option1" style="top:399px;left:280px;">
                  <input type="radio" class = "customradio" name="option1" style="top:512px;left:280px;">
                            </div>
      ')
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
