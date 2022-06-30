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
library(glue)
library(DT)
library(catR)
library(tidyverse)
library(shinyWidgets)
library(markdown)

source("cat-irt.R")


d = read.csv("acom_Tscaled_ItemParameters_Content.csv")
d$itnum <- as.numeric(seq(1:59))
d$order = NA
d$response = NA
d$response_num = NA
d$clarify = NA
d$theta = NA
d$sem = NA

nitems = 12


getTxt <- function(v) {
  txt = d$item_content[v$itnum]
  return(txt)
}

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "ACOM test app",
  id = "mainpage",
  header =
    div(includeCSS("www/style.css"),
        useShinyjs()),
  tabPanel(
    "INTRO",
    value = "intro",
    tabsetPanel(
      id = "intropage",
      type = "hidden",
      tabPanelBody(
        value = "intro1",
        column(
          width = 8,
          offset = 2,
          div(align = "center",
          h3("Aphasia Communication Outcome Measure")
          ),
          div(align = "center",
          div(style = "display: inline-block; text-align: left;margin-top:40px;",
          selectInput("test", "Select a Test Version:",
                      choices = c("Adaptive/Full Length ACOM"=59,
                                  "Adaptive/Short Form ACOM" = 12,
                                  "Adaptive/Custom Length ACOM" = "custom"
                                  #,"Short ACOM" = "12"
                                  )),
          shinyjs::disabled(
            sliderInput("custom", label = "Custom test:",
                      min = 12, max = 59, step = 1, value = 59)
            ),
          uiOutput("test_text"),
          
          textInput("participant", "Enter Participant ID"),
          textInput("examiner", "Enter Examiner ID"),
          )),br(),
          div(align = "center",
            actionButton("next1", "Next")
          )
        )
      ),
      tabPanelBody(value = "intro2",
                   column(
                     width = 8,
                     offset = 2,
               
                     div(style = "font-size:2rem;margin-top:30px;margin-bottom:30px;", includeMarkdown("instructions1.md")),
                     div(align = "center",
                         actionButton("back2", "Back"), actionButton("next2", "Next"))
                   )),
      tabPanelBody(
        value = "intro3",
        column(
          width = 8,
          offset = 2, align = "center",
          tags$h3("You will rate how effectively you perform tasks using the following categories:"),
          tags$img(src="acom_scale_radio_q.png", style="width:90%;"),
        div(align = "center",actionButton("back3", "Back"), actionButton("next3", "Next"))
        )
      ),
      tabPanelBody(
        value = "intro4",
        column(
          width = 8,
          offset = 2,
        div(style = "font-size:2rem;padding:50px;", includeMarkdown("instructions3.md")),
        br(), br(), br(), br(),
        div(align = "center",actionButton("back4", "Back"), actionButton("start", "Start"))
        )
      )
    )
  ),
  tabPanel(
    "ACOM",
    value = "acom",
    shiny::fluidRow(column(
      width = 10,
      offset = 1,
      align = "center",
      uiOutput("questionText"),
    )),
    shiny::fluidRow(style = "min-height:560px;",
                    shiny::column(
                      width = 8, offset = 4,
                      div(id = "stim",
                          div(
                            class = "container",
                            tags$img(src = "acom_scale_radio.png", style = "height:550px;position:absolute;"),
                            radioButtons(
                              "select",
                              label = NULL,
                              choices = c(
                                "Doesn't apply to me",
                                "Not very",
                                "Somewhat",
                                "Mostly",
                                "Completely"
                              ),
                              inline = TRUE,
                              selected = character(0)
                            )#"Mostly")
                          ))
                      
                    )),
    shiny::fluidRow(
      shiny::column(
        width = 10,
        offset = 1,
        align = "center",
        actionButton("enter", "Enter")
      )
    )
  ),
  tabPanel(
    "Results",
    value = "results",
    column(
      width = 12,
      offset = 0,
      align = "center",
      div(DT::dataTableOutput("responses"))
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v = reactiveValues(
    i = 1,
    itnum = 1,
    results = d,
    num_enters = 0
  )
  
  observeEvent(input$test,{
    if(input$test=="custom"){
      shinyjs::enable("custom")
    } else {
      shinyjs::disable("custom")
    }
  })
  
  observeEvent(input$next1, {
    updateTabsetPanel(session, "intropage", selected = "intro2")
  })
  observeEvent(input$next2, {
    updateTabsetPanel(session, "intropage", selected = "intro3")
  })
  observeEvent(input$next3, {
    updateTabsetPanel(session, "intropage", selected = "intro4")
  })
  
  observeEvent(input$back2, {
    updateTabsetPanel(session, "intropage", selected = "intro1")
  })
  observeEvent(input$back3, {
    updateTabsetPanel(session, "intropage", selected = "intro2")
  })
  observeEvent(input$back4, {
    updateTabsetPanel(session, "intropage", selected = "intro3")
  })
  
  observeEvent(input$start,{
    updateNavbarPage(session = session, "mainpage", selected = "acom")
    v$results$examiner = input$examiner
    v$results$participant = input$participant
    v$test_length = ifelse(input$test=="custom", input$custom,
                           input$test)
  })
  
  observeEvent(input$enter, {
    # check that a response was entered
    if (is.null(input$select)) {
      shiny::showNotification("Enter a response")
    }
    # don't gor further otherwise
    req(input$select)
    
    # If you select "Doesn't apply to me" open a modal...
    if (input$select == "Doesn't apply to me") {
      showModal(
        modalDialog(
          title = "Is this due to your communication difficulties or some other reason?",
          fluidRow(
            align = "center",
            shinyWidgets::radioGroupButtons(
              "clarify",
              label = NULL,
              choices = c(
                "No, due to some other reason" = "no",
                "Yes, due to my communication difficulties" =
                  "yes"
              ),
              selected = character(0)
              
            )
          ),
          footer = NULL,
          easyClose = FALSE
        )
      )
    } else {
      v$num_enters = v$num_enters + 1
    }
    
    
  })
  
  # close the modal when one of the buttons is pressed
  observeEvent(input$clarify, {
    req(input$clarify)
    v$num_enters = v$num_enters + 1
    removeModal()
  })
  
  observeEvent(v$num_enters, {
    # don't do this on start up
    req(v$num_enters > 0)
    
    
    # can't feed a null value to a function
    if (is.null(input$clarify)) {
      v$clarify = NA
    } else {
      v$clarify = input$clarify
    }
    
    responses = response_to_numeric(input$select, v$clarify)
    v$results$response_num[v$itnum] = responses$response_num
    v$results$response[v$itnum] = responses$response
    v$results$clarify[v$itnum] = responses$clarify
    
    cat_data = goCAT(v)
    
    v$results$theta[v$itnum] = cat_data$theta
    v$results$sem[v$itnum] = cat_data$sem
    v$results$order[v$itnum] = v$i
    
    print(head(v$results, 3))
    
    # if you've reached the max number of responses...go to results
    if (sum(!is.na(v$results$response_num)) == v$test_length) {
      updateNavbarPage(session = session, "mainpage", selected = "results")
    } else {
      # otherwise, iterate on the i
      updateRadioGroupButtons(session, "clarify", selected = character(0))
      updateRadioButtons(session, "select", selected = character(0))
      
      v$i = v$i + 1
      v$itnum = cat_data$next_item
      
    }
    
    
  })
  
  
  output$questionText <- renderUI({
    txt = getTxt(v = v)
    tags$p(glue::glue("How effectively do you {txt}?"), style = "font-size:2.5rem;margin-top:15px;")
  })
  
  output$responses <- DT::renderDataTable({
    req(input$mainpage == "results")
    v$results %>% select(-discrim,-b1,-b2,-b3)
  }, options = list(rowCallback = JS(rowCallback)), rownames = FALSE)
  
  output$test_text <- renderUI({
    
    if(input$test == 59){
      txt = "Administer a 59 item ACOM with the first 12 items adaptively administered and the last 47 items randomly administered"
    } else if (input$test == 12) {
      txt = "Administer a 12 item short version of the ACOM with adaptive item administration"
    } else {
      txt = "Choose any number of items between 12 adn 59. Increasing the number of items will improve the precision of the test."
    }
    
    return(div(txt, style = "width:300px;height:80px;"))

  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
