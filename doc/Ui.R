library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)

vars <- c(
  "Tree",
  "Drink Fountain",
  "Toilet")
dataset <- c(
  "Legnth",
  "Slope",
  "Tree",
  "Drink Fountain",
  "Toliet"
)
# Choices for drop-downs
shinyUI(
  
  
  navbarPage(tags$img(tags$span("Jogger",class='edit'),src='https://maxcdn.icons8.com/Share/icon/Animals//running_rabbit1600.png',
                      align = 'left',height = 40, width = 40,class = 'fixed'), 
  id="nav",
  
  tabPanel(tags$em("Jogger's map"),
           div(class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               leafletOutput("map", width="100%", height="100%"),
               
               # Shiny versions prior to 0.11 should use class="modal" instead.
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Jogger's path"),
                             textInput("start",label='Where you start?',value = "columbia university, new york"),
                             
                             # verbatimTextOutput("test"),
                             radioButtons("end_dis", "Please choose ending location or distance",
                                          c("end" = 1,"distance" = 2)),
                             textInput("stop",label='Where you stop?(Optional)',value = "times square, new york"),
                             sliderInput("distance", label = "Distance: ", min = 0, max = 10, value = 5),
                             
                              
                             
          
                             
                             
                             h5("Return using original path."),
                             checkboxInput("return", label = "Yes"),
                             
                             #changed layout to match input of code 
                             sliderInput("tree", "Tree:", min=0, max=10, value=5),
                             sliderInput("slope", label = "Slope", min=0, max=10, value=5),
                             sliderInput("foutain", label = "Drinking Foutain: ", min=0, max=10, value=5),
                             sliderInput("restroom", label = "Restroom", min=0, max=10, value=5),
                             sliderInput("width", label = "Sidewalk Width", min = 0, max = 10, value = 5),
                             
                             submitButton("Update")
                             # if we need to add some initial plot here

               ),
               ## where our data come from and copyright?
               tags$div(id="cite",
                        'Data compiled for ', tags$em('New York Opensource Data'), ' until 2016'
               )
           )
  ),
  tabPanel(tags$em("Path conditions"),
           tags$div(class='beauty',
                    fluidRow(
                      column(3,
                             selectInput("states", "States", choices = c('a','b'), selected = 'a', multiple=TRUE)
                      ),
                      column(3,
                             selectInput("input.states", "fasfas",choices = c('dasd','ff','sfaas'),selected = 'dasd',
                                         multiple=TRUE)
                      ),
                      column(3,
                             selectInput("input.states","asdadsa", choices = c('sda','dasd','ff'),selected = 'sda',multiple = TRUE
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    verbatimTextOutput("path_condition")
                    # show the conditions here:
                    #DT::dataTableOutput("ziptable")
           )
  ),
  tabPanel(tags$em("Factor Exploration"),
           tags$div(class='beauty',
                    fluidRow(
                      column(3,
                             h4("Data Set Selection"),
                             selectInput("dataset","Variable Selection",dataset),
                             br()
                      ),
                      column(8, offset = 1,
                             verbatimTextOutput("summary")
                      )
                    ),
                    title = "Distribution",
                      
                    plotlyOutput(outputId = "distribution"),
                      
                    hr(),
                    
                    DT::dataTableOutput("table")
           )
  )
  
  #conditionalPanel("false", icon("crosshair"))
  )
)
