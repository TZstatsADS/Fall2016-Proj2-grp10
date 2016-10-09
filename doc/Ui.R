library(shiny)
library(leaflet)
library(DT)

vars <- c(
  "River",
  "Tree",
  "Drink Fountain",
  "Toilet",
  "Subway")
# Choices for drop-downs
shinyUI(



navbarPage("Jogger", id="nav",
           
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
                                      textInput("stop",label='Where you stop?',value = "times square, new york"),
                                      selectInput("pre1", label = "1st Preference", choices = vars,selected='River'),
                                      selectInput("pre2", label = "2nd Preference", choices = vars, selected = "Subway"),
                                      selectInput("pre3", label = "3rd Preference", choices = vars, selected = 'Tree'),
                                      selectInput("pre4", label = "4rd Preference", choices = vars, selected = 'River'),
                                      selectInput("pre5", label = "5rd Preference", choices = vars, selected = 'Toilet')
    

                                     # if we need to add some initial plot here
                                     # plotOutput(),
                                     # plotOutput()
                        ),
                        ## where our data come from and copyright?
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('New York Opensource Data'), ' until 2016'
                        )
                    )
           ),
           
           tabPanel(tags$em("Path conditions"),
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
                    hr()
                    # show the conditions here:
                    #DT::dataTableOutput("ziptable")
           )
           
           #conditionalPanel("false", icon("crosshair"))
)
)