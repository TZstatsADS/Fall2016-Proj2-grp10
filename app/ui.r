library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)

vars <- c(
  "Tree",
  "Drink Fountain",
  "Toilet")
dataset_selection <- c(
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
                             numericInput("distance", label = "Distance: ", min = 0, max = 10, value = 5),
                             
                              
                             
          
                             
                             
                             h5("Return using original path."),
                             checkboxInput("return", label = "No"),
                             
                             #changed layout to match input of code 
                             numericInput("tree", "Tree:", min=0, max=10, value=5),
                             numericInput("slope", label = "Slope", min=0, max=10, value=5),
                             numericInput("foutain", label = "Drinking Fountain: ", min=0, max=10, value=5),
                             numericInput("restroom", label = "Restroom", min=0, max=10, value=5),
                             numericInput("width", label = "Sidewalk Width", min = 0, max = 10, value = 5),
                             
                             submitButton("Update")
                             # if we need to add some initial plot here

               ),
               
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = "auto", bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Path Information"),
                             h4("The following is detailed information on your path."),
                             
                             h4("To view in greater detail, please select the following:"),
                             checkboxInput("RP_layer","Restroom"),
                             checkboxInput("FO_layer", "Fountain"),
                             
                             submitButton("Update"),
                             
                             h4("\nEstimated distance of course:"),
                             verbatimTextOutput("summary_text")
               ),
 
               ## where our data come from and copyright?
               tags$div(id="cite",
                        'Data compiled for ', tags$em('New York Opensource Data'), ' until 2016'
               )
           )
  ),

  tabPanel(tags$em("Factor Exploration"),
           tags$div(class='beauty',
                    div(class="outer",
                        leafletOutput("map2", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Path Information"),
                                      h4("The following is detailed information on your path."),
                                      
                                      h4("To view in greater detail, please select the following:"),
                                      checkboxInput("SP_TR", "Slope and Trees"),
                                      numericInput("tree_pre", label = "   Trees:", min=1, max=100, value=50),
                                      numericInput("slope_pre", label = "   Flatness:", min=1, max=100, value=50),
                                      checkboxInput("RP_layer_all","Restroom"),
                                      checkboxInput("FO_layer_all", "Fountain"),
                                      
                                      submitButton("Update")
                                      )
                    )
           )
  ),
  
  # tabPanel(tag$em("Data Sets"),
  #          tags$div(class='beauty',
  #                   fluidRow(
  #                     selectInput("dataset","Please choose a dataset to display.", dataset_selection)
  #                   ),
  #                   
  #                   fluidRow(
  #                     DT::dataTableOutput("table")
  #                   )
  #           )
  # 
  # ),
  
  tabPanel(tags$em("About"),
           tags$div(class='beauty',
                    tags$h2("Our Team Members",class='trims'),
                    tags$br(),
                    tags$img(tags$ul( class = 'ulb',
                                      tags$li(tags$span("Rong Li",class = 'forname'),class ='ulb'),
                                      tags$li(tags$span('Alex Saez',class = 'forname'),class ='ulb'),
                                      tags$li(tags$span('Catherine Zhao',class ='forname'),class ='ulb'),
                                      tags$li(tags$span('Huilong An',class = 'forname'),class ='ulb'),
                                      tags$li(tags$span('Ying Zhu',class ='forname'),class ='ulb'),
                                      tags$br()
                    ),
                    src='http://orig04.deviantart.net/4c19/f/2013/358/c/f/coloured_sm3dw_character_icon_stamps_by_geno2925-d6z9hv6.png', width = 'AUTO',
                    height = 'AUTO', class = 'fixed',align = 'center'),
                    tags$br(),
                    tags$hr(),
                    tags$h2('About Our App',class='trims'),
                    tags$p("This application is designed for New York joggers to explore their
                           jogging path. Itâs based on New York Open Data and allows users to 
                           customize their preferences on trees, slopes, fountains and restrooms. 
                           Furthermore, after input start location, the application has two modes
                           for exploration process. One is to input the exact destination and the
                           other one is to input the expect distance between destination and start 
                           location. The app will automatically show the optimal jogging path according 
                           to userâs preference along with the detail information of this path.",class='trimsP'),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br()
           )
           #conditionalPanel("false", icon("crosshair"))
  )
    
)
)
