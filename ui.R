# Authors:
#   Wong Hui Yeok (S2124360)
#   Jonathan Kiew Weng Kiat (S2043163)
#   Ng Boon Jane (S2117897)
#   Hong Zi Shen (S2114600)
# Title: Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia
# Class: WQD 7001 Principle of Data Science
# Date:  19/12/21

# install.packages(c("leaflet", "shinythemes", "plotly"))
library(leaflet)
library(shinythemes)
library(plotly)
library(rintrojs)
library(DT)

# Read CSV files
gps_df <- read.csv('hospitals_C19_cleaned.csv')

# Get all the states in Malaysia
state <- append('All', gps_df$state)

# Define UI for application
ui <- fluidPage(
  introjsUI(),
  theme=shinytheme("flatly"),
  
  # Application title
  h4("Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia", style="font-weight:bold; margin-top:20px;"),
  sidebarLayout(
    sidebarPanel(
      actionButton("tour","Start Tour", class = "btn btn-primary btn-lg"),
      introBox(
        selectInput("state", "State", state, "All"),
        data.step = 1,
        data.intro = "Enter the number of panels for your abstract. A set of controls for each panel is automatically generated."
      ),
    
      introBox(
        selectInput("hospital", "Hospital", c(""), "All"),
        data.step = 2,
        data.intro = "Enter the number of panels for your abstract. A set of controls for each panel is automatically generated."
      ),
      # selectInput("state", "State", state, "All"),
      # selectInput("hospital", "Hospital", c(""), "All"),
      style = "margin-top: 15px;",
      width=3
    ),
    mainPanel(
      tags$style(HTML("
        .nav-tabs>li>a {
           background-color: #000;
           color: #FFF;
        }
        .nav-tabs>li>a:hover {
           color: grey;
        }
        .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus {
           color: black;
           font-weight: bold;
        }")),
      tabsetPanel(
        type = "tabs",
        tabPanel("Map",
           introBox(
             leafletOutput(
                "map",
                height = '600px'
             ),
             data.step = 3,
             data.intro = "Took a look at this histogram"
           )

        ),
        tabPanel("Table", 
          br(),
          introBox(
            dataTableOutput(
              'table',
              height='600px'
            ),
            data.step = 4,
            data.intro = "Took a look at this histogram"
          )

          
        ),
        tabPanel("Graph", 
         introBox(
           plotlyOutput('graph',
                        height = '600px'
           ),
           data.step = 5,
           data.intro = "Took a look at this histogram"
         ),

          style = "overflow-y:scroll; overflow-x: hidden; max-height: 1000px;",
        ),
      ),
      style = "margin-top: 15px;",
      width = 9
    )
  )
)
