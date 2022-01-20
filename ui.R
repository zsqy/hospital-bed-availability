# Authors:
#   Wong Hui Yeok (S2124360)
#   Jonathan Kiew Weng Kiat (S2043163)
#   Ng Boon Jane (S2117897)
#   Hong Zi Shen (S2114600)
# Title: Analysis on Hospital Beds Availability for Covid-19 Treatment in Malaysia
# Class: WQD 7001 Principle of Data Science
# Date:  19/12/21

# install.packages(c("leaflet", "shinythemes", "plotly", "DT", "rintrojs"))
library(leaflet)
library(shinythemes)
library(plotly)
library(DT)
library(rintrojs)

# Read CSV files
gps_df <- read.csv('./Data Cleaning & Generation/hospitals_C19_cleaned.csv')

# Get all the states in Malaysia
state <- append('All', gps_df$state)

# Define UI for application
ui <- fluidPage(
  introjsUI(),
  theme=shinytheme("flatly"),
  
  # Application title
  h4("Hospital Beds On Spot APP", style="font-weight:bold; margin-top:20px; display:inline-block;"),
  actionButton("do", "About", style="float:right; margin-top:15px; padding:5px; background-color: hsl(223deg 46% 41%); border-color: hsl(223deg 46% 41%);"),
  actionButton("tour","Start Tour", style="float:right; margin-top:15px; padding:5px; margin-right:5px; background-color: hsl(223deg 46% 41%); border-color: hsl(223deg 46% 41%);"),
  sidebarLayout(
    # inputs UI
    sidebarPanel(
      introBox(
        selectInput("state", "State", state, "All"),
        data.step = 1,
        data.intro = "Select to filter by state."
      ),
      
      introBox(
        selectInput("hospital", "Hospital", c(""), "All"),
        data.step = 2,
        data.intro = "Select to filter by hospital (Hospital choices is affected by state input)."
      ),
      style = "margin-top: 15px;",
      width=3
    ),
    # outputs UI
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
        tabPanel("Map",
           br(),
           introBox(
             leafletOutput("map", height = '600px'),
             data.step = 3,
             data.intro = "Hospitals location and information showing in each individual pinpoint on a map"
           )
        ),
        tabPanel("Table", 
          br(),
          introBox(
            dataTableOutput('table', height='600px'),
            data.step = 4,
            data.intro = "Hospital information showing in a 2-dimensional row and column format"
          )
          
        ),
        tabPanel("Graph",
          br(),
          introBox(
            plotlyOutput('graph', height = '600px'),
            data.step = 5,
            data.intro = "Hospital information are plotted in line graph format"
          ),
          style = "overflow-y:scroll; overflow-x: hidden; max-height: 1000px;",
        ),
      ),
      style = "margin-top: 15px;",
      width = 9
    )
  )
)
