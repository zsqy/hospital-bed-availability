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

# Read CSV files
gps_df <- read.csv('hospitals_C19_cleaned.csv')

# Get all the states in Malaysia
state <- append('All', gps_df$state)

# Define UI for application
ui <- fluidPage(
  theme=shinytheme("flatly"),
  
  # Application title
  h4("Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia", style="font-weight:bold; margin-top:20px;"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "State", state, "All"),
      selectInput("hospital", "Hospital", c(""), "All"),
      style = "margin-top: 15px;"
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
        tabPanel("Map",
          leafletOutput("map"),
        ),
        tabPanel("Table", 
        ),
        tabPanel("Graph", 
          plotlyOutput('graph'),
          style = "overflow-y:scroll; overflow-x: hidden; max-height: 1000px;",
        ),
      ),
      style = "margin-top: 15px;"
    )
  )
)
