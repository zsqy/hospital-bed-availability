# Authors:
#   Wong Hui Yeok (S2124360)
#   Jonathan Kiew Weng Kiat (S2043163)
#   Ng Boon Jane (S2117897)
#   Hong Zi Shen (S2114600)
# Title: Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia
# Class: WQD 7001 Principle of Data Science
# Date:  19/12/21

# install.packages(c("leaflet", "shinythemes", "rio"))
library(leaflet)
library(shinythemes)
library(rio)

# Read CSV files
gps_df <- read.csv('hospitals_C19_cleaned.csv')
df_ori <- import("https://raw.githubusercontent.com/HuiYeok1107/HospitalsCapacity/master/hospitals_occupancy.csv?token=AL5ZPSBUXMKN6BKUZEQDLKTBYARGI")

# Get all the states in Malaysia
state <- append('All', gps_df$state)

# Define UI for application
ui <- fluidPage(
  theme=shinytheme("flatly"),
  
  # Application title
  h4("Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "State", state, "All"),
      selectInput("hospital", "Hospital", c(""), "All"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
          leafletOutput("map")
        ),
        tabPanel("Table", 
        ),
        tabPanel("Graph",
        )
      )
    )
  )
)
