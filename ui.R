# Authors:
#   Wong Hui Yeok (S2124360)
#   Jonathan Kiew Weng Kiat (S2043163)
#   Ng Boon Jane (S2117897)
#   Hong Zi Shen (S2114600)
# Title: Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia
# Class: WQD 7001 Principle of Data Science
# Date:  19/12/21

# install.packages(c("leaflet", "shinythemes", "rio", "shinydashboard"))
library(leaflet)
library(rio)
library(shinydashboard)
library(shinythemes)

# Read CSV files
gps_df <- read.csv('hospitals_C19_cleaned.csv')
df_ori <- import("https://raw.githubusercontent.com/HuiYeok1107/HospitalsCapacity/master/hospitals_occupancy.csv?token=AL5ZPSBUXMKN6BKUZEQDLKTBYARGI")

# Get all the states in Malaysia
state <- append('All', gps_df$state)

# Define UI for application
header <- dashboardHeader(
  title = "Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia",
  titleWidth = 600
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      ')
    )
  ),
  tabItems(
    tabItem(
      tabName = "dashboard",
      h2("Dashboard"),
      fluidRow(
        # tags$style(".fa-bed {color:#888888"),
        valueBoxOutput("totalOccupied", width = 2),
        valueBoxOutput("averageOccupancy", width = 2),
        valueBoxOutput("totalBlack", width = 2),
        valueBoxOutput("totalRed", width = 2),
        valueBoxOutput("totalOrange", width = 2),
        valueBoxOutput("totalGreen", width = 2)
      ),
      fluidRow(
        box(
          title = "Filters",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 3,
          selectInput("state", "State", state, "All"),
          selectInput("hospital", "Hospital", c(""), "All")
        ),
        box(
          title = "Statistics",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 9,
          tabBox(
            width = 12,
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
    ),
    tabItem(
      tabName = "widgets",
      h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)
