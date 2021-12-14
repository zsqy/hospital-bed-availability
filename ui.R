# source("state.R")
state <- list(
  "All" = "All",
  "Johor" = "Johor",
  "Kedah" = "Kedah",
  "Kelantan" = "Kelantan",
  "Melaka" = "Melaka",
  "Negeri Sembilan" = "Negeri Sembilan",
  "Pahang" = "Pahang",
  "Penang" = "Penang",
  "Perak" = "Perak",
  "Perlis" = "Perlis",
  "Putrajaya" = "Putrajaya",
  "Sabah" = "Sabah",
  "Sarawak" = "Sarawak",
  "Selangor" = "Selangor",
  "Terengganu" = "Terengganu",
  "WP KL" = "WP KL",
  "WP Labuan" = "WP Labuan"
)

# Define UI for application
ui <- fluidPage(
  theme=shinytheme("flatly"),
  
  # Application title
  h4("Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "State", state, "All"),
      selectInput("hospital", "Hospital", c(""), "All"),
      # selectInput("occupancy", "Occupayncy", occupancy, "All"),
      # dateInput("date", "Date"),
      # timeInput("time", "Time", seconds = FALSE, value = strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"))
    ),
    mainPanel(
      leafletOutput("map")
    ),
  )
)
