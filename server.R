# install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(rio)

# Read CSV files
gps_df <- read.csv('hospitals_C19_cleaned.csv')
# df_ori <- import("https://raw.githubusercontent.com/HuiYeok1107/HospitalsCapacity/master/hospitals_occupancy.csv?token=AL5ZPSBUXMKN6BKUZEQDLKTBYARGI")
df_ori <- read.csv('hospitals_occupancy.csv')

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$state, {
    # Get all hospitals list    
    hospitals_list <- gps_df$hospital
    # Get hospitals list based on the selected state
    if (input$state != "All") {
      hospitals_list <- gps_df[gps_df$state == input$state, 'hospital']
    }
    # Update hospitals drop-down list selection
    updateSelectInput(session, "hospital", choices = c("All", hospitals_list))
  })
  
  data <- reactive({
    # get latest stats
    df <- tail(df_ori, n=100)

    # filter state
    if (input$state != "All") {
      df <- filter(df, state == input$state)
    }

    # filter hospital
    if (input$hospital != "All") {
      df <- filter(df, hospital == input$hospital)
    }

    # add a ratio column
    df <- mutate(df, ratio = occupancy / allocated_beds)

    # merge df and gps_df
    df <- merge(df, gps_df[, c('longitude', 'latitude', 'hospital')], by = "hospital")
    df$latitude <- as.numeric(df$latitude)
    df$longitude <- as.numeric(df$longitude)

    # create icons
    icon.black <- makeAwesomeIcon(icon = "bed", markerColor = "black", library = "fa")
    icon.red <- makeAwesomeIcon(icon = "bed", markerColor = "red", library = "fa")
    icon.orange <- makeAwesomeIcon(icon = "bed", markerColor = "orange", library = "fa")
    icon.green <- makeAwesomeIcon(icon = "bed", markerColor = "green", library = "fa")

    # define colors according to ratio
    df_black <- filter(df, ratio == 1)
    df_red <- filter(df, (ratio >= 0.80) & (ratio < 1))
    df_orange <- filter(df, (ratio >= 0.60) & (ratio < 0.80))
    df_green <- filter(df, ratio < 0.60)

    # create leaflet map
    map <- leaflet() %>%
      addTiles() %>%
      # add black markers
      addAwesomeMarkers(
        lng = df_black$longitude,
        lat = df_black$latitude,
        label = paste(
          df_black$datetime_sixMonths, ": ",
          df_black$hospital, ": ",
          df_black$occupancy, "/", df_black$allocated_beds,
          " (", round(df_black$ratio * 100, 2), "%)",
          sep = ""
        ),
        labelOptions = labelOptions(textsize = "12px"),
        # labelOptions = if (nrow(df_black) < 5) labelOptions(noHide = T, ),
        icon = icon.black
      ) %>%
      # add red markers
      addAwesomeMarkers(
        lng = df_red$longitude,
        lat = df_red$latitude,
        label = paste(
          df_red$datetime_sixMonths, ": ",
          df_red$hospital, ": ",
          df_red$occupancy, "/", df_red$allocated_beds,
          " (", round(df_red$ratio * 100, 2), "%)",
          sep = ""
        ),
        labelOptions = labelOptions(textsize = "12px"),
        icon = icon.red
      ) %>%
      # add orange markers
      addAwesomeMarkers(
        lng = df_orange$longitude,
        lat = df_orange$latitude,
        label = paste(
          df_orange$datetime_sixMonths, ": ",
          df_orange$hospital, ": ",
          df_orange$occupancy, "/", df_orange$allocated_beds,
          " (", round(df_orange$ratio * 100, 2), "%)",
          sep = ""
        ),
        labelOptions = labelOptions(textsize = "12px"),
        icon = icon.orange
      ) %>%
      # add green markers
      addAwesomeMarkers(
        lng = df_green$longitude,
        lat = df_green$latitude,
        label = paste(
          df_green$datetime_sixMonths, ": ",
          df_green$hospital, ": ",
          df_green$occupancy, "/", df_green$allocated_beds,
          " (", round(df_green$ratio * 100, 2), "%)",
          sep = ""
        ),
        labelOptions = labelOptions(textsize = "12px"),
        icon = icon.green
      ) %>%
      # add legend
      addLegend(
        position = "bottomright",
        colors = c("black", "red", "orange", "green"),
        labels = c("Full", "80%-99%", "60%-79%", "Less than 60%"),
        opacity = 1,
        title = "Beds Occupancy"
      )

    # get number of occupied bed
    totalOccupied <- valueBox(
      paste(sum(df$occupancy), "/", sum(df$allocated_beds)),
      "Total Occupied Bed",
      color = "blue"
    )

    # get occupancy rate
    totalOccupancy <- valueBox(
      sprintf("%0.2f%%", sum(df$occupancy) / sum(df$allocated_beds) * 100),
      "Total Occupancy Rate",
      color = "blue"
    )

    # get number of black hospital
    totalBlack <- valueBox(
      count(df_black),
      "# Hospital Full",
      #icon = icon("bed"),
      color = "black"
    )

    # get number of red hospital
    totalRed <- valueBox(
      count(df_red),
      "# Hospital 80%-99%",
      color = "red"
    )

    # get number of orange hospital
    totalOrange <- valueBox(
      count(df_orange),
      "# Hospital 60%-79%",
      color = "orange"
    )

    # get number of green hospital
    totalGreen <- valueBox(
      count(df_green),
      "# Hospital Less Than 60%",
      color = "green"
    )

    return(list(
      "map" = map,
      "totalOccupied" = totalOccupied,
      "totalOccupancy" = totalOccupancy,
      "totalBlack" = totalBlack,
      "totalRed" = totalRed,
      "totalOrange" = totalOrange,
      "totalGreen" = totalGreen
      ))
  })

  output$map <- renderLeaflet({
    data()$map
  })

  output$totalOccupied <- renderInfoBox({
    data()$totalOccupied
  })

  output$averageOccupancy <- renderInfoBox({
    data()$totalOccupancy
  })

  output$totalBlack <- renderInfoBox({
    data()$totalBlack
  })

  output$totalRed <- renderInfoBox({
    data()$totalRed
  })

  output$totalOrange <- renderInfoBox({
    data()$totalOrange
  })
  
  output$totalGreen <- renderInfoBox({
    data()$totalGreen
  })
}

