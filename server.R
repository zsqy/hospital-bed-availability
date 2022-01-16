# install.packages(c("dplyr", "lubridate", "plotly"))
library(dplyr)
library(lubridate)
library(plotly)

# Read CSV files
gps_df <- read.csv('hospitals_C19_cleaned.csv')
df_ori <- read.csv('hospitals_occupancy.csv')

# Extract only the latest 6 months of data
currentDateTime = as.POSIXct(paste(as.Date(Sys.time()), hour(Sys.time()), ':', '0', sep=""), tz="Asia/Kuala_Lumpur")
start_month = month(currentDateTime) - month(5)
year = year(currentDateTime)
if (start_month < 0) {
  start_month = 12 + start_month
  year = year - 1
}
start_date = as.POSIXct((paste(year, '-', start_month, '-', '1', sep="")), tz="Asia/Kuala_Lumpur")
for (i in seq(1, nrow(df_ori), by=100)) {
  if (df_ori[i,'datetime_sixMonths'] == start_date){
    start_idx = i
    break
  }
}
for (i in seq(nrow(df_ori), 1, by=-100)) {
  if (df_ori[i,'datetime_sixMonths'] == currentDateTime){
    stop_idx = i
    break
  }
}
df_ori = df_ori[start_idx:stop_idx,]

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$do, {
    showModal(modalDialog(
      title = "Important message",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed convallis ac turpis non tempor. Proin vel velit at quam faucibus aliquet id vel eros. Proin at sem suscipit, sollicitudin nulla vel, malesuada ex. Nunc mattis vel dui vel varius. Nulla elementum sapien id posuere vulputate. Donec viverra placerat quam, a malesuada ante. Nullam imperdiet nec ex sed rhoncus. Nullam sapien velit, eleifend vitae aliquam vel, bibendum ut purus. Ut lacus massa, laoreet convallis tortor aliquam, auctor accumsan sem. Nam bibendum ac arcu in rhoncus.

Fusce ultricies, mauris in tempor mollis, lectus dui mattis risus, ut aliquet nunc nunc nec dolor. Proin mattis at odio a pulvinar. Suspendisse vitae bibendum libero. Maecenas vestibulum nunc id justo condimentum mollis. Quisque at tellus vel ipsum mollis commodo. Sed bibendum est sed porta suscipit. Quisque aliquam ultricies elit, a posuere velit tincidunt vel. Aenean at sapien ut nibh euismod pharetra. Curabitur nec velit elementum, consequat augue vel, luctus dui.

Sed eleifend finibus tincidunt. Donec venenatis vulputate risus, vel aliquam velit. Nullam luctus elit felis, eu pellentesque felis dapibus eu. Aliquam a dapibus felis. Quisque feugiat nibh a mauris tincidunt finibus eu id mi. Cras eleifend, diam lacinia convallis consequat, erat quam vestibulum magna, sed hendrerit est est et ipsum. Vivamus auctor id urna sit amet posuere. Nulla placerat eget mi sed elementum. Integer malesuada sagittis orci, vitae euismod arcu consequat sit amet. Nullam quis justo ut mauris luctus varius. Mauris dapibus tristique mi, ac accumsan lacus consectetur et.

Nunc gravida rhoncus aliquet. Mauris vitae iaculis nisl. Cras sodales mauris in mauris bibendum cursus. Morbi in porta enim. Sed pulvinar, dui ac eleifend posuere, ante justo rutrum odio, vitae malesuada erat dui et mi. Curabitur eget odio fermentum, tempor velit quis, tincidunt urna. Curabitur rhoncus enim nec convallis sollicitudin. Etiam hendrerit lacus et lacus laoreet iaculis. Vivamus vehicula scelerisque erat et ultricies. Interdum et malesuada fames ac ante ipsum primis in faucibus. Nam dui arcu, tempus id felis quis, vulputate tincidunt urna.

Integer ac dignissim ante. Vestibulum vel massa vel sem hendrerit consequat. Suspendisse dui ligula, condimentum non euismod nec, tristique non mauris. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Nulla sollicitudin magna non porttitor posuere. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Cras porta magna non massa porttitor, nec tempor dui auctor. Duis sit amet odio non arcu gravida condimentum ut ut ligula. Nunc eu mi tincidunt enim venenatis varius. Praesent dapibus justo eget turpis pharetra, sed sodales ligula tincidunt. Sed ut placerat leo. Donec pharetra dolor massa, sed posuere odio maximus vel. Praesent ultricies dolor a condimentum convallis.

Vestibulum aliquet ex vitae tempor sagittis. Duis id urna ac eros imperdiet eleifend at sed erat. Nulla elementum lacinia risus, in tempus dui sollicitudin id. Vestibulum sit amet orci eu nunc consequat bibendum vel a nulla. Nam fermentum aliquam nunc, eu interdum sem ullamcorper vitae. Vivamus id blandit erat. Aliquam magna nisl, pretium nec velit id, commodo hendrerit urna.

Morbi non sagittis ipsum. Nunc egestas lectus in leo viverra, vitae porta mauris pulvinar. Mauris aliquam dapibus libero sed pharetra. Sed nunc elit, viverra eget pretium eu, eleifend et ipsum. Donec ligula velit, pellentesque eu lacinia in, consequat nec odio. Aliquam neque sem, finibus non velit sed, pulvinar maximus tortor. Nunc purus ligula, porttitor et scelerisque vitae, rutrum et libero.

Etiam id interdum mauris, a facilisis nisi. Vivamus a augue sed lectus consequat consequat. Suspendisse malesuada nibh a leo rhoncus ullamcorper. Quisque magna est, aliquam non vulputate cursus, volutpat ac ante. Nullam hendrerit enim ex, sit amet tempor mi accumsan sit amet. Duis dapibus tristique ultrices. Pellentesque volutpat sem enim, fermentum pretium lectus mollis nec. Integer sodales placerat ex, vel congue justo pharetra quis. Pellentesque interdum scelerisque urna, et semper justo convallis vel. Nullam eu viverra ligula.

Integer mattis enim mauris, quis ornare tellus tincidunt ut. Vestibulum sit amet ultrices leo. Duis maximus hendrerit diam, et fermentum augue congue vitae. Sed ante dolor, auctor et fermentum nec, tincidunt id purus. Etiam tempor dignissim urna ut tincidunt. Etiam elementum erat id porta elementum. Vivamus quis enim erat. In dui ex, vulputate elementum maximus ut, hendrerit quis massa. Praesent pulvinar, neque vitae aliquam molestie, odio nunc vestibulum tortor, nec sollicitudin nisi nulla eu mauris. Suspendisse interdum fringilla bibendum. Mauris varius aliquet pretium. Nunc in fermentum diam, ac scelerisque est. In neque nulla, tristique consequat orci at, pharetra tincidunt mi. Aenean convallis, neque et hendrerit ornare, arcu orci auctor lectus, ut pretium enim urna eu erat. Maecenas suscipit aliquam neque sed cursus.

Sed euismod felis mi, venenatis faucibus neque efficitur sit amet. Fusce tempus arcu non faucibus fermentum. Vivamus condimentum quam nibh, et sagittis sem elementum et. Nulla eu dapibus justo. Vivamus tincidunt, sapien nec placerat condimentum, mi quam faucibus ex, sodales ornare lorem augue ut leo. Praesent finibus, enim nec mattis imperdiet, elit augue ornare metus, viverra ornare sapien elit ac ipsum. Nunc id risus ut risus rhoncus semper. Aliquam erat volutpat. Curabitur facilisis dictum cursus. Donec iaculis efficitur commodo.

"
    ))
  })
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
  output$map <- renderLeaflet({
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
    leaflet() %>%
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
  })
  
  # reactive pre processing
  my_df <- reactive({
    df <- tail(df_ori, n=100)
    results <- df
  
    # filter state
    if (input$state == "All") {
      results 
    } 
    else{
      results <- filter(df, state == input$state)
    }
    
    #filter hospital 
    if (input$hospital == "All") {
      results 
    } 
    else{
      results <- filter(results, hospital == input$hospital)
    }
    
    results %>% 
      select(state, hospital, allocated_beds, occupancy) %>% 
      mutate(spaces_available=allocated_beds-occupancy) %>% 
      arrange(desc(spaces_available))
  
  })
  output$table <- renderDataTable({
    my_df()},
    caption = 'Table 1: Records of hospital availability.',
    extensions = c("Buttons"),
    options = list(
      autoWidth=FALSE,
      pageLength = 10,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = "csv", text = "Download Current Page", filename = "page",
             exportOptions = list(
               modifier = list(page = "current")
             )
        ),
        list(extend = "csv", text = "Download Full Results", filename = "data",
             exportOptions = list(
               modifier = list(page = "all")
             )
        )
      )
    )
    

  )
  
  output$graph <- renderPlotly({
    if (input$state != "All" | (input$state == "All" & input$hospital != "All")) {
      # all hospitals from a single state is selected
      if (input$hospital == "All") {
        state_hospitals <- df_ori[df_ori$state == input$state, ]
        hospitals <- unique(state_hospitals$hospital)
      } else {
        # only a single hospital is selected
        state_hospitals <- df_ori[,]
        hospitals <- input$hospital
      }
      # plots variables initialization
      hospitals_figs <- list()
      plotCount <- 1
      annotations <- list()
      nRows <- if (length(hospitals) <= 2) length(hospitals) else ceiling(length(hospitals)/2)
      
      for (hospital in hospitals){
        # calculate monthly average occupancy (%) of each hospital
        df <- state_hospitals[state_hospitals$hospital == hospital,]
        df <- df[strftime(strptime(df$datetime_sixMonths, "%Y-%m-%e %H:%M"), "%H")=="23",]
        agg_df <- aggregate(df$occupancy, by=list(year(df$datetime_sixMonths), month(df$datetime_sixMonths)), FUN=length)
        agg_df$occ_sum <- aggregate(df$occupancy, by=list(month(df$datetime_sixMonths)), FUN=sum)$x
        agg_df <- mutate(agg_df, percentage = (occ_sum / x)/unique(df$allocated_beds) * 100)
        colnames(agg_df) <- c('year','month_num','day_len', 'occ_sum', 'percentage')
        agg_df <- agg_df[order(agg_df$year, agg_df$month_num),] 
        agg_df$date <- as.Date(paste(agg_df$year, agg_df$month_num, 1, sep = "/")) 
        
        # plot line graph 
        fig1 <- plot_ly(agg_df[, c('year', 'month_num', 'percentage', 'date')], 
                        x=~date, y=~percentage, 
                        height=round((plotCount+1)/2)*300, type='scatter', 
                        mode='lines+markers', line=list(color='#000000', width = 1), 
                        hoverinfo='text',
                        text = ~paste('</br> Date: ', month.abb[month_num], year,
                                      '</br> Beds Occupancy (%): ', sprintf(percentage, fmt = '%#.2f'), '%')) %>%
          layout(title=hospital,
                 xaxis=list(title=list(text="Date", standoff=10), type="date", tickformat="%b<br>%Y"),
                 yaxis=list(title="Monthly Average <br> Beds Occupancy (%)"))
        hospitals_figs[[plotCount]] <- fig1
        
        # set plot title position and details
        annotationsGapY <- list()
        annotationsGapY[[1]] <- 1
        annotationsGapY[[2]] <- 0.375
        annotationsGapY[[3]] <- 0.245
        annotationsGapY[[4]] <- 0.175
        annotationsGapY[[5]] <- 0.14
        annotationsGapY[[6]] <- 0.11
        annotationsGapY[[10]] <- 0.065
        
        currentRow = ceiling(plotCount/2)
        yPos = if (nRows == 1)
          annotationsGapY[[1]]
        else if (length(hospitals) == 2 &&
                 plotCount == 2)
          annotationsGapY[[2]]
        else
          1 - (((1 - annotationsGapY[[nRows]]) / (nRows - 1)) * (currentRow - 1))
        
        annotations[[plotCount]] <- list(
          text = paste("<b>",hospital,"</b>"),
          x = if(length(hospitals) <= 2) 0.5 else if(plotCount%%2==1) 0.2 else 0.8,
          y = yPos,
          font = list(size=13),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
        plotCount = plotCount + 1
      }
      
      heightsRatios <- list()
      heightsRatios[[1]] <- c(1)
      heightsRatios[[2]] <- c(.5,.5)
      heightsRatios[[3]] <- c(.32,.36,.32)
      heightsRatios[[4]] <- c(.23,.27,.27,.23)
      heightsRatios[[5]] <- c(.1775,.215,.215,.215,.1775)
      heightsRatios[[6]] <- c(.14,.18,.18,.18,.18,.14)
      heightsRatios[[10]] <-  c(.08375, .1040625, .1040625, .1040625, .1040625, .1040625, .1040625, .1040625, .1040625, .08375)
      
      yMargins <- list()
      yMargins[[1]] <- 1
      yMargins[[2]] <- 0.12
      yMargins[[3]] <- 0.07
      yMargins[[4]] <- 0.05
      yMargins[[5]] <- 0.04
      yMargins[[6]] <- 0.035
      yMargins[[10]] <- 0.02
      
      # combine all the plots in a single canvas
      fig <- subplot(hospitals_figs, titleX= TRUE, titleY = TRUE, nrows=nRows, margin=c(0.08,0.08,yMargins[[nRows]],yMargins[[nRows]]), heights=heightsRatios[[nRows]])
      fig <- fig %>%layout(margin = list(l=0, r=0, b=80, t=85, pad=0), title = list(text="<b style='font-style: oblique;'>Hospitals Beds Occupancy Trend By Hospital</b>", xanchor="middle", yanchor="middle", pad = list(t=20, b = 5000, l = 0, r = 0 )),
                           font = list(family = 'Arial', size = 11), 
                           plot_bgcolor='#e5ecf6',
                           showlegend=FALSE, annotations= annotations)
      fig$sizingPolicy$padding <- "0"
      fig
      
    } else {
      # find the monthly average occupancy in each hospital
      df <- df_ori[strftime(strptime(df_ori$datetime_sixMonths, "%Y-%m-%e %H:%M"), "%H")=="23",]
      agg_df <- aggregate(df$occupancy, by=list(df$state, df$hospital, year(df$datetime_sixMonths), month(df$datetime_sixMonths)), FUN=sum)
      agg_df$day_len <- aggregate(df$occupancy, by=list(df$hospital, month(df$datetime_sixMonths)), FUN=length)$x
      colnames(agg_df) <- c('state','hospital','year', 'month_num', 'occ_sum', 'day_len')
      agg_df <- mutate(agg_df, avg_occ = round(occ_sum / day_len))
      
      # sum up the monthly average occupancy in each hospital by state
      agg_df2 <- aggregate(agg_df$avg_occ, by=list(agg_df$state, agg_df$month_num, agg_df$year), FUN=sum)
      colnames(agg_df2) <- c('state', 'month_num', 'year', 'sum_avg_hosps_occ')
      
      # sum up the total allocated beds in each state
      total_allocated_beds_state <- aggregate(gps_df$allocated_beds, by=list(gps_df$state), FUN=sum)
      colnames(total_allocated_beds_state) <- c('state', 'total_alloc_beds')
      
      # find the hospitals beds occupancy in each state and month
      agg_df2 <- merge(agg_df2, total_allocated_beds_state, by="state")
      agg_df2 <- mutate(agg_df2, percentage = (sum_avg_hosps_occ/total_alloc_beds) * 100)

      # plots variables initialization
      states_figs <- list()
      plotCount <- 1
      annotations <- list()
      states <- unique(agg_df2$state)
      nRows <- ceiling(length(states)/2)
      
      for (state in states) {
        # filter a single state data
        state_df <- agg_df2[agg_df2$state==state, ]
        state_df <- state_df[order(state_df$year, state_df$month_num), ]
        state_df$date <- as.Date(paste(state_df$year, state_df$month_num, 1, sep = "/")) 
        
        # plot line graph
        fig1 <- plot_ly(state_df[, c('month_num', 'year', 'percentage', 'date')],
                        x=~date, y=~percentage,
                        height=round((plotCount+1)/2)*300, type='scatter',
                        mode='lines+markers', line=list(color='#000000', width = 1),
                        hoverinfo='text',
                        text = ~paste('</br> Date: ', month.abb[month_num], year,
                                      '</br> Beds Occupancy (%): ', sprintf(percentage, fmt = '%#.2f'), '%')) %>%
          layout(title=state,
                 xaxis=list(title=list(text="Months", standoff=10), type="date", tickformat="%b<br>%Y"),
                 yaxis=list(title="Monthly Average <br> Beds Occupancy (%)"))
        states_figs[[plotCount]] <- fig1
        
        # set plot title position and details
        currentRow = ceiling(plotCount/2)
        yPos = if (nRows == 1)
          1
        else
          1 - (((1 - 0.075) / (nRows - 1)) * (currentRow - 1))
        
        annotations[[plotCount]] <- list(
          text = paste("<b>",state,"</b>"),
          x = if(plotCount%%2==1) 0.2 else 0.8,
          y = yPos,
          font = list(size=13),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
        plotCount <- plotCount + 1
      }
      
      # combine all the plots in a single canvas
      fig <- subplot(states_figs, titleX= TRUE, titleY = TRUE, nrows = nRows, margin=c(0.08,0.08,0.0275,0.0275), heights=c(0.1046, 0.1318, 0.1318, 0.1318, 0.1318, 0.1318, 0.1318, 0.1046))
      fig <- fig %>%layout(margin = list(l=0, r=0, b=80, t=85, pad=0), title = list(text="<b style='font-style: oblique;'>Hospitals Beds Occupancy Trend By State</b>", xanchor="middle", yanchor="middle", pad = list(t=20, b = 5000, l = 0, r = 0 )),
                           font = list(family = 'Arial', size = 11),
                           plot_bgcolor='#e8e8e8',
                           showlegend=FALSE, annotations=annotations)
      fig$sizingPolicy$padding <- "0"
      fig

    }

  })
}