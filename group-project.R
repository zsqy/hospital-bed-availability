#
# Authors:
#   Wong Hui Yeok (S2124360)
#   Jonathan Kiew Weng Kiat (S2043163)
#   Ng Boon Jane (S2117897)
#   Hong Zi Shen (S2114600)
# Title: Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia
# Class: WQD 7001 Principle of Data Science
# Date:  13/12/21
#

# install.packages(c("leaflet", "sp", "shinyTime"))
library(data.table)
library(dplyr)
library(leaflet)
library(lubridate)
library(shinyTime)
library(sp)

# source("hospital-gps.R")
gps <- list(
  c(latitude = 6.441467153730291, longitude = 100.19081223007383, hospital = "Hospital Tuanku Fauziah"),
  c(latitude = 5.677899687233473, longitude = 100.9258379583245, hospital = "Hospital Baling"),
  c(latitude = 5.669773866245674, longitude = 100.5174146006331, hospital = "Hospital Sultan Abdul Halim"),
  c(latitude = 6.279020409659067, longitude = 100.41905477202778, hospital = "Hospital Jitra"),
  c(latitude = 5.393302292730488, longitude = 100.57331525114424, hospital = "Hospital Kulim"),
  c(latitude = 6.252800628635039, longitude = 100.61035085984904, hospital = "Hospital Kuala Nerang"),
  c(latitude = 5.812390367867593, longitude = 100.72762170421478, hospital = "Hospital Sik"),
  c(latitude = 5.820792440387709, longitude = 100.38480088011063, hospital = "Hospital Yan"),
  c(latitude = 6.149101834514572, longitude = 100.40678119828902, hospital = "Hospital Sultanah Bahiyah"),
  c(latitude = 6.32497139707105, longitude = 99.79868826478894, hospital = "Hospital Sultanah Maliha"),
  c(latitude = 5.512828036754378, longitude = 100.43708788650797, hospital = "Hospital Kepala Batas"),
  c(latitude = 5.417197864956217, longitude = 100.31193805643447, hospital = "Hospital Pulau Pinang"),
  c(latitude = 4.604068809478814, longitude = 101.09101318172394, hospital = "Hospital Raja Permaisuri Bainun"),
  c(latitude = 3.8389609697464504, longitude = 101.40555763970671, hospital = "Hospital Slim River"),
  c(latitude = 4.004797540596965, longitude = 101.04077391306777, hospital = "Hospital Teluk Intan"),
  c(latitude = 4.185777296832183, longitude = 100.66245810419609, hospital = "Hospital Seri Manjung"),
  c(latitude = 4.851286400238309, longitude = 100.73715188011148, hospital = "Hospital Taiping"),
  c(latitude = 3.242813137451986, longitude = 101.64647965778487, hospital = "Hospital Selayang"),
  c(latitude = 2.9930999929382853, longitude = 101.79339297291864, hospital = "Hospital Kajang"),
  c(latitude = 3.128489228980603, longitude = 101.76440581102587, hospital = "Hospital Ampang"),
  c(latitude = 3.0205060151088947, longitude = 101.44148153074154, hospital = "Hospital Tengku Ampuan Rahimah"),
  c(latitude = 2.803011719100442, longitude = 101.4943106994253, hospital = "Hospital Banting"),
  c(latitude = 3.0742834259850693, longitude = 101.49894400275093, hospital = "Hospital Shah Alam"),
  c(latitude = 2.97613812815372, longitude = 101.7202103653586, hospital = "Hospital Serdang"),
  c(latitude = 3.220440272542542, longitude = 101.58348343052883, hospital = "Hospital Sungai Buloh"),
  c(latitude = 3.1057422694886188, longitude = 101.7289613975053, hospital = "Hospital Rehabilitasi Cheras"),
  c(latitude = 3.209115685639382, longitude = 101.73971618161254, hospital = "Hospital Angkatan Tentera Tuanku Mizan"),
  c(latitude = 3.1717182051312878, longitude = 101.70310000170338, hospital = "Hospital Kuala Lumpur"),
  c(latitude = 3.1139335673528565, longitude = 101.65340083070319, hospital = "Pusat Perubatan Universiti Malaya"),
  c(latitude = 3.099438404820198, longitude = 101.72590605726359, hospital = "Hospital Canselor Tunku Mukhriz"),
  c(latitude = 2.9292779056554643, longitude = 101.67490370616383, hospital = "Hospital Putrajaya"),
  c(latitude = 2.484968933606015, longitude = 102.23389056421593, hospital = "Hospital Tampin"),
  c(latitude = 2.950586588222094, longitude = 102.0590862637491, hospital = "Hospital Jelebu"),
  c(latitude = 2.9033824056014215, longitude = 102.40750985935269, hospital = "Hospital Jempol"),
  c(latitude = 2.710172771177003, longitude = 101.94325276166603, hospital = "Hospital Tuanku Jaafar"),
  c(latitude = 2.2173956085736686, longitude = 102.26159249299351, hospital = "Hospital Melaka"),
  c(latitude = 1.8377411869161429, longitude = 102.94125429305626, hospital = "Hospital Sultanah Nora Ismail"),
  c(latitude = 1.499673393848159, longitude = 103.74246778065739, hospital = "Hospital Sultanah Aminah"),
  c(latitude = 1.6399388369240449, longitude = 103.62458076363743, hospital = "Hospital Temenggong Seri Maharaja Tun Ibrahim"),
  c(latitude = 1.5464772462644758, longitude = 103.79147335918626, hospital = "Hospital Sultan Ismail"),
  c(latitude = 1.7352191231804732, longitude = 103.90009791712336, hospital = "Hospital Kota Tinggi"),
  c(latitude = 2.4296137836869116, longitude = 103.84586767052292, hospital = "Hospital Mersing"),
  c(latitude = 2.057908420300772, longitude = 102.57394134462001, hospital = "Hospital Pakar Sultanah Fatimah"),
  c(latitude = 2.2722564626593673, longitude = 102.54767751040605, hospital = "Hospital Tangkak"),
  c(latitude = 1.4986419251301173, longitude = 103.385883019182, hospital = "Hospital Pontian"),
  c(latitude = 2.4934084381062602, longitude = 102.85972839034125, hospital = "Hospital Segamat"),
  c(latitude = 1.5216242230497232, longitude = 103.70713191477027, hospital = "Hospital Permai"),
  c(latitude = 2.0068590855500537, longitude = 103.34847411917463, hospital = "Hospital Enche Besar Hajjah Khalsom"),
  c(latitude = 3.525422778218594, longitude = 101.90597504174038, hospital = "Hospital Bentong"),
  c(latitude = 4.466945331310232, longitude = 101.39190627734295, hospital = "Hospital Sultanah Hajjah Kalsom"),
  c(latitude = 3.7732984813298867, longitude = 102.55260772614827, hospital = "Hospital Jengka"),
  c(latitude = 3.5024084079747637, longitude = 103.38066810162107, hospital = "Hospital Pekan"),
  c(latitude = 3.810108890355235, longitude = 101.85202851740424, hospital = "Hospital Raub"),
  c(latitude = 3.0539705120377563, longitude = 103.09149545943097, hospital = "Hospital Muadzam Shah"),
  c(latitude = 3.801035050448373, longitude = 103.32212634599918, hospital = "Hospital Tengku Ampuan Afzan"),
  c(latitude = 3.4533973428143887, longitude = 102.45394777941193, hospital = "Hospital Sultan Haji Ahmad Shah"),
  c(latitude = 4.182743695577721, longitude = 102.05621431722038, hospital = "Hospital Kuala Lipis"),
  c(latitude = 5.51433039451956, longitude = 102.7700746021463, hospital = "Hospital Setiu"),
  c(latitude = 4.752058053463764, longitude = 103.41567928188674, hospital = "Hospital Dungun"),
  c(latitude = 5.324055545715528, longitude = 103.15128524627885, hospital = "Hospital Sultanah Nur Zahirah"),
  c(latitude = 5.073430027672127, longitude = 103.04512875339732, hospital = "Hospital Hulu Terengganu"),
  c(latitude = 5.701396848097475, longitude = 101.84463369525804, hospital = "Hospital Jeli"),
  c(latitude = 5.763316984188483, longitude = 102.2264843617636, hospital = "Hospital Machang"),
  c(latitude = 6.014185115690029, longitude = 102.11938949788426, hospital = "Hospital Pasir Mas"),
  c(latitude = 6.100120440814965, longitude = 102.28299103745388, hospital = "Hospital Universiti Sains Malaysia"),
  c(latitude = 6.125448300829001, longitude = 102.24659176865282, hospital = "Hospital Raja Perempuan Zainab II"),
  c(latitude = 6.132552571408707, longitude = 102.23754738562904, hospital = "Hospital Sultan Ismail Petra"),
  c(latitude = 6.18989868308538, longitude = 102.15784054425748, hospital = "Hospital Tumpat"),
  c(latitude = 5.536091664071429, longitude = 102.20004481332946, hospital = "Hospital Kuala Krai Lama"),
  c(latitude = 5.810172542942461, longitude = 102.15281682879541, hospital = "Hospital Tanah Merah"),
  c(latitude = 6.457529002028379, longitude = 116.77608507378457, hospital = "Hospital Kota Marudu"),
  c(latitude = 6.906801148904837, longitude = 116.8386937219192, hospital = "Hospital Kudat"),
  c(latitude = 4.4830220190063095, longitude = 118.60530618188055, hospital = "Hospital Semporna"),
  c(latitude = 5.966739734630111, longitude = 116.09481049159838, hospital = "Hospital Queen Elizabeth"),
  c(latitude = 6.013963935879575, longitude = 116.12004707312289, hospital = "Hospital Wanita dan Kanak-kanak Likas"),
  c(latitude = 4.249945183802013, longitude = 117.88141881937585, hospital = "Hospital Tawau"),
  c(latitude = 5.026870378225761, longitude = 118.31454263971735, hospital = "Hospital Lahad Datu"),
  c(latitude = 5.859092434863604, longitude = 118.10364254849151, hospital = "Hospital Duchess of Kent"),
  c(latitude = 5.370352007689071, longitude = 116.18231758236777, hospital = "Hospital Keningau"),
  c(latitude = 1.4296292034524716, longitude = 110.1431904725179, hospital = "Hospital Bau"),
  c(latitude = 1.3741537097231271, longitude = 111.57426032815104, hospital = "Hospital Betong"),
  c(latitude = 2.745399076534325, longitude = 111.94632035084669, hospital = "Hospital Dalat"),
  c(latitude = 2.5219791910093026, longitude = 111.41351022403411, hospital = "Hospital Daro"),
  c(latitude = 2.1029725285053305, longitude = 112.15290398825871, hospital = "Hospital Kanowit"),
  c(latitude = 2.0140367516013415, longitude = 112.94333161923089, hospital = "Hospital Kapit"),
  c(latitude = 4.854555046738444, longitude = 115.40505214195052, hospital = "Hospital Lawas"),
  c(latitude = 4.764434251159114, longitude = 115.01963761747156, hospital = "Hospital Limbang"),
  c(latitude = 1.6757757096233998, longitude = 109.85310987958567, hospital = "Hospital Lundu"),
  c(latitude = 4.1785685961627825, longitude = 114.32619135282376, hospital = "Hospital Marudi"),
  c(latitude = 2.9010448964463893, longitude = 112.07697437505374, hospital = "Hospital Mukah"),
  c(latitude = 1.7473358262219312, longitude = 111.34028793041061, hospital = "Hospital Saratok"),
  c(latitude = 2.132135174611575, longitude = 111.49248449931738, hospital = "Hospital Sarikei"),
  c(latitude = 1.1763945167802925, longitude = 110.56425353241372, hospital = "Hospital Serian"),
  c(latitude = 1.3701972583174549, longitude = 110.78687677468496, hospital = "Hospital Simunjan"),
  c(latitude = 1.2345838986992868, longitude = 111.46327862391054, hospital = "Hospital Sri Aman"),
  c(latitude = 1.5438818209331289, longitude = 110.34044038915657, hospital = "Hospital Umum Sarawak"),
  c(latitude = 4.37454624830287, longitude = 114.00043467522188, hospital = "Hospital Miri"),
  c(latitude = 3.2299099301703458, longitude = 113.10156654405424, hospital = "Hospital Bintulu"),
  c(latitude = 2.2966103591491596, longitude = 111.89218940845167, hospital = "Hospital Sibu"),
  c(latitude = 5.311294547248574, longitude = 115.23097267075373, hospital = "Hospital Labuan")
)
gps_df <- as.data.frame(do.call(rbind, gps))

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

# source("occupancy.R")
occupancy <- list(
  "All" = "All",
  "100%" = "full",
  "90%-99%" = "very full",
  "80%-89%" = "quite full",
  "< 80%" = "not full"
)

# Define UI for application
ui <- fluidPage(
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$state, {
    x <<- input$state
    # read csv
    df <- read.csv("https://raw.githubusercontent.com/HuiYeok1107/HospitalsCapacity/master/hospitals_occupancy.csv?token=AL5ZPSBUXMKN6BKUZEQDLKTBYARGI")

    # filter state
    if (input$state != "All") {
      df <- filter(df, state == input$state)
    }

    # filter date
    # date <- paste(input$date, paste(hour(input$time), minute(input$time), sep = ":"))
    # df <- filter(df, as.POSIXct(datetime_sixMonths) <= as.POSIXct(date))

    # get only latest stats
    df <- filter(df, datetime_sixMonths == max(datetime_sixMonths))

    updateSelectInput(session, "hospital", choices = c("All", df$hospital))
  })
  output$map <- renderLeaflet({
    # read csv
    df <- read.csv("https://raw.githubusercontent.com/HuiYeok1107/HospitalsCapacity/master/hospitals_occupancy.csv?token=AL5ZPSBUXMKN6BKUZEQDLKTBYARGI")

    # filter state
    if (input$state != "All") {
      df <- filter(df, state == input$state)
    }

    # filter date
    # date <- paste(input$date, paste(hour(input$time), minute(input$time), sep = ":"))
    # df <- filter(df, as.POSIXct(datetime_sixMonths) <= as.POSIXct(date))

    # get only latest stats
    df <- filter(df, datetime_sixMonths == max(datetime_sixMonths))

    # filter hospital
    if (input$hospital != "All") {
      df <- filter(df, hospital == input$hospital)
    }

    # add a ratio column
    df <- mutate(df, ratio = occupancy / allocated_beds)

    # filter occupancy
    # if (input$occupancy != "All") {
    #   df <- switch(input$occupancy,
    #     "full" = filter(df, ratio == 1),
    #     "very full" = filter(df, (ratio >= 0.90) & (ratio < 1)),
    #     "quite full" = filter(df, (ratio >= 0.80) & (ratio < 0.90)),
    #     "not full" = filter(df, ratio < 0.80),
    #   )
    # }

    # merge df and gps_df
    df <- merge(df, gps_df, by = "hospital")
    df$latitude <- as.numeric(df$latitude)
    df$longitude <- as.numeric(df$longitude)

    # create icons
    icon.black <- makeAwesomeIcon(icon = "bed", markerColor = "black", library = "fa")
    icon.red <- makeAwesomeIcon(icon = "bed", markerColor = "red", library = "fa")
    icon.orange <- makeAwesomeIcon(icon = "bed", markerColor = "orange", library = "fa")
    icon.green <- makeAwesomeIcon(icon = "bed", markerColor = "green", library = "fa")

    # define colors according to ratio
    df_black <- filter(df, ratio == 1)
    df_red <- filter(df, (ratio >= 0.90) & (ratio < 1))
    df_orange <- filter(df, (ratio >= 0.80) & (ratio < 0.90))
    df_green <- filter(df, ratio < 0.80)

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
        labels = c("100%", "90%-99%", "80%-89%", "< 80%"),
        opacity = 1,
        title = "Occupancy"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
