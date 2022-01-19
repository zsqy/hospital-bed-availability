library(readxl)
library(writexl)

# load hospitals list excel file
hospitals = read_excel('hospitals_C19_cleaned.xlsx')

# Generate data from x date to y date
Sys.setenv(TZ = "Asia/Kuala_Lumpur")
datetime_sixMonths = c(seq(from=as.POSIXct("2021-8-1 0:00", tz="Asia/Kuala_Lumpur"), to=as.POSIXct("2022-4-30 23:00", tz="Asia/Kuala_Lumpur"), by="hour"))

hospitals_datetime = data.frame()
for (i in 1:nrow(hospitals)){
  hospital_datetime = cbind(hospitals[i, ], datetime_sixMonths)
  hospitals_datetime = rbind(hospitals_datetime, hospital_datetime)
}

# Initialize hospitals occupancy to -1
hospitals_datetime$occupancy = -1

# generate hourly random hospitals occupancy +=20% based on previous hour occupancy
# generate occupancy for first row hospital 
allocated_beds_max = hospitals[hospitals$hospital == hospitals_datetime[1, 'hospital'], 'allocated_beds'] 
allocated_beds_min = round(allocated_beds_max/2)
hospitals_datetime[1, 'occupancy'] = round(runif(1, min=as.numeric(allocated_beds_min), max=as.numeric(allocated_beds_max)))
for (i in 2:nrow(hospitals_datetime)){
  
  if ((hospitals_datetime[i, 'hospital'] == hospitals_datetime[i-1, 'hospital']) != TRUE){
    allocated_beds_max = hospitals[hospitals$hospital == hospitals_datetime[i, 'hospital'], 'allocated_beds'] 
    allocated_beds_min = round(allocated_beds_max/2)
    hospitals_datetime[i, 'occupancy'] = round(runif(1, min=as.numeric(allocated_beds_min), max=as.numeric(allocated_beds_max)))
    print(hospitals_datetime[i, c('hospital', 'occupancy')])
  }
  else {
    last_hour_cap = hospitals_datetime[i-1, 'occupancy']
    print('last hour cap')
    print(hospitals_datetime[i-1, c('hospital', 'occupancy')])
    new_min = last_hour_cap - round(last_hour_cap * 0.1)
    new_max =  last_hour_cap + round(last_hour_cap * 0.2)
    if (new_max > hospitals[hospitals$hospital == hospitals_datetime[i, 'hospital'], 'allocated_beds']) {
      new_max = hospitals[hospitals$hospital == hospitals_datetime[i, 'hospital'], 'allocated_beds']
    }
    hospitals_datetime[i, 'occupancy'] = round(runif(1, min=as.numeric(new_min), max=as.numeric(new_max)))
  }
}

# sort from previous to latest datetime
hospitals_datetime = hospitals_datetime[order(hospitals_datetime$datetime_sixMonths),]
# reset index
row.names(hospitals_datetime) = NULL


# write.csv(hospitals_datetime, '.\\hospitals_occupancy.csv', row.names=FALSE)




