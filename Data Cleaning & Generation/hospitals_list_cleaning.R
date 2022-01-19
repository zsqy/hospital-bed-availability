# install.packages("readxl")
# install.packages('writexl')
library(readxl)
library(writexl)

# read excel file
hospitals = read_excel('C:/Users/Owner/Documents/Master of Data Science/Principles of Data Science/Assignment/Assignment 2/list_of_hopitals_covid19_treatment.xlsx')

# extract rows 
hospitals = hospitals[185:nrow(hospitals),]

# rename columns
colnames(hospitals) = c('index', 'state', 'hospital', 'mode')

# drop NA rows
hospitals = hospitals[!(is.na(hospitals$hospital)),]

# drop index column
hospitals = subset(hospitals, select = -c(index))

# fill in missing state values with its respective state value
state_value = ""
for (i in 1:nrow(hospitals)){
  if (!is.na(hospitals[i,'state'])){
    state_value = hospitals[i,'state']
  } else {
    hospitals[i,'state'] = state_value
  }
}

# view dataframe
View(hospitals)

# save dataframe to excel file
# write_xlsx(hospitals, 'C:/Users/Owner/Documents/Master of Data Science/Principles of Data Science/Assignment/Assignment 2/hospitals_C19_cleaned.xlsx')
