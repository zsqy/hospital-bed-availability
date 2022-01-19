# Analysis on Hospitals Beds Availability for Covid-19 Treatment in Malaysia

The ongoing global Covid-19 pandemic has caused the overwhelming capacity of hospitals in offering beds for Covid-19 treatment. The people infected with Covid-19 virus were struggling to look for hospital that still has beds for Covid-19 infected person. They have to spend their time to go or call hospital by hospital to ask whether the hospital still has beds available for Covid-19 treatment and thus unable to receive the immediate treatment they need. 

The data on the hourly beds availability of hospitals across different states are not disclosed by the government. Hence, to demonstrate the value of our data product, this data is self-generated using R. 

## Data Cleaning & Generation
1. Obtain the list of hospitals in Malaysia (in PDF form) that offer Covid-19 treatment from [Ministry of Health Website](https://covid-19.moh.gov.my/garis-panduan/garis-panduan-kkm/ANNEX_3_SENARAI_PUSAT_SARINGAN_COVID-19_DAN_HOSPITAL_MENGENDALIKAN_KES_COVID19_11062021.pdf) (refer to `list_of_hopitals_covid19_treatment.pdf`).
2. Convert the hospitals list in PDF form to excel file using online OCR tool (refer to `list_of_hopitals_covid19_treatment.xlsx`).
3. Clean the hospitals list in excel file using R (refer to `hospitals_list_cleaning.R`, `hospitals_C19_cleaned.csv`).
4. With the list of cleaned Malaysia hospitals data, generate hourly beds availability for all the 100 Malaysia hospitals in the past 6 months (refer to `hospitals_occupancy.csv`).


## Shiny App Dashboard

Using the hospitals beds occupancy data (`hospitals_occupancy.csv`), we answered the following questions using multiple visually enhancing ways together with filtering mechanism in R Shiny App:<br>

* What are the number of beds available for Covid-19 treatment for the current hour in Malaysia hospitals?
* What are the hospitals occupancy trend for the past six months in Malaysia?

Refer to our `server.R` and `ui.R` files to explore how this dashboard is created! Code comments are written to help you better understand our code!


---

This project is created by University Malaya's Master of Data Science students for the coursework of WQD7001 Principles of Data Science:

1. Wong Hui Yeok (S2124360)
2. Jonathan Kiew Weng Kiat (S2043163)
3. Ng Boon Jane (S2117897)
4. Hong Zi Shen (S2114600)
