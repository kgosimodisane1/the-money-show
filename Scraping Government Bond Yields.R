library(tidyverse)
library(jsonlite)
library(httr)
library(rvest)
library(htmltools)
library(writexl)
library(RODBC)

#We want to scrape historical government bond yield data
#2 websites to get data from is 'Trading Economics' and 'World Government Bonds'

#IMPORT DATA

result1 <- read_html("https://www.worldgovernmentbonds.com/country/south-africa/")

three.month <- result1 %>% 
  html_elements(xpath = '//*[@class="w3-center bd-gray-100"]') %>%
  html_text() %>%
  .[str_detect(., "\\d+[.]\\d+[%]")]

three.month <- as.list.data.frame(three.month)

#Removing leading and trailing whitespaces
trimmed_string <- trimws(three.month)

#Removing non-numeric characters except for the decimal point
bond.yields <- gsub("[^0-9.]", "", trimmed_string) %>%
  as.numeric()

bond.yields <- bond.yields/100

bond.yields <- as.data.frame(bond.yields) 

#Converting data into time-series format

crnt.yld <- t(bond.yields[1:7, ])

colnames(crnt.yld) <- c("3.month", "5.year", "10.year", "12.year", 
                     "20.year", "25.year", "30.year")

Date <- Sys.Date()

crnt.yld <- merge(Date, crnt.yld)

crnt.yld <- column_to_rownames(crnt.yld, var = "x")

#Updating the dataset

sa.gov.yld.ds <- rbind(sa.gov.yld.ds, crnt.yld)
#view(sa.gov.yld.ds)

write_xlsx(rownames_to_column(sa.gov.yld.ds, var = "Date"), "C:/Users/lenovo/Documents/SA_Bonds.xlsx")

write_xlsx(rownames_to_column(sa.gov.yld.ds, var = "Date"), 
           "C:/Users/lenovo/OneDrive/Documents/SA_Bonds.xlsx")

#Attempting to convert data to access
#uhm <- odbcConnect("C:/Users/lenovo/OneDrive/Documents/SA_Bonds.accdb")
#sqlSave(uhm, sa.gov.yld.ds)
