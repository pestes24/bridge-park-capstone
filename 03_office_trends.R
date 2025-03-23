library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(lubridate)

R.version

install.packages("urbnthemes")

library(urbnthemes)

#paths - can just Find & Replace when switching 
peter_path <- "C:/Users/peter/Documents/GitHub/bridge-park-capstone/"
#henry_path <- "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/" 

peter_export <- "C:/Users/peter/Documents/NYU/Bridge_Park_Capstone"
#henry_export <- ""

# <- read_excel(file.path(peter_path, "/census_crosswalk/nhgis_bg2020_tr2010_36.csv")) %>% 
#   clean_names() %>%


# OFFICE DATA -----------------------------------------------------------------------
#anacostia data
#change file names to match in local drive
AnacostiaOFFICERentInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaRentOfficeInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaOFFICERentNoInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaRentOfficeNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaOFFICESalePriceInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaOfficeSalePriceInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaOFFICESalePriceNoInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaOfficeSalePriceNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

ana_OFFICE_trends_rent <- AnacostiaOFFICERentInflation %>% 
  left_join(AnacostiaOFFICERentNoInflation, by = "period", suffix = c("_inflation", "_noinflation")) 

ana_OFFICE_trends_sales <- AnacostiaOFFICESalePriceInflation %>% 
  left_join(anacostiaOFFICESalePriceNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

# write.csv(ana_OFFICE_trends_rent, file.path(peter_export, "ana_OFFICE_trends_rent.csv"))
# write.csv(ana_OFFICE_trends_sales, file.path(peter_export, "ana_OFFICE_trends_sales.csv"))


#EOTR data
#change file names to match in local drive
EastoftheRiverOFFICERentInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeRentInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverOFFICERentNoInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeRentNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverOFFICESalePriceInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeSalePriceInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverOFFICESalePriceNoInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeSalePriceNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")


eotr_OFFICE_trends_rent <- EastoftheRiverOFFICERentInflation %>% 
  left_join(EastoftheRiverOFFICERentNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

eotr_OFFICE_trends_sales <- EastoftheRiverOFFICESalePriceInflation %>% 
  left_join(EastoftheRiverOFFICESalePriceNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

# write.csv(eotr_OFFICE_trends_rent, file.path(peter_export, "eotr_OFFICE_trends_rent.csv"))
# write.csv(eotr_OFFICE_trends_sales, file.path(peter_export, "eotr_OFFICE_trends_sales.csv"))


#DC data ---------------------------------------------------------------
dcOFFICERentInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeRentInflation.xlsx") %>% 
  clean_names() %>%
  mutate(geo = "dc")

dcOFFICERentNoInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeRentNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "dc") 

dcOFFICESalePriceInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeSalePriceInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dcOFFICESalePriceNoInflation <- read_excel("/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeSalePriceNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dc_OFFICE_trends_rent <- dcOFFICERentInflation %>% 
  left_join(dcOFFICERentNoInflation, by = "period", suffix = c("_inflation", "_noinflation")) 

dc_OFFICE_trends_sales <- dcOFFICESalePriceInflation %>% 
  left_join(dcOFFICESalePriceNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

# write.csv(dc_OFFICE_trends_rent, file.path(peter_export, "dc_OFFICE_trends_rent.csv"))
# write.csv(dc_OFFICE_trends_sales, file.path(peter_export, "dc_OFFICE_trends_sales.csv"))
