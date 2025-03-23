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

# JOINING across geos -----------------------------------------------------------
office_trends_rent <- ana_OFFICE_trends_rent %>%
  bind_rows(eotr_OFFICE_trends_rent) %>% 
  bind_rows(dc_OFFICE_trends_rent) %>% 
  mutate(across(starts_with("asking"), as.double),
         year = as.numeric(str_sub(period, 1, 4)),  # Extract year
         quarter = as.numeric(str_sub(period, 7, 7)),  # Extract quarter number
         period_date = make_date(year, (quarter - 1) * 3 + 1, 1)  # Convert to first day of the quarter
  ) %>% 
  pivot_wider(names_from = geo_inflation, # COMMENT for some reason, R could not find the object geo here; had to make it geo_inflation in order to do the pivot wider
  values_from = c("market_asking_rent_inflation","market_asking_rent_noinflation","asking_rent_inflation","asking_rent_noinflation")) %>% 
  filter(period %in% c("2024 Q4", "2024 Q3", "2024 Q2", "2024 Q1", "2023 Q4", "2023 Q3", "2023 Q2", "2023 Q1", "2022 Q4", "2022 Q3", "2022 Q2", "2022 Q1", "2021 Q4", "2021 Q3", "2021 Q2", "2021 Q1", "2020 Q4", "2020 Q3", "2020 Q2", "2020 Q1", "2019 Q4", "2019 Q3", "2019 Q2", "2019 Q1", "2018 Q4", "2018 Q3", "2018 Q2", "2018 Q1", "2017 Q4", "2017 Q3", "2017 Q2", "2017 Q1", "2016 Q4", "2016 Q3", "2016 Q2", "2016 Q1", "2015 Q4", "2015 Q3", "2015 Q2", "2015 Q1")) 
  
#filtering for 2015-2024
#lots of missing data for office

office_trends_sales <- ana_OFFICE_trends_sales %>% 
  bind_rows(eotr_OFFICE_trends_sales) %>% 
  mutate(across(starts_with("sale"), as.double)) %>% 
  bind_rows(dc_OFFICE_trends_sales) %>% 
  mutate(year = as.numeric(str_sub(period, 1, 4)),  # Extract year
         quarter = as.numeric(str_sub(period, 7, 7)),  # Extract quarter number
         period_date = make_date(year, (quarter - 1) * 3 + 1, 1)  # Convert to first day of the quarter
  ) %>% 
  pivot_wider(names_from = geo_inflation, # COMMENT for some reason, R could not find the object geo here; had to make it geo_inflation in order to do the pivot wider
  values_from = c("sale_price_per_sf_inflation", "sale_price_per_sf_noinflation")) %>% 
  filter(period %in% c("2024 Q4", "2024 Q3", "2024 Q2", "2024 Q1", "2023 Q4", "2023 Q3", "2023 Q2", "2023 Q1", "2022 Q4", "2022 Q3", "2022 Q2", "2022 Q1", "2021 Q4", "2021 Q3", "2021 Q2", "2021 Q1", "2020 Q4", "2020 Q3", "2020 Q2", "2020 Q1", "2019 Q4", "2019 Q3", "2019 Q2", "2019 Q1", "2018 Q4", "2018 Q3", "2018 Q2", "2018 Q1", "2017 Q4", "2017 Q3", "2017 Q2", "2017 Q1", "2016 Q4", "2016 Q3", "2016 Q2", "2016 Q1", "2015 Q4", "2015 Q3", "2015 Q2", "2015 Q1")) 
#filtering for 2015-2024

# Figures using inflation data ------------------------------------------------

rent_inflation_plot <- office_trends_rent %>% 
mutate(geo_inflation = factor(geo_inflation, levels = c("Anacostia", "eotr", "dc"))) %>%
  ggplot(aes(
    x = period,
    y = market_asking_rent_inflation_Anacostia, # COMMENT because R couldn't find Geo earlier, I think it didn't make a new column with all of the values in it and it 
  

  
