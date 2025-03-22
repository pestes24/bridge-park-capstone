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
library(urbnthemes)

#paths - can just Find & Replace when switching 
peter_path <- "C:/Users/peter/Documents/GitHub/bridge-park-capstone/"
#henry_path <- "/Users/hhoffmann/Documents/GitHub/" 

peter_export <- "C:/Users/peter/Documents/NYU/Bridge_Park_Capstone"
#henry_export <- ""

# <- read_excel(file.path(peter_path, "/census_crosswalk/nhgis_bg2020_tr2010_36.csv")) %>% 
#   clean_names() %>%

#anacostia data
AnacostiaRetailRentInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailRentInflation.xlsx") %>% 
  clean_names() 
  
AnacostiaRetailRentNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailRentNoInflation.xlsx") %>% 
  clean_names() 

AnacostiaRetailSalePriceInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailSalePriceInflation.xlsx") %>% 
  clean_names() 

anacostiaRetailSalePriceNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailSalePriceNoInflation.xlsx") %>% 
  clean_names()

ana_retail_trends <- AnacostiaRetailRentInflation %>% 
  left_join(AnacostiaRetailRentNoInflation) %>% 
  left_join(AnacostiaRetailSalePriceInflation) %>% 
  left_join(anacostiaRetailSalePriceNoInflation)
write.csv(file.path(peter_export, "ana_retail_trends.csv"))


#EOTR data
EastoftheRiverRetailRentInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverRetailInflation.xlsx") %>% 
  clean_names() 

EastoftheRiverRetailRentNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverRetailRentNoInflation.xlsx") %>% 
  clean_names() 

EastoftheRiverRetailSalePriceInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverRetailSalePriceInflation.xlsx") %>% 
  clean_names() 

EastoftheRiverRetailSalePriceNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverSalePriceNoInflation.xlsx") %>% 
  clean_names()

EastoftheRiver_retail_trends <- EastoftheRiverRetailRentInflation %>% 
  left_join(EastoftheRiverRetailRentNoInflation) %>% 
  left_join(EastoftheRiverRetailSalePriceInflation) %>% 
  left_join(EastoftheRiverRetailSalePriceNoInflation)
write.csv(file.path(peter_export, "EastoftheRiver_retail_trends.csv"))

#DC data ---------------------------------------------------------------
dcRetailRentInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailInflation.xlsx") %>% 
  clean_names() 

dcRetailRentNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailNoInflation.xlsx") %>% 
  clean_names() 

dcRetailSalePriceInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailSalePriceInflation.xlsx") %>% 
  clean_names() 

dcRetailSalePriceNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailSalePriceNoInflation.xlsx") %>% 
  clean_names()

dc_retail_trends <- dcRetailRentInflation %>% 
  left_join(dcRetailRentNoInflation) %>% 
  left_join(dcRetailSalePriceInflation) %>% 
  left_join(dcRetailSalePriceNoInflation)

write.csv(file.path(peter_export, "dc_retail_trends.csv"))
