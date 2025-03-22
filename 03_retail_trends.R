library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(urbnthemes)

#paths - can just Find & Replace when switching 
peter_path <- "C:/Users/peter/Documents/GitHub/bridge-park-capstone/"
#henry_path <- "/Users/hhoffmann/Documents/GitHub/" 

peter_export <- "C:/Users/peter/Documents/NYU/Bridge_Park_Capstone"
#henry_export <- ""

# <- read_excel(file.path(peter_path, "/census_crosswalk/nhgis_bg2020_tr2010_36.csv")) %>% 
#   clean_names() %>%


# RETAIL DATA -----------------------------------------------------------------------
#anacostia data
AnacostiaRetailRentInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailRentInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")
  
AnacostiaRetailRentNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailRentNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaRetailSalePriceInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailSalePriceInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

anacostiaRetailSalePriceNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/AnacostiaRetailSalePriceNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

ana_retail_trends_rent <- AnacostiaRetailRentInflation %>% 
  left_join(AnacostiaRetailRentNoInflation, by = "period", suffix = c("_inflation", "_noinflation")) 

ana_retail_trends_sales <- AnacostiaRetailSalePriceInflation %>% 
  left_join(anacostiaRetailSalePriceNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

# write.csv(ana_retail_trends_rent, file.path(peter_export, "ana_retail_trends_rent.csv"))
# write.csv(ana_retail_trends_sales, file.path(peter_export, "ana_retail_trends_sales.csv"))


#EOTR data
EastoftheRiverRetailRentInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverRetailInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverRetailRentNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverRetailRentNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverRetailSalePriceInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverRetailSalePriceInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverRetailSalePriceNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/EastoftheRiverSalePriceNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "eotr")


eotr_retail_trends_rent <- EastoftheRiverRetailRentInflation %>% 
  left_join(EastoftheRiverRetailRentNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

eotr_retail_trends_sales <- EastoftheRiverRetailSalePriceInflation %>% 
  left_join(EastoftheRiverRetailSalePriceNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))

# write.csv(eotr_retail_trends_rent, file.path(peter_export, "eotr_retail_trends_rent.csv"))
# write.csv(eotr_retail_trends_sales, file.path(peter_export, "eotr_retail_trends_sales.csv"))


#DC data ---------------------------------------------------------------
dcRetailRentInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailInflation.xlsx") %>% 
  clean_names() %>%
  mutate(geo = "dc")

dcRetailRentNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "dc") 

dcRetailSalePriceInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailSalePriceInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dcRetailSalePriceNoInflation <- read_excel("C:/Users/peter/Documents/GitHub/bridge-park-capstone/DCRetailSalePriceNoInflation.xlsx") %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dc_retail_trends_rent <- dcRetailRentInflation %>% 
  left_join(dcRetailRentNoInflation, by = "period", suffix = c("_inflation", "_noinflation")) 

dc_retail_trends_sales <- dcRetailSalePriceInflation %>% 
  left_join(dcRetailSalePriceNoInflation, by = "period", suffix = c("_inflation", "_noinflation"))


# write.csv(dc_retail_trends_rent, file.path(peter_export, "dc_retail_trends_rent.csv"))
# write.csv(dc_retail_trends_sales, file.path(peter_export, "dc_retail_trends_sales.csv"))


# JOINED across geos ---------------
retail_trends_rent <- ana_retail_trends_rent %>% 
  full_join(eotr_retail_trends_rent)
, by = "period", suffix = c("_ana", "_eotr"))  %>% 
  left_join(dc_retail_trends_rent, by = "period", suffix = c("", "_noinflation"))

retail_trends_sales <- ana_retail_trends_sales %>% 
  left_join(eotr_retail_trends_sales, by = "period", suffix = c("_ana", "_eotr")) %>% 
  left_join(dc_retail_trends_sales)



# OFFICE DATA -----------------------------------------------------------------------


