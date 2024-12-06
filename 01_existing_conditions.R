library(tidyverse)
library(janitor)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(lubridate)

#certificate <- read_csv("/Users/hhoffmann/Downloads/Certificate_of_Occupancy.csv") %>% 
certificate <- read_csv("C:/Users/peter/Documents/GitHub/Certificate_of_Occupancy.csv") %>% 
  clean_names() %>% 
  filter(!is.na(ward)) %>% 
  mutate(
   issue_date = ymd_hms(issue_date, tz = "America/New_York"), 
   ward = str_remove(ward,"Ward ")
  )

certificates_2022_today <- certificate %>% 
  
#cleancertificate <- clean_names(Certificate_of_Occupancy)
#cleancertificate <- as.numeric(cleancertificate)  


certificateselect <- cleancertificate %>%
  select(issue_date, ward, description_of_occupancy, approved_zoning_use, property_owner) %>%
  filter(issue_date >= as.Date("2022/01/01 05:00:00+00"), issue_date <= as.Date("2024/12/01 05:00:00+00")) %>%
  
  mutate(
    count(ward)
  )