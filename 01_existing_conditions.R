library(tidyverse)
library(janitor)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(lubridate)

#certificate <- read_csv("/Users/hhoffmann/Downloads/Certificate_of_Occupancy.csv") %>% #we can just comment each others out when you need to run this?
certificate <- read_csv("C:/Users/peter/Documents/GitHub/Certificate_of_Occupancy.csv") %>% 
  clean_names() %>% 
  filter(!is.na(ward)) %>% 
  mutate(
   issue_date = ymd_hms(issue_date, tz = "America/New_York"), 
   ward = str_remove(ward,"Ward ")
  ) %>% 
  filter( #filter for residential uses only
    #str_detect(description_of_occupancy, "APARTMENT|APT|FAMILY|RESIDENTIAL|RESIDENTAIL|UNIT|CONDOMINIUM|COOPERATIVE|FLAT") |
    str_detect(approved_building_code_use, "CONDO|COOP|FLAT|HOUSE")
  ) 
#72,259 total certificates issued.
#15,924 residential certificates issued.


certificates_2022_today <- certificate %>% 
  filter(issue_date >= as.Date("2022/01/01 05:00:00+00"))
#10,027 total certificates issued. 
#2,323 residential certificates issued.


certificates_2022_today_w8 <- certificate %>% 
  filter(issue_date >= as.Date("2022/01/01 05:00:00+00"),
         ward == 8) 
#953 total certificates issued.
#374 residential certificates issued.


certificates_ward_summary <- certificates_2022_today %>% 
  group_by(ward) %>% 
  summarize(
    number = n()
  )



# certificateselect <- cleancertificate %>%
#   select(issue_date, ward, description_of_occupancy, approved_zoning_use, property_owner) %>%
#   filter(issue_date >= as.Date("2022/01/01 05:00:00+00"), issue_date <= as.Date("2024/12/01 05:00:00+00")) %>%
#   
#   mutate(
#     count(ward)
#   )

