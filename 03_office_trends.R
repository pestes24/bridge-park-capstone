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
#peter_path <- "C:/Users/peter/Documents/GitHub/bridge-park-capstone/CoStar_Rent_Data"
henry_path <- "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/" 

peter_export <- "C:/Users/peter/Documents/NYU/Bridge_Park_Capstone"
#henry_export <- ""

# <- read_excel(file.path(peter_path, "/census_crosswalk/nhgis_bg2020_tr2010_36.csv")) %>% 
#   clean_names() %>%


# OFFICE DATA -----------------------------------------------------------------------
#anacostia data
#change file names to match in local drive
AnacostiaOFFICERentInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaRentOfficeInflation.xlsx"
  #file.path(peter_path,"AnacostiaRentOfficeInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaOFFICERentNoInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaRentOfficeNoInflation.xlsx"
  #file.path(peter_path,"AnacostiaRentOfficeNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaOFFICESalePriceInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaOfficeSalePriceInflation.xlsx" 
  #file.path(peter_path,"AnacostiaOfficeSalePriceInflation.xlsx")
  ) %>%
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaOFFICESalePriceNoInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/AnacostiaOfficeSalePriceNoInflation.xlsx"
  #file.path(peter_path,"AnacostiaOfficeSalePriceNoInflation.xlsx")
  ) %>%
  clean_names() %>% 
  mutate(geo = "Anacostia")

ana_OFFICE_trends_rent <- AnacostiaOFFICERentInflation %>% 
  left_join(AnacostiaOFFICERentNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation")) 

ana_OFFICE_trends_sales <- AnacostiaOFFICESalePriceInflation %>% 
  left_join(AnacostiaOFFICESalePriceNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

# write.csv(ana_OFFICE_trends_rent, file.path(peter_export, "ana_OFFICE_trends_rent.csv"))
# write.csv(ana_OFFICE_trends_sales, file.path(peter_export, "ana_OFFICE_trends_sales.csv"))


#EOTR data
#change file names to match in local drive
EastoftheRiverOFFICERentInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeRentInflation.xlsx"
  #file.path(peter_path,"EastoftheRiverOfficeRentInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverOFFICERentNoInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeRentNoInflation.xlsx"
  #file.path(peter_path,"EastoftheRiverOfficeRentNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverOFFICESalePriceInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeSalePriceInflation.xlsx"
  #file.path(peter_path,"EastoftheRiverOfficeSalePriceInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverOFFICESalePriceNoInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/EastoftheRiverOfficeSalePriceNoInflation.xlsx"
  #file.path(peter_path,"EastoftheRiverOfficeSalePriceNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")


eotr_OFFICE_trends_rent <- EastoftheRiverOFFICERentInflation %>% 
  left_join(EastoftheRiverOFFICERentNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

eotr_OFFICE_trends_sales <- EastoftheRiverOFFICESalePriceInflation %>% 
  left_join(EastoftheRiverOFFICESalePriceNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

# write.csv(eotr_OFFICE_trends_rent, file.path(peter_export, "eotr_OFFICE_trends_rent.csv"))
# write.csv(eotr_OFFICE_trends_sales, file.path(peter_export, "eotr_OFFICE_trends_sales.csv"))


#DC data ---------------------------------------------------------------
dcOFFICERentInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeRentInflation.xlsx"
  #file.path(peter_path,"DCOfficeRentInflation.xlsx")
  ) %>% 
  clean_names() %>%
  mutate(geo = "dc")

dcOFFICERentNoInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeRentNoInflation.xlsx"
  #file.path(peter_path,"DCOfficeRentNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "dc") 

dcOFFICESalePriceInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeSalePriceInflation.xlsx"
  #file.path(peter_path,"DCOfficeSalePriceInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dcOFFICESalePriceNoInflation <- read_excel(
  "/Users/hhoffmann/Documents/Bridge Park Capstone Rent Trends Final/DCOfficeSalePriceNoInflation.xlsx"
  #file.path(peter_path,"DCOfficeSalePriceNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dc_OFFICE_trends_rent <- dcOFFICERentInflation %>% 
  left_join(dcOFFICERentNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation")) 

dc_OFFICE_trends_sales <- dcOFFICESalePriceInflation %>% 
  left_join(dcOFFICESalePriceNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

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
  #pivot_wider(names_from = geo_inflation, 
               # COMMENT for some reason, R could not find the object geo here; had to make it geo_inflation in order to do the pivot wider
              #values_from = c("market_asking_rent_inflation","market_asking_rent_noinflation","asking_rent_inflation","asking_rent_noinflation")) %>% 
              # REPLY: I actually ended up dropping the pivot_wider in the other file, sorry! 
              # It was messing up the data structure for the plot
              # so that's part of it. The other thing is that I changed all the joins to by = c("period","geo") rather than just by = "period"
              # that prevented geo from getting a suffix (_inflation), which was why it wasn't finding "geo"
              # sorry! I pushed those changes but then didn't tell you, my bad!
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
  # pivot_wider(names_from = geo_inflation, 
  # COMMENT for some reason, R could not find the object geo here; had to make it geo_inflation in order to do the pivot wider
  # PETER NOTE: same as above, remove pivot_wider
  #             values_from = c("sale_price_per_sf_inflation", "sale_price_per_sf_noinflation")) %>% 
  filter(period %in% c("2024 Q4", "2024 Q3", "2024 Q2", "2024 Q1", "2023 Q4", "2023 Q3", "2023 Q2", "2023 Q1", "2022 Q4", "2022 Q3", "2022 Q2", "2022 Q1", "2021 Q4", "2021 Q3", "2021 Q2", "2021 Q1", "2020 Q4", "2020 Q3", "2020 Q2", "2020 Q1", "2019 Q4", "2019 Q3", "2019 Q2", "2019 Q1", "2018 Q4", "2018 Q3", "2018 Q2", "2018 Q1", "2017 Q4", "2017 Q3", "2017 Q2", "2017 Q1", "2016 Q4", "2016 Q3", "2016 Q2", "2016 Q1", "2015 Q4", "2015 Q3", "2015 Q2", "2015 Q1")) 
#filtering for 2015-2024
# COMMENT because R couldn't find Geo earlier, I think it didn't make a new column with all of the values in it  
# PETER NOTE: yes I think that's what the issue was!
# I just changed the variables names and added geom_line, and it's close now! 
# If you want to look at my ggplot code, you can play around with it and build out a similar version 

# GGPLOT Figures using inflation data ------------------------------------------------

# RENT INFLATION PLOT
  rent_inflation_plot <- office_trends_rent %>% 
mutate(geo = factor(geo, levels = c("Anacostia", "eotr", "dc"))) %>%
  ggplot(aes(
    x = period_date, 
    #PETER NOTE: to plot this as a date I created period_date above - use that here instead of period
    # (still useful to have period for filtering like you did above)
    #changed the y value below to just the general all areas variable, rather than specifically Anacostia
    y = market_asking_rent_inflation,
    color = geo
    ))+
  geom_line(linewidth = 1 #slightly thicker line
  ) + 
  scale_x_date(expand = expansion(mult = c(0.02, 0.02)), 
               date_breaks = "1 year",
               date_labels = "%Y"
  ) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), 
                     breaks = seq(30, 70, by = 10),  
                     labels = scales::dollar_format(),  
                     limits = c(30, 70)
  ) +
  labs(title = "Trends in Office Rents",
       subtitle = "(per square foot, adjusted for inflation)",
       x = "Year",
       y = "Market Asking Rent ",
       color = "Geography"
  ) +
       guides(
         color = guide_legend(title = "Geography", labels = c("Anacostia", "East of the River", "Washington, DC"))
       ) + # COMMENT - not working here, but I was trying to figure out how to relabel the legend from the names up top. I assumed those names are correlated to the geos in the excel file. 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line()
  ) +
  print(rent_inflation_plot)
# COMMENT idea here could be that we include highest and lowest rental numbers across the ten year period on the graph
  
  
# SALES INFLATION PLOT
  sales_inflation_plot_gg <- office_trends_sales %>%
  #filter(period %in% c("", "", "")) %>%
  mutate(geo = factor(geo, levels = c("Anacostia", "eotr", "dc"))) %>%
  ggplot(aes(
    x = period_date, 
    y = sale_price_per_sf_inflation, 
    color = geo
  )) +
  geom_line(linewidth = 1 #slightly thicker line
  ) + 
  # geom_smooth(method = lm, 
  #             se = FALSE, 
  #             linewidth = .8,
  #             linetype="dashed"
  #             ) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.02)), 
               date_breaks = "1 year",
               date_labels = "%Y"
  ) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), 
                     breaks = seq(0, 1200, by = 200),  
                     labels = scales::dollar_format(),  
                     limits = c(0, 1200)
  ) + #COMMENT - I'm getting some weirdness here and I saw this online too; the y-axis changes I am making are not showing up in the plot for some reason; the code still runs, just no changes in the plot itself even after I clear it out
  labs(title = "Trends in Sales Prices",
       x = "",
       y = "Sales Prices (per square foot, adjusted for inflation)",
       color = "",
       caption = "Source: CoStar. ADD HERE Inflation adjustment info"
  ) +
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line()
  ) +
    print(sales_inflation_plot_gg)
  
  

  


