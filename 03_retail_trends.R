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
library(gapminder)
library(zoo)
library(showtext)

set_urbn_defaults(style = "print")

# Installation of urbnthemes
# install.packages("remotes")
# remotes::install_github("UrbanInstitute/urbnthemes", build_vignettes = TRUE)
# Fonts
# The Urban Institute uses Lato font for publications. After installing urbnthemes, submit urbnthemes::lato_test() to see if Lato is imported and registered.
# 
# If Lato isn’t imported and registered, install Lato and then submit urbnthemes::lato_install(). If you are on a Windows, you may need to install ghostscript and then submit Sys.setenv(R_GSCMD = "link to the ghostscript .exe") before running urbnthemes::lato_install().
# 
# Waffle charts with glyphs require fontawesome. fontawesome_test() and fontawesome_install() are the fontawesome versions of the above functions. Be sure to install fontawesome from here.
# 
# Usage
# Always load library(urbnthemes) after library(ggplot2) or library(tidyverse).

#Adding Montserrat
font_add("montserrat", "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Regular.ttf")
showtext_auto()
font_add(family = "montserrat",
         regular = "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Regular.ttf",
         bold = "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Bold.ttf",
         italic = "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Italic.ttf")



#paths - can just Find & Replace when switching -------------------------------
peter_path <- "C:/Users/peter/Documents/GitHub/bridge-park-capstone/CoStar_Rent_Data/"
#henry_path <- "/Users/hhoffmann/Documents/GitHub/" 

peter_export <- "C:/Users/peter/Documents/NYU/Bridge_Park_Capstone"
#henry_export <- ""


#Relevant Period
years <- as.character(2015:2024)  # Create a vector of years



# RETAIL DATA -----------------------------------------------------------------------
#anacostia data
AnacostiaRetailRentInflation <- read_excel(
  file.path(peter_path,"AnacostiaRetailRentInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")
  
AnacostiaRetailRentNoInflation <- read_excel(
  file.path(peter_path,"AnacostiaRetailRentNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

AnacostiaRetailSalePriceInflation <- read_excel(
  file.path(peter_path,"AnacostiaRetailSalePriceInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

anacostiaRetailSalePriceNoInflation <- read_excel(
  file.path(peter_path,"AnacostiaRetailSalePriceNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "Anacostia")

ana_retail_trends_rent <- AnacostiaRetailRentInflation %>% 
  left_join(AnacostiaRetailRentNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation")) 

ana_retail_trends_sales <- AnacostiaRetailSalePriceInflation %>% 
  left_join(anacostiaRetailSalePriceNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

# write.csv(ana_retail_trends_rent, file.path(peter_export, "ana_retail_trends_rent.csv"))
# write.csv(ana_retail_trends_sales, file.path(peter_export, "ana_retail_trends_sales.csv"))


#EOTR data
EastoftheRiverRetailRentInflation <- read_excel(
  file.path(peter_path,"EastoftheRiverRetailInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverRetailRentNoInflation <- read_excel(
  file.path(peter_path,"EastoftheRiverRetailRentNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverRetailSalePriceInflation <- read_excel(
  file.path(peter_path,"EastoftheRiverRetailSalePriceInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")

EastoftheRiverRetailSalePriceNoInflation <- read_excel(
  file.path(peter_path,"EastoftheRiverRetailSalePriceNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "eotr")


eotr_retail_trends_rent <- EastoftheRiverRetailRentInflation %>% 
  left_join(EastoftheRiverRetailRentNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

eotr_retail_trends_sales <- EastoftheRiverRetailSalePriceInflation %>% 
  left_join(EastoftheRiverRetailSalePriceNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))

# write.csv(eotr_retail_trends_rent, file.path(peter_export, "eotr_retail_trends_rent.csv"))
# write.csv(eotr_retail_trends_sales, file.path(peter_export, "eotr_retail_trends_sales.csv"))


#DC data ---------------------------------------------------------------
dcRetailRentInflation <- read_excel(
  file.path(peter_path,"DCRetailInflation.xlsx")
  ) %>% 
  clean_names() %>%
  mutate(geo = "dc")

dcRetailRentNoInflation <- read_excel(
  file.path(peter_path,"DCRetailNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "dc") 

dcRetailSalePriceInflation <- read_excel(
  file.path(peter_path,"DCRetailSalePriceInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dcRetailSalePriceNoInflation <- read_excel(
  file.path(peter_path,"DCRetailSalePriceNoInflation.xlsx")
  ) %>% 
  clean_names() %>% 
  mutate(geo = "dc")

dc_retail_trends_rent <- dcRetailRentInflation %>% 
  left_join(dcRetailRentNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation")) 

dc_retail_trends_sales <- dcRetailSalePriceInflation %>% 
  left_join(dcRetailSalePriceNoInflation, by = c("period","geo"), suffix = c("_inflation", "_noinflation"))


# write.csv(dc_retail_trends_rent, file.path(peter_export, "dc_retail_trends_rent.csv"))
# write.csv(dc_retail_trends_sales, file.path(peter_export, "dc_retail_trends_sales.csv"))






# JOINING across geos -----------------------------------------------------------
retail_trends_rent <- ana_retail_trends_rent %>% 
  bind_rows(eotr_retail_trends_rent) %>% 
  bind_rows(dc_retail_trends_rent) %>% 
  mutate(across(starts_with("asking"), as.double),
         year = as.numeric(str_sub(period, 1, 4)),  # Extract year
         quarter = as.numeric(str_sub(period, 7, 7)),  # Extract quarter number
         period_date = make_date(year, (quarter - 1) * 3 + 1, 1)  # Convert to first day of the quarter
         ) %>% 
  # pivot_wider(names_from = geo, 
  #             values_from = c("market_asking_rent_inflation","market_asking_rent_noinflation","asking_rent_inflation","asking_rent_noinflation")) %>% 
  filter(substr(period, 1, 4) %in% years) 
  #filtering for 2015-2024 
  #2018 inflation data missing for Anacostia

retail_trends_sales <- ana_retail_trends_sales %>% 
  bind_rows(eotr_retail_trends_sales) %>% 
  mutate(across(starts_with("sale"), as.double)) %>% 
  bind_rows(dc_retail_trends_sales) %>% 
  mutate(year = as.numeric(str_sub(period, 1, 4)),  # Extract year
         quarter = as.numeric(str_sub(period, 7, 7)),  # Extract quarter number
         period_date = make_date(year, (quarter - 1) * 3 + 1, 1)  # Convert to first day of the quarter
         ) %>% 
  # pivot_wider(names_from = geo, 
  #             values_from = c("sale_price_per_sf_inflation", "sale_price_per_sf_noinflation")) %>% 
  filter(substr(period, 1, 4) %in% years) #filtering for 2015-2024






# Figures using inflation data ------------------------------------------------

rent_inflation_plot <- retail_trends_rent %>%
  #filter(period %in% c("", "", "")) %>%
  mutate(geo = factor(geo, levels = c("Anacostia", "eotr", "dc"))) %>%
  ggplot(aes(
    x = period_date, 
    y = market_asking_rent_inflation, 
    color = geo
    )) +
  geom_line(linewidth = .8 #slightly thicker line
            ) + 
  scale_x_date(expand = expansion(mult = c(0.02, 0.02)), 
               date_breaks = "1 year",
               date_labels = "%Y"
               ) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), 
                     breaks = seq(5, 65, by = 5),  
                     labels = scales::dollar_format(),  
                     limits = c(0, 60)
                     ) + 
  scale_color_discrete(
    name="Geography", 
    labels = c("Anacostia", "East of the River", "Washington, DC")
  ) +
  labs(title = "Figure 4: Trends in Asking Rents, Retail Properties",
       subtitle = "(per square foot, adjusted for inflation)",
       x = "",
       y = "Market Asking Rent (per square foot)",
       color = "",
       caption = "Source: CoStar. Note: all rent figures have been adjusted for inflation and converted to 2024 dollars."
       ) +
  theme_urbn_print() + 
  theme(
    text = element_text(family = "montserrat", size = 16)
  )

rent_inflation_plot



sales_inflation_plot <- retail_trends_sales %>%
  #filter(period %in% c("", "", "")) %>%
  mutate(geo = factor(geo, levels = c("Anacostia", "eotr", "dc"))) %>%
  ggplot(aes(
    x = period_date, 
    y = sale_price_per_sf_inflation, 
    color = geo
  )) +
  geom_line(linewidth = .8 #slightly thicker line
    ) + 
  # geom_smooth(method = lm, 
  #             se = FALSE, 
  #             linewidth = .8,
  #             linetype="dashed"
  #             ) +
  # Commenting out and creating new trend line graph below
  scale_x_date(expand = expansion(mult = c(0.02, 0.02)), 
               date_breaks = "1 year",
               date_labels = "%Y"
  ) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), 
                     breaks = seq(500, 3500, by = 500),  
                     labels = scales::dollar_format(),  
                     limits = c(0, 3500)
  ) + 
  scale_color_discrete(
    name="Geography", 
    labels = c("Anacostia", "East of the River", "Washington, DC")
  ) +
  labs(title = "Figure 5b: Trends in Sales Prices, Retail Properties",
       subtitle = "(per square foot, adjusted for inflation)",
       x = "",
       y = "Sales Prices (per square foot, adjusted for inflation)",
       color = "",
       caption = str_wrap("Source: CoStar. Note: all sales price figures have been adjusted for inflation and converted to 2024 dollars.")
  ) +
  theme_urbn_print() + 
  theme(
    text = element_text(family = "montserrat")
  )

sales_inflation_plot


sales_inflation_plot_trendline <- retail_trends_sales %>%
  #filter(period %in% c("", "", "")) %>%
  mutate(geo = factor(geo, levels = c("Anacostia", "eotr", "dc"))) %>%
  ggplot(aes(
    x = period_date, 
    y = sale_price_per_sf_inflation, 
    color = geo
  )) +
  # geom_point(#linewidth = .8 #slightly thicker line
  # ) + 
  geom_smooth(method = lm, 
              se = FALSE, 
              linewidth = .8,
              linetype="dashed"
  ) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.02)), 
               date_breaks = "1 year",
               date_labels = "%Y"
  ) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), 
                     breaks = seq(250, 1000, by = 250),  
                     labels = scales::dollar_format(),  
                     limits = c(0, 1050)
  ) + 
  scale_color_discrete(
    name = "Geography", 
    labels = c("Anacostia", "East of the River", "Washington, DC")
  ) +
  labs(title = "Figure 5: Trends in Sales Prices, Retail Properties",
       subtitle = "(per square foot, adjusted for inflation)",
       x = "",
       y = "Sales Prices (per square foot)",
       color = "",
       caption = "Source: CoStar. Note: all sales price figures have been adjusted for inflation and converted to 2024 dollars."
  ) +
  theme_urbn_print() + 
  theme(
    #legend.text = element_text(margin = margin(l = 1)),  # add margin to the left of legend text
    #legend.key.width = unit(.75, "lines"),               # increase space reserved for keys
    #legend.spacing.x = unit(2.5, "pt"),
    #legend.key = element_rect(fill = NA),                # avoid key background interfering
    text = element_text(family = "montserrat", size = 16)           # keep your font override
  )

sales_inflation_plot_trendline

