library(dplyr)
library(tidyverse)
library(tidyr)
library(janitor)

library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)

library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(osmdata)

library(mapview)
library(htmltools)
library(htmlwidgets)
library(viridisLite)
library(devtools)
library(forcats)

#library(webshot2) # might need to get this to work to do exporting, check later
#install_github("wch/webshot")


#paths - can just Find & Replace when switching 
peter_path <- "C:/Users/peter/Documents/GitHub/bridge-park-capstone/"
#henry_path <- "/Users/hhoffmann/Documents/GitHub/" 

peter_export <- "C:/Users/peter/Documents/NYU/Bridge_Park_Capstone"
#henry_export <- ""
  
#reading in shapefiles 
# roads <- read_sf(file.path(peter_path, "Roadway_SubBlock/Roadway_SubBlock.shp")) %>% 
#   st_as_sf() %>%
#   st_transform(crs = 4326) %>%
#   clean_names() # %>%
#   #st_filter(nys_boundary) %>% 
#   #st_intersection(nys_shoreline)


bridge_park <- read_sf(file.path(peter_path,"Bridge Park.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

bp_buffer <- read_sf(file.path(peter_path, "BridgeParkBuffer.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

ward_WOTR <- read_sf(file.path(peter_path, "/Wards_from_2012/Wards_from_2012.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names() %>% 
  select(
    ward,
    geometry
    ) %>% 
  filter(!(ward %in% c(7, 8)))

ward_EOTR <- read_sf(file.path(peter_path, "/Wards_from_2012/Wards_from_2012.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names() %>% 
  select(
    ward,
    geometry
  ) %>% 
  filter(ward %in% c(7, 8))

#tracts file and data dictionary here: https://opendata.dc.gov/datasets/DCGIS::census-tracts-in-2020/about 
tracts <- read_sf(file.path(peter_path, "Census_Tracts_in_2020/Census_Tracts_in_2020.shp"))%>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

tracts_study_area <- tracts %>% 
  filter(tract %in% c("007401","007406","007407","007503","007504","007601","007605"))



#Reading in Business Addresses
#used Small Business Checklist & Geocodio
small_biz <- read.csv(file.path(peter_path, "bp_walkshed_small_businesses.csv")) %>% 
  clean_names() %>% 
  rename(address = address_for_geocoding) %>% 
  select(
    -country,
    -place_name,
    -place_fips,
    -metro_micro_statistical_area_name,
    -metro_micro_statistical_area_type,
    -combined_statistical_area_name,
    -combined_statistical_area_code,
    -metropolitan_division_area_name,
    -metropolitan_division_area_code,
    -county_subdivision_name,
    -county_subdivision_fips,
    -county_subdivision_class_code,
    -county_subdivision_class_description
    ) 

small_biz_geo <- small_biz %>% 
  select(
    name,
    latitude,
    longitude,
    census_tract_code,
    acs_economics_number_of_households_total_value
  )

#Dataset of small businesses with business types, transactions, tax, and ownership data
small_biz_owner <- read.csv(file.path(peter_path, "small_businesses_ownership.csv")) %>% 
  clean_names() %>% 
  left_join(small_biz_geo) %>% 
  select(
    -contact_info
  ) %>% 
  mutate(
    most_recent_sale_amt = as.numeric(most_recent_sale_amt),
    land_value_2025 = as.numeric(land_value_2025),
    building_value_2025 = as.numeric(building_value_2025),
    assessment_value_total = as.numeric(assessment_value_total)
    ) %>% 
  mutate(
    type_dc_categories = case_when(
      str_starts(type_dc_categories, "Beauty") ~ "Barber Shop / Hair Salon",
      str_starts(type_dc_categories, "General") ~ "General Sales / Services",
      str_starts(type_dc_categories, "Reg") ~ "Regulated Business",
      str_starts(type_dc_categories, "Not") ~ "Nonprofit",
      TRUE ~ as.character(type_dc_categories)
    ),
    sale_category = cut(
      most_recent_sale_amt,
      breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000),
      labels = c("$0-$500K", "$500K-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M"),
      include.lowest = TRUE
    ),
    sale_category = case_when(is.na(sale_category) ~ "No Recent Transaction in OTR Database",
                              TRUE ~ sale_category
    ),
    sale_category = factor(sale_category,
                           levels = c("$0-$500K", "$500K-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M")  # Define the order of categories
    ),
    value_category = cut(
      assessment_value_total,
      breaks = c(0, 1000000, 2500000, 5000000, 10000000,35000000),
      labels = c("$0-$1M", "$1M-$2.5M", "$2.5M-$5M", "$5M-$10M", "$10M+"),
      include.lowest = TRUE
    ),
    value_category = case_when(is.na(value_category) ~ "No Value in OTR Database",
                              TRUE ~ value_category
    ),
    value_category = factor(value_category,
                            levels = c("$0-$1M", "$1M-$2.5M", "$2.5M-$5M", "$5M-$10M", "$10M+", "No Value in OTR Database")),
  )


# create palettes -------------------------------------------------------------
pal_biz <- colorFactor(
  palette = "viridis",
  domain = small_biz_owner$type_dc_categories)

sale_colors <- c(
  "$0-$1M", "$1M-$2.5M", "$2.5M-$5M", "$5M-$10M", "$10M+"
)

pal_sale <- colorFactor(
  palette = c(viridis(5), "grey"),  # colors from Viridis + Grey for NA
  domain = c(levels(small_biz_owner$sale_category), NA)
)


value_colors <- c(
  "$0-$1M" = "#c7e9c0",  # Light green for $0-$1M
  "$1M-$2.5M" = "#a1d99b",  # Slightly darker green for $1M-$2.5M
  "$2.5M-$5M" = "#31a354",  # Darker green for $2.5M-$5M
  "$5M-$10M" = "#006d2c",  # Even darker green for $5M-$10M
  "$10M+" = "#00441b",  # Darkest green for $10M+
  "No Value in OTR Database" = "grey"  # Grey for NAs
)

pal_values <- colorFactor(
  palette = value_colors,   
  domain = c(levels(small_biz_owner$value_category)) 
)
  
  









#Creating Labels for Interactive Maps ------------------------------------------
tracts_study_area$popup_label <- paste("Tract: ", tracts_study_area$tract, "<br>",
                                "Population: ", comma(tracts_study_area$p0010001),"<br>",
                                "Percent Black (alone): ", percent(tracts_study_area$p0010004/tracts_study_area$p0010002, accuracy = .1),"<br>"
                                ) %>%
  lapply(HTML)
                           # Ref: 
                           # p0010001 = "Total Pop"
                           # p0010004 = "Pop of 1 race: Black"; p0010002 = "Total Pop of 1 Race"


# df2$popup_label <- paste("Zip Code: ", shp_data$ZCTA5CE20, "<br>",
#                               "Religious Property Count: ", comma(shp_data$Rel_prop_Count)) %>%
#   lapply(HTML)

small_biz$popup_label <- paste("<b>",small_biz$name,"</b>", "<br>"
                               ) %>%
  lapply(HTML)

small_biz_owner$popup_label <- paste("<b>",small_biz_owner$name,"</b>", "<br>",
                               "Address: ",small_biz_owner$address, "<br>",
                               "Business Type: ",small_biz_owner$type_of_business, "<br>",
                               "Most Recent Sale Date (if available): ", small_biz_owner$most_recent_sale_year, "<br>", 
                               "Most Recent Sale Price (if available): ", dollar(small_biz_owner$most_recent_sale_amt), "<br>",
                               "Most Recent Assessed Property Value: ", dollar(small_biz_owner$assessment_value_total)
) %>%
  lapply(HTML)



# Icons for business types 

# icons <- awesomeIconList(
#   "Barber Shop / Hair Salon" = makeIcon(
#     iconUrl = "C:/Users/peter/Downloads/favicon.ico",
#     iconWidth = 25,
#     iconHeight = 25)
#   )

#   "General Sales / Services"
#   "Regulated Business"
#   "Nonprofit"
# )

#Relevant Readings: 
# https://roh.engineering/posts/2021/10/awesome-marker-legends-in-leaflet/
# https://rstudio.github.io/leaflet/reference/awesomeIcons.html 
# https://github.com/lennardv2/Leaflet.awesome-markers
# https://github.com/pointhi/leaflet-color-markers
# https://www.jla-data.net/eng/leaflet-markers-in-r/


# Maps! -----------------------------------------------------------------------

#Series of maps showing the neighborhood and various components we will be referencing. 
#Call outs with specific reference to names of areas of neighborhood - historical names and newer names. 
#Reverence for long-term residents' understanding of places names.

#Map showing businesses within BP 1 mile walkshed -------------------------------- 
map_sbs_buffer <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = bp_buffer,
    fillColor = NULL,
    color = "#27ae60",
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = FALSE),
    opacity = .7,
    weight = 1.2,
    fillOpacity = 0,
    #group ="Count of FBO Owned Properties - Zip Code",
    #smoothFactor = 0.2,
    #label = shp_data$popup_label,
    #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
    ) %>%
  addMarkers(icon = ~ icons[ticker], # lookup based on ticker
             label = ~ address) %>%
  addCircleMarkers(
    data = small_biz_owner,
    ~longitude, ~latitude, 
    #popup = ~as.character(name), 
    label = small_biz_owner$popup_label,
    color = ~pal_biz(type_dc_categories), #"#aed6f1", # could make custom markers for businesses by type
    radius = 3,
    stroke = FALSE, 
    fillOpacity = 1,
    clusterOptions = markerClusterOptions(freezeAtZoom = 21)
    ) %>% 
  addLegend(
    pal = pal_biz,
    values = small_biz_owner$type_dc_categories,
    position = "topleft",
    title = "Businesses <1 mile from the Bridge Park",
    #group = "Count of FBO Owned Properties - Zip Code",
    # labFormat = function(type, cuts, p) {
    #   # Define custom bin labels in order
    #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
    #
    #   # Return labels for each cut
    #   return(custom_labels)
    # },
    #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
    opacity = 1) %>%
  # addPolygons(
  #   data = bridge_park,
  #   fillColor = "#FFFF00",
  #   color = "#273538",
  #   highlightOptions = highlightOptions(color = "white", weight = 1,
  #                                       bringToFront = TRUE),
  #   opacity = .7,
  #   weight = 1,
  #   fillOpacity = 0,
  #   #group ="Count of FBO Owned Properties - Zip Code",
  #   #smoothFactor = 0.2,
  #   #label = shp_data$popup_label,
  #   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  addControl(
    html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection",  # Replace with your source note
    position = "bottomright"
  ) %>% 
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-control {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-legend {
          font-family: 'Lato', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_sbs_buffer







# Map showing businesses with a recent sale ----------------------------------
map_sbs_sales <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
addCircleMarkers(
  data = small_biz_owner,
  ~longitude, ~latitude, 
  #popup = ~as.character(name), 
  label = small_biz_owner$popup_label,
  color = case_when(
    small_biz_owner$sale_category == "$0-$500K" ~ "#a1d99b",  # Darker light green  
    small_biz_owner$sale_category == "$500K-$1M" ~ "#74c476",  # More saturated green  
    small_biz_owner$sale_category == "$1M-$1.5M" ~ "#238b45",  # Strong medium green  
    small_biz_owner$sale_category == "$1.5M-$2M" ~ "#006d2c",  # Very dark green  
    small_biz_owner$sale_category == "$2M-$2.5M" ~ "#00441b",  # Deepest green  
    small_biz_owner$sale_category == "No Recent Transaction in OTR Database" ~ "#00441b",  # Deepest green  
    TRUE ~ "grey"
  ), 
  radius = case_when(
    small_biz_owner$sale_category == "$0-$500K" ~ 3,       # Small radius for $0-$500K
    small_biz_owner$sale_category == "$500K-$1M" ~ 3.5,      # Larger radius for $500K-$1M
    small_biz_owner$sale_category == "$1M-$1.5M" ~ 4,      # Even larger radius for $1M-$1.5M
    small_biz_owner$sale_category == "$1.5M-$2M" ~ 4.5,      # Larger still for $1.5M-$2M
    small_biz_owner$sale_category == "$2M-$2.5M" ~ 5,     # Largest radius for $2M-$2.5M
    TRUE ~ 2.5                               # Default radius if no match
  ),
  stroke = FALSE, 
  fillOpacity = 1#,
  #clusterOptions = markerClusterOptions(freezeAtZoom = 21)
  ) %>% 
  addLegend(
    #pal = pal_sale,
    #values = small_biz_owner$sale_category,
    colors = c("#a1d99b", "#74c476", "#238b45", "#006d2c", "#00441b", "grey"),
    labels = c("$0-$500K", "$500K-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "No Recent Transaction in OTR Database"),
    position = "topleft",
    title = "Small Business Locations - Recent Property Transaction",
    #group = "Count of FBO Owned Properties - Zip Code",
    # labFormat = function(type, cuts, p) {
    #   # Define custom bin labels in order
    #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
    #
    #   # Return labels for each cut
    #   return(custom_labels)
    # },
    #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
    opacity = 1) %>%
  # addPolygons(
  #   data = bridge_park,
  #   fillColor = "#FFFF00",
  #   color = "#273538",
  #   highlightOptions = highlightOptions(color = "white", weight = 1,
  #                                       bringToFront = TRUE),
  #   opacity = .7,
  #   weight = 1,
  #   fillOpacity = 0,
  #   #group ="Count of FBO Owned Properties - Zip Code",
  #   #smoothFactor = 0.2,
#   #label = shp_data$popup_label,
#   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
addControl(
  html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection",  # Replace with your source note
  position = "bottomright"
  ) %>% 
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-control {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-legend {
          font-family: 'Lato', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_sbs_sales



# Filtered for adjacent/overlapping Anacostia-side tracks
# Tracts: 007401,007406,007407,007503,007504,007601,007605

# Map showing Anacostia with census tract info 
map_anacostia_ref <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = ward_WOTR,
    fillColor = NULL,
    color = "#273538",
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = FALSE),
    opacity = 1,
    weight = .15,
    fillOpacity =.75,
    #group ="Count of FBO Owned Properties - Zip Code",
    #smoothFactor = 0.2,
    #label = shp_data$popup_label,
    #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  ) %>% 
  addPolygons(
    data = ward_EOTR,
    fillColor = NULL,
    color = "#273538",
    highlightOptions = highlightOptions(color = "white", 
                                        weight = 1,
                                        bringToFront = FALSE),
    opacity = 1,
    weight = 1.2,
    fillOpacity =.25,
    #group ="Count of FBO Owned Properties - Zip Code",
    #smoothFactor = 0.2,
    #label = shp_data$popup_label,
    #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  ) %>% 
  addPolygons(
    data = tracts_study_area, 
    fillColor = NULL, #~pal_pop(population_group),
    color = "#273538",
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = FALSE),
    opacity = .5,
    weight = .8,
    fillOpacity = .3,
    #group = "TBD",
    #smoothFactor = 0.2,
    label = tracts_study_area$popup_label,
    labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  ) %>%
  # addLegend(
  #   pal = pal_pop,
  #   values = tracts$population_group,
  #   position = "topright",
  #   title = "Population - Urban Areas",
  #   group = "Population - Urban Areas",
  #   opacity = 1) 
  #%>% 
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-control {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-legend {
          font-family: 'Lato', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_anacostia_ref



# Map showing businesses with a recent sale ----------------------------------
map_sbs_prop_values <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(
    data = small_biz_owner,
    ~longitude, ~latitude, 
    #popup = ~as.character(name), 
    label = small_biz_owner$popup_label,
    color = ~case_when(
      value_category == "$0-$1M" ~ "#a1d99b",  # Darker light green  
      value_category == "$1M-$2.5M" ~ "#74c476",  # More saturated green  
      value_category == "$2.5M-$5M" ~ "#238b45",  # Strong medium green  
      value_category == "$5M-$10M" ~ "#006d2c",  # Very dark green  
      value_category == "$10M+" ~ "#00441b",  # Deepest green  
      value_category == "No Value in OTR Database" ~ "grey",  # Grey for NAs
      TRUE ~ "black"  # Default fallback (optional)
    ), 
    radius = case_when(
      small_biz_owner$value_category == "$0-$1M" ~ 3.5,      # Larger radius for $500K-$1M
      small_biz_owner$value_category == "$1M-$2.5M" ~ 3.75,      # Even larger radius for $1M-$1.5M
      small_biz_owner$value_category == "$2.5M-$5M" ~ 4,      # Larger still for $1.5M-$2M
      small_biz_owner$value_category == "$5M-$10M" ~ 4.25,     # Largest radius for $2M-$2.5M
      small_biz_owner$value_category == "$10M+" ~ 4.5,     # Largest radius for $2M-$2.5M
      small_biz_owner$value_category == "No Value in OTR Database" ~ 3,
      TRUE ~ 3                              # Default radius if no match
    ),
    stroke = FALSE, 
    fillOpacity = 2#,
    #clusterOptions = markerClusterOptions(freezeAtZoom = 21)
  ) %>% 
  addLegend(
    #pal = pal_values,
    #values = small_biz_owner$value_category,    
    colors = c("#c7e9c0", "#a1d99b", "#31a354", "#006d2c", "#00441b", "grey"),  
    labels = c("$0-$1M", "$1M-$2.5M", "$2.5M-$5M", "$5M-$10M", "$10M+", "No Value in OTR Database"),  
    position = "topleft",
    title = "Small Business Locations - Assessed Property Value",
    #group = "Count of FBO Owned Properties - Zip Code",
    # labFormat = function(type, cuts, p) {
    #   # Define custom bin labels in order
    #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
    #
    #   # Return labels for each cut
    #   return(custom_labels)
    # },
    #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
    opacity = 1) %>%
  # addPolygons(
  #   data = bridge_park,
  #   fillColor = "#FFFF00",
  #   color = "#273538",
  #   highlightOptions = highlightOptions(color = "white", weight = 1,
  #                                       bringToFront = TRUE),
  #   opacity = .7,
  #   weight = 1,
  #   fillOpacity = 0,
  #   #group ="Count of FBO Owned Properties - Zip Code",
  #   #smoothFactor = 0.2,
#   #label = shp_data$popup_label,
#   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  addControl(
    html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection",  # Replace with your source note
    position = "bottomright"
    ) %>% 
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-control {
          font-family: 'Lato', sans-serif !important;
        }
        .leaflet-legend {
          font-family: 'Lato', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_sbs_prop_values


#-- CODE USEFUL IF MAKING FULLY INTERACTIVE MAP -------------------------------
# ) %>%
# addLayersControl(
#   baseGroups = c(
#     "Population - Urban Areas",
#     "Count of FBO Owned Properties - Zip Code"
#   ),
#   position = "topleft",
#   options = layersControlOptions(collapsed = FALSE)
# ) %>%
# htmlwidgets::onRender("
#   function(el, x) {
#     // add option to zoom
#     L.control.zoom({ position: 'bottomleft' }).addTo(this);
# 
#     // legend toggle function
#     var updateLegend = function () {
#       var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
#       document.querySelectorAll('.legend').forEach(a => a.hidden=true);
#       document.querySelectorAll('.legend').forEach(l => {
#         if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
#       });
#     };
#     updateLegend();
#     this.on('baselayerchange', e => updateLegend());
# 
#     // add title
#     $('.leaflet-control-layers-list').prepend('Population and Count of FBO Properties (Quintiles)<hr>');
#   }
# ")