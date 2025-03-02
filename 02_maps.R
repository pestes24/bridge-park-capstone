library(dplyr)
library(tidyverse)
library(tidyr)
library(janitor)

library(lubridate)
library(stringr)
library(scales)


library(ggplot2)
library(sf)
library(leaflet)
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
roads <- read_sf(file.path(peter_path, "Roadway_SubBlock/Roadway_SubBlock.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names() # %>%
  #st_filter(nys_boundary) %>% 
  #st_intersection(nys_shoreline)


bridge_park <- read_sf(file.path(peter_path,"Bridge Park.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

bp_buffer <- read_sf(file.path(peter_path, "BridgeParkBuffer.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

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
    ) %>% 
  mutate(
    type = "TBD"
  )

small_biz_geo <- small_biz %>% 
  select(
    name,
    address,
    latitude,
    longitude,
    census_tract_code,
    acs_economics_number_of_households_total_value,
    type
  )



# create palettes -- optional  ------------------------------------------------
# pal_pop <- colorFactor(
#   palette = "viridis",
#   domain = df$variable)


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

small_biz$popup_label <- paste("<b>",small_biz$name,"</b>", "<br>",
                               "Business Type: ", small_biz$type
                               ) %>%
  lapply(HTML)


# Maps! -----------------------------------------------------------------------
#Series of maps showing the neighborhood and various components we will be referencing. 
#Call outs with specific reference to names of areas of neighborhood - historical names and newer names. 
#Reverence for long-term residents' understanding of places names.

# Map showing businesses within BP 1 mile walkshed
map_buffer <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
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
# addLegend(
#   pal = pal_fbo_zip2,
#   values = shp_data$Rel_prop_Count,
#   position = "topright",
#   title = "Count of FBO Owned Properties - Zip Code",
#   group = "Count of FBO Owned Properties - Zip Code",
#   # labFormat = function(type, cuts, p) {
#   #   # Define custom bin labels in order
#   #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
#   #   
#   #   # Return labels for each cut
#   #   return(custom_labels)
#   # },
#   #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
#   opacity = 1) %>% 
  addCircleMarkers(
    data = small_biz_geo,
    ~longitude, ~latitude, 
    #popup = ~as.character(name), 
    label = small_biz$popup_label,
    color = "#aed6f1", #~pal(type), # could make custom markers for businesses by type
    radius = 3,
    stroke = FALSE, 
    fillOpacity = 1
    ) 
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
  # ) %>%
# addLegend(
#   pal = pal_fbo_zip2,
#   values = shp_data$Rel_prop_Count,
#   position = "topright",
#   title = "Count of FBO Owned Properties - Zip Code",
#   group = "Count of FBO Owned Properties - Zip Code",
#   # labFormat = function(type, cuts, p) {
#   #   # Define custom bin labels in order
#   #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
#   #   
#   #   # Return labels for each cut
#   #   return(custom_labels)
#   # },
#   #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
#   opacity = 1) %>%
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

map_buffer


# Filtered for adjacent/overlapping Anacostia-side tracks
# Tracts: 007401,007406,007407,007503,007504,007601,007605

# Map showing Anacostia with census tract info 
anacostia_ref_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
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
  ) #%>%
  # addLegend(
  #   pal = pal_pop,
  #   values = tracts$population_group,
  #   position = "topright",
  #   title = "Population - Urban Areas",
  #   group = "Population - Urban Areas",
  #   opacity = 1)

anacostia_ref_map
