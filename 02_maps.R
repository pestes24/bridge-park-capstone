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
  st_transform(crs = 4326)%>%
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
  

# create palettes -- optional  ------------------------------------------------
# pal_pop <- colorFactor(
#   palette = "viridis",
#   domain = df$variable)


#Creating Labels for Interactive Maps ------------------------------------------
tracts$popup_label <- paste("Tract: ", tracts$tract, "<br>",
                            "Population: ", comma(tracts$p0010001),"<br>"
                            ) %>%
  lapply(HTML)

# df2$popup_label <- paste("Zip Code: ", shp_data$ZCTA5CE20, "<br>",
#                               "Religious Property Count: ", comma(shp_data$Rel_prop_Count)) %>%
#   lapply(HTML)



# Maps! -----------------------------------------------------------------------
#Series of maps showing the neighborhood and various components we will be referencing. 
#Call outs with specific reference to names of areas of neighborhood - historical names and newer names. 
#Reverence for long-term residents' understanding of places names.
map_buffer <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = bp_buffer,
    fillColor = NULL,
    color = "#273538",
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = TRUE),
    opacity = .7,
    weight = 1,
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
  addPolygons(
    data = tracts,
    fillColor = "p0010001", #~pal_pop(population_group),
    color = "#273538",
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = TRUE),
    opacity = .5,
    weight = .8,
    fillOpacity = .5,
    #group = "TBD",
    #smoothFactor = 0.2,
    label = tracts$popup_label,
    labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  ) %>%
  # addLegend(
  #   pal = pal_pop,
  #   values = cdps_urban$population_group,
  #   position = "topright",
  #   title = "Population - Urban Areas",
  #   group = "Population - Urban Areas",
  #   opacity = 1) %>%
  addPolygons(
    data = bp_buffer,
    fillColor = NULL,
    color = "#273538",
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = TRUE),
    opacity = .7,
    weight = 1,
    fillOpacity = 0,
    #group ="Count of FBO Owned Properties - Zip Code",
    #smoothFactor = 0.2,
    #label = shp_data$popup_label,
    #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
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
  ) #%>%
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
