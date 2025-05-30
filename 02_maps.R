library(dplyr)
library(tidyverse)
library(tidyr)
library(janitor)
library(gt)

library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)

library(sf)
library(leaflet)
library(leaflet.extras)
library(osmdata)
library(tigris)

library(mapview)
library(htmltools)
library(htmlwidgets)
library(viridisLite)
library(devtools)
library(forcats)
library(ggplot2)
library(urbnthemes)
library(gapminder)
library(zoo)
library(showtext)

set_urbn_defaults(style = "print")

#library(webshot2) # might need to get this to work to do exporting, check later
#install_github("wch/webshot")

font_add("montserrat", "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Regular.ttf")
showtext_auto()
font_add(family = "montserrat",
         regular = "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Regular.ttf",
         bold = "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Bold.ttf",
         italic = "C:/Users/peter/Desktop/Fonts/Montserrat/static/Montserrat-Italic.ttf")



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

#DC Geos
dc <- read_sf(file.path(peter_path, "/lehd_shp_gs.shp")) %>%
  clean_names() %>% 
  filter(stusps == "DC")

rivers <- read_sf(file.path(peter_path, "/Waterbodies_2021/Waterbodies_2021.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326)  %>%
  clean_names() %>% 
  filter(descriptio == "River")


wards <- read_sf(file.path(peter_path, "/Wards_from_2012/Wards_from_2012.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names() #%>% 
# select(
#   ward,
#   geometry
#   )

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
  filter(ward %in% c(7, 8)) #%>%
#   st_difference(rivers) # this isn't working here or below
  #Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
  #Loop 0 is not valid: Edge 51113 has duplicate vertex with edge 51121




#tracts file and data dictionary here: https://opendata.dc.gov/datasets/DCGIS::census-tracts-in-2020/about 
tracts <- read_sf(file.path(peter_path, "Census_Tracts_in_2020/Census_Tracts_in_2020.shp"))%>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

tracts_study_area <- tracts %>% 
  filter(tract %in% c("007401","007406","007407","007503","007504","007601","007605"))


# BP and Study Area
bridge_park <- read_sf(file.path(peter_path,"Bridge Park.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names()

bp_buffer <- read_sf(file.path(peter_path, "BridgeParkBuffer.shp")) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  clean_names() 

#create a mask for DC
dc_minus_buffer_mask <- st_difference(dc, bp_buffer) 

dc_minus_buffer_mask_eotr <- dc_minus_buffer_mask %>% 
  st_intersection(ward_EOTR) 



#TEST Mask
# dc_minus_buffer_mask_plot <- dc_minus_buffer_mask %>%
#   ggplot()+
#   geom_sf()
# 
# dc_minus_buffer_mask_plot

# bp_buffer_eotr <- st_intersection(bp_buffer, rivers) %>% 
#   st_difference(wards_EOTR)


#if needed, EOTR shapefile: https://opendata.dc.gov/datasets/DCGIS::east-of-the-river-1/explore






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
#Business types from: https://dlcp.dc.gov/service/business-licensing-division 
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
      str_starts(type_dc_categories, "Reg") ~ "Other Regulated Business", #see list in link above; includes spas, gyms, auto repair, funerals, smoke shops, etc. 
      str_starts(type_dc_categories, "Not") ~ "Nonprofit",
      TRUE ~ as.character(type_dc_categories)
    ),
    sale_category = cut(
      most_recent_sale_amt,
      breaks = c(0, 1000000, 1500000, 2000000, 2500000,250000000),
      labels = c("$0-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "2.5M+"),
      include.lowest = TRUE
    ),
    sale_category = case_when(is.na(sale_category) ~ "No Recent Transaction in OTR Database",
                              TRUE ~ sale_category
    ),
    sale_category = factor(sale_category,
                           levels = c("$0-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "2.5M+")  # Define the order of categories
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
  ) %>%
  mutate(type_detail = case_when(
    grepl("Busboys|Savages", name, ignore.case = TRUE) ~ "Restaurant (Sit-Down)",
    grepl("Grounded", name, ignore.case = TRUE) ~ "Café",
    type_dc_categories == "Food Services" ~ "Takeout Food or Convenience Store",
    TRUE ~ type_dc_categories
  ))

# small_biz_food <- small_biz_owner %>% 
#   filter(type_dc_categories == "Food Services") 
# 
# food_type_counts <- small_biz_food %>%
#   count(type_detail) %>%
#   arrange(desc(n))

#Property Owner - includes Vacant Spaces
#Business types from: https://dlcp.dc.gov/service/business-licensing-division 
prop_owner_names <- c("Business Tenant (or most recent if vacant)",
                      "Address",	
                      "Type of Business",
                      "type_dc_categories",
                      "SSL",	
                      "Use Code",	
                      "Property Owner (as listed in https://mytax.dc.gov/_/#10)",
                      "Most Recent Sale (Year)",	
                      "Most Recent Sale ($)",	
                      "Most Recent Sale Notes",	
                      "Land Area (square feet)",
                      "Tax Class",
                      "Land Value - 2025",
                      "Building Value - 2025",	
                      "Assessment Value (Total)",	
                      "Tax Relief",	
                      "Location Bonus",
                      "Latitude",
                      "Longitude",
                      "Notes")

prop_owner <- read.csv(file.path(peter_path, "Anacostia Commercial Properties Long List.csv")) %>% 
  clean_names() %>% 
  mutate(
    most_recent_sale_amt = as.numeric(most_recent_sale),
    land_value_2025 = as.numeric(land_value_2025),
    building_value_2025 = as.numeric(building_value_2025),
    assessment_value_total = as.numeric(assessment_value_total),
    name = business_tenant_or_most_recent_if_vacant,
    owner_name = property_owner_as_listed_in_https_mytax_dc_gov_10,
    vacant_flag = case_when(
      vacant_flag == 0 ~ "Not Vacant",
      vacant_flag == 1 ~ "Vacant",
      is.na(tax_class) ~ NA_character_
    )
  ) %>%
  mutate(
    type_dc_categories = case_when(
      str_starts(type_dc_categories, "Beauty") ~ "Barber Shop / Hair Salon",
      str_starts(type_dc_categories, "General") ~ "General Sales / Services",
      str_starts(type_dc_categories, "Reg") ~ "Other Regulated Business", #see list in link above; includes spas, gyms, auto repair, funerals, smoke shops, etc. 
      str_starts(type_dc_categories, "Not") ~ "Nonprofit",
      TRUE ~ as.character(type_dc_categories)
    ),
    sale_category = cut(
      most_recent_sale_amt,
      breaks = c(0, 1000000, 1500000, 2000000, 2500000, 250000000),
      labels = c("$0-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "2.5M+"),
      include.lowest = TRUE
    ),
    sale_category = case_when(is.na(sale_category) ~ "No Recent Transaction in OTR Database",
                              TRUE ~ sale_category
    ),
    sale_category = factor(sale_category,
                           levels = c("$0-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "2.5M+")  # Define the order of categories
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
  ) %>%
  mutate(type_detail = case_when(
    grepl("Busboys|Savages", name, ignore.case = TRUE) ~ "Restaurant (Sit-Down)",
    grepl("Grounded", name, ignore.case = TRUE) ~ "Café",
    type_dc_categories == "Food Services" ~ "Takeout Food or Convenience Store",
    TRUE ~ type_dc_categories
  ))

# Create the count labels
biz_counts <- small_biz_owner %>%
  count(type_detail) %>%
  mutate(label = paste0(type_detail, " (", n, ")"))


# create palettes -------------------------------------------------------------
pal_biz <- colorFactor(
  palette = "viridis",
  domain = small_biz_owner$type_dc_categories)

pal_biz_counts <- colorFactor(palette = "Set2", 
                              domain = unique(small_biz_owner$type_detail))
# pal_biz_detail <- colorFactor(
#   palette = "viridis",
#   domain = food_type_counts$type_detail)

sale_colors <- c(
  "$0-$1M" = "#c7e9c0", 
  "$1M-$1.5M" = "#a1d99b", 
  "$1.5M-$2M" = "#31a354", 
  "$2M-$2.5M" = "#006d2c", 
  "2.5M+" = "#00441b",
  "No Value in OTR Database" = "grey"  # Grey for NAs
)

pal_sale <- colorFactor(
  palette = sale_colors,
    #c(viridis(5), "grey"),  # colors from Viridis + Grey for NA
  domain = c(levels(prop_owner$sale_category))
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
  domain = c(levels(prop_owner$value_category)) 
)
  
  


# BASIC REFERENCE MAPS ---------------------------------------------------------

anacostia_marker <- st_as_sf(
  data.frame(
    lon = -76.9884,
    lat = 38.8670), 
  coords = c("lon", "lat"),
  crs = 4326)


# reference_map <- dc %>% 
#   ggplot() + 
#   geom_sf(fill = "lightgray", 
#           color = "black"
#           ) +
#   geom_sf(data = rivers,
#           fill = "lightblue",
#           color = "darkgray",
#           stroke = 1
#           ) +
#   geom_sf(data = bp_buffer_eotr,
#           fill = "lightgreen",
#           size = 55,
#           color = "lightgreen")+
#   geom_sf_text(data = anacostia_marker, 
#                label = "\u2605", 
#                size = 10, 
#                color = "gold") +
#   # geom_sf(data = anacostia_marker, 
#   #         color = "green", 
#   #         shape = 8,
#   #         size = 3
#   # ) +
#   theme_void(
#   ) +
#   labs(title = "The Study Area in Context",
#        caption = "Source: DC Open Data",
#        x = "",
#        y = "",
#        )# +
#   #remove_ticks()
# 
# 
# reference_map


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
                               "Business Type: ",small_biz_owner$type_dc_categories, "<br>",
                               "Most Recent Sale Date (if available): ", small_biz_owner$most_recent_sale_year, "<br>", 
                               "Most Recent Sale Price (if available): ", dollar(small_biz_owner$most_recent_sale_amt), "<br>",
                               "Most Recent Assessed Property Value: ", dollar(small_biz_owner$assessment_value_total)
) %>%
  lapply(HTML)

small_biz$popup_label <- paste("<b>",small_biz$name,"</b>", "<br>"
) %>%
  lapply(HTML)

#same as above but for prop_owner
prop_owner$popup_label <- paste("<b>",prop_owner$name,"</b>", "<br>",
                                "Address: ",prop_owner$address, "<br>",
                                "Owner (if available)",prop_owner$owner_name, "<br>",
                                "Most Recent Sale Date (if available): ", prop_owner$most_recent_sale_year, "<br>", 
                                "Most Recent Sale Price (if available): ", dollar(prop_owner$most_recent_sale_amt), "<br>",
                                "Most Recent Assessed Property Value: ", dollar(prop_owner$assessment_value_total)
                                
) %>%
  lapply(HTML)


# Icons for business types 

#one method
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

#but should be better to just pull from existing icon list
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



font_header <- tags$head(
  tags$link(
    href = "https://fonts.googleapis.com/css2?family=Montserrat&display=swap", 
    rel = "stylesheet"
  ),
  tags$style(HTML("
    .leaflet-container, 
    .leaflet-control,
    .leaflet-tooltip,
    .leaflet-popup-content {
      font-family: 'Montserrat', sans-serif !important;
    }
  "))
)

#Map showing businesses within BP 1 mile walkshed ------ ORIGINAL---------- 
map_sbs_buffer <- leaflet(options = leafletOptions(zoomControl = FALSE)
                          ) %>%
  setView(lng = -76.98892, lat = 38.86713, zoom = 15.25) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = dc_minus_buffer_mask, #bp_buffer,
    fillColor = "darkgray",
    color = "darkgray", #"#27ae60",
    highlightOptions = highlightOptions(color = "white", 
                                        weight = 1,
                                        bringToFront = FALSE),
    opacity = .7,
    weight = 1.2,
    fillOpacity = .5,
    options = pathOptions()
    #group ="Count of FBO Owned Properties - Zip Code",
    #smoothFactor = 0.2,
    #label = shp_data$popup_label,
    #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
    ) %>%
  # addPolygons(
  #   data = ward_WOTR, #bp_buffer,
  #   fillColor = "darkgray",
  #   color = "darkgray", #"#27ae60",
  #   highlightOptions = highlightOptions(color = "white", 
  #                                       weight = 1,
  #                                       bringToFront = FALSE),
  #   opacity = .7,
  #   weight = 1.2,
  #   fillOpacity = .5,
  #   options = pathOptions()
  #   #group ="Count of FBO Owned Properties - Zip Code",
  #   #smoothFactor = 0.2,
  #   #label = shp_data$popup_label,
  #   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  # ) %>%
  # addMarkers(icon = ~ icons[ticker], # lookup based on ticker
  #            label = ~ address) %>%
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
    position = "topleft",#"bottomright", #
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
  addPolygons(
    data = sf::st_zm(bridge_park), #
    fillColor = "darkgreen",
    color = "darkgreen",
    highlightOptions = highlightOptions(
      color = "white",
      weight = 1,
      bringToFront = TRUE
      ),
    opacity = .7,
    weight = 3,
    label = "Future Site of the Bridge Park",  # Static label
    labelOptions = labelOptions(
      noHide = TRUE,  # Keeps the label visible at all times
      direction = "left",  # Position the label above the polygon
      offset = c(0, 15)  # Adjust the label's position a bit upwards
    )
    # fillOpacity = 0,
    # options = pathOptions()
    ) %>%
  # Notes,could add above^
  #   #group = "Count of FBO Owned Properties - Zip Code",
  #   #smoothFactor = 0.2,
  #   #label = shp_data$popup_label,
  #   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20)
  addControl( #commenting out temporarily
    html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection",  # Replace with your source not
    position = "bottomright"
    ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container {
          font-family: 'Montserrat', sans-serif !important;
        }
        .leaflet-control {
          font-family: 'Montserrat', sans-serif !important;
        }
        .leaflet-legend {
          font-family: 'Montserrat', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
      // Access the Leaflet layers (assuming your GeoJSON or SF layer is added here)
    var map = el; // Assuming 'el' is your map
    map.eachLayer(function(layer) {
      if (layer instanceof L.GeoJSON) {
        // Modify the stroke thickness of each GeoJSON layer (or replace with your specific layer type)
        layer.setStyle({
          weight: 3, // This controls the line thickness
          color: 'black', // Border color
          opacity: 1,
          fillColor: 'yellow', // Example fill color for polygons or markers
          fillOpacity: 0.6
        });
      }
    });
  }
")

map_sbs_buffer

saveWidget(map_sbs_buffer, file.path(peter_export, "map_sbs_walkshed.html"))


# map_sbs_buffer2 <- tagList(
#   font_header,  # Injects the font into the HTML document
#   
#   leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#     setView(lng = -76.98892, lat = 38.86713, zoom = 15.25) %>%
#     addProviderTiles(providers$CartoDB.Positron) %>%
#     addPolygons(
#       data = bp_buffer,
#       color = "#27ae60",
#       fillOpacity = 0,
#       weight = 1.2,
#       highlightOptions = highlightOptions(color = "white", weight = 1, bringToFront = FALSE)
#     ) %>%
#     addCircleMarkers(
#       data = small_biz_owner,
#       lng = ~longitude,
#       lat = ~latitude,
#       label = ~popup_label,
#       color = ~pal_biz(type_dc_categories),
#       radius = 3,
#       stroke = FALSE,
#       fillOpacity = 1,
#       clusterOptions = markerClusterOptions(freezeAtZoom = 21)
#     ) %>%
#     addLegend(
#       pal = pal_biz,
#       values = small_biz_owner$type_dc_categories,
#       position = "bottomright",
#       opacity = 1
#     ) %>%
#     addPolygons(
#       data = sf::st_zm(bridge_park),
#       fillColor = "darkgreen",
#       color = "darkgreen",
#       weight = 3,
#       opacity = 0.7,
#       highlightOptions = highlightOptions(color = "white", weight = 1, bringToFront = TRUE),
#       label = "Future Site of the Bridge Park",
#       labelOptions = labelOptions(noHide = TRUE, direction = "left", offset = c(0, 15))
#     )
# )
# 
# map_sbs_buffer2
# saveWidget(map_sbs_buffer2, file.path(peter_export, "map_sbs_buffer2.html"))


#Maps showing businesses by sub-categories within BP 1 mile walkshed ------ DETAILED BIZ ---------- 
create_small_biz_map <- function(data, 
                                 category_filter = NULL,
                                 color_by = "type_dc_categories",
                                 palette = "Dark2",
                                 title = NULL) {
  
  # Filter data if a filter is specified
  if (!is.null(category_filter)) {
    data <- data[data$type_dc_categories %in% category_filter, ]
  }
  
  # Set color palette based on the color_by variable
  pal <- colorFactor(
    palette = palette, 
    domain = data[[color_by]]
  )
  
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    setView(lng = -76.98892, lat = 38.86713, zoom = 15.25) %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    
    addPolygons(
      data = dc_minus_buffer_mask_eotr, #bp_buffer,
      fillColor = "darkgray",
      color = "darkgray", #"#27ae60",
      highlightOptions = highlightOptions(color = "white", 
                                          weight = 1,
                                          bringToFront = FALSE),
      opacity = .7,
      weight = 1.2,
      fillOpacity = .5,
      options = pathOptions()
    ) %>%
    
    addCircleMarkers(
      data = data,
      ~longitude, ~latitude, 
      label = ~popup_label,
      color = ~pal(data[[color_by]]),
      radius = 3,
      stroke = FALSE, 
      fillOpacity = 1,
      clusterOptions = markerClusterOptions(freezeAtZoom = 21)
    ) %>% 
    
    addLegend(
      pal = pal,
      values = data[[color_by]],
      position = "topleft", #"bottomright",
      title = title,
      opacity = 1
    ) %>%
    
    addPolygons(
      data = sf::st_zm(bridge_park),
      fillColor = "darkgreen",
      color = "darkgreen",
      highlightOptions = highlightOptions(color = "white", weight = 1, bringToFront = TRUE),
      opacity = .7,
      weight = 3,
      label = "Future Site of the Bridge Park",
      labelOptions = labelOptions(noHide = TRUE, direction = "left", offset = c(0, 15))
    ) %>%
    
    htmlwidgets::onRender("
      function(el, x) {
        var style = document.createElement('style');
        style.innerHTML = `
          .leaflet-container {
            font-family: 'Montserrat', sans-serif !important;
          }
          .leaflet-control {
            font-family: 'Montserrat', sans-serif !important;
          }
          .leaflet-legend {
            font-family: 'Montserrat', sans-serif !important;
          }
        `;
        document.head.appendChild(style);
        var map = el;
        map.eachLayer(function(layer) {
          if (layer instanceof L.GeoJSON) {
            layer.setStyle({
              weight: 3,
              color: 'black',
              opacity: 1,
              fillColor: 'yellow',
              fillOpacity: 0.6
            });
          }
        });
      }
    ")
  }


#Title: "Small Businesses within one mile of the Bridge Park"
#Subtitle: "Food-Serving Establishments" 
map_biz_food <- create_small_biz_map(
  data = small_biz_owner,
  category_filter = c("Food Services"),
  color_by = "type_of_business",
  palette = "Dark2", #optional, defaults to Dark2,
  title = "Food-Serving Establishments"
)
map_biz_food


#Title: "Small Businesses within one mile of the Bridge Park"
#Subtitle: "General Sales / Services"
map_biz_general <- create_small_biz_map(
  data = small_biz_owner,
  category_filter = c("General Sales / Services"),
  color_by = "type_of_business",
  #palette = , #optional, defaults to Dark2,
  title = "General Sales / Services"
)
map_biz_general


#Title: "Small Businesses within one mile of the Bridge Park"
#Subtitle: "Not food and not general retail"
map_biz_misc <- create_small_biz_map(
  data = small_biz_owner,
  category_filter = c("Health Services", "Other Regulated Business", "Barber Shop / Hair Salon", "Nonprofit"),
  color_by = "type_of_business",
  #palette = #optional, defaults to Dark2
  title = "Not food and not general retail"
  )
map_biz_misc



# Map showing businesses with a recent sale ----------------------------------
# Version 1 - no toggle
# map_props_sales <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
#   addTiles() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
# addCircleMarkers(
#   data = prop_owner,
#   ~longitude, ~latitude, 
#   #popup = ~as.character(name), 
#   label = ~popup_label,
#   color = case_when(
#     prop_owner$sale_category == "$0-$500K" ~ "#a1d99b",  # Darker light green  
#     prop_owner$sale_category == "$500K-$1M" ~ "#74c476",  # More saturated green  
#     prop_owner$sale_category == "$1M-$1.5M" ~ "#238b45",  # Strong medium green  
#     prop_owner$sale_category == "$1.5M-$2M" ~ "#006d2c",  # Very dark green  
#     prop_owner$sale_category == "$2M-$2.5M" ~ "#00441b",  # Deepest green  
#     prop_owner$sale_category == "No Recent Transaction in OTR Database" ~ "#00441b",  # Deepest green  
#     TRUE ~ "grey"
#   ), 
#   radius = case_when(
#     prop_owner$sale_category == "$0-$500K" ~ 3,       # Small radius for $0-$500K
#     prop_owner$sale_category == "$500K-$1M" ~ 3.5,      # Larger radius for $500K-$1M
#     prop_owner$sale_category == "$1M-$1.5M" ~ 4,      # Even larger radius for $1M-$1.5M
#     prop_owner$sale_category == "$1.5M-$2M" ~ 4.5,      # Larger still for $1.5M-$2M
#     prop_owner$sale_category == "$2M-$2.5M" ~ 5,     # Largest radius for $2M-$2.5M
#     TRUE ~ 2.5                               # Default radius if no match
#   ),
#   stroke = FALSE, 
#   fillOpacity = 1#,
#   #clusterOptions = markerClusterOptions(freezeAtZoom = 21)
#   ) %>% 
#   addLegend(
#     #pal = pal_sale,
#     #values = prop_owner$sale_category,
#     colors = c("#a1d99b", "#74c476", "#238b45", "#006d2c", "#00441b", "grey"),
#     labels = c("$0-$500K", "$500K-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "No Recent Transaction in OTR Database"),
#     position = "topleft",
#     title = "Small Business Locations - Recent Property Transaction",
#     #group = "Count of FBO Owned Properties - Zip Code",
#     # labFormat = function(type, cuts, p) {
#     #   # Define custom bin labels in order
#     #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
#     #
#     #   # Return labels for each cut
#     #   return(custom_labels)
#     # },
#     #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
#     opacity = 1) %>%
#   # addPolygons(
#   #   data = bridge_park,
#   #   fillColor = "#FFFF00",
#   #   color = "#273538",
#   #   highlightOptions = highlightOptions(color = "white", weight = 1,
#   #                                       bringToFront = TRUE),
#   #   opacity = .7,
#   #   weight = 1,
#   #   fillOpacity = 0,
#   #   #group ="Count of FBO Owned Properties - Zip Code",
#   #   #smoothFactor = 0.2,
# #   #label = shp_data$popup_label,
# #   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
# addControl(
#   html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection. As of 5/1/25.",  # Replace with your source note
#   position = "bottomright"
#   ) %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var style = document.createElement('style');
#       style.innerHTML = `
#         .leaflet-container {
#           font-family: 'Montserrat', sans-serif !important;
#         }
#         .leaflet-control {
#           font-family: 'Montserrat', sans-serif !important;
#         }
#         .leaflet-legend {
#           font-family: 'Montserrat', sans-serif !important;
#         }
#       `;
#       document.head.appendChild(style);
#     }
#   ")
# 
# map_props_sales

# Version 2 - toggle -----------------------------------------------------------
map_props_sales <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -76.98892, lat = 38.86713, zoom = 15.25) %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = dc_minus_buffer_mask, #bp_buffer,
    fillColor = "darkgray",
    color = "darkgray", #"#27ae60",
    highlightOptions = highlightOptions(color = "white", 
                                        weight = 1,
                                        bringToFront = FALSE),
    opacity = .7,
    weight = 1.2,
    fillOpacity = .5,
    options = pathOptions()
  ) %>%
  # All properties
  # addCircleMarkers(
  #   data = prop_owner,
  #   ~longitude, ~latitude,
  #   group = "All Properties",
  #   label = ~popup_label,
  #   color = ~case_when(
  #     sale_category == "$0-$1M" ~ "#a1d99b",
  #     sale_category == "$1M-$1.5M" ~ "#74c476",
  #     sale_category == "$1.5M-$2M" ~ "#238b45",
  #     sale_category == "$2M-$2.5M" ~ "#006d2c",
  #     sale_category == "$2.5M+" ~ "#00441b",
  #     sale_category == "No Recent Transaction in OTR Database" ~ "grey",
  #     TRUE ~ "grey"
  #   ),
  #   radius = ~case_when(
  #     sale_category == "$0-$500K" ~ 3,
  #     sale_category == "$500K-$1M" ~ 3.5,
  #     sale_category == "$1M-$1.5M" ~ 4,
  #     sale_category == "$1.5M-$2M" ~ 4.5,
  #     sale_category == "$2M-$2.5M" ~ 5,
  #     sale_category == "No Recent Transaction in OTR Database" ~ 2.5,
  #     TRUE ~ 2.5
  #   ),
  #   stroke = FALSE,
  #   fillOpacity = 1
  # ) %>%
  
  # Vacant properties
  addCircleMarkers(
    data = filter(prop_owner, vacant_flag == "Vacant"),
    ~longitude, ~latitude,
    group = "Vacant Properties",
    label = ~popup_label,
    fillColor = ~pal_sale(sale_category),
    # color = ~ifelse(vacant_flag == "Vacant", case_when(
    #   sale_category == "$0-$1M" ~ "#a1d99b",
    #   sale_category == "$1M-$1.5M" ~ "#74c476",
    #   sale_category == "$1.5M-$2M" ~ "#238b45",
    #   sale_category == "$2M-$2.5M" ~ "#006d2c",
    #   sale_category == "$2.5M+" ~ "#00441b",
    #   sale_category == "No Recent Transaction in OTR Database" ~ "grey",
    #   TRUE ~ "grey"
    #   )
    # ),
    fillOpacity = 1,
    radius = 4,
    stroke = T,
    weight = 0.5,
    color = "black"
    ) %>%
  
  # Occupied properties
  addCircleMarkers(
    data = filter(prop_owner, vacant_flag == "Not Vacant"),
    ~longitude, ~latitude,
    group = "Occupied Properties",
    label = ~popup_label,
    fillColor = ~pal_sale(sale_category),
    # color = ~ifelse(vacant_flag == "Not Vacant", case_when(
    #   sale_category == "$0-$1M" ~ "#a1d99b",
    #   sale_category == "$1M-$1.5M" ~ "#74c476",
    #   sale_category == "$1.5M-$2M" ~ "#238b45",
    #   sale_category == "$2M-$2.5M" ~ "#006d2c",
    #   sale_category == "$2.5M+" ~ "#00441b",
    #   sale_category == "No Recent Transaction in OTR Database" ~ "grey"#,
    #   #TRUE ~ "grey"
    #   )
    # ),
    fillOpacity = 1,
    radius = 4,
    stroke = T,
    weight = 0.5,
    color = "black"
  ) %>%
  
  # Legend
  addLegend(
    colors = c("#a1d99b", "#74c476", "#238b45", "#006d2c", "#00441b", "grey"),
    labels = c("$0-$1M", "$1M-$1.5M", "$1.5M-$2M", "$2M-$2.5M", "$2.5M+", "No Recent Transaction in OTR Database"),
    position = "topleft",
    title = "Small Business Locations – Most Recent Property Transaction",
    opacity = 1
  ) %>%
  
  # Layer toggle control
  addLayersControl(
    overlayGroups = c("Vacant Properties", "Occupied Properties"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Source note
  addControl(
    html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection. As of 5/1/25.",
    position = "bottomright"
  ) %>%
  
  # Font styling
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container,
        .leaflet-control,
        .leaflet-legend {
          font-family: 'Montserrat', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_props_sales

# Save the map
saveWidget(map_props_sales, file.path(peter_export, "map_props_sales.html"))



# Filtered for adjacent/overlapping Anacostia-side tracks
# Tracts: 007401,007406,007407,007503,007504,007601,007605

# Map showing Anacostia with census tract info -------------------------------
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
  # addPolygons(
  #   data = ward_EOTR,
  #   fillColor = NULL,
  #   color = "#273538",
  #   highlightOptions = highlightOptions(color = "white", 
  #                                       weight = 1,
  #                                       bringToFront = FALSE),
  #   opacity = 1,
  #   weight = 1.2,
  #   fillOpacity =.25,
  #   #group ="Count of FBO Owned Properties - Zip Code",
  #   #smoothFactor = 0.2,
  #   #label = shp_data$popup_label,
  #   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
  # ) %>% 
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
          font-family: 'Montserrat', sans-serif !important;
        }
        .leaflet-control {
          font-family: 'Montserrat', sans-serif !important;
        }
        .leaflet-legend {
          font-family: 'Montserrat', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_anacostia_ref



# Map showing businesses with a recent sale ----------------------------------
# Version 1 - no toggle
# map_prop_values <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
#   addTiles() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addCircleMarkers(
#     data = prop_owner,
#     ~longitude, ~latitude, 
#     #popup = ~as.character(name), 
#     label = ~popup_label,
#     color = ~case_when(
#       value_category == "$0-$1M" ~ "#a1d99b",  # Darker light green  
#       value_category == "$1M-$2.5M" ~ "#74c476",  # More saturated green  
#       value_category == "$2.5M-$5M" ~ "#238b45",  # Strong medium green  
#       value_category == "$5M-$10M" ~ "#006d2c",  # Very dark green  
#       value_category == "$10M+" ~ "#00441b",  # Deepest green  
#       value_category == "No Value in OTR Database" ~ "grey",  # Grey for NAs
#       TRUE ~ "black"  # Default fallback (optional)
#     ), 
#     radius = case_when(
#       prop_owner$value_category == "$0-$1M" ~ 3.5,      # Larger radius for $500K-$1M
#       prop_owner$value_category == "$1M-$2.5M" ~ 3.75,      # Even larger radius for $1M-$1.5M
#       prop_owner$value_category == "$2.5M-$5M" ~ 4,      # Larger still for $1.5M-$2M
#       prop_owner$value_category == "$5M-$10M" ~ 4.25,     # Largest radius for $2M-$2.5M
#       prop_owner$value_category == "$10M+" ~ 4.5,     # Largest radius for $2M-$2.5M
#       prop_owner$value_category == "No Value in OTR Database" ~ 3,
#       TRUE ~ 3                              # Default radius if no match
#     ),
#     stroke = FALSE, 
#     fillOpacity = 2#,
#     #clusterOptions = markerClusterOptions(freezeAtZoom = 21)
#   ) %>% 
#   addLegend(
#     #pal = pal_values,
#     #values = prop_owner$value_category,    
#     colors = c("#c7e9c0", "#a1d99b", "#31a354", "#006d2c", "#00441b", "grey"),  
#     labels = c("$0-$1M", "$1M-$2.5M", "$2.5M-$5M", "$5M-$10M", "$10M+", "No Value in OTR Database"),  
#     position = "topleft",
#     title = "Small Business Locations - Assessed Property Value",
#     #group = "Count of FBO Owned Properties - Zip Code",
#     # labFormat = function(type, cuts, p) {
#     #   # Define custom bin labels in order
#     #   custom_labels <- c("0", "1-5", "6-9", "10-13", "14-17", "18-300")
#     #
#     #   # Return labels for each cut
#     #   return(custom_labels)
#     # },
#     #labels = c("0", "1-5", "6-9", "10-13", "14-17", "18+"),
#     opacity = 1) %>%
#   # addPolygons(
#   #   data = bridge_park,
#   #   fillColor = "#FFFF00",
#   #   color = "#273538",
#   #   highlightOptions = highlightOptions(color = "white", weight = 1,
#   #                                       bringToFront = TRUE),
#   #   opacity = .7,
#   #   weight = 1,
#   #   fillOpacity = 0,
#   #   #group ="Count of FBO Owned Properties - Zip Code",
#   #   #smoothFactor = 0.2,
# #   #label = shp_data$popup_label,
# #   #labelOptions = labelOptions(direction = "bottom", offset = c(0, 20))
#   addControl(
#     html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection",  # Replace with your source note
#     position = "bottomright"
#     ) %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var style = document.createElement('style');
#       style.innerHTML = `
#         .leaflet-container {
#           font-family: 'Montserrat', sans-serif !important;
#         }
#         .leaflet-control {
#           font-family: 'Montserrat', sans-serif !important;
#         }
#         .leaflet-legend {
#           font-family: 'Montserrat', sans-serif !important;
#         }
#       `;
#       document.head.appendChild(style);
#     }
#   ")
# 
# map_prop_values
# 

# Version 2 - toggle
map_prop_values <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -76.98892, lat = 38.86713, zoom = 15.25) %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = dc_minus_buffer_mask, #bp_buffer,
    fillColor = "darkgray",
    color = "darkgray", #"#27ae60",
    highlightOptions = highlightOptions(color = "white", 
                                        weight = 1,
                                        bringToFront = FALSE),
    opacity = .7,
    weight = 1.2,
    fillOpacity = .5,
    options = pathOptions()
  ) %>%
  # All properties
  # addCircleMarkers(
  #   data = prop_owner,
  #   ~longitude, ~latitude,
  #   group = "All Properties",
  #   label = ~popup_label,
  #   color = ~case_when(
  #     value_category == "$0-$1M" ~ "#a1d99b",
  #     value_category == "$1M-$2.5M" ~ "#74c476",
  #     value_category == "$2.5M-$5M" ~ "#238b45",
  #     value_category == "$5M-$10M" ~ "#006d2c",
  #     value_category == "$10M+" ~ "#00441b",
  #     value_category == "No Value in OTR Database" ~ "grey",
  #     TRUE ~ "black"
  #   ),
  #   radius = ~case_when(
  #     value_category == "$0-$1M" ~ 3.5,
  #     value_category == "$1M-$2.5M" ~ 3.75,
  #     value_category == "$2.5M-$5M" ~ 4,
  #     value_category == "$5M-$10M" ~ 4.25,
  #     value_category == "$10M+" ~ 4.5,
  #     value_category == "No Value in OTR Database" ~ 3,
  #     TRUE ~ 3
  #   ),
  #   stroke = FALSE,
  #   fillOpacity = 2
  # ) %>%
  
  # Vacant properties
  addCircleMarkers(
    data = filter(prop_owner, vacant_flag == "Vacant"),
    ~longitude, ~latitude,
    group = "Vacant Properties",
    label = ~popup_label,
    fillColor = ~pal_values(value_category),
    # color = ~case_when(
    #   value_category == "$0-$1M" ~ "#a1d99b",
    #   value_category == "$1M-$2.5M" ~ "#74c476",
    #   value_category == "$2.5M-$5M" ~ "#238b45",
    #   value_category == "$5M-$10M" ~ "#006d2c",
    #   value_category == "$10M+" ~ "#00441b",
    #   value_category == "No Value in OTR Database" ~ "grey",
    #   TRUE ~ "black"
    # ),
    fillOpacity = 2,
    radius = 4,
    stroke = TRUE,
    weight = 0.5,
    color = "black"
  ) %>%
  
  # Occupied properties
  addCircleMarkers(
    data = filter(prop_owner, vacant_flag == "Not Vacant"),
    ~longitude, ~latitude,
    group = "Occupied Properties",
    label = ~popup_label,
    fillColor = ~pal_values(value_category),
    # color = ~case_when(
    #   value_category == "$0-$1M" ~ "#a1d99b",
    #   value_category == "$1M-$2.5M" ~ "#74c476",
    #   value_category == "$2.5M-$5M" ~ "#238b45",
    #   value_category == "$5M-$10M" ~ "#006d2c",
    #   value_category == "$10M+" ~ "#00441b",
    #   value_category == "No Value in OTR Database" ~ "grey",
    #   TRUE ~ "black"
    # ),
    fillOpacity = 2,
    radius = 4,
    stroke = TRUE,
    weight = 0.5,
    color = "black"
  ) %>%
  
  # Legend for value categories (same for all groups)
  addLegend(
    colors = c("#a1d99b", "#74c476", "#238b45", "#006d2c", "#00441b", "grey"),
    labels = c("$0-$1M", "$1M-$2.5M", "$2.5M-$5M", "$5M-$10M", "$10M+", "No Value in OTR Database"),
    position = "topleft",
    title = "Anacostia Properties – Assessed Property Value",
    opacity = 1
  ) %>%
  
  # Layer toggle control
  addLayersControl(
    overlayGroups = c("Vacant Properties", "Occupied Properties"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Source note
  addControl(
    html = "Sources: DC Office of Tax and Revenue, DC Department of Licensing and Consumer Protection. \nAs of 5/1/25.",
    position = "bottomright"
  ) %>%
  
  # Font styling
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = `
        .leaflet-container,
        .leaflet-control,
        .leaflet-legend {
          font-family: 'Montserrat', sans-serif !important;
        }
      `;
      document.head.appendChild(style);
    }
  ")

map_prop_values
saveWidget(map_prop_values, file.path(peter_export, "map_prop_values.html"))



# Could Do a Per Square Foot Map





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

