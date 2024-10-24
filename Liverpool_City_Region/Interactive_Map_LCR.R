# 1. Setup ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(tidytransit)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)
library(ggspatial)
library(classInt)
library(leaflet)
library(sp)
library(htmlwidgets)
library(htmltools)
setwd("~/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access/Liverpool_City_Region")

# 2. Load Data ------------------------------------------------------------
# Load Liverpool Geo data from file
LCR_dataset_interactive <- read_sf("../../Data/LCR_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
         "PT_Job_Access_Index" = "PT_Jb_A_I",
         "PT_Job_Access_Index_bus" = "PT_J_A_I_B",
         "PT_Job_Access_Index_train" = "PT_J_A_I_T",
         "PT_Job_Access_Index_walk" = "PT_J_A_I_W",         
         "Jobs" = "Employd",
         "LSOA_Area" = "LSOA__2",
         "Traveltime_empcent" = "Tr__E_C",
         "PT_Job_Access_Index_Demand" = "PT_Jb_A_I_") %>%
  st_transform(4326) %>%
  mutate(PT_Job_Access_Index_bus = PT_Job_Access_Index_bus - PT_Job_Access_Index_walk,
         PT_Job_Access_Index_train = PT_Job_Access_Index_train - PT_Job_Access_Index_walk,
         LSOA_Code = as.character(LSOA_Code),
         LSOA_Area = round(LSOA_Area,2),
         PT_Job_Access_Index = round(PT_Job_Access_Index,2),
         ) %>%
  dplyr::select(LSOA_Code, 
                LSOA_Name, 
                Jobs, 
                LSOA_Area, 
                Traveltime_empcent, 
                PT_Job_Access_Index, 
                PT_Job_Access_Index_bus, 
                PT_Job_Access_Index_train, 
                PT_Job_Access_Index_walk, 
                PT_Job_Access_Index_Demand) %>%
  as("Spatial")


# spatial_polygons_df <- as(sf_object, "Spatial")


# LCR Boundary + buffer
Boundaries <- read_sf("../../Data/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) %>%
  as("Spatial")
# LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=200)

# Town centres
towns_centres_LCR <- st_read("../../Data/towns_centres_LCR.shp") %>%
  filter(buffer == "N") %>%
  as_Spatial()


PTJA_pal <- colorNumeric(palette = "Spectral",                       
                       domain = LCR_dataset_interactive@data$PT_Job_Access_Index,
                       n=5,
                       reverse = TRUE)

# Interactive Map
map <- LCR_dataset_interactive %>%
  leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
#  setView(lat = 53.407438, lng = -2.981652, zoom = 10) %>%
  addPolygons(data = LCR_boundary, 
              color = "red", 
              weight = 1,
              fill = FALSE) %>%
  addPolygons(data = LCR_dataset_interactive,
              color = ~PTJA_pal(PT_Job_Access_Index),
              fillOpacity = .8,
              weight = 1, 
              label = ~lapply(paste0(
                                     "<strong>LSOA Code: </strong>", as.character(LSOA_Code), "<br>",
                                     "<strong>LSOA Name: </strong>", LSOA_Name, "<br>"
                                   ),  htmltools::HTML),
              highlight = highlightOptions(weight =3, color ="red",                                         
                                           bringToFront =TRUE),
              popup = ~lapply(paste0(
                                      "<strong>LSOA Code: </strong>", as.character(LSOA_Code), "<br>",
                                      "<strong>LSOA Name: </strong>", LSOA_Name, "<br>",
                      #                "<strong>Population: </strong>", Population, "<br>",
                                      "<strong>Jobs: </strong>", Jobs, "<br>",
                                      "<strong>LSOA Area: </strong>", LSOA_Area, "<br>",
                                      "<strong>Time to nearest town centre: </strong>", Traveltime_empcent, "<br>",
                                      "<strong>PT Job Access Index: </strong>", PT_Job_Access_Index, "<br>"),
                                      htmltools::HTML)) %>%
  addCircleMarkers(data = towns_centres_LCR,
                   label = ~id,
                   radius = 1,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 4,
                   color = "#03F") 

# Save Map
saveWidget(map, file="../Interactive_Map_LCR.html")

#               "<strong>PT Job Access Index Bus: </strong>", PT_Job_Access_Index_bus, "<br>",
#                "<strong>PT Job Access Index Train: </strong>", PT_Job_Access_Index_train, "<br>",
#                "<strong>PT Job Access Index Walk: </strong>", PT_Job_Access_Index_walk, "<br>",
#                "<strong>PT Job Access Index Demand: </strong>", PT_Job_Access_Index_Demand
