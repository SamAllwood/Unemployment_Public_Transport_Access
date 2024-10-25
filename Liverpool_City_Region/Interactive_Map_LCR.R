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
  as("Spatial") # translates the shapefile into a spatial polygons dataframe


## spatial_polygons_df <- as(sf_object, "Spatial")

# LCR Boundary + buffer
Boundaries <- read_sf("../../Data/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) %>%
  as("Spatial")
LCR_bound_small_buffer <- Boundaries %>% 
  filter(CAUTH23NM == "Liverpool City Region") %>%
  st_buffer(dist=1500) %>%
  st_transform(4326) 


# Read Towns and City boundaries
towns <- st_read("../../Data/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_LCR <- towns[st_within(towns, LCR_bound_small_buffer, sparse = FALSE), ] %>%
  as("Spatial")


# Town centres
towns_centres_LCR <- st_read("../../Data/towns_centres_LCR.shp") %>%
  filter(buffer == "N") %>%
  as_Spatial()


PTJA_pal <- colorNumeric(palette = "Spectral",                       
                       domain = LCR_dataset_interactive@data$PT_Job_Access_Index,
                       n=5,
                       reverse = TRUE)
PTJA_D_pal <- colorNumeric(palette = "Spectral",                       
                         domain = LCR_dataset_interactive@data$PT_Job_Access_Index_Demand,
                         n=5,
                         reverse = TRUE)
# Interactive Map
(map <- LCR_dataset_interactive %>%
  leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
#  setView(lat = 53.407438, lng = -2.981652, zoom = 10) %>%
  addPolygons(data = LCR_boundary, 
              color = "red", 
              weight = 1,
              fill = FALSE) %>%
  addPolygons(data = LCR_dataset_interactive,
              color = ~PTJA_D_pal(PT_Job_Access_Index_Demand),
              fillOpacity = .8,
              weight = 1, 
              group = "PTJA-D",
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
                                      "<strong>PT Job Access Index Demand: </strong>", PT_Job_Access_Index_Demand ),
                                      htmltools::HTML)) %>%
    addPolygons(data = LCR_dataset_interactive,
                color = ~PTJA_pal(PT_Job_Access_Index),
                fillOpacity = .8,
                weight = 1, 
                group = "PTJA",
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
                  "<strong>PT Job Access Index: </strong>", PT_Job_Access_Index ),
                  htmltools::HTML)) %>%
  addCircleMarkers(data = towns_centres_LCR,
                   popup = ~id,
                   radius = 1,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 4,
                   color = "#03F",
                   group = "Towns") %>%
  addLegend("topright", 
            pal = PTJA_D_pal, 
            values = ~PT_Job_Access_Index_Demand,
            title = "PTJA-D",
            opacity = 1,
            group = "PTJA-D") %>%
    addLegend("topright", 
              pal = PTJA_pal, 
              values = ~PT_Job_Access_Index,
              title = "PTJA",
              opacity = 1,
              group = "PTJA") %>%
  addPolygons(data = towns_within_LCR, 
              color = "black", 
              weight = 2,
              fill = FALSE) %>%  
  addLayersControl("Layers", 
                   position = "topleft",
                   baseGroups = c("PTJA", "PTJA-D"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar((position = "bottomleft"))
)

# Save Map
saveWidget(map, file="../docs/index.html")

#               "<strong>PT Job Access Index Bus: </strong>", PT_Job_Access_Index_bus, "<br>",
#                "<strong>PT Job Access Index Train: </strong>", PT_Job_Access_Index_train, "<br>",
#                "<strong>PT Job Access Index Walk: </strong>", PT_Job_Access_Index_walk, "<br>",
#                "<strong>PT Job Access Index Demand: </strong>", PT_Job_Access_Index_Demand
