## Interactive map (Leaflet) showing PTJA and PTJA-D across WYCA
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

## Read WYCA datasets
WYCA_dataset_interactive <- read_sf("../Data/WYCA_dataset.shp") %>%
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
         Job_density = round(Jobs/LSOA_Area,2),
  ) %>%
  dplyr::select(LSOA_Code, 
                LSOA_Name, 
                Jobs, 
                LSOA_Area, 
                Traveltime_empcent, 
                PT_Job_Access_Index, 
                PT_Job_Access_Index_bus,  
                PT_Job_Access_Index_walk, 
                PT_Job_Access_Index_Demand,
                Job_density) %>%
  as("Spatial")

## Colour palettes definition
PTJA_D_pal <- colorNumeric(palette = "Spectral",                       
                           domain = WYCA_dataset_interactive@data$PT_Job_Access_Index_Demand,
                           n=5,
                           reverse = TRUE)
jobs_pal <- colorQuantile(palette = "Spectral",                       
                          domain = WYCA_dataset_interactive@data$Job_density,
                          n=5,
                          reverse = TRUE)



# Interactive Map
(map_WYCA <- WYCA_dataset_interactive %>%
    leaflet() %>%
    addProviderTiles("OpenStreetMap") %>%
    #  setView(lat = 53.407438, lng = -2.981652, zoom = 10) %>%
    addPolygons(
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
                  "<strong>Jobs: </strong>", Jobs, "<br>",
                  "<strong>Time to nearest town centre: </strong>", Traveltime_empcent, "<br>"),
                  htmltools::HTML)) %>%
    #    addPolygons(data = interactive_datasets,
    #                color = ~PTJA_pal(PT_Job_Access_Index),
    #                fillOpacity = .8,
    #                weight = 1, 
    #                group = "PTJA",
    #                label = ~lapply(paste0(
    #                  "<strong>LSOA Code: </strong>", as.character(LSOA_Code), "<br>",
    #                  "<strong>LSOA Name: </strong>", LSOA_Name, "<br>"
    #                ),  htmltools::HTML),
    #                highlight = highlightOptions(weight =3, color ="red",                                         
    #                                             bringToFront =TRUE),
    #                popup = ~lapply(paste0(
    #                  "<strong>LSOA Code: </strong>", as.character(LSOA_Code), "<br>",
    #                  "<strong>LSOA Name: </strong>", LSOA_Name, "<br>",
    #                                  "<strong>Population: </strong>", Population, "<br>",
    #                  "<strong>Jobs: </strong>", Jobs, "<br>",
    #                  "<strong>LSOA Area: </strong>", LSOA_Area, "<br>",
    #                  "<strong>Time to nearest town centre: </strong>", Traveltime_empcent, "<br>",
    #                  "<strong>PT Job Access Index: </strong>", PT_Job_Access_Index ),
    #                  htmltools::HTML)) %>%
    addPolygons(
                color = ~jobs_pal(Job_density),
                fillOpacity = .8,
                weight = 1, 
                group = "Jobs",
                label = ~lapply(paste0(
                  "<strong>LSOA Code: </strong>", as.character(LSOA_Code), "<br>",
                  "<strong>LSOA Name: </strong>", LSOA_Name, "<br>"
                ),  htmltools::HTML),
                highlight = highlightOptions(weight =3, color ="red",                                         
                                             bringToFront =TRUE),
                popup = ~lapply(paste0(
                  "<strong>LSOA Code: </strong>", as.character(LSOA_Code), "<br>",
                  "<strong>LSOA Name: </strong>", LSOA_Name, "<br>",
                  "<strong>Jobs: </strong>", Jobs, "<br>",
                  "<strong>Time to nearest town centre: </strong>", Traveltime_empcent, "<br>"),
                  htmltools::HTML)) %>%
    #  addLegend("topright", 
    #            pal = PTJA_D_pal, 
    #            values = ~PT_Job_Access_Index_Demand,
    #            title = "PTJA-D",
    #            opacity = 1,
    #            group = "PTJA-D") %>%
    #  addLegend("topright", 
    #            pal = PTJA_pal, 
    #            values = ~PT_Job_Access_Index,
    #            title = "PTJA",
    #            opacity = 1,
    #            group = "PTJA") %>%
    #  addLegend("bottomright", 
    #            pal = jobs_pal, 
    #            values = ~Job_density,
    #            title = "Jobs",
    #            opacity = 1,
    #            group = "Jobs") %>%
    addLayersControl( 
      position = "topleft",
      baseGroups = c("PTJA-D", "Jobs (Quintiles"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar((position = "bottomleft"))%>%
    hideGroup("PTJA-D") 
)

# Save Map
htmlwidgets::saveWidget(map_WYCA, file="docs/index_WYCA.html", selfcontained = FALSE)
