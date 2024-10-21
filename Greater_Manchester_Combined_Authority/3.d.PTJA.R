## Script for Public Transport Job Accessibility distribution across GMCA
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

# 2. Load Data ------------------------------------------------------------
# Load Manchester Geo data from file
MANCH_dataset_PTJA <- read_sf("Data/MANCH_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
         "TravelTime_Jobcentre" = "Tr__J_C",
         "PT_Job_Access_Index" = "PT_Jb_A_I",
         "PT_Job_Access_Index_bus" = "PT_J_A_I_B",
         "PT_Job_Access_Index_tram" = "PT_J_A_I_T",
         "PT_Job_Access_Index_walk" = "PT_J_A_I_W",         
         "Employed_Population" = "Employd",
         "LSOA_Area" = "LSOA__2",
         "Traveltime_empcent" = "Tr__E_C",
         "PT_Job_Access_Index_Demand" = "PT_Jb_A_I_") %>%
  st_transform(4326) %>%
  mutate(PT_Job_Access_Index_bus = PT_Job_Access_Index_bus - PT_Job_Access_Index_walk,
         PT_Job_Access_Index_tram = PT_Job_Access_Index_tram - PT_Job_Access_Index_walk) 

# Load Metrolink Shapefile
Metrolink <- st_read("Data/GM_Metrolink_MapData/SHP-format/Metrolink_Lines_Functional.shp")
Metrolink$LineName <- "Metrolink"
# GMCA Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=25)

# Town centres
towns_centroids <- read_sf("Data/towns_centroids.shp")
towns_centroids_Man <- towns_centroids %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

# # Plot PTJA index in GMCA ---------------------------------------------
MANCH_dataset_PTJA %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="red",linewidth=1.5)+
  geom_sf(aes(fill = PT_Job_Access_Index), col = NA) +
  scale_fill_gradient(low="BLACK", high="WHITE") +
  labs(fill = "PTJA Index")+
  geom_sf(data=Metrolink, colour="red",linewidth=0.5)+
  theme_void()


# Plot PTJA index in GMCA by quantile
# Calculate PTJA quantile breaks
breaks_PTJA <- classIntervals(MANCH_dataset_PTJA$PT_Job_Access_Index, n = 5, style = "quantile")$brks
breaks_PTJA[1] <- 00
# Create a factor variable for PTJA coloring
MANCH_dataset_PTJA$color_PTJA <- cut(MANCH_dataset_PTJA$PT_Job_Access_Index, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
MANCH_dataset_PTJA$color_PTJA <- as.factor(MANCH_dataset_PTJA$color_PTJA)
MANCH_dataset_PTJA$color_PTJA_bus <- cut(MANCH_dataset_PTJA$PT_Job_Access_Index_bus, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
MANCH_dataset_PTJA$color_PTJA_bus <- as.factor(MANCH_dataset_PTJA$color_PTJA_bus)
MANCH_dataset_PTJA$color_PTJA_tram <- cut(MANCH_dataset_PTJA$PT_Job_Access_Index_tram, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
MANCH_dataset_PTJA$color_PTJA_tram <- as.factor(MANCH_dataset_PTJA$color_PTJA_tram)
labels_PTJA <- c("Lowest PTJA Quintile","","","","Highest PTJA Quintile")
# metrolink line colouring to include in legend
line_colours <- setNames(rep("black", length(unique(Metrolink$LineName))), unique(Metrolink$LineName)) 

# Plot PTJA by quintile in GMCA
(PTJA <- MANCH_dataset_PTJA %>%
    dplyr::select(color_PTJA, geometry) %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.0) +
  geom_sf(data = MANCH_dataset_PTJA, aes(fill = color_PTJA), color = NA) + # color = NA removes the LSOA boundaries
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_PTJA) +
  labs(fill = "Public Transport
       Job Accessibility 
       (quintiles)",
       color = "Metrolink")+
  geom_sf(data = Metrolink, 
          aes(color = LineName), 
      #    colour="black", 
          linewidth = 0.8) +
  geom_sf(data=towns_centroids_Man, shape = 21, fill = 'white', size = 1.5) +
  geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
  geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
  geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
  geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
  geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
  geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
  geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
  geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
 scale_color_manual(values = line_colours)+
    annotation_north_arrow()+
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
  theme_void())

ggsave(file = "Plots/PTJA.jpeg", device = "jpeg", plot = PTJA)

# bus contribution map
(PTJA_bus <- MANCH_dataset_PTJA %>%
    select(color_PTJA_bus, geometry) %>%
    ggplot() +
    geom_sf(data=GMCA_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = MANCH_dataset_PTJA, aes(fill = color_PTJA_bus), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = c("Lowest","","","","Highest")) +
    labs(fill = "Public Transport Job 
         Accessibility: Bus",
         color = "Metrolink")+
    geom_sf(data = Metrolink, 
            aes(color = LineName), 
                colour="black", 
            linewidth = 0.8) +
    theme_void())

# tram contribution map
(PTJA_tram <- MANCH_dataset_PTJA %>%
    dplyr::select(color_PTJA_tram, geometry) %>%
    ggplot() +
    geom_sf(data=GMCA_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = MANCH_dataset_PTJA, aes(fill = color_PTJA_tram), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = c("Lowest","","","","Highest")) +
    labs(fill = "Public Transport Job 
         Accessibility: Tram",
         color = "Metrolink")+
    geom_sf(data = Metrolink, 
            aes(color = LineName), 
            colour="black", 
            linewidth = 0.8) +
    theme_void())
mapview(MANCH_dataset_PTJA$color_PTJA_tram)


# Adjusted for Demand Potential -------------------------------------------
# Calculate PTJA-D quantile breaks
breaks_PTJA_D <- classIntervals(MANCH_dataset_PTJA$PT_Job_Access_Index_Demand, 
                                n = 5, style = "quantile")$brks
breaks_PTJA_D[1] <- 00
# Create a factor variable for PTJA coloring
MANCH_dataset_PTJA$color_PTJA_D <- cut(MANCH_dataset_PTJA$PT_Job_Access_Index_Demand, 
                                       breaks = breaks_PTJA_D, 
                                       include.lowest = TRUE, 
                                       labels = FALSE) %>%
  as.factor()


labels_PTJA_D <- c("Lowest PTJA-D Quintile","","","","Highest PTJA-D Quintile")
# metrolink line colouring to include in legend
line_colours <- setNames(rep("black", length(unique(Metrolink$LineName))), unique(Metrolink$LineName)) 

# Plot PTJA by quintile in GMCA
(PTJA_D <- MANCH_dataset_PTJA %>%
    dplyr::select(color_PTJA_D, geometry) %>%
    ggplot() +
    geom_sf(data=GMCA_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = MANCH_dataset_PTJA, aes(fill = color_PTJA_D), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = labels_PTJA_D) +
    labs(fill = "Public Transport
       Job Accessibility 
       (Demand-adjusted)
       (quintiles)",
         color = "Metrolink")+
    geom_sf(data = Metrolink, 
            aes(color = LineName), 
            #    colour="black", 
            linewidth = 0.8) +
    geom_sf(data=towns_centroids_Man, shape = 21, fill = 'white', size = 1.5) +
    geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
    geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
    geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
    geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
    geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
    geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
    geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
    geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
    scale_color_manual(values = line_colours)+
    annotation_north_arrow()+
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
    theme_void())

ggsave(file = "Images/PTJA_D.jpeg", device = "jpeg", plot = PTJA_D)
