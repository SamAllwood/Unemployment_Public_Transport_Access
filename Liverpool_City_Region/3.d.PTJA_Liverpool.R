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

setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")

# 2. Load Data ------------------------------------------------------------

# Load Liverpool Geo data from file
LCR_dataset_PTJA <- read_sf("Data/LCR_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
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


# LCR Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=25)

# Town centres
towns_centroids_LCR <- read_sf("Data/towns_centroids_LCR.shp")

# # Plot PTJA index in LCR ---------------------------------------------
LCR_dataset_PTJA %>%
  ggplot() +
  geom_sf(data=LCR_boundary, colour="red",linewidth=1.5)+
  geom_sf(aes(fill = PT_Job_Access_Index), col = NA) +
  scale_fill_gradient(low="BLACK", high="WHITE") +
  labs(fill = "PTJA Index")+
  theme_void()


# Plot PTJA index in LCR by quantile
# Calculate PTJA quantile breaks
breaks_PTJA <- classIntervals(LCR_dataset_PTJA$PT_Job_Access_Index, n = 5, style = "quantile")$brks
breaks_PTJA[1] <- 00
# Create a factor variable for PTJA coloring
LCR_dataset_PTJA$color_PTJA <- cut(LCR_dataset_PTJA$PT_Job_Access_Index, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
LCR_dataset_PTJA$color_PTJA <- as.factor(LCR_dataset_PTJA$color_PTJA)
LCR_dataset_PTJA$color_PTJA_bus <- cut(LCR_dataset_PTJA$PT_Job_Access_Index_bus, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
LCR_dataset_PTJA$color_PTJA_bus <- as.factor(LCR_dataset_PTJA$color_PTJA_bus)
LCR_dataset_PTJA$color_PTJA_tram <- cut(LCR_dataset_PTJA$PT_Job_Access_Index_tram, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
LCR_dataset_PTJA$color_PTJA_tram <- as.factor(LCR_dataset_PTJA$color_PTJA_tram)
labels_PTJA <- c("Lowest PTJA Quintile","","","","Highest PTJA Quintile")

# Plot PTJA by quintile in LCR
(PTJA <- LCR_dataset_PTJA %>%
    dplyr::select(color_PTJA, geometry) %>%
  ggplot() +
  geom_sf(data=LCR_boundary, colour="black",linewidth=1.0) +
  geom_sf(data = LCR_dataset_PTJA, aes(fill = color_PTJA), color = NA) + # color = NA removes the LSOA boundaries
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_PTJA) +
  labs(fill = "Public Transport
       Job Accessibility 
       (quintiles)") +
  geom_sf(data=towns_centroids_LCR, shape = 21, fill = 'white', size = 1.5) +
#  geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
#  geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
#  geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
#  geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
#  geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
#  geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
#  geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
#  geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
    annotation_north_arrow()+
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
  theme_void())

ggsave(file = "3.Liverpool_Maps/PTJA.jpeg", device = "jpeg", plot = PTJA)

# bus contribution map
(PTJA_bus <- LCR_dataset_PTJA %>%
    select(color_PTJA_bus, geometry) %>%
    ggplot() +
    geom_sf(data=LCR_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = LCR_dataset_PTJA, aes(fill = color_PTJA_bus), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = c("Lowest","","","","Highest")) +
    labs(fill = "Public Transport Job 
         Accessibility: Bus")+
    theme_void())

# tram contribution map
(PTJA_tram <- LCR_dataset_PTJA %>%
    dplyr::select(color_PTJA_tram, geometry) %>%
    ggplot() +
    geom_sf(data=LCR_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = LCR_dataset_PTJA, aes(fill = color_PTJA_tram), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = c("Lowest","","","","Highest")) +
    labs(fill = "Public Transport Job 
         Accessibility: Tram") +
    theme_void())


# Adjusted for Demand Potential -------------------------------------------
# Calculate PTJA-D quantile breaks
breaks_PTJA_D <- classIntervals(LCR_dataset_PTJA$PT_Job_Access_Index_Demand, 
                                n = 5, style = "quantile")$brks
breaks_PTJA_D[1] <- 00
# Create a factor variable for PTJA coloring
LCR_dataset_PTJA$color_PTJA_D <- cut(LCR_dataset_PTJA$PT_Job_Access_Index_Demand, 
                                       breaks = breaks_PTJA_D, 
                                       include.lowest = TRUE, 
                                       labels = FALSE) %>%
  as.factor()


labels_PTJA_D <- c("Lowest PTJA-D Quintile","","","","Highest PTJA-D Quintile")

# Plot PTJA by quintile in LCR
(PTJA_D <- LCR_dataset_PTJA %>%
    dplyr::select(color_PTJA_D, geometry) %>%
    ggplot() +
    geom_sf(data=LCR_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = LCR_dataset_PTJA, aes(fill = color_PTJA_D), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = labels_PTJA_D) +
    labs(fill = "Public Transport
       Job Accessibility 
       (Demand-adjusted)
       (quintiles)")+
    geom_sf(data=towns_centroids_LCR, shape = 21, fill = 'white', size = 1.5) +
 #   geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
#    geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
#    geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
#    geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
#    geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
#    geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
#    geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
#    geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
    annotation_north_arrow()+
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm")) +
    theme_void())

ggsave(file = "3.Liverpool_Maps/PTJA_D.jpeg", device = "jpeg", plot = PTJA_D)
