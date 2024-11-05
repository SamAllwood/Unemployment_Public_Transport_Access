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
# Load WYCA Geo data from file
WYCA_dataset_PTJA <- read_sf("../Data/WYCA_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
         "PT_Job_Access_Index" = "PT_Jb_A_I",
         "PT_Job_Access_Index_bus" = "PT_J_A_I_B",
         "PT_Job_Access_Index_train" = "PT_J_A_I_T",
         "PT_Job_Access_Index_walk" = "PT_J_A_I_W",         
         "Employed_Population" = "Employd",
         "LSOA_Area" = "LSOA__2",
         "Traveltime_empcent" = "Tr__E_C",
         "PT_Job_Access_Index_Demand" = "PT_Jb_A_I_") %>%
  st_transform(4326) %>%
  mutate(PT_Job_Access_Index_bus = PT_Job_Access_Index_bus - PT_Job_Access_Index_walk,
         PT_Job_Access_Index_train = PT_Job_Access_Index_train - PT_Job_Access_Index_walk) 


# WYCA Boundary + buffer
Boundaries <- read_sf("../Data/CAUTH_DEC_2023_EN_BFC.shp")
WYCA_boundary <- Boundaries %>% filter(CAUTH23NM == "West Yorkshire") %>%
  st_transform(4326) 
WYCA_bound_small_buffer <- WYCA_boundary %>% st_buffer(dist=25)

# Town centres
towns_centres_WYCA <- read_sf("../Data/towns_centres_WYCA.shp") %>%
  filter(buffer == "N")

# # Plot PTJA index in WYCA ---------------------------------------------
WYCA_dataset_PTJA %>%
  ggplot() +
  geom_sf(data=WYCA_boundary, colour="red",linewidth=1.5)+
  geom_sf(aes(fill = PT_Job_Access_Index), col = NA) +
  scale_fill_gradient(low="BLACK", high="WHITE") +
  labs(fill = "PTJA Index")+
  theme_void()


# Plot PTJA index in WYCA by quantile
# Calculate PTJA quantile breaks
breaks_PTJA <- classIntervals(WYCA_dataset_PTJA$PT_Job_Access_Index, n = 5, style = "quantile")$brks
breaks_PTJA[1] <- 00
# Create a factor variable for PTJA coloring
WYCA_dataset_PTJA$color_PTJA <- cut(WYCA_dataset_PTJA$PT_Job_Access_Index, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
WYCA_dataset_PTJA$color_PTJA <- as.factor(WYCA_dataset_PTJA$color_PTJA)
WYCA_dataset_PTJA$color_PTJA_bus <- cut(WYCA_dataset_PTJA$PT_Job_Access_Index_bus, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
WYCA_dataset_PTJA$color_PTJA_bus <- as.factor(WYCA_dataset_PTJA$color_PTJA_bus)
WYCA_dataset_PTJA$color_PTJA_train <- cut(WYCA_dataset_PTJA$PT_Job_Access_Index_train, breaks = breaks_PTJA, include.lowest = TRUE, labels = FALSE)
WYCA_dataset_PTJA$color_PTJA_train <- as.factor(WYCA_dataset_PTJA$color_PTJA_train)
labels_PTJA <- c("Lowest PTJA Quintile","","","","Highest PTJA Quintile")

# Plot PTJA by quintile in WYCA
(PTJA <- WYCA_dataset_PTJA %>%
    dplyr::select(color_PTJA, geometry) %>%
  ggplot() +
  geom_sf(data=WYCA_boundary, colour="black",linewidth=1.0) +
  geom_sf(data = WYCA_dataset_PTJA, aes(fill = color_PTJA), color = NA) + # color = NA removes the LSOA boundaries
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_PTJA) +
  labs(fill = "PTJA 
       (quintiles)",
       title = "Public Transport Job Accessibility in West Yorkshire Combined Authority") +
  geom_sf(data=towns_centres_WYCA, shape = 21, fill = 'white', size = 1.5) +
    geom_label( x=-1.756, y=53.83, label="Bradford", size=3) +
    geom_label( x=-1.859, y=53.75, label="Halifax", size=3) +
    geom_label( x=-1.782, y=53.675, label="Huddersfield", size=3) +
    geom_label( x=-1.545, y=53.825, label="Leeds", size=3) +
    geom_label( x=-1.5058, y=53.70, label="Wakefield", size=3)+
    annotation_north_arrow(height = unit(1, "cm"),
                           width = unit(1, "cm"),) +
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
  theme_void())

ggsave(file = "WYCA/Images/PTJA.jpeg", device = "jpeg", plot = PTJA)

# bus contribution map
(PTJA_bus <- WYCA_dataset_PTJA %>%
    dplyr::select(color_PTJA_bus, geometry) %>%
    ggplot() +
    geom_sf(data=WYCA_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = WYCA_dataset_PTJA, aes(fill = color_PTJA_bus), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = c("Lowest",
                                 "",
                                 "",
                                 "",
                                 "Highest")) +
    labs(fill = "Public Transport Job 
         Accessibility: Bus")+
    geom_sf(data=towns_centres_WYCA, shape = 21, fill = 'white', size = 1.5) +
    theme_void())

# tram contribution map
(PTJA_train <- WYCA_dataset_PTJA %>%
    dplyr::select(color_PTJA_train, geometry) %>%
    ggplot() +
    geom_sf(data=WYCA_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = WYCA_dataset_PTJA, aes(fill = color_PTJA_train), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = c("Lowest","","","","Highest")) +
    labs(fill = "Public Transport Job 
         Accessibility: Train") +
    theme_void())


# Adjusted for Demand Potential -------------------------------------------
# Calculate PTJA-D quantile breaks
breaks_PTJA_D <- classIntervals(WYCA_dataset_PTJA$PT_Job_Access_Index_Demand, 
                                n = 5, 
                                style = "quantile")$brks
breaks_PTJA_D[1] <- 00
# Create a factor variable for PTJA coloring
WYCA_dataset_PTJA$color_PTJA_D <- cut(WYCA_dataset_PTJA$PT_Job_Access_Index_Demand, 
                                       breaks = breaks_PTJA_D, 
                                       include.lowest = TRUE, 
                                       labels = FALSE) %>%
  as.factor()


labels_PTJA_D <- c("Lowest PTJA-D Quintile","","","","Highest PTJA-D Quintile")

# Plot PTJA by quintile in WYCA
(PTJA_D <- WYCA_dataset_PTJA %>%
    dplyr::select(color_PTJA_D, geometry) %>%
    ggplot() +
    geom_sf(data=WYCA_boundary, colour="black",linewidth=1.0) +
    geom_sf(data = WYCA_dataset_PTJA, aes(fill = color_PTJA_D), color = NA) + # color = NA removes the LSOA boundaries
    scale_fill_brewer(palette = "Spectral", 
                      direction = -1,
                      labels = labels_PTJA_D) +
    labs(fill = "PTJA-D
       (quintiles)",
         title = "Public Transport Job Accessibility (Demand-adjusted) in 
         West Yorkshire Combined Authority") +
    geom_sf(data=towns_centres_WYCA, shape = 21, fill = 'white', size = 1.5) +
    geom_label( x=-1.756, y=53.83, label="Bradford", size=3) +
    geom_label( x=-1.859, y=53.75, label="Halifax", size=3) +
    geom_label( x=-1.782, y=53.675, label="Huddersfield", size=3) +
    geom_label( x=-1.545, y=53.825, label="Leeds", size=3) +
    geom_label( x=-1.5058, y=53.70, label="Wakefield", size=3)+
    annotation_north_arrow( height = unit(1, "cm"),
                            width = unit(1, "cm")) +
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm")) +
    theme_void())

ggsave(file = "WYCA/Images/PTJA_D.jpeg", device = "jpeg", plot = PTJA_D)
