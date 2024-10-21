## Script for map showing unemployment rate distribution across GMCA by LSOA
# 1. Setup ----------------------------------------------------------------

library(tidyverse)
library(knitr)
library(tidytransit)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)
library(classInt)
library(RColorBrewer)
library(terra)
library(ggspatial)

# 2. Load Data ------------------------------------------------------------
# Load Manchester Geo data from file
MANCH_dataset_Unemp <- read_sf("../Data/MANCH_dataset_full_sf.shp") %>%
  rename("LSOA_Code" = LSOA_Cd,
         "LSOA_Name" = LSOA_Nm,
         "LSOA_Area" = LSOA_Ar,
         "PT_Job_Access_Index" = PT_Jb_A_I,
         "Job_Locations" = Emply_P,
         "Traveltime_empcent" = Trvltm_,
         "Townsuburb" = twnsbrb,
         "Economically_active_residents" = Ecnmcl_,
         "Unemployed_pop" = Unmplyd,
         "Unemployment_rate" = Unmply_,
         "No_car_rate" = N_cr_rt,
         "White_percent" = "% White",
         "Single_parent_households" = Sng_P_H,
         "Single_parent_household_rate" = "%",
         "Urban_rural" = Urbn_Rr,
         "Pop_16" = Tt___16,
         "Low_qual" = Lw_Qlfd,
         "Total_residents" = Ttl_rsd
  ) %>%
  st_transform(4326)
sum(MANCH_dataset_Unemp$Economically_active_residents)

# GMCA Boundary + buffer
Boundaries <- read_sf("../Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=200)
# Create a 20km buffer around the GMCA boundary line
buffered_GMCA_boundary <- st_buffer(GMCA_boundary, dist = 20000) %>% 
  st_transform(4326)

# Read Towns and City boundaries
towns <- st_read("../Data/Major_Towns_and_Cities_Dec_2015_Boundaries_V2_2022/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_GMCA <- towns[st_within(towns, GMCA_bound_small_buffer, sparse = FALSE), ]

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("../Data/LAD_Dec_2021_GB_BFC_2022/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within GMCA
LADs_MANCH <- LADs %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

# Load Metrolink Shapefile
Metrolink <- st_read("../Data/GM_Metrolink_MapData/SHP-format/Metrolink_Lines_Functional.shp")
Metrolink$LineName <- "Metrolink"
# town centroids
towns_centroids <- read_sf("../Data/towns_centroids.shp") #manually updated in 1. TravelTimeMatrix.R
towns_centroids_Man <- towns_centroids %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

## Plot Unemployment rate in GMCA ----------------------------------------
# Plot Unemployment without quantile breaks
MANCH_dataset_Unemp %>%
  filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5) +
  geom_sf(aes(fill = Unemployment_rate), color = NA) +
  scale_fill_distiller(palette = "Spectral") +
  labs(fill="Unemployment Rate")+
  geom_sf(data=LADs_MANCH, fill = NA, col = "red", size = 1) +
  theme_void()

# Calculate quantile breaks
breaks_unemp <- classIntervals(MANCH_dataset_Unemp$Unemployment_rate, n = 5, style = "quantile")$brks
# Create a factor variable for coloring
MANCH_dataset_Unemp$color <- cut(MANCH_dataset_Unemp$Unemployment_rate, breaks = breaks_unemp, include.lowest = TRUE, labels = FALSE)
MANCH_dataset_Unemp$color <- as.factor(MANCH_dataset_Unemp$color)
labels <- c("<=3.1%",
            "3.2% - 4.2%",
            "4.3% - 5.8%",
            "5.9% - 8.1%",
            ">=8.2%")

# Plot Unemployment Quintiles
(Unemp_rate <- MANCH_dataset_Unemp %>%
    select (color, Unemployment_rate) %>%
    ggplot() +
      geom_sf(data=GMCA_boundary, colour="black",linewidth=1.0) +
      geom_sf(data=LADs_MANCH, fill=NA, color = "black", size = 1) +
      geom_sf(aes(fill = color), color = NA) + # 'color = NA' removes the LSOA boundaries
      scale_fill_brewer(palette = "Spectral", 
                        direction = -1,
                        labels = labels) +
      labs(fill = "Unemployment 
           rate (quintiles)") +
      geom_sf(data=towns_within_GMCA, fill = NA, col = "black", linewidth = 0.3) +
      geom_sf(data=towns_centroids_Man, shape = 21, fill = 'white', size = 1.5) +
      geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
      geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
      geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
      geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
      geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
      geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
      geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
      geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
    annotation_north_arrow()+
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
            theme_void() )

ggsave(file = "Plots/Unemp_rate.jpeg", device = "jpeg", plot = Unemp_rate)

# Local Authority District (LAD) map
(LADs <- MANCH_dataset_Unemp %>%
    ggplot() +
    geom_sf(data=LADs_MANCH, aes(fill = LAD21NM), color = "black", size = 1) +
    scale_fill_brewer(palette = "Set1", name = "LAD Boundaries") + # Customize legend for LAD boundaries
  theme_void() )
    
# Manchester City Centre unemployment
MANCH_city_unemp <- MANCH_dataset_Unemp %>% filter(TCITY15=="Manchester")
# Calculate quantile breaks
breaks_unemp_Man <- classIntervals(MANCH_city_unemp$Unemployment_rate, n = 5, style = "quantile")$brks
# Create a factor variable for coloring
MANCH_city_unemp$color <- cut(MANCH_city_unemp$Unemployment_rate, breaks = breaks_unemp_Man, include.lowest = TRUE, labels = FALSE)
MANCH_city_unemp$color <- as.factor(MANCH_city_unemp$color)
labels_Man_unemp <- c("=<4.4%",
            "4.5% - 6.4%",
            "6.5% - 8.1%",
            "8.2% - 10.4%",
            "=>10.5%")
MAN_city_boundary <- towns_within_GMCA %>% filter(TCITY15NM=="Manchester")

(Unemp_rate_Manch_city <- MANCH_city_unemp %>%
  select (color, Unemployment_rate) %>%
  ggplot() +
#  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.0) +
#  geom_sf(data=LADs_MANCH, fill=NA, color = "black", size = 1) +
  geom_sf(aes(fill = color), color = NA) + # 'color = NA' removes the LSOA boundaries
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_Man_unemp) +
  labs(fill = "Unemployment 
       rate (quintiles)") +
  geom_sf(data=MAN_city_boundary, fill = NA, col = "black", linewidth = 0.2) +
  annotation_north_arrow()+
    annotation_scale(location = "tr")+
    theme_void())
ggsave(file = "Plots/Unemp_rate_Man_city.jpeg", device = "jpeg", plot = Unemp_rate_Manch_city)
