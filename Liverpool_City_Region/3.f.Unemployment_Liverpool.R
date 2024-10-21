
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
setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")
# 2. Load Data ------------------------------------------------------------

# Load Liverpool Geo data from file
LCR_dataset_Unemp <- read_sf("Data/LCR_dataset_full_sf.shp") %>%
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
sum(LCR_dataset_Unemp$Economically_active_residents)
# LCR Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=200)
# Create a 20km buffer around the LCR boundary line
buffered_LCR_boundary <- st_buffer(LCR_boundary, dist = 20000) %>% 
  st_transform(4326)

# Read Towns and City boundaries
towns <- st_read("Data/Major_Towns_and_Cities_Dec_2015_Boundaries_V2_2022/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_LCR <- towns[st_within(towns, LCR_bound_small_buffer, sparse = FALSE), ]

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("Data/LAD_Dec_2021_GB_BFC_2022/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within LCR
LADs_LCR <- LADs %>% filter(as.vector(st_within(., LCR_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

# town centroids
towns_centroids_LCR <- read_sf("Data/towns_centroids_LCR.shp") #manually updated in 1. TravelTimeMatrix.R

## Plot Unemployment rate in LCR ----------------------------------------
# Plot Unemployment without quantile breaks
LCR_dataset_Unemp %>%
  filter(as.vector(st_within(., LCR_bound_small_buffer, sparse = FALSE))) %>% 
  ggplot() +
  geom_sf(data=LCR_boundary, colour="black",linewidth=1.5) +
  geom_sf(aes(fill = Unemployment_rate), color = NA) +
  scale_fill_distiller(palette = "Spectral") +
  labs(fill="Unemployment Rate")+
  geom_sf(data=LADs_LCR, fill = NA, col = "red", linewidth = 0.5) +
  theme_void()

# Calculate quantile breaks
breaks_unemp <- classIntervals(LCR_dataset_Unemp$Unemployment_rate, n = 5, style = "quantile")$brks
# Create a factor variable for coloring
LCR_dataset_Unemp$color <- cut(LCR_dataset_Unemp$Unemployment_rate, breaks = breaks_unemp, include.lowest = TRUE, labels = FALSE)
LCR_dataset_Unemp$color <- as.factor(LCR_dataset_Unemp$color)
labels <- c("<=3.2%",
            "3.2% - 4.2%",
            "4.2% - 5.6%",
            "5.6% - 7.7%",
            ">=7.7%")

# Plot Unemployment Quintiles
(Unemp_rate <- LCR_dataset_Unemp %>%
    select (color, Unemployment_rate) %>%
    ggplot() +
      geom_sf(data=LCR_boundary, colour="black",linewidth=1.0) +
      geom_sf(data=LADs_LCR, fill=NA, color = "black", size = 1) +
      geom_sf(aes(fill = color), color = NA) + # 'color = NA' removes the LSOA boundaries
      scale_fill_brewer(palette = "Spectral", 
                        direction = -1,
                        labels = labels) +
      labs(fill = "Unemployment 
           rate (quintiles)") +
      geom_sf(data=towns_within_LCR, fill = NA, col = "black", linewidth = 0.3) +
      geom_sf(data=towns_centroids_LCR, shape = 21, fill = 'white', size = 1.5) +
 #     geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
#      geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
#      geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
#      geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
#      geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
#      geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
#      geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
#      geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
    annotation_north_arrow(height = unit(1, "cm"),
                           width = unit(1, "cm")) +
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
            theme_void() )

ggsave(file = "3.Liverpool_Maps/Unemp_rate_LCR.jpeg", device = "jpeg", plot = Unemp_rate)

# Local Authority District (LAD) map
(LADs <- LCR_dataset_Unemp %>%
    ggplot() +
    geom_sf(data=LADs_LCR, aes(fill = LAD21NM), color = "black", size = 1) +
    scale_fill_brewer(palette = "Set1", name = "LAD Boundaries") + # Customize legend for LAD boundaries
  theme_void() )
    
# Liverpool City Centre unemployment
LCR_city_unemp <- LCR_dataset_Unemp %>% filter(TCITY15=="Liverpool")
# Calculate quantile breaks
breaks_unemp_Liv <- classIntervals(LCR_city_unemp$Unemployment_rate, n = 5, style = "quantile")$brks
# Create a factor variable for coloring
LCR_city_unemp$color <- cut(LCR_city_unemp$Unemployment_rate, 
                            breaks = breaks_unemp_Liv, 
                            include.lowest = TRUE, 
                            labels = FALSE) %>% 
  as.factor()
labels_Liv_unemp <- c("=<3.8%",
            "3.8% - 5.7%",
            "5.7% - 7.2%",
            "7.2% - 9.3%",
            "=>9.3%")
LCR_city_boundary <- towns_within_LCR %>% filter(TCITY15NM=="Liverpool")

(Unemp_rate_LCR_city <- LCR_city_unemp %>%
  select (color, Unemployment_rate) %>%
  ggplot() +
  geom_sf(aes(fill = color), color = NA) + # 'color = NA' removes the LSOA boundaries
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_Liv_unemp) +
  labs(fill = "Unemployment 
       rate (quintiles)") +
  geom_sf(data=LCR_city_boundary, fill = NA, col = "black", linewidth = 0.2) +
  annotation_north_arrow()+
    annotation_scale(location = "tr")+
    theme_void())

ggsave(file = "Plots/Unemp_rate_Liv_city.jpeg", device = "jpeg", plot = Unemp_rate_LCR_city)
