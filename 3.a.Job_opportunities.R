## Script for choropleth map of job opportunities in GMCA
# 1. Setup ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(tidytransit)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)
library(classInt)
library(ggspatial)

# 2. Load Data ------------------------------------------------------------
# Load Manchester Geo data from file
MANCH_dataset_jobs <- read_sf("Data/MANCH_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
         "TravelTime_Jobcentre" = "Tr__J_C",
         "PT_Job_Access_Index" = "PT_Jb_A_I",
         "Employed_Population" = "Employd",
         "LSOA_Area" = "LSOA__2",
         "Traveltime_empcent" = "Tr__E_C")

# GMCA Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=200)

# Town centres
towns_centroids <- read_sf("Data/towns_centroids.shp")
towns_centroids_Man <- towns_centroids %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

#Town boundaries
towns <- st_read("Data/Major_Towns_and_Cities_Dec_2015_Boundaries_V2_2022/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_GMCA_buffer <- towns[st_within(towns, GMCA_bound_small_buffer, sparse = FALSE), ]
#towns_centroids <- st_centroid(towns_within_GMCA_buffer) %>%
#  rename(id = TCITY15CD) %>% st_transform(4326)

# # Plot job locations levels across GMCA -------------------------------------------
# create job density variable
MANCH_dataset_jobs$job_density <- MANCH_dataset_jobs$Employed_Population/MANCH_dataset_jobs$LSOA_Area

# Calculate PTJA quantile breaks
breaks_jobs <- classIntervals(MANCH_dataset_jobs$job_density, n = 5, style = "quantile")$brks

# Create a factor variable for PTJA coloring
MANCH_dataset_jobs$job_density_colour <- cut(MANCH_dataset_jobs$job_density, 
                                             breaks = breaks_jobs, 
                                             include.lowest = TRUE, 
                                             labels = FALSE) %>%
                                          as.factor()
labels_jobs <- c("Lowest quintile","","","","Highest quintile")

(jobs <- MANCH_dataset_jobs %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = job_density_colour), col = NA) +
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_jobs) +
  labs(fill = "Job density (quintiles)")+
    geom_sf(data=towns_within_GMCA_buffer, fill = NA, col = "black", linewidth = 0.3) +
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

ggsave(file = "Plots/jobs.jpeg", device = "jpeg", plot = jobs)

# Create job locations variable as % of GMCA jobs
MANCH_dataset_jobs$Employed_Population_percent <- MANCH_dataset_jobs$Employed_Population*100/sum(MANCH_dataset_jobs$Employed_Population)

# plot as % of total jobs in GMCA
MANCH_dataset_jobs %>%
  filter(job_density<50000)%>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = job_density), col = NA) +
  scale_fill_gradient(low="blue", high="green") +
  labs(fill = "Job density
       (jobs / sq.km)")+
  annotation_north_arrow()+
  annotation_scale(location = "bl",
                   pad_x = unit(2.5, "cm"),
                   pad_y = unit(0, "cm"))+
  theme_void()
