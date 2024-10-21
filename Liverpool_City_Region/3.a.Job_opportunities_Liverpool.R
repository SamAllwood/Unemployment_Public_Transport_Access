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

setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")

# 2. Load Data ------------------------------------------------------------

# Load Liverpool Geo data from file
LCR_dataset_jobs <- read_sf("Data/LCR_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
         "PT_Job_Access_Index" = "PT_Jb_A_I",
         "Employed_Population" = "Employd",
         "LSOA_Area" = "LSOA__2",
         "Traveltime_empcent" = "Tr__E_C")

# LCR Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=200)

# Town centres
towns_centroids_LCR <- read_sf("Data/towns_centroids_LCR.shp") %>% 
  st_transform(4326) %>%
  filter(buffer == "N") 

#Town boundaries
towns <- st_read("Data/Major_Towns_and_Cities_Dec_2015_Boundaries_V2_2022/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_LCR_buffer <- towns[st_within(towns, LCR_bound_small_buffer, sparse = FALSE), ]

# # Plot job locations levels across LCR -------------------------------------------
# create job density variable
LCR_dataset_jobs$job_density <- LCR_dataset_jobs$Employed_Population/LCR_dataset_jobs$LSOA_Area

# Calculate PTJA quantile breaks
breaks_jobs <- classIntervals(LCR_dataset_jobs$job_density, n = 5, style = "quantile")$brks

# Create a factor variable for PTJA coloring
LCR_dataset_jobs$job_density_colour <- cut(LCR_dataset_jobs$job_density, breaks = breaks_jobs, include.lowest = TRUE, labels = FALSE)
LCR_dataset_jobs$job_density_colour <- as.factor(LCR_dataset_jobs$job_density_colour)
labels_jobs <- c("Lowest quintile","","","","Highest quintile")
(jobs <- LCR_dataset_jobs %>%
  ggplot() +
  geom_sf(data=LCR_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = job_density_colour), col = NA) +
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_jobs) +
  labs(fill = "Job density (quintiles)")+
    geom_sf(data=towns_within_LCR_buffer, fill = NA, col = "black", linewidth = 0.3) +
  geom_sf(data=towns_centroids_LCR, shape = 21, fill = 'white', size = 1.5) +
    geom_label( x=-2.88, y=53.40, label="Liverpool", size=3) +
    geom_label( x=-2.73, y=53.43, label="St. Helens", size=3) +
    geom_label( x=-3.10, y=53.64, label="Southport", size=3) +
    geom_label( x=-3.12, y=53.39, label="Birkenhead", size=3) +
    annotation_north_arrow(  height = unit(1, "cm"),
                             width = unit(1, "cm")) +
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
  theme_void() )

ggsave(file = "3.Liverpool_maps/jobs_LCR.jpeg", device = "jpeg", plot = jobs)

# Create job locations variable as % of LCR jobs
LCR_dataset_jobs$Employed_Population_percent <- LCR_dataset_jobs$Employed_Population*100/sum(LCR_dataset_jobs$Employed_Population)
# plot as % of total jobs in LCR
LCR_dataset_jobs %>%
  filter(job_density<50000)%>%
  ggplot() +
  geom_sf(data=LCR_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = job_density), col = NA) +
  scale_fill_gradient(low="blue", high="green") +
  labs(fill = "Job density
       (jobs / sq.km)")+
  annotation_north_arrow()+
  annotation_scale(location = "bl",
                   pad_x = unit(2.5, "cm"),
                   pad_y = unit(0, "cm"))+
  theme_void()
